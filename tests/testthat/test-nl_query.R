# Fixtures
dm <- data.frame(
  DOMAIN = "DM",
  USUBJID = c("S1-001", "S1-002"),
  AGE = c(45, 70), SEX = c("F", "M"),
  ARM = c("Treatment", "Placebo"),
  RFSTDTC = c("2023-03-01", "2023-02-01"),
  stringsAsFactors = FALSE
)
ae <- data.frame(
  DOMAIN = "AE",
  USUBJID = c("S1-001", "S1-001", "S1-002"),
  AETERM = c("Headache", "Nausea", "Rash"),
  AESEV = c("MILD", "SEVERE", "MODERATE"),
  AESER = c("N", "Y", "N"),
  AESTDTC = c("2023-03-10", "2023-04-01", "2023-02-15"),
  stringsAsFactors = FALSE
)
vs <- data.frame(
  DOMAIN = "VS", USUBJID = c("S1-001", "S1-002"),
  VISITNUM = c(1, 1), VSDTC = c("2023-03-05", "2023-02-10"),
  stringsAsFactors = FALSE
)
datasets <- list(DM = dm, AE = ae, VS = vs)

test_that("nlq_schema lists domains, columns and small categoricals, no rows", {
  s <- nlq_schema(datasets)
  expect_setequal(names(s$domains), c("DM", "AE", "VS"))
  expect_equal(s$subjects, c("S1-001", "S1-002"))
  ae_tab <- s$domains$AE
  expect_true(all(c("column", "type", "values") %in% names(ae_tab)))
  sev <- ae_tab[ae_tab$column == "AESEV", ]
  expect_true(grepl("SEVERE", sev$values))
  # numeric columns do not list value levels
  age <- s$domains$DM[s$domains$DM$column == "AGE", ]
  expect_equal(age$type, "numeric")
  expect_equal(age$values, "")
  expect_equal(nlq_schema(list())$domains, list())
})

test_that("nlq_compare handles each operator, case and numeric coercion", {
  col <- c("MILD", "SEVERE", "MODERATE")
  expect_equal(nlq_compare(col, "eq", "severe"), c(FALSE, TRUE, FALSE))
  expect_equal(nlq_compare(col, "ne", "MILD"), c(FALSE, TRUE, TRUE))
  expect_equal(nlq_compare(col, "contains", "mod"), c(FALSE, FALSE, TRUE))
  expect_equal(nlq_compare(col, "in", "mild, severe"), c(TRUE, TRUE, FALSE))
  age <- c(45, 70, NA)
  expect_equal(nlq_compare(age, "gt", "65"), c(FALSE, TRUE, FALSE))
  expect_equal(nlq_compare(age, "le", "45"), c(TRUE, FALSE, FALSE))
  # NA never matches
  expect_false(nlq_compare(age, "eq", "999")[3])
})

test_that("nlq_validate_plan catches bad domain, column and operator", {
  ok <- nlq_validate_plan(
    list(action = "filter", domain = "AE",
         filters = list(list(column = "AESEV", op = "eq", value = "SEVERE"))),
    datasets)
  expect_true(ok$ok)

  bad_dom <- nlq_validate_plan(list(action = "filter", domain = "ZZ"), datasets)
  expect_false(bad_dom$ok)

  bad_col <- nlq_validate_plan(
    list(action = "filter", domain = "AE",
         filters = list(list(column = "NOPE", op = "eq", value = "x"))),
    datasets)
  expect_false(bad_col$ok)

  bad_op <- nlq_validate_plan(
    list(action = "filter", domain = "AE",
         filters = list(list(column = "AESEV", op = "regex", value = "x"))),
    datasets)
  expect_false(bad_op$ok)
})

test_that("nlq_validate_plan checks subject actions", {
  expect_true(nlq_validate_plan(
    list(action = "subject_timeline", subject = "S1-001"), datasets)$ok)
  # missing subject is an error
  expect_false(nlq_validate_plan(list(action = "subject_kpis"), datasets)$ok)
  # unknown subject is a warning, not an error
  v <- nlq_validate_plan(
    list(action = "subject_overview", subject = "NOPE"), datasets)
  expect_true(v$ok)
  expect_length(v$warnings, 1)
})

test_that("nlq_execute_plan filters and selects deterministically", {
  res <- nlq_execute_plan(
    list(action = "filter", domain = "AE",
         filters = list(list(column = "AESEV", op = "eq", value = "SEVERE")),
         columns = c("USUBJID", "AETERM")),
    datasets)
  expect_equal(nrow(res), 1)
  expect_equal(res$AETERM, "Nausea")
  expect_setequal(names(res), c("USUBJID", "AETERM"))

  # AND logic across two filters
  res2 <- nlq_execute_plan(
    list(action = "filter", domain = "AE", logic = "and",
         filters = list(list(column = "AESER", op = "eq", value = "N"),
                        list(column = "AESEV", op = "eq", value = "MILD"))),
    datasets)
  expect_equal(res2$AETERM, "Headache")

  # numeric comparison on DM
  res3 <- nlq_execute_plan(
    list(action = "filter", domain = "DM",
         filters = list(list(column = "AGE", op = "gt", value = "65"))),
    datasets)
  expect_equal(res3$USUBJID, "S1-002")
})

test_that("nlq_execute_plan dispatches subject actions to subject_* helpers", {
  tl <- nlq_execute_plan(list(action = "subject_timeline", subject = "S1-001"),
                         datasets)
  expect_true(all(c("Date", "Domain", "Event") %in% names(tl)))

  k <- nlq_execute_plan(list(action = "subject_kpis", subject = "S1-001"),
                        datasets)
  expect_true(all(c("Metric", "Value") %in% names(k)))
  expect_equal(k$Value[k$Metric == "n_ae"], "2")

  expect_error(
    nlq_execute_plan(list(action = "filter", domain = "ZZ"), datasets),
    "Invalid query plan")
})

test_that("nlq_plan_code emits runnable code for both action kinds", {
  code <- nlq_plan_code(
    list(action = "filter", domain = "AE", logic = "and",
         filters = list(list(column = "AESEV", op = "eq", value = "SEVERE"),
                        list(column = "AGE", op = "gt", value = "65")),
         columns = c("USUBJID", "AETERM")))
  expect_true(grepl("library(dplyr)", code, fixed = TRUE))
  expect_true(grepl("AESEV == \"SEVERE\"", code, fixed = TRUE))
  expect_true(grepl("AGE > 65", code, fixed = TRUE))
  expect_true(grepl("select(USUBJID, AETERM)", code, fixed = TRUE))
  expect_silent(parse(text = code))

  scode <- nlq_plan_code(list(action = "subject_timeline", subject = "S1-001"))
  expect_true(grepl("subject_timeline(datasets, \"S1-001\")", scode, fixed = TRUE))
  expect_silent(parse(text = scode))
})

test_that("nl_query runs end to end with an injected planner (no network)", {
  stub <- function(question, schema, ...) {
    list(action = "filter", domain = "AE",
         filters = list(list(column = "AESEV", op = "eq", value = "SEVERE")))
  }
  out <- nl_query("show me severe adverse events", datasets, propose = stub)
  expect_null(out$error)
  expect_true(out$valid$ok)
  expect_equal(nrow(out$result), 1)
  expect_true(grepl("filter", out$code, fixed = TRUE))

  # an invalid plan from the planner surfaces as an error, not a crash
  bad <- function(question, schema, ...) list(action = "filter", domain = "ZZ")
  out2 <- nl_query("junk", datasets, propose = bad)
  expect_false(is.null(out2$error))
  expect_null(out2$result)

  # a planner that errors is caught
  boom <- function(question, schema, ...) stop("api down")
  out3 <- nl_query("x", datasets, propose = boom)
  expect_match(out3$error, "api down")
})
