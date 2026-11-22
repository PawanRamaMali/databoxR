ae <- data.frame(
  DOMAIN = "AE",
  USUBJID = c("S1-001", "S1-001", "S1-002"),
  AETERM = c("Headache", "Nausea", "Rash"),
  AESEV = c("MILD", "SEVERE", "MODERATE"),
  AESER = c("N", "Y", "N"),
  stringsAsFactors = FALSE
)
datasets <- list(AE = ae)

test_that("ask_plan_summary describes filter and subject plans", {
  s1 <- ask_plan_summary(list(action = "filter", domain = "AE", logic = "and",
    filters = list(list(column = "AESEV", op = "eq", value = "SEVERE")),
    columns = c("USUBJID", "AETERM")))
  expect_true(grepl("filter AE where AESEV eq SEVERE", s1))
  expect_true(grepl("keep USUBJID, AETERM", s1))

  s2 <- ask_plan_summary(list(action = "subject_timeline", subject = "S1-001"))
  expect_true(grepl("subject_timeline for subject S1-001", s2))

  expect_equal(ask_plan_summary(NULL), "")
})

test_that("mod_ask_server answers through an injected planner", {
  stub <- function(question, schema, ...) {
    list(action = "filter", domain = "AE",
         filters = list(list(column = "AESEV", op = "eq", value = "SEVERE")),
         columns = c("USUBJID", "AETERM"))
  }
  shiny::testServer(
    mod_ask_server,
    args = list(datasets = shiny::reactive(datasets), propose = stub),
    {
      session$setInputs(question = "show severe adverse events", ask = 1)
      a <- answer()
      expect_null(a$error)
      expect_equal(nrow(a$result), 1)
      expect_equal(a$result$AETERM, "Nausea")
      expect_true(grepl("filter", a$code, fixed = TRUE))
    }
  )
})

test_that("mod_ask_server reports when no data is loaded", {
  stub <- function(question, schema, ...) list(action = "filter", domain = "AE")
  shiny::testServer(
    mod_ask_server,
    args = list(datasets = shiny::reactive(NULL), propose = stub),
    {
      session$setInputs(question = "anything", ask = 1)
      expect_match(answer()$error, "Load one or more datasets")
    }
  )
})
