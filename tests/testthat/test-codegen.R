test_that("r_literal quotes and escapes correctly", {
  expect_equal(r_literal("AE"), "\"AE\"")
  expect_equal(r_literal(c("A", "B")), "c(\"A\", \"B\")")
  expect_equal(r_literal(42), "42")
  expect_equal(r_literal(c(1, 2, 3)), "c(1, 2, 3)")
  expect_equal(r_literal(TRUE), "TRUE")
  expect_equal(r_literal(c(TRUE, FALSE)), "c(TRUE, FALSE)")
  # embedded quote is escaped so the result is still valid R
  expect_equal(r_literal('say "hi"'), "\"say \\\"hi\\\"\"")
  expect_equal(r_literal(character(0)), "character(0)")
  # large numbers are not written in scientific notation
  expect_equal(r_literal(1e6), "1000000")
})

test_that("r_literal output always parses back to the original value", {
  for (v in list("AE", c("A", "B"), 42, c(1.5, 2.5), TRUE, 1e6)) {
    expect_equal(eval(parse(text = r_literal(v))), v)
  }
})

test_that("r_literal preserves full numeric precision (no 7-digit rounding)", {
  for (v in list(pi, exp(1), 0.1, 0.123456789012345, 1 / 3, 1e-8, c(pi, 1 / 7))) {
    expect_identical(eval(parse(text = r_literal(v))), v)
  }
  # readable where it can be
  expect_equal(r_literal(0.1), "0.1")
  expect_equal(r_literal(42), "42")
})

test_that("backtick_if_needed only quotes non-syntactic names", {
  expect_equal(backtick_if_needed("USUBJID"), "USUBJID")
  expect_equal(backtick_if_needed("AGE"), "AGE")
  expect_equal(backtick_if_needed("my col"), "`my col`")
})

test_that("format_filter_condition handles each operator", {
  expect_equal(format_filter_condition(list(var = "AGE", op = ">", value = 30)),
               "AGE > 30")
  expect_equal(format_filter_condition(list(var = "SEX", op = "==", value = "F")),
               "SEX == \"F\"")
  expect_equal(format_filter_condition(list(var = "ARM", op = "%in%", value = c("A", "B"))),
               "ARM %in% c(\"A\", \"B\")")
  expect_equal(format_filter_condition(list(var = "AGE", op = "between", value = c(18, 65))),
               "dplyr::between(AGE, 18, 65)")
  expect_equal(format_filter_condition(list(var = "AEDECOD", op = "is.na")),
               "is.na(AEDECOD)")
  expect_equal(format_filter_condition(list(var = "AEDECOD", op = "not.na")),
               "!is.na(AEDECOD)")
  expect_equal(format_filter_condition(list(var = "AETERM", op = "contains", value = "head")),
               "stringr::str_detect(AETERM, \"head\")")
})

test_that("format_filter_condition is robust to bad / incomplete specs", {
  expect_equal(format_filter_condition(NULL), "")
  expect_equal(format_filter_condition(list(var = "")), "")
  expect_equal(format_filter_condition(list(var = "AGE", op = "between", value = 18)), "")
  expect_equal(format_filter_condition(list(var = "AGE", op = "bogus", value = 1)), "")
})

test_that("operator aliases normalise", {
  expect_equal(format_filter_condition(list(var = "X", op = "isna")), "is.na(X)")
  expect_equal(format_filter_condition(list(var = "X", op = "notna")), "!is.na(X)")
  expect_equal(format_filter_condition(list(var = "X", op = "in", value = 1)),
               "X %in% 1")
})

test_that("build_dplyr_expr returns bare name when nothing applied", {
  expect_equal(build_dplyr_expr("data"), "data")
  expect_equal(build_dplyr_expr("data", filters = list(), cols = NULL), "data")
})

test_that("build_dplyr_expr composes filter and select", {
  out <- build_dplyr_expr(
    "data",
    filters = list(list(var = "AGE", op = ">", value = 30)),
    cols = c("USUBJID", "AGE")
  )
  expect_true(grepl("dplyr::filter(AGE > 30)", out, fixed = TRUE))
  expect_true(grepl("dplyr::select(USUBJID, AGE)", out, fixed = TRUE))
  # produced code is syntactically valid
  expect_silent(parse(text = out))
})

test_that("build_load_expr emits a valid, parseable assignment", {
  out <- build_load_expr("path/to/dm.xpt")
  expect_equal(out, "data <- read_sdtm_adam(\"path/to/dm.xpt\")")
  expect_silent(parse(text = out))
})

test_that("build_full_script assembles a runnable, parseable script", {
  load_expr <- build_load_expr("dm.xpt")
  transform_expr <- build_dplyr_expr(
    "data",
    filters = list(list(var = "AGE", op = ">=", value = 18)),
    cols = c("USUBJID", "AGE")
  )
  script <- build_full_script(load_expr, transform_expr, plot_expr = "plot(data$AGE)")
  expect_true(grepl("library(databoxR)", script, fixed = TRUE))
  expect_true(grepl("# Load dataset", script, fixed = TRUE))
  expect_true(grepl("# Apply filters and column selection", script, fixed = TRUE))
  expect_true(grepl("# Visualize", script, fixed = TRUE))
  expect_silent(parse(text = script))
})

test_that("build_full_script omits transform block when nothing is transformed", {
  script <- build_full_script(build_load_expr("dm.xpt"), build_dplyr_expr("data"))
  expect_false(grepl("Apply filters", script, fixed = TRUE))
  expect_silent(parse(text = script))
})

test_that("generated dplyr pipeline round-trips against a manual dplyr call", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    USUBJID = paste0("S", 1:6),
    AGE = c(17, 25, 40, 65, 70, 30),
    SEX = c("F", "M", "F", "M", "F", "M"),
    stringsAsFactors = FALSE
  )

  code <- build_dplyr_expr(
    "df",
    filters = list(
      list(var = "AGE", op = "between", value = c(18, 65)),
      list(var = "SEX", op = "==", value = "F")
    ),
    cols = c("USUBJID", "AGE")
  )

  generated <- eval(parse(text = code))
  manual <- dplyr::select(
    dplyr::filter(df, dplyr::between(AGE, 18, 65), SEX == "F"),
    USUBJID, AGE
  )

  expect_equal(generated, manual)
})
