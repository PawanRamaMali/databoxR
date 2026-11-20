# These tests rely on metacore (Suggests) and its bundled define.xml.
skip_if_no_define <- function() {
  skip_if_not_installed("metacore")
}

define_path <- function() {
  system.file("extdata", "ADaM_define_CDISC_pilot3.xml", package = "metacore")
}

test_that("load_define validates its input", {
  expect_error(load_define(NULL), "Path parameter is required")
  expect_error(load_define(""), "Path parameter is required")
  expect_error(load_define("no_such_file.xml"), "File does not exist")
  tmp <- tempfile(fileext = ".txt"); file.create(tmp); on.exit(unlink(tmp))
  expect_error(load_define(tmp), "define.xml file")
})

test_that("load_define parses a real define.xml into a Metacore object", {
  skip_if_no_define()
  mc <- load_define(define_path())
  expect_true(inherits(mc, "Metacore"))
})

test_that("define_datasets returns dataset-level metadata", {
  skip_if_no_define()
  mc <- load_define(define_path())
  ds <- define_datasets(mc)
  expect_s3_class(ds, "data.frame")
  expect_true(all(c("dataset", "label") %in% names(ds)))
  expect_true("ADSL" %in% ds$dataset)
})

test_that("define_var_metadata builds a per-dataset data dictionary", {
  skip_if_no_define()
  mc <- load_define(define_path())

  all_meta <- define_var_metadata(mc)
  expect_s3_class(all_meta, "data.frame")
  expect_true(all(c("dataset", "variable", "label", "type") %in% names(all_meta)))
  expect_true(length(unique(all_meta$dataset)) > 1)

  adsl <- define_var_metadata(mc, dataset = "ADSL")
  expect_true(all(adsl$dataset == "ADSL"))
  expect_true(all(c("STUDYID", "USUBJID") %in% adsl$variable))
  # STUDYID has a known label in the pilot define
  expect_equal(adsl$label[adsl$variable == "STUDYID"], "Study Identifier")
})

test_that("describe_r_type maps R classes to define-like types", {
  expect_equal(describe_r_type(1:5), "numeric")
  expect_equal(describe_r_type(c(1.5, 2.5)), "numeric")
  expect_equal(describe_r_type(letters), "text")
  expect_equal(describe_r_type(c(TRUE, FALSE)), "logical")
  expect_equal(describe_r_type(as.Date("2020-01-01")), "date")
})

test_that("compare_define_to_data surfaces matches, mismatches and gaps", {
  skip_if_no_define()
  mc <- load_define(define_path())

  # Build a small fake ADSL: one correct label, one wrong label,
  # one extra variable not in the define.
  adsl_meta <- define_var_metadata(mc, "ADSL")
  studyid_label <- adsl_meta$label[adsl_meta$variable == "STUDYID"]

  data <- data.frame(STUDYID = "X", USUBJID = "X-001", EXTRA = 1,
                     stringsAsFactors = FALSE)
  attr(data$STUDYID, "label") <- studyid_label      # matches define
  attr(data$USUBJID, "label") <- "Wrong Label Here" # mismatch

  cmp <- compare_define_to_data(data, mc, "ADSL")
  expect_s3_class(cmp, "data.frame")

  # STUDYID: in both, label matches
  row_sid <- cmp[cmp$variable == "STUDYID", ]
  expect_true(row_sid$in_define && row_sid$in_data)
  expect_true(isTRUE(row_sid$label_match))

  # USUBJID: in both, label mismatch
  row_uid <- cmp[cmp$variable == "USUBJID", ]
  expect_false(isTRUE(row_uid$label_match))

  # EXTRA: in data, not in define
  row_extra <- cmp[cmp$variable == "EXTRA", ]
  expect_true(row_extra$in_data)
  expect_false(row_extra$in_define)

  # A define variable absent from the tiny data frame is flagged in_data = FALSE
  expect_true(any(!cmp$in_data & cmp$in_define))
})
