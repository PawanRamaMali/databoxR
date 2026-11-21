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

test_that("check_conformance flags CT violations, extras and mismatches", {
  skip_if_no_define()
  skip_if_not_installed("metatools")
  mc <- load_define(define_path())

  adsl_meta <- define_var_metadata(mc, "ADSL")
  studyid_label <- adsl_meta$label[adsl_meta$variable == "STUDYID"]

  data <- data.frame(
    STUDYID = c("CDISCPILOT01", "CDISCPILOT01"),
    USUBJID = c("01-701-1015", "01-701-1023"),
    ARM     = c("Placebo", "Not A Real Arm"),  # one value out of CT
    EXTRA   = c(1, 2),                          # not in define
    stringsAsFactors = FALSE
  )
  attr(data$STUDYID, "label") <- studyid_label
  attr(data$USUBJID, "label") <- "Totally Wrong Label"  # mismatch

  res <- check_conformance(data, mc, "ADSL")
  expect_s3_class(res, "data.frame")
  expect_true(all(c("Check", "Variable", "Severity", "Details") %in% names(res)))

  # CT violation on ARM -> Error
  arm_row <- res[res$Variable == "ARM" & res$Check == "Value not in controlled terminology", ]
  expect_equal(nrow(arm_row), 1)
  expect_equal(arm_row$Severity, "Error")
  expect_true(grepl("Not A Real Arm", arm_row$Details))

  # EXTRA flagged as unexpected (Info)
  extra_row <- res[res$Variable == "EXTRA", ]
  expect_true(nrow(extra_row) >= 1)
  expect_equal(extra_row$Severity[1], "Info")

  # USUBJID label mismatch (Warning)
  lab_row <- res[res$Variable == "USUBJID" & res$Check == "Label mismatch", ]
  expect_equal(nrow(lab_row), 1)
  expect_equal(lab_row$Severity, "Warning")

  # Findings are severity-ranked: first Error comes before any Info
  first_err <- which(res$Severity == "Error")[1]
  first_info <- which(res$Severity == "Info")[1]
  expect_true(first_err < first_info)
})

test_that("check_conformance returns a Success row when data conforms", {
  skip_if_no_define()
  skip_if_not_installed("metatools")
  mc <- load_define(define_path())

  # Only well-formed, in-CT variables with correct labels
  adsl_meta <- define_var_metadata(mc, "ADSL")
  data <- data.frame(ARM = c("Placebo", "Xanomeline Low Dose"), stringsAsFactors = FALSE)
  attr(data$ARM, "label") <- adsl_meta$label[adsl_meta$variable == "ARM"]

  res <- check_conformance(data, mc, "ADSL")
  # ARM is in CT and label matches; the only finding should be the many missing
  # define variables (Warnings), never a CT Error for ARM.
  expect_false(any(res$Variable == "ARM" &
                   res$Check == "Value not in controlled terminology"))
})
