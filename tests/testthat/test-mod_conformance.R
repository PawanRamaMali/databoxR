ae <- data.frame(
  DOMAIN = "AE", USUBJID = c("S1-001", "S1-002"),
  AETERM = c("Headache", "Rash"), AESEV = c("MILD", "MODERATE"),
  stringsAsFactors = FALSE
)
dm <- data.frame(
  DOMAIN = "DM", USUBJID = c("S1-001", "S1-002"),
  AGE = c(40, 50), stringsAsFactors = FALSE
)
datasets <- list(AE = ae, DM = dm)

# Injected define API, so the module runs without metacore.
fake_mc <- structure(list(tag = "MC"), class = "FakeMetacore")
load_fn <- function(path) fake_mc
ds_fn <- function(mc) data.frame(
  dataset = c("DM", "AE"), label = c("Demographics", "Adverse Events"),
  stringsAsFactors = FALSE)
check_fn <- function(data, mc, dataset) data.frame(
  Check = "Missing variable", Variable = "AEDECOD", Severity = "Warning",
  Details = "Expected by define but absent from data", stringsAsFactors = FALSE)
cmp_fn <- function(data, mc, dataset) data.frame(
  variable = c("AETERM", "AEDECOD"), in_define = c(TRUE, TRUE),
  in_data = c(TRUE, FALSE), stringsAsFactors = FALSE)

make_xml <- function() {
  p <- tempfile(fileext = ".xml")
  writeLines("<ODM/>", p)
  p
}

test_that("define_ready is false until a define parses", {
  shiny::testServer(
    mod_conformance_server,
    args = list(datasets = shiny::reactive(datasets), load_fn = load_fn,
                check_fn = check_fn, ds_fn = ds_fn, cmp_fn = cmp_fn),
    {
      expect_false(mc_ok())
      session$setInputs(define = list(datapath = make_xml(), name = "define.xml"))
      expect_true(mc_ok())
      expect_equal(define_ds_names(), c("DM", "AE"))
    }
  )
})

test_that("findings run against the picked datasets", {
  shiny::testServer(
    mod_conformance_server,
    args = list(datasets = shiny::reactive(datasets), load_fn = load_fn,
                check_fn = check_fn, ds_fn = ds_fn, cmp_fn = cmp_fn),
    {
      session$setInputs(define = list(datapath = make_xml(), name = "define.xml"))
      session$setInputs(data_ds = "AE", def_ds = "AE")
      f <- findings()
      expect_equal(nrow(f), 1)
      expect_equal(f$Severity, "Warning")
      expect_equal(f$Variable, "AEDECOD")
    }
  )
})

test_that("a parse failure is surfaced, not fatal", {
  bad_loader <- function(path) stop("not a valid define")
  shiny::testServer(
    mod_conformance_server,
    args = list(datasets = shiny::reactive(datasets), load_fn = bad_loader,
                check_fn = check_fn, ds_fn = ds_fn, cmp_fn = cmp_fn),
    {
      session$setInputs(define = list(datapath = make_xml(), name = "define.xml"))
      expect_false(mc_ok())
      expect_s3_class(mc(), "conf_error")
    }
  )
})

test_that("check failures become an Error finding rather than crashing", {
  boom <- function(data, mc, dataset) stop("check exploded")
  shiny::testServer(
    mod_conformance_server,
    args = list(datasets = shiny::reactive(datasets), load_fn = load_fn,
                check_fn = boom, ds_fn = ds_fn, cmp_fn = cmp_fn),
    {
      session$setInputs(define = list(datapath = make_xml(), name = "define.xml"))
      session$setInputs(data_ds = "AE", def_ds = "AE")
      f <- findings()
      expect_equal(f$Severity, "Error")
      expect_match(f$Details, "check exploded")
    }
  )
})
