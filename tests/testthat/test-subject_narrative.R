dm <- data.frame(
  DOMAIN = "DM", USUBJID = c("S1-001", "S1-002"),
  AGE = c(54, 61), SEX = c("F", "M"), ARM = c("Drug A", "Placebo"),
  SITEID = c("01", "02"), RFSTDTC = c("2021-01-05", "2021-02-01"),
  stringsAsFactors = FALSE
)
ae <- data.frame(
  DOMAIN = "AE", USUBJID = c("S1-001", "S1-001", "S1-002"),
  AETERM = c("Headache", "Nausea", "Rash"),
  AESEV = c("MILD", "SEVERE", "MODERATE"),
  AESER = c("N", "Y", "N"),
  AESTDTC = c("2021-01-10", "2021-01-20", "2021-02-05"),
  stringsAsFactors = FALSE
)
vs <- data.frame(
  DOMAIN = "VS", USUBJID = c("S1-001", "S1-001"),
  VISITNUM = c(1, 2), VSTESTCD = c("SYSBP", "SYSBP"),
  VSDTC = c("2021-01-05", "2021-01-19"),
  stringsAsFactors = FALSE
)
datasets <- list(DM = dm, AE = ae, VS = vs)

test_that("subject_narrative_facts returns NULL when there is nothing to say", {
  expect_null(subject_narrative_facts(NULL, "S1-001"))
  expect_null(subject_narrative_facts(datasets, ""))
  expect_null(subject_narrative_facts(datasets, "NOBODY"))
})

test_that("subject_narrative_facts derives grounded facts", {
  f <- subject_narrative_facts(datasets, "S1-001")
  expect_equal(f$subject, "S1-001")
  expect_equal(f$demographics$Age, "54")
  expect_equal(f$demographics$Sex, "F")
  expect_equal(f$kpis$n_ae, 2L)
  expect_equal(f$kpis$n_serious_ae, 1L)
  expect_equal(f$kpis$worst_ae_severity, "SEVERE")
  expect_equal(unname(f$domains[["AE"]]), 2L)
  expect_equal(nrow(f$ae_events), 2L)
  expect_true("Nausea" %in% f$ae_events$term)
  expect_equal(f$timeline$first_date, "2021-01-05")
  expect_true(f$timeline$duration_days > 0)
})

test_that("subject_narrative_facts_text is data-only and readable", {
  expect_equal(subject_narrative_facts_text(NULL), "")
  txt <- subject_narrative_facts_text(subject_narrative_facts(datasets, "S1-001"))
  expect_true(grepl("Subject: S1-001", txt, fixed = TRUE))
  expect_true(grepl("2 total, 1 serious", txt, fixed = TRUE))
  expect_true(grepl("Nausea (severe) [serious]", txt, fixed = TRUE))
  expect_true(grepl("Timeline:", txt, fixed = TRUE))
})

test_that("subject_narrative runs through an injected narrator", {
  seen <- NULL
  narrate <- function(facts_text, facts = NULL, ...) {
    seen <<- facts_text
    "Subject S1-001 had two adverse events."
  }
  res <- subject_narrative(datasets, "S1-001", narrate = narrate)
  expect_null(res$error)
  expect_equal(res$narrative, "Subject S1-001 had two adverse events.")
  # The narrator is given the grounded facts text, nothing else.
  expect_identical(seen, res$facts_text)
  expect_true(grepl("Subject: S1-001", res$facts_text, fixed = TRUE))
})

test_that("subject_narrative reports a missing subject without calling the narrator", {
  called <- FALSE
  narrate <- function(facts_text, facts = NULL, ...) {
    called <<- TRUE; "x"
  }
  res <- subject_narrative(datasets, "NOBODY", narrate = narrate)
  expect_false(called)
  expect_null(res$narrative)
  expect_match(res$error, "No records")
})

test_that("subject_narrative surfaces narrator errors", {
  narrate <- function(facts_text, facts = NULL, ...) stop("api down")
  res <- subject_narrative(datasets, "S1-001", narrate = narrate)
  expect_null(res$narrative)
  expect_match(res$error, "api down")
  expect_true(nzchar(res$facts_text))
})

test_that("mod_subject_server generates a narrative through an injected narrator", {
  narrate <- function(facts_text, facts = NULL, ...) "Grounded summary text."
  shiny::testServer(
    mod_subject_server,
    args = list(datasets = shiny::reactive(datasets), narrate = narrate),
    {
      session$setInputs(subject = "S1-001")
      session$setInputs(gen_narrative = 1)
      a <- narr()
      expect_null(a$error)
      expect_equal(a$narrative, "Grounded summary text.")
      # Switching subject clears the stale summary.
      session$setInputs(subject = "S1-002")
      expect_null(narr())
    }
  )
})
