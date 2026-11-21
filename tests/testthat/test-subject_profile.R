# Small fixtures shared across tests
dm <- data.frame(
  STUDYID = "S1", DOMAIN = "DM",
  USUBJID = c("S1-001", "S1-002"),
  SUBJID = c("001", "002"),
  AGE = c(45, 60), SEX = c("F", "M"),
  RACE = c("ASIAN", "WHITE"),
  ARM = c("Treatment", "Placebo"),
  SITEID = c("SITE01", "SITE02"),
  stringsAsFactors = FALSE
)

ae <- data.frame(
  STUDYID = "S1", DOMAIN = "AE",
  USUBJID = c("S1-001", "S1-001", "S1-002"),
  AESEQ = c(1, 2, 1),
  AETERM = c("Headache", "Nausea", "Rash"),
  AESTDTC = c("2023-03-10", "2023-04-01", "2023-02-15"),
  stringsAsFactors = FALSE
)

vs <- data.frame(
  STUDYID = "S1", DOMAIN = "VS",
  USUBJID = c("S1-001", "S1-002"),
  VSTESTCD = c("SYSBP", "SYSBP"),
  VSSTRESN = c(120, 130),
  VSDTC = c("2023-03-05", "2023-02-20"),
  stringsAsFactors = FALSE
)

datasets <- list(DM = dm, AE = ae, VS = vs)

test_that("find_subject_col prefers USUBJID then SUBJID", {
  expect_equal(find_subject_col(dm), "USUBJID")
  expect_equal(find_subject_col(data.frame(SUBJID = "x")), "SUBJID")
  expect_null(find_subject_col(data.frame(FOO = 1)))
  expect_null(find_subject_col(NULL))
  # case-insensitive
  expect_equal(find_subject_col(data.frame(usubjid = "x")), "usubjid")
})

test_that("domain_key uses DOMAIN when constant, else file name", {
  expect_equal(domain_key(dm), "DM")
  expect_equal(domain_key(ae), "AE")
  # mixed DOMAIN falls back to file name
  mixed <- data.frame(DOMAIN = c("AE", "DM"), USUBJID = c("a", "b"))
  expect_equal(domain_key(mixed, "lb.xpt"), "LB")
  # no DOMAIN column
  expect_equal(domain_key(data.frame(USUBJID = "a"), "cm.csv"), "CM")
})

test_that("list_subjects returns the sorted union across domains", {
  expect_equal(list_subjects(datasets), c("S1-001", "S1-002"))
  expect_equal(list_subjects(list()), character(0))
  expect_equal(list_subjects(NULL), character(0))
  # a domain with no subject column is ignored
  expect_equal(
    list_subjects(list(DM = dm, X = data.frame(FOO = 1))),
    c("S1-001", "S1-002")
  )
})

test_that("subject_records returns per-domain subsets and drops empties", {
  recs <- subject_records(datasets, "S1-001")
  expect_setequal(names(recs), c("DM", "AE", "VS"))
  expect_equal(nrow(recs$DM), 1)
  expect_equal(nrow(recs$AE), 2)
  expect_equal(nrow(recs$VS), 1)

  # a subject present only in some domains
  only_dm <- subject_records(list(DM = dm, AE = ae[ae$USUBJID == "S1-001", ]),
                             "S1-002")
  expect_equal(names(only_dm), "DM")

  expect_length(subject_records(datasets, "NOPE"), 0)
  expect_length(subject_records(datasets, ""), 0)
})

test_that("subject_overview counts records per domain", {
  ov <- subject_overview(datasets, "S1-001")
  expect_equal(ov$Domain, c("DM", "AE", "VS"))
  expect_equal(ov$Records, c(1L, 2L, 1L))
  expect_equal(nrow(subject_overview(datasets, "NOPE")), 0)
})

test_that("subject_demographics pulls DM fields and omits missing ones", {
  demo <- subject_demographics(datasets, "S1-001")
  expect_true(all(c("Field", "Value") %in% names(demo)))
  kv <- setNames(demo$Value, demo$Field)
  expect_equal(unname(kv["Age"]), "45")
  expect_equal(unname(kv["Sex"]), "F")
  expect_equal(unname(kv["Arm"]), "Treatment")
  # DM located via DOMAIN column even when list is not named "DM"
  demo2 <- subject_demographics(list(x = dm, AE = ae), "S1-002")
  expect_equal(unname(setNames(demo2$Value, demo2$Field)["Sex"]), "M")
  # no DM present
  expect_equal(nrow(subject_demographics(list(AE = ae), "S1-001")), 0)
})

test_that("subject_timeline merges dated events and sorts by date", {
  tl <- subject_timeline(datasets, "S1-001")
  expect_true(all(c("Date", "Domain", "Event") %in% names(tl)))
  # VS 2023-03-05 comes before AE 2023-03-10 and 2023-04-01
  expect_equal(as.character(tl$Date[1]), "2023-03-05")
  expect_equal(tl$Domain[1], "VS")
  expect_true(all(diff(tl$Date) >= 0))
  expect_equal(tl$Event[tl$Domain == "AE"], c("Headache", "Nausea"))
})

test_that("subject_timeline returns zero rows when no dates are present", {
  nodate <- list(DM = data.frame(USUBJID = "a", AGE = 30))
  tl <- subject_timeline(nodate, "a")
  expect_equal(nrow(tl), 0)
  expect_true(all(c("Date", "Domain", "Event") %in% names(tl)))
})

test_that("subject_timeline adds a CDISC study day from RFSTDTC", {
  dm2 <- data.frame(
    DOMAIN = "DM", USUBJID = "S1-001", RFSTDTC = "2023-03-01",
    stringsAsFactors = FALSE
  )
  ds2 <- list(DM = dm2, AE = ae[ae$USUBJID == "S1-001", ])
  tl <- subject_timeline(ds2, "S1-001")
  expect_true("Day" %in% names(tl))
  # AE on 2023-03-10 is 10 days after ref -> study day 10 (no day 0)
  ae_row <- tl[tl$Domain == "AE" & tl$Event == "Headache", ]
  expect_equal(ae_row$Day, 10L)
  # explicit ref_date overrides
  tl2 <- subject_timeline(ds2, "S1-001", ref_date = as.Date("2023-03-09"))
  expect_equal(tl2$Day[tl2$Event == "Headache"], 2L)
})

test_that("subject_kpis summarises events and severity", {
  ae_sev <- data.frame(
    DOMAIN = "AE", USUBJID = "S1-001",
    AETERM = c("Headache", "Rash", "Nausea"),
    AESEV = c("MILD", "SEVERE", "MODERATE"),
    AESER = c("N", "Y", "N"),
    stringsAsFactors = FALSE
  )
  vs2 <- data.frame(
    DOMAIN = "VS", USUBJID = "S1-001",
    VISITNUM = c(1, 1, 2, 3),
    stringsAsFactors = FALSE
  )
  ds2 <- list(DM = dm, AE = ae_sev, VS = vs2)
  k <- subject_kpis(ds2, "S1-001")
  expect_equal(k$n_ae, 3L)
  expect_equal(k$n_serious_ae, 1L)
  expect_equal(k$worst_ae_severity, "SEVERE")
  expect_equal(k$n_visits, 3L)
  expect_equal(k$n_domains, 3L)

  # a subject with no AEs
  k0 <- subject_kpis(list(DM = dm), "S1-002")
  expect_equal(k0$n_ae, 0L)
  expect_equal(k0$n_serious_ae, 0L)
  expect_true(is.na(k0$worst_ae_severity))
})

test_that("subject_profile_code emits runnable, parseable filters", {
  code <- subject_profile_code("S1-001", c("DM", "AE"))
  expect_true(grepl("library(dplyr)", code, fixed = TRUE))
  expect_true(grepl("subject_id <- \"S1-001\"", code, fixed = TRUE))
  expect_true(grepl("dm_subject <- dplyr::filter(DM, USUBJID == subject_id)",
                    code, fixed = TRUE))
  expect_true(grepl("ae_subject <- dplyr::filter(AE, USUBJID == subject_id)",
                    code, fixed = TRUE))
  expect_silent(parse(text = code))
  # no domains still parses
  expect_silent(parse(text = subject_profile_code("X", character(0))))
})

test_that("find_domain and get_col resolve by name or DOMAIN value", {
  expect_identical(find_domain(datasets, "AE"), ae)
  # located via DOMAIN column when the list name differs
  expect_identical(find_domain(list(x = ae), "AE"), ae)
  expect_null(find_domain(datasets, "LB"))
  expect_equal(get_col(dm, "sex"), dm$SEX)
  expect_null(get_col(dm, "NOPE"))
})

test_that("helpers round-trip against generated demo data", {
  demo <- generate_demo_data(n_subjects = 20)
  subs <- list_subjects(demo)
  expect_length(subs, 20)
  s <- subs[1]
  ov <- subject_overview(demo, s)
  expect_true("DM" %in% ov$Domain)
  expect_equal(ov$Records[ov$Domain == "DM"], 1L)
  # every subject has VS measurements in the demo generator
  expect_true("VS" %in% ov$Domain)
  demo_tbl <- subject_demographics(demo, s)
  expect_true(nrow(demo_tbl) > 0)
})
