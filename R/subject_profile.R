# Cross-domain subject drill-down (Phase 4)
#
# Pure, Shiny-free helpers that take a named list of loaded domain data frames
# (e.g. list(DM = ..., AE = ..., VS = ...)) and let a single subject be followed
# across every domain: their demographics, per-domain record counts, the raw
# per-domain records, and a merged chronological timeline of dated events.
# Keeping these free of any Shiny or file-IO dependency makes the logic
# unit-testable from plain data frames.

#' Identify the subject-ID column in a dataset
#'
#' Looks for the CDISC unique-subject identifier, preferring \code{USUBJID} and
#' falling back to \code{SUBJID}. Matching is case-insensitive.
#'
#' @param data A data.frame.
#' @return The matching column name, or NULL if none is present.
#' @keywords internal
find_subject_col <- function(data) {
  if (is.null(data) || is.null(names(data)) || length(names(data)) == 0) {
    return(NULL)
  }
  nms <- names(data)
  up <- toupper(nms)
  for (cand in c("USUBJID", "SUBJID")) {
    hit <- which(up == cand)
    if (length(hit)) return(nms[hit[1]])
  }
  NULL
}

#' Derive a domain key for a loaded dataset
#'
#' Uses the value of the \code{DOMAIN} column when it is present and constant,
#' otherwise the file name without extension. Always upper-cased.
#'
#' @param data A data.frame.
#' @param fallback A fallback name (e.g. the source file name).
#' @return A single upper-case string.
#' @keywords internal
domain_key <- function(data, fallback = "DATA") {
  up <- toupper(names(data))
  if ("DOMAIN" %in% up) {
    dcol <- names(data)[up == "DOMAIN"][1]
    v <- unique(as.character(data[[dcol]]))
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v) == 1) return(toupper(v))
  }
  toupper(tools::file_path_sans_ext(fallback))
}

#' List every subject present across a set of domains
#'
#' @param datasets A named list of data.frames.
#' @return A sorted character vector of unique subject IDs (USUBJID/SUBJID).
#' @export
list_subjects <- function(datasets) {
  if (is.null(datasets) || !length(datasets)) return(character(0))
  ids <- character(0)
  for (d in datasets) {
    col <- find_subject_col(d)
    if (!is.null(col)) ids <- c(ids, as.character(d[[col]]))
  }
  ids <- ids[!is.na(ids) & nzchar(ids)]
  sort(unique(ids))
}

#' Pull one subject's records from every domain
#'
#' @param datasets A named list of data.frames.
#' @param subject_id A single subject ID.
#' @return A named list holding, for each domain that contains the subject, the
#'   subset of rows for that subject (domains with no matching rows are dropped).
#' @export
subject_records <- function(datasets, subject_id) {
  out <- list()
  if (is.null(datasets) || !length(datasets) ||
      is.null(subject_id) || !nzchar(subject_id)) {
    return(out)
  }
  for (nm in names(datasets)) {
    d <- datasets[[nm]]
    col <- find_subject_col(d)
    if (is.null(col)) next
    sub <- d[!is.na(d[[col]]) & as.character(d[[col]]) == subject_id, , drop = FALSE]
    if (nrow(sub) > 0) {
      rownames(sub) <- NULL
      out[[nm]] <- sub
    }
  }
  out
}

#' Count a subject's records per domain
#'
#' @param datasets A named list of data.frames.
#' @param subject_id A single subject ID.
#' @return A data.frame with columns Domain and Records.
#' @export
subject_overview <- function(datasets, subject_id) {
  recs <- subject_records(datasets, subject_id)
  if (!length(recs)) {
    return(data.frame(Domain = character(0), Records = integer(0),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    Domain = names(recs),
    Records = vapply(recs, nrow, integer(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Locate a domain in a set of datasets by name or DOMAIN value
#'
#' @param datasets A named list of data.frames.
#' @param domain The domain code to find (e.g. "DM", "AE").
#' @return The matching data.frame, or NULL.
#' @keywords internal
find_domain <- function(datasets, domain) {
  if (is.null(datasets) || !length(datasets)) return(NULL)
  domain <- toupper(domain)
  nms <- names(datasets)
  hit <- which(toupper(nms) == domain)
  if (length(hit)) return(datasets[[hit[1]]])
  for (d in datasets) {
    up <- toupper(names(d))
    if ("DOMAIN" %in% up) {
      dcol <- names(d)[up == "DOMAIN"][1]
      if (domain %in% toupper(as.character(d[[dcol]]))) return(d)
    }
  }
  NULL
}

#' Locate the demographics (DM) dataset in a set of domains
#'
#' @param datasets A named list of data.frames.
#' @return The DM data.frame, or NULL if none can be identified.
#' @keywords internal
find_dm <- function(datasets) {
  find_domain(datasets, "DM")
}

#' A column from a data.frame by case-insensitive name
#'
#' @param df A data.frame.
#' @param name A column name.
#' @return The column vector, or NULL if absent.
#' @keywords internal
get_col <- function(df, name) {
  idx <- which(toupper(names(df)) == toupper(name))
  if (length(idx)) df[[idx[1]]] else NULL
}

#' Demographics summary for one subject
#'
#' Reads the common DM variables (age, sex, race, arm, site, etc.) for the
#' subject and returns them as a tidy two-column table. Missing or empty fields
#' are omitted.
#'
#' @param datasets A named list of data.frames.
#' @param subject_id A single subject ID.
#' @return A data.frame with columns Field and Value.
#' @export
subject_demographics <- function(datasets, subject_id) {
  empty <- data.frame(Field = character(0), Value = character(0),
                      stringsAsFactors = FALSE)
  dm <- find_dm(datasets)
  if (is.null(dm)) return(empty)
  col <- find_subject_col(dm)
  if (is.null(col)) return(empty)
  row <- dm[!is.na(dm[[col]]) & as.character(dm[[col]]) == subject_id, , drop = FALSE]
  if (nrow(row) == 0) return(empty)
  row <- row[1, , drop = FALSE]

  fields <- c(
    USUBJID = "Subject", SITEID = "Site", COUNTRY = "Country",
    AGE = "Age", AGEU = "Age unit", SEX = "Sex", RACE = "Race",
    ETHNIC = "Ethnicity", ARMCD = "Arm code", ARM = "Arm",
    ACTARM = "Actual arm", RFSTDTC = "Reference start",
    RFENDTC = "Reference end"
  )
  up <- toupper(names(row))
  parts <- list()
  for (key in names(fields)) {
    idx <- which(up == key)
    if (!length(idx)) next
    val <- row[[idx[1]]]
    if (length(val) == 0 || is.na(val) || !nzchar(as.character(val))) next
    parts[[length(parts) + 1L]] <- data.frame(
      Field = unname(fields[[key]]),
      Value = as.character(val),
      stringsAsFactors = FALSE
    )
  }
  if (!length(parts)) return(empty)
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

#' First column whose name ends with one of the given suffixes
#'
#' @param nms Character vector of column names.
#' @param suffixes Character vector of upper-case suffixes, tried in order.
#' @return The matching column name, or NULL.
#' @keywords internal
first_matching <- function(nms, suffixes) {
  if (!length(nms)) return(NULL)
  up <- toupper(nms)
  for (suf in suffixes) {
    hit <- which(endsWith(up, suf))
    if (length(hit)) return(nms[hit[1]])
  }
  NULL
}

#' Parse an ISO-8601-ish value to a Date
#'
#' Takes the leading \code{YYYY-MM-DD} of a character or Date value. Values that
#' do not parse become NA.
#'
#' @param x A vector.
#' @return A Date vector.
#' @keywords internal
parse_iso_date <- function(x) {
  s <- substr(as.character(x), 1, 10)
  suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
}

#' Reference start date (RFSTDTC) for one subject
#'
#' @param datasets A named list of data.frames.
#' @param subject_id A single subject ID.
#' @return A Date (NA if it cannot be determined).
#' @keywords internal
subject_ref_date <- function(datasets, subject_id) {
  dm <- find_dm(datasets)
  if (is.null(dm)) return(as.Date(NA))
  col <- find_subject_col(dm)
  if (is.null(col)) return(as.Date(NA))
  row <- dm[!is.na(dm[[col]]) & as.character(dm[[col]]) == subject_id, , drop = FALSE]
  if (nrow(row) == 0) return(as.Date(NA))
  ref <- get_col(row, "RFSTDTC")
  if (is.null(ref)) ref <- get_col(row, "RFXSTDTC")
  if (is.null(ref)) return(as.Date(NA))
  parse_iso_date(ref[1])
}

#' Merged chronological timeline for one subject
#'
#' Scans each domain's records for a start-date column (\code{--STDTC},
#' \code{--DTC}, ...) and an event-label column (\code{--TERM}, \code{--DECOD},
#' \code{--TRT}, \code{--TEST}), then stacks the dated events across domains and
#' orders them by date. This is the cross-domain view: one subject's journey
#' through the study in one table. When a reference start date is available the
#' CDISC study day is added (day 1 is the reference date; there is no day 0).
#'
#' @param datasets A named list of data.frames.
#' @param subject_id A single subject ID.
#' @param ref_date Optional reference start Date; derived from DM RFSTDTC when
#'   NULL.
#' @return A data.frame with columns Date, Day, Domain, Event (zero rows if no
#'   dated events are found).
#' @export
subject_timeline <- function(datasets, subject_id, ref_date = NULL) {
  empty <- data.frame(Date = as.Date(character(0)), Day = integer(0),
                      Domain = character(0), Event = character(0),
                      stringsAsFactors = FALSE)
  recs <- subject_records(datasets, subject_id)
  if (!length(recs)) return(empty)

  rows <- list()
  for (nm in names(recs)) {
    d <- recs[[nm]]
    date_col <- first_matching(names(d), c("STDTC", "DTC", "STDT", "DT"))
    if (is.null(date_col)) next
    dates <- parse_iso_date(d[[date_col]])
    keep <- !is.na(dates)
    if (!any(keep)) next
    label_col <- first_matching(names(d), c("TERM", "DECOD", "TRT", "TEST"))
    labels <- if (!is.null(label_col)) as.character(d[[label_col]]) else rep(nm, nrow(d))
    rows[[length(rows) + 1L]] <- data.frame(
      Date = dates[keep],
      Domain = nm,
      Event = labels[keep],
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)

  if (is.null(ref_date)) ref_date <- subject_ref_date(datasets, subject_id)
  if (length(ref_date) == 1 && !is.na(ref_date)) {
    delta <- as.integer(out$Date - ref_date)
    out$Day <- ifelse(delta >= 0, delta + 1L, delta)
  } else {
    out$Day <- NA_integer_
  }

  out <- out[order(out$Date), c("Date", "Day", "Domain", "Event"), drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Pick one domain out of an already-subset record list
#'
#' @param recs A named list of per-domain subject records.
#' @param domain The domain code to pick.
#' @return The matching data.frame, or NULL.
#' @keywords internal
pick_domain <- function(recs, domain) {
  find_domain(recs, domain)
}

#' Headline counts for one subject
#'
#' Summarises a subject across domains into a handful of KPIs suitable for value
#' boxes: the number of domains they appear in, total records, adverse-event
#' count, serious adverse events, worst AE severity, and number of visits.
#'
#' @param datasets A named list of data.frames.
#' @param subject_id A single subject ID.
#' @return A named list of KPI values.
#' @export
subject_kpis <- function(datasets, subject_id) {
  recs <- subject_records(datasets, subject_id)
  out <- list(
    n_domains = length(recs),
    n_records = if (length(recs)) sum(vapply(recs, nrow, integer(1))) else 0L,
    n_ae = 0L,
    n_serious_ae = 0L,
    worst_ae_severity = NA_character_,
    n_visits = NA_integer_
  )

  ae <- pick_domain(recs, "AE")
  if (!is.null(ae) && nrow(ae) > 0) {
    out$n_ae <- nrow(ae)
    ser <- get_col(ae, "AESER")
    if (!is.null(ser)) {
      out$n_serious_ae <- sum(toupper(trimws(as.character(ser))) %in% c("Y", "YES"),
                              na.rm = TRUE)
    }
    sev <- get_col(ae, "AESEV")
    if (!is.null(sev)) {
      rank <- c(MILD = 1L, MODERATE = 2L, SEVERE = 3L)
      present <- rank[toupper(trimws(as.character(sev)))]
      present <- present[!is.na(present)]
      if (length(present)) out$worst_ae_severity <- names(rank)[max(present)]
    }
  }

  vs <- pick_domain(recs, "VS")
  if (!is.null(vs) && nrow(vs) > 0) {
    vcol <- get_col(vs, "VISITNUM")
    if (is.null(vcol)) vcol <- get_col(vs, "VISIT")
    if (!is.null(vcol)) out$n_visits <- length(unique(vcol[!is.na(vcol)]))
  }

  out
}

#' Reproducible R code that extracts one subject across domains
#'
#' @param subject_id A single subject ID.
#' @param domains Character vector of domain object names to filter.
#' @param libraries Packages to emit \code{library()} calls for.
#' @return A single multi-line string of runnable R.
#' @export
subject_profile_code <- function(subject_id, domains, libraries = "dplyr") {
  lines <- character(0)
  if (length(libraries)) {
    lines <- c(lines, vapply(libraries, function(p) paste0("library(", p, ")"),
                             character(1)), "")
  }
  lines <- c(lines, paste0("subject_id <- ", r_literal(subject_id)))
  if (length(domains)) {
    lines <- c(lines, "")
    for (dm in domains) {
      obj <- paste0(tolower(dm), "_subject")
      lines <- c(lines,
                 paste0(obj, " <- dplyr::filter(", dm, ", USUBJID == subject_id)"))
    }
  }
  paste(lines, collapse = "\n")
}
