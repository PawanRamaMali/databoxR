# Per-subject narrative (grounded)
#
# Same trust model as nl_query: R does the arithmetic, the model only phrases it.
# subject_narrative_facts() reduces one subject's real records to a compact set
# of grounded facts (demographics, event counts, AE list, timeline span). Those
# facts, and only those facts, are handed to a narrator that returns a short
# plain-language paragraph. The model is told to add nothing that is not in the
# facts, and the app always shows the facts next to the narrative so every claim
# is checkable. The narrator is injectable, so the whole flow is testable without
# a language model.

#' Grounded facts for one subject
#'
#' Reduces a subject's records across every loaded domain to a compact, fully
#' derived set of facts: demographics, headline KPIs, per-domain record counts,
#' the adverse-event list, and the span of the dated timeline. Nothing here is
#' inferred; every value comes straight from the data. This is the only input a
#' narrator is given, which is what keeps a generated summary grounded.
#'
#' @param datasets A named list of data.frames.
#' @param subject_id A single subject ID.
#' @return A named list of facts, or NULL if the subject has no records.
#' @export
subject_narrative_facts <- function(datasets, subject_id) {
  if (is.null(datasets) || !length(datasets) ||
      is.null(subject_id) || !nzchar(subject_id)) {
    return(NULL)
  }
  recs <- subject_records(datasets, subject_id)
  if (!length(recs)) return(NULL)

  demo <- subject_demographics(datasets, subject_id)
  demographics <- stats::setNames(as.list(as.character(demo$Value)), demo$Field)

  kpis <- subject_kpis(datasets, subject_id)
  overview <- subject_overview(datasets, subject_id)
  domains <- stats::setNames(as.integer(overview$Records), overview$Domain)

  ae <- pick_domain(recs, "AE")
  ae_events <- data.frame(
    term = character(0), severity = character(0),
    serious = character(0), start = character(0),
    stringsAsFactors = FALSE
  )
  if (!is.null(ae) && nrow(ae) > 0) {
    term <- get_col(ae, "AETERM")
    if (is.null(term)) term <- get_col(ae, "AEDECOD")
    sev <- get_col(ae, "AESEV")
    ser <- get_col(ae, "AESER")
    start <- get_col(ae, "AESTDTC")
    n <- nrow(ae)
    chr <- function(x) if (is.null(x)) rep(NA_character_, n) else as.character(x)
    ae_events <- data.frame(
      term = chr(term), severity = chr(sev),
      serious = chr(ser), start = chr(start),
      stringsAsFactors = FALSE
    )
  }

  tl <- subject_timeline(datasets, subject_id)
  timeline <- list(n_events = nrow(tl), first_date = NA_character_,
                   last_date = NA_character_, duration_days = NA_integer_)
  if (nrow(tl) > 0) {
    d <- tl$Date[!is.na(tl$Date)]
    if (length(d)) {
      timeline$first_date <- format(min(d), "%Y-%m-%d")
      timeline$last_date <- format(max(d), "%Y-%m-%d")
      timeline$duration_days <- as.integer(max(d) - min(d))
    }
  }

  list(
    subject = subject_id,
    demographics = demographics,
    kpis = kpis,
    domains = domains,
    ae_events = ae_events,
    timeline = timeline
  )
}

#' Render subject facts as compact text
#'
#' Turns the list from [subject_narrative_facts()] into the plain-text block that
#' is handed to a narrator. It is deterministic and data-only, so it doubles as a
#' readable, no-model summary of exactly what the narrator was told.
#'
#' @param facts A list from [subject_narrative_facts()].
#' @return A single multi-line string (empty string for NULL facts).
#' @export
subject_narrative_facts_text <- function(facts) {
  if (is.null(facts)) return("")
  lines <- character(0)
  lines <- c(lines, paste0("Subject: ", facts$subject))

  demo <- facts$demographics
  if (length(demo)) {
    pairs <- vapply(names(demo), function(k) paste0(k, ": ", demo[[k]]),
                    character(1))
    lines <- c(lines, paste0("Demographics - ", paste(pairs, collapse = "; ")))
  }

  dom <- facts$domains
  if (length(dom)) {
    pairs <- vapply(names(dom), function(k) paste0(k, " ", dom[[k]]), character(1))
    lines <- c(lines, paste0("Records by domain - ", paste(pairs, collapse = "; ")))
  }

  k <- facts$kpis
  sev <- if (is.na(k$worst_ae_severity)) "none" else tolower(k$worst_ae_severity)
  visits <- if (is.na(k$n_visits)) "unknown" else k$n_visits
  lines <- c(lines, sprintf(
    "Adverse events: %d total, %d serious, worst severity %s. Visits: %s.",
    k$n_ae, k$n_serious_ae, sev, visits))

  ae <- facts$ae_events
  if (nrow(ae) > 0) {
    desc <- vapply(seq_len(nrow(ae)), function(i) {
      parts <- ae$term[i]
      if (!is.na(ae$severity[i]) && nzchar(ae$severity[i]))
        parts <- paste0(parts, " (", tolower(ae$severity[i]), ")")
      if (!is.na(ae$serious[i]) &&
          toupper(trimws(ae$serious[i])) %in% c("Y", "YES"))
        parts <- paste0(parts, " [serious]")
      parts
    }, character(1))
    lines <- c(lines, paste0("AE list - ", paste(desc, collapse = "; ")))
  }

  tl <- facts$timeline
  if (!is.na(tl$first_date)) {
    lines <- c(lines, sprintf(
      "Timeline: %d dated events from %s to %s (%d days).",
      tl$n_events, tl$first_date, tl$last_date, tl$duration_days))
  }

  paste(lines, collapse = "\n")
}

#' Generate a grounded narrative for one subject
#'
#' Builds the subject's facts, hands them (and only them) to a narrator, and
#' returns the narrator's short plain-language summary together with the facts it
#' was given. The default narrator calls Claude via [subject_narrative_claude()];
#' pass your own `narrate` (a function of the facts text) to run without a model,
#' for example in tests. The model is never shown raw data or asked for anything
#' but prose, so the result stays grounded in the computed facts.
#'
#' @param datasets A named list of data.frames.
#' @param subject_id A single subject ID.
#' @param narrate A function taking the facts text (character) and returning a
#'   narrative string. Defaults to [subject_narrative_claude()].
#' @param ... Passed to `narrate`.
#' @return A list with elements `facts`, `facts_text`, `narrative`, and `error`
#'   (one of narrative/error is non-NULL on success/failure).
#' @export
subject_narrative <- function(datasets, subject_id, narrate = NULL, ...) {
  facts <- subject_narrative_facts(datasets, subject_id)
  if (is.null(facts)) {
    return(list(facts = NULL, facts_text = "", narrative = NULL,
                error = "No records found for this subject."))
  }
  facts_text <- subject_narrative_facts_text(facts)
  if (is.null(narrate)) narrate <- subject_narrative_claude

  out <- tryCatch(
    narrate(facts_text, facts = facts, ...),
    error = function(e) structure(conditionMessage(e), class = "nlq_error")
  )
  if (inherits(out, "nlq_error")) {
    return(list(facts = facts, facts_text = facts_text, narrative = NULL,
                error = as.character(out)))
  }
  list(facts = facts, facts_text = facts_text,
       narrative = as.character(out), error = NULL)
}
