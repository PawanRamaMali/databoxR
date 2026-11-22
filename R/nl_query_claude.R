# Claude planner for nl_query (optional, uses the ellmer package)
#
# This is the one place that talks to a language model. It is deliberately thin:
# it hands Claude the data-free schema and gets back a *structured* query plan
# (never free-form code or data), which nl_query() then validates and executes
# deterministically. ellmer is a Suggests dependency; without it (or without an
# API key) this stops with a clear message and the deterministic core is
# unaffected.

# Render the schema as compact text for the system prompt (no row data).
#' @noRd
nlq_schema_text <- function(schema) {
  if (!length(schema$domains)) return("(no datasets are loaded)")
  blocks <- vapply(names(schema$domains), function(nm) {
    tab <- schema$domains[[nm]]
    cols <- vapply(seq_len(nrow(tab)), function(i) {
      v <- if (nzchar(tab$values[i])) paste0(" {", tab$values[i], "}") else ""
      paste0("    - ", tab$column[i], " (", tab$type[i], ")", v)
    }, character(1))
    paste0("  ", nm, ":\n", paste(cols, collapse = "\n"))
  }, character(1))
  paste0("Domains and columns:\n", paste(blocks, collapse = "\n"))
}

#' @noRd
nlq_system_prompt <- function(schema) {
  paste0(
    "You translate a question about CDISC SDTM/ADaM clinical trial data into a ",
    "structured query plan. You never return data or code, only the plan.\n\n",
    "Rules:\n",
    "- Use only domains and columns listed below; never invent names.\n",
    "- For a question about one subject's records, timeline, demographics or ",
    "counts, use the matching subject_* action and set `subject` to the exact ",
    "USUBJID.\n",
    "- Otherwise use action 'filter': pick one `domain`, add `filters` ",
    "(column, op, value), set `logic`, and optionally `columns` to keep.\n",
    "- Operators: eq, ne, gt, ge, lt, le, contains, in (in takes a ",
    "comma-separated value). Put every value in `value` as text.\n\n",
    nlq_schema_text(schema),
    "\n\nKnown subjects (USUBJID): ",
    if (length(schema$subjects))
      paste(utils::head(schema$subjects, 50), collapse = ", ") else "(none)"
  )
}

# The constrained plan schema, as ellmer types.
#' @noRd
nlq_plan_type <- function() {
  ellmer::type_object(
    "A query plan over the loaded clinical datasets",
    action = ellmer::type_enum(
      c("filter", "subject_timeline", "subject_overview",
        "subject_kpis", "subject_demographics"),
      "Which operation to run"),
    domain = ellmer::type_string(
      "Domain to query for a filter action (e.g. AE, VS, DM)", required = FALSE),
    filters = ellmer::type_array(
      ellmer::type_object(
        column = ellmer::type_string("Column name in the domain"),
        op = ellmer::type_enum(c("eq", "ne", "gt", "ge", "lt", "le",
                                 "contains", "in"), "Comparison operator"),
        value = ellmer::type_string("Comparison value, as text")
      ),
      "Filters for a filter action", required = FALSE),
    logic = ellmer::type_enum(c("and", "or"),
      "How to combine filters", required = FALSE),
    columns = ellmer::type_array(ellmer::type_string("Column name"),
      "Columns to keep in the result", required = FALSE),
    subject = ellmer::type_string(
      "Exact USUBJID for a subject_* action", required = FALSE)
  )
}

# Normalise ellmer's structured output into the plain list the core consumes.
#' @noRd
nlq_normalise_plan <- function(raw) {
  filters <- list()
  f <- raw$filters
  if (!is.null(f) && NROW(f)) {
    f <- as.data.frame(f, stringsAsFactors = FALSE)
    filters <- lapply(seq_len(nrow(f)), function(i) {
      list(column = as.character(f$column[i]),
           op = as.character(f$op[i]),
           value = as.character(f$value[i]))
    })
  }
  cols <- raw$columns
  if (is.null(cols)) cols <- character(0)
  list(
    action = raw$action %||% "filter",
    domain = raw$domain %||% "",
    filters = filters,
    logic = raw$logic %||% "and",
    columns = as.character(unlist(cols)),
    subject = raw$subject %||% ""
  )
}

#' Propose a query plan with Claude via ellmer
#'
#' Default planner for [nl_query()]. Sends the question and the data-free schema
#' to Claude and returns a structured query plan. Requires the `ellmer` package
#' and an `ANTHROPIC_API_KEY`.
#'
#' @param question A natural-language question (character).
#' @param schema A schema from [nlq_schema()].
#' @param model Claude model id (default `"claude-opus-5-5"`).
#' @param chat An optional pre-built ellmer chat object (mainly for testing).
#' @return A query plan (named list) for [nlq_validate_plan()] / [nlq_execute_plan()].
#' @export
nlq_propose_claude <- function(question, schema, model = "claude-opus-5-5",
                               chat = NULL) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("The 'ellmer' package is required for nlq_propose_claude(). ",
         "Install it, or pass your own `propose` function to nl_query().")
  }
  if (is.null(chat)) {
    chat <- ellmer::chat_anthropic(
      system_prompt = nlq_system_prompt(schema), model = model)
  }
  raw <- chat$chat_structured(question, type = nlq_plan_type())
  nlq_normalise_plan(raw)
}
