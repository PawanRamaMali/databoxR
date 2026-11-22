# Natural-language query (grounded, deterministic execution)
#
# Direction-1 prototype: a natural-language question is turned into a *constrained
# query plan* by an LLM, and that plan is validated against the real data and
# executed deterministically in R. The model only ever proposes which domain,
# columns, operators (from a fixed whitelist) and comparison values to use; R
# does the comparing. Nothing is `eval`'d, so a wrong plan returns wrong-but-real
# rows, never fabricated data. The LLM step is an injectable seam (see nl_query),
# so this whole file is testable offline with hand-written plans.

# Operators the planner may use. Each maps to a deterministic R comparison.
.nlq_ops <- c("eq", "ne", "gt", "ge", "lt", "le", "contains", "in")

# Subject actions route to the existing, tested subject_* helpers.
.nlq_subject_actions <- c("subject_timeline", "subject_overview",
                          "subject_kpis", "subject_demographics")

.nlq_actions <- c("filter", .nlq_subject_actions)

#' Describe loaded datasets for a query planner
#'
#' Builds a compact, data-free schema (domains, their columns, column types and
#' a few distinct values for small categorical columns) suitable for handing to
#' a language model so it can propose a query plan. No row-level data leaves.
#'
#' @param datasets A named list of data frames (loaded domains).
#' @param max_levels Maximum distinct values to list for a categorical column.
#' @return A list with `subjects` (character) and `domains`: a named list, one
#'   entry per domain, each a data frame of `column`, `type`, `values`.
#' @export
nlq_schema <- function(datasets, max_levels = 8) {
  if (is.null(datasets) || !length(datasets)) {
    return(list(subjects = character(0), domains = list()))
  }
  doms <- lapply(names(datasets), function(nm) {
    df <- datasets[[nm]]
    if (is.null(df) || !ncol(df)) return(NULL)
    cols <- lapply(names(df), function(cn) {
      v <- df[[cn]]
      type <- if (is.numeric(v)) "numeric" else "character"
      vals <- ""
      if (!is.numeric(v)) {
        u <- unique(stats::na.omit(as.character(v)))
        if (length(u) && length(u) <= max_levels) {
          vals <- paste(u, collapse = ", ")
        }
      }
      data.frame(column = cn, type = type, values = vals,
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, cols)
  })
  names(doms) <- names(datasets)
  doms <- doms[!vapply(doms, is.null, logical(1))]
  list(subjects = list_subjects(datasets), domains = doms)
}

# Coerce a scalar string value and a column vector into a logical keep-mask for
# one (column, op, value). Case-insensitive for text; numeric when both sides
# are numeric. NA comparisons are treated as FALSE (not kept).
#' @noRd
nlq_compare <- function(col, op, value) {
  n <- length(col)
  false <- rep(FALSE, n)
  if (!length(col)) return(logical(0))

  num_col <- suppressWarnings(as.numeric(col))
  num_val <- suppressWarnings(as.numeric(value))
  numeric_mode <- is.numeric(col) || (!all(is.na(num_col)) && !is.na(num_val))

  out <- switch(op,
    eq = if (numeric_mode && !is.na(num_val)) num_col == num_val
         else toupper(as.character(col)) == toupper(value),
    ne = if (numeric_mode && !is.na(num_val)) num_col != num_val
         else toupper(as.character(col)) != toupper(value),
    gt = num_col >  num_val,
    ge = num_col >= num_val,
    lt = num_col <  num_val,
    le = num_col <= num_val,
    contains = grepl(tolower(value), tolower(as.character(col)), fixed = TRUE),
    `in` = {
      parts <- toupper(trimws(strsplit(value, ",", fixed = TRUE)[[1]]))
      toupper(as.character(col)) %in% parts
    },
    false
  )
  out[is.na(out)] <- FALSE
  out
}

#' Validate a query plan against the loaded data
#'
#' Checks that the plan's action, domain, columns and operators all refer to
#' things that actually exist, before any execution.
#'
#' @param plan A query plan (named list), as produced by a planner.
#' @param datasets A named list of data frames.
#' @return A list with `ok` (logical), `errors` and `warnings` (character).
#' @export
nlq_validate_plan <- function(plan, datasets) {
  errors <- character(0)
  warnings <- character(0)
  add_e <- function(...) errors <<- c(errors, paste0(...))
  add_w <- function(...) warnings <<- c(warnings, paste0(...))

  action <- plan$action %||% "filter"
  if (!action %in% .nlq_actions) {
    add_e("Unknown action '", action, "'.")
    return(list(ok = FALSE, errors = errors, warnings = warnings))
  }

  if (action %in% .nlq_subject_actions) {
    sid <- plan$subject %||% ""
    if (!nzchar(sid)) {
      add_e("Action '", action, "' needs a subject id.")
    } else if (length(datasets) && !(sid %in% list_subjects(datasets))) {
      add_w("Subject '", sid, "' was not found in the loaded data.")
    }
    return(list(ok = length(errors) == 0, errors = errors, warnings = warnings))
  }

  # action == "filter"
  dom <- plan$domain %||% ""
  df <- if (nzchar(dom)) find_domain(datasets, dom) else NULL
  if (is.null(df)) {
    add_e("Domain '", dom, "' was not found in the loaded data.")
    return(list(ok = FALSE, errors = errors, warnings = warnings))
  }
  for (f in plan$filters %||% list()) {
    if (is.null(get_col(df, f$column %||% ""))) {
      add_e("Column '", f$column %||% "", "' is not in domain '", dom, "'.")
    }
    if (!(f$op %||% "") %in% .nlq_ops) {
      add_e("Operator '", f$op %||% "", "' is not supported.")
    }
  }
  for (cn in plan$columns %||% character(0)) {
    if (is.null(get_col(df, cn))) {
      add_e("Selected column '", cn, "' is not in domain '", dom, "'.")
    }
  }
  list(ok = length(errors) == 0, errors = errors, warnings = warnings)
}

# Deterministically run a validated filter plan. Pure data subsetting.
#' @noRd
nlq_execute_filter <- function(plan, datasets) {
  df <- find_domain(datasets, plan$domain)
  filters <- plan$filters %||% list()
  logic <- tolower(plan$logic %||% "and")
  if (length(filters)) {
    masks <- lapply(filters, function(f) {
      nlq_compare(get_col(df, f$column), f$op, as.character(f$value))
    })
    mask <- Reduce(if (logic == "or") `|` else `&`, masks)
    df <- df[mask, , drop = FALSE]
  }
  cols <- plan$columns %||% character(0)
  cols <- cols[!is.na(cols) & nzchar(cols)]
  if (length(cols)) {
    keep <- names(df)[tolower(names(df)) %in% tolower(cols)]
    if (length(keep)) df <- df[, keep, drop = FALSE]
  }
  rownames(df) <- NULL
  df
}

#' Execute a query plan deterministically
#'
#' Validates the plan, then runs it: a `filter` action subsets the named domain
#' with vectorised comparisons, and a subject action calls the matching
#' `subject_*` helper. Never evaluates model-supplied text as code.
#'
#' @param plan A query plan (named list).
#' @param datasets A named list of data frames.
#' @return A data frame of results (zero rows if nothing matches).
#' @export
nlq_execute_plan <- function(plan, datasets) {
  v <- nlq_validate_plan(plan, datasets)
  if (!v$ok) stop("Invalid query plan: ", paste(v$errors, collapse = " "))
  action <- plan$action %||% "filter"
  if (action == "filter") return(nlq_execute_filter(plan, datasets))

  sid <- plan$subject
  res <- switch(action,
    subject_timeline     = subject_timeline(datasets, sid),
    subject_overview     = subject_overview(datasets, sid),
    subject_demographics = subject_demographics(datasets, sid),
    subject_kpis = {
      k <- subject_kpis(datasets, sid)
      data.frame(Metric = names(k),
                 Value = vapply(k, function(x) as.character(x %||% NA), character(1)),
                 row.names = NULL, stringsAsFactors = FALSE)
    }
  )
  res
}

#' Generate reproducible R code for a query plan
#'
#' Emits the dplyr (filter action) or `databoxR::subject_*` (subject action)
#' call equivalent to the plan, so a result can be reproduced outside the app.
#'
#' @param plan A query plan (named list).
#' @return A single character string of R code.
#' @export
nlq_plan_code <- function(plan) {
  action <- plan$action %||% "filter"
  if (action %in% .nlq_subject_actions) {
    return(paste0("databoxR::", action, "(datasets, ",
                  r_literal(plan$subject %||% ""), ")"))
  }
  lines <- c("library(dplyr)", "")
  op_r <- c(eq = "==", ne = "!=", gt = ">", ge = ">=", lt = "<", le = "<=")
  conds <- vapply(plan$filters %||% list(), function(f) {
    col <- f$column
    val <- as.character(f$value)
    switch(f$op,
      contains = paste0("grepl(", r_literal(val), ", ", col, ", fixed = TRUE)"),
      `in`     = paste0(col, " %in% c(",
                        paste(vapply(trimws(strsplit(val, ",", fixed = TRUE)[[1]]),
                                     r_literal, character(1)), collapse = ", "), ")"),
      paste0(col, " ", op_r[[f$op]], " ",
             if (!is.na(suppressWarnings(as.numeric(val)))) val else r_literal(val))
    )
  }, character(1))
  pipe <- plan$domain
  if (length(conds)) {
    joiner <- if (tolower(plan$logic %||% "and") == "or") " | " else ", "
    pipe <- paste0(pipe, " |>\n  filter(", paste(conds, collapse = joiner), ")")
  }
  cols <- plan$columns %||% character(0)
  cols <- cols[!is.na(cols) & nzchar(cols)]
  if (length(cols)) {
    pipe <- paste0(pipe, " |>\n  select(", paste(cols, collapse = ", "), ")")
  }
  paste(c(lines, pipe), collapse = "\n")
}

#' Answer a natural-language question against loaded datasets
#'
#' Orchestrates the grounded pipeline: build a schema, ask a planner (an
#' injectable function, so this is testable without a network) for a query plan,
#' validate it, and execute it deterministically. The planner only proposes a
#' structured plan; all data access happens here in R.
#'
#' @param question A natural-language question (character).
#' @param datasets A named list of data frames.
#' @param propose A function of `(question, schema)` returning a query plan.
#'   Defaults to [nlq_propose_claude()], which uses the ellmer package.
#' @param ... Passed to `propose`.
#' @return A list: `plan`, `valid` (validation result), `result` (data frame or
#'   NULL), `code` (reproducible R) and `error` (character or NULL).
#' @export
nl_query <- function(question, datasets, propose = nlq_propose_claude, ...) {
  schema <- nlq_schema(datasets)
  out <- list(plan = NULL, valid = NULL, result = NULL, code = NULL, error = NULL)
  plan <- tryCatch(propose(question, schema, ...),
                   error = function(e) {
                     out$error <<- conditionMessage(e); NULL
                   })
  if (is.null(plan)) return(out)
  out$plan <- plan
  out$valid <- nlq_validate_plan(plan, datasets)
  if (!out$valid$ok) {
    out$error <- paste(out$valid$errors, collapse = " ")
    return(out)
  }
  out$code <- tryCatch(nlq_plan_code(plan), error = function(e) NULL)
  out$result <- tryCatch(nlq_execute_plan(plan, datasets),
                         error = function(e) {
                           out$error <<- conditionMessage(e); NULL
                         })
  out
}
