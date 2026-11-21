# Reproducible code generation
#
# Pure, Shiny-free helpers that turn the Explorer's current load / filter /
# column-selection state into runnable R code. Keeping these free of any Shiny
# dependency makes the riskiest logic unit-testable without launching the app,
# and lets the same generator feed the preview panel, the "Download R script"
# export, and (later) the define.xml / Dataset-JSON load lines.

#' Render an R value as a source-code literal
#'
#' Converts an R value into a string of valid R source that reproduces it.
#' Character values are quoted and escaped, logicals become TRUE/FALSE, numbers
#' are printed without scientific notation, and length > 1 values are wrapped in
#' c(). Used to splice user-supplied filter values safely into generated code.
#'
#' @param v A length >= 1 atomic vector (character, numeric, integer, or logical).
#' @return A single string of R source code.
#' @keywords internal
#' @noRd
r_literal <- function(v) {
  if (length(v) == 0) {
    return("character(0)")
  }

  if (is.character(v) || is.factor(v)) {
    lit <- encodeString(as.character(v), quote = "\"")
    lit[is.na(v)] <- "NA_character_"
  } else if (is.logical(v)) {
    lit <- ifelse(is.na(v), "NA", ifelse(v, "TRUE", "FALSE"))
  } else {
    # numeric / integer
    lit <- vapply(v, num_literal, character(1))
  }

  if (length(lit) == 1) lit else paste0("c(", paste(lit, collapse = ", "), ")")
}

#' Shortest decimal literal that round-trips a numeric value exactly
#'
#' \code{format()}'s default of 7 significant digits silently rounds values such
#' as pi, so generated code would filter on a different number than the app. This
#' finds the fewest significant digits (15-17) whose text parses back to exactly
#' the same double, keeping output readable while reproducible.
#'
#' @param x A length-1 numeric value.
#' @return A single string of R source code.
#' @keywords internal
#' @noRd
num_literal <- function(x) {
  if (is.na(x)) return("NA")
  for (d in 15:17) {
    s <- format(x, digits = d, scientific = FALSE, trim = TRUE)
    if (isTRUE(as.numeric(s) == x)) return(s)
  }
  format(x, digits = 17, scientific = FALSE, trim = TRUE)
}

#' Backtick a column name only when it is not a syntactic R name
#'
#' @param x A single column name.
#' @return The name, wrapped in backticks if it would otherwise be non-syntactic.
#' @keywords internal
#' @noRd
backtick_if_needed <- function(x) {
  if (identical(make.names(x), x)) x else paste0("`", x, "`")
}

#' Render a single filter specification as a dplyr::filter() condition
#'
#' A filter spec is a list with elements:
#'   \itemize{
#'     \item \code{var}  - column name (character scalar)
#'     \item \code{op}   - one of "==", "!=", ">", ">=", "<", "<=", "\%in\%",
#'                         "between", "is.na", "not.na", "contains"
#'     \item \code{value}- the comparison value(s); length-2 for "between",
#'                         ignored for "is.na"/"not.na"
#'   }
#'
#' @param f A filter spec list.
#' @return A single condition string, or "" if the spec is incomplete.
#' @keywords internal
#' @noRd
format_filter_condition <- function(f) {
  if (is.null(f) || is.null(f$var) || !nzchar(f$var)) return("")
  var <- backtick_if_needed(f$var)
  op <- f$op %||% "=="

  # normalise aliases
  op <- switch(op,
    "isna" = , "is_na" = "is.na",
    "notna" = , "not_na" = , "!is.na" = "not.na",
    "in" = "%in%",
    op
  )

  switch(op,
    "is.na"    = paste0("is.na(", var, ")"),
    "not.na"   = paste0("!is.na(", var, ")"),
    "%in%"     = paste0(var, " %in% ", r_literal(f$value)),
    "between"  = {
      if (length(f$value) < 2) return("")
      paste0("dplyr::between(", var, ", ", r_literal(f$value[1]),
             ", ", r_literal(f$value[2]), ")")
    },
    "contains" = paste0("stringr::str_detect(", var, ", ", r_literal(f$value), ")"),
    # binary comparison operators
    "==" = , "!=" = , ">" = , ">=" = , "<" = , "<=" =
      paste0(var, " ", op, " ", r_literal(f$value)),
    # unknown operator -> drop it rather than emit invalid code
    ""
  )
}

#' Build a dplyr pipeline string from filter and column-selection state
#'
#' @param dataset_name Name of the data object the pipeline starts from.
#' @param filters A list of filter specs (see \code{format_filter_condition}).
#' @param cols Character vector of columns to keep (NULL/empty = keep all).
#' @return A single string. When there are no filters and no selection, just
#'   \code{dataset_name} is returned (no pipeline).
#' @keywords internal
#' @noRd
build_dplyr_expr <- function(dataset_name = "data", filters = NULL, cols = NULL) {
  steps <- character(0)

  if (length(filters)) {
    conds <- vapply(filters, format_filter_condition, character(1))
    conds <- conds[nzchar(conds)]
    if (length(conds)) {
      steps <- c(steps, paste0("dplyr::filter(", paste(conds, collapse = ", "), ")"))
    }
  }

  if (length(cols)) {
    sel <- paste(vapply(cols, backtick_if_needed, character(1)), collapse = ", ")
    steps <- c(steps, paste0("dplyr::select(", sel, ")"))
  }

  if (!length(steps)) return(dataset_name)

  paste0(dataset_name, " |>\n  ", paste(steps, collapse = " |>\n  "))
}

#' Build the data-loading line of a reproducible script
#'
#' @param path File path to the dataset.
#' @param dataset_name Name to assign the loaded data to.
#' @return An assignment string, e.g. \code{data <- read_sdtm_adam("dm.xpt")}.
#' @keywords internal
#' @noRd
build_load_expr <- function(path, dataset_name = "data") {
  paste0(dataset_name, " <- read_sdtm_adam(", r_literal(path), ")")
}

#' Assemble a complete, runnable reproducible R script
#'
#' @param load_expr Load line from \code{build_load_expr}.
#' @param transform_expr Pipeline string from \code{build_dplyr_expr}, or NULL /
#'   the bare dataset name when nothing is transformed.
#' @param plot_expr Optional plotting code string.
#' @param dataset_name Name of the data object (used for the transform assignment).
#' @param libraries Character vector of packages to emit \code{library()} calls for.
#' @return A single multi-line string.
#' @keywords internal
#' @noRd
build_full_script <- function(load_expr, transform_expr = NULL, plot_expr = NULL,
                              dataset_name = "data", libraries = "databoxR") {
  lines <- character(0)

  if (length(libraries)) {
    lines <- c(lines, vapply(libraries, function(p) paste0("library(", p, ")"),
                             character(1)), "")
  }

  lines <- c(lines, "# Load dataset", load_expr)

  has_transform <- !is.null(transform_expr) &&
    nzchar(transform_expr) &&
    !identical(transform_expr, dataset_name)

  if (has_transform) {
    lines <- c(lines, "", "# Apply filters and column selection",
               paste0(dataset_name, " <- ", transform_expr))
  }

  if (!is.null(plot_expr) && nzchar(plot_expr)) {
    lines <- c(lines, "", "# Visualize", plot_expr)
  }

  paste(lines, collapse = "\n")
}
