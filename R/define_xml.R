# Define.xml support (Phase 2)
#
# A thin layer over the pharmaverse `metacore` package. Parsing a define.xml is
# delegated to metacore (kept in Suggests and guarded), but everything that
# reads the resulting object back out - the data-dictionary view and the
# "expected vs actual" smart hints - is plain base R so it needs no heavy deps
# and is unit-testable from a parsed object.

#' Load a define.xml into a metacore object
#'
#' Wraps \code{metacore::define_to_metacore()}. The define.xml is parsed into a
#' tidy metadata object (datasets, variables, value-level metadata, codelists,
#' derivations) that the other functions here read from.
#'
#' @param path Path to a define.xml file.
#' @param verbose Passed to metacore; one of "silent", "message", "warn",
#'   "collapse" (default "silent").
#' @return A \code{Metacore} object.
#' @export
load_define <- function(path, verbose = "silent") {
  if (missing(path) || is.null(path) || length(path) == 0 || !nzchar(path)) {
    stop("Path parameter is required", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }
  if (!tolower(tools::file_ext(path)) %in% c("xml")) {
    stop("A define.xml file (.xml) is required, got: ", basename(path), call. = FALSE)
  }
  if (!requireNamespace("metacore", quietly = TRUE)) {
    stop("The 'metacore' package is required to read define.xml. ",
         "Install it with install.packages(\"metacore\").", call. = FALSE)
  }
  metacore::define_to_metacore(path, verbose = verbose)
}

#' Dataset-level metadata from a define
#'
#' @param mc A \code{Metacore} object (from \code{load_define}).
#' @return A data.frame with one row per dataset: dataset, label, structure.
#' @export
define_datasets <- function(mc) {
  ds <- as.data.frame(mc$ds_spec, stringsAsFactors = FALSE)
  keep <- intersect(c("dataset", "label", "structure"), names(ds))
  ds[, keep, drop = FALSE]
}

#' Variable-level metadata (data dictionary) from a define
#'
#' Joins the define's variable, value-level and dataset-variable tables into a
#' single per-dataset data dictionary.
#'
#' @param mc A \code{Metacore} object.
#' @param dataset Optional dataset name to filter to.
#' @return A data.frame: dataset, variable, order, label, type, length,
#'   mandatory, origin, codelist.
#' @export
define_var_metadata <- function(mc, dataset = NULL) {
  dv <- as.data.frame(mc$ds_vars, stringsAsFactors = FALSE)
  vs <- as.data.frame(mc$var_spec, stringsAsFactors = FALSE)
  val <- as.data.frame(mc$value_spec, stringsAsFactors = FALSE)

  # Strip any "DATASET.VARIABLE" scoping so joins line up on the bare name.
  dv$variable <- sub("^.*\\.", "", dv$variable)
  vs$variable <- sub("^.*\\.", "", vs$variable)

  out <- merge(dv, vs, by = "variable", all.x = TRUE, sort = FALSE)

  # One value-spec row per dataset+variable (origin + codelist id).
  if (all(c("dataset", "variable") %in% names(val)) && nrow(val) > 0) {
    val$variable <- sub("^.*\\.", "", val$variable)
    val_small <- val[!duplicated(val[c("dataset", "variable")]),
                     intersect(c("dataset", "variable", "origin", "code_id"), names(val)),
                     drop = FALSE]
    out <- merge(out, val_small, by = c("dataset", "variable"), all.x = TRUE, sort = FALSE)
  }

  names(out)[names(out) == "code_id"] <- "codelist"
  if ("order" %in% names(out)) out <- out[order(out$dataset, out$order), , drop = FALSE]

  keep <- intersect(
    c("dataset", "variable", "order", "label", "type", "length",
      "mandatory", "origin", "codelist"),
    names(out)
  )
  out <- out[, keep, drop = FALSE]

  if (!is.null(dataset)) out <- out[out$dataset == dataset, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Human-readable type of a loaded data column
#'
#' @param x A vector (a data.frame column).
#' @return One of "date", "numeric", "logical", "text".
#' @keywords internal
describe_r_type <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) "date"
  else if (is.numeric(x)) "numeric"
  else if (is.logical(x)) "logical"
  else "text"
}

#' Compare a loaded dataset against its define metadata (smart hints)
#'
#' For each variable present in the data and/or the define, reports whether it
#' appears in each, and lays the expected (define) label and type next to the
#' actual (data) label and type, flagging label mismatches.
#'
#' @param data A loaded data.frame (e.g. from \code{read_sdtm_adam}).
#' @param mc A \code{Metacore} object.
#' @param dataset The dataset name in the define to compare against.
#' @return A data.frame, one row per variable, with comparison columns.
#' @export
compare_define_to_data <- function(data, mc, dataset) {
  meta <- define_var_metadata(mc, dataset)

  actual_vars <- names(data)
  actual_labels <- vapply(data, function(x) {
    l <- attr(x, "label", exact = TRUE)
    if (is.null(l)) NA_character_ else as.character(l)[1]
  }, character(1))
  actual_types <- vapply(data, describe_r_type, character(1))

  all_vars <- union(meta$variable, actual_vars)

  mi <- match(all_vars, meta$variable)
  ai <- match(all_vars, actual_vars)

  exp_label <- meta$label[mi]
  act_label <- actual_labels[ai]

  label_match <- ifelse(is.na(exp_label) | is.na(act_label), NA,
                        trimws(exp_label) == trimws(act_label))

  res <- data.frame(
    variable       = all_vars,
    in_define      = all_vars %in% meta$variable,
    in_data        = all_vars %in% actual_vars,
    expected_label = exp_label,
    actual_label   = act_label,
    label_match    = label_match,
    expected_type  = meta$type[mi],
    actual_type    = actual_types[ai],
    stringsAsFactors = FALSE
  )
  rownames(res) <- NULL
  res
}
