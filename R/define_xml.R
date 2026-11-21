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

  out <- dv
  out$variable <- sub("^.*\\.", "", dv$variable)

  # Resolve each dataset+variable to one var_spec row via a 1-to-1 match (never
  # a join), preferring a dataset-scoped "DATASET.VARIABLE" spec over a bare
  # "VARIABLE" spec. This keeps the right label/type when a variable is defined
  # differently across datasets, and cannot duplicate rows.
  scoped_key <- paste(out$dataset, out$variable, sep = ".")
  idx <- match(scoped_key, vs$variable)
  bare <- match(out$variable, vs$variable)
  idx[is.na(idx)] <- bare[is.na(idx)]
  for (col in intersect(c("type", "length", "label", "format"), names(vs))) {
    out[[col]] <- vs[[col]][idx]
  }

  # Value-level origin + codelist, matched 1-to-1 on dataset + bare variable.
  if (all(c("dataset", "variable") %in% names(val)) && nrow(val) > 0) {
    val$variable <- sub("^.*\\.", "", val$variable)
    val <- val[!duplicated(val[c("dataset", "variable")]), , drop = FALSE]
    vmatch <- match(paste(out$dataset, out$variable, sep = "."),
                    paste(val$dataset, val$variable, sep = "."))
    if ("origin" %in% names(val))  out$origin   <- val$origin[vmatch]
    if ("code_id" %in% names(val)) out$codelist <- val$code_id[vmatch]
  }

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
#' @noRd
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

#' Metadata-driven conformance checks against a define
#'
#' Produces a tidy, severity-ranked set of findings comparing a loaded dataset
#' to its define metadata:
#' \itemize{
#'   \item missing variables (Error if the define marks them mandatory,
#'         otherwise Warning) and unexpected variables not in the define (Info);
#'   \item variable label mismatches (Warning); and
#'   \item values that fall outside the define's controlled terminology (Error),
#'         via \code{metatools::get_bad_ct()}.
#' }
#'
#' @param data A loaded data.frame.
#' @param mc A \code{Metacore} object.
#' @param dataset The dataset name in the define to check against.
#' @return A data.frame with columns Check, Variable, Severity, Details.
#' @export
check_conformance <- function(data, mc, dataset) {
  findings <- list()
  add <- function(check, variable, severity, details) {
    findings[[length(findings) + 1L]] <<- data.frame(
      Check = check, Variable = variable, Severity = severity,
      Details = details, stringsAsFactors = FALSE
    )
  }

  meta <- define_var_metadata(mc, dataset)
  data_vars <- names(data)

  # 1. Variable presence
  for (v in setdiff(meta$variable, data_vars)) {
    m <- meta$mandatory[meta$variable == v][1]
    mand <- isTRUE(m) ||
      (is.character(m) && toupper(trimws(m)) %in% c("YES", "Y", "TRUE"))
    add("Missing variable", v,
        if (mand) "Error" else "Warning",
        if (mand) "Required by define but absent from data"
        else "Expected by define but absent from data")
  }
  for (v in setdiff(data_vars, meta$variable)) {
    add("Unexpected variable", v, "Info", "Present in data but not in define")
  }

  # 2. Label mismatches
  cmp <- compare_define_to_data(data, mc, dataset)
  mism <- cmp[!is.na(cmp$label_match) & !cmp$label_match, , drop = FALSE]
  for (i in seq_len(nrow(mism))) {
    add("Label mismatch", mism$variable[i], "Warning",
        sprintf("define: '%s' | data: '%s'",
                mism$expected_label[i], mism$actual_label[i]))
  }

  # 3. Controlled terminology (via metatools)
  if (requireNamespace("metatools", quietly = TRUE) &&
      requireNamespace("metacore", quietly = TRUE)) {
    ds_meta <- tryCatch(
      suppressWarnings(suppressMessages(metacore::select_dataset(mc, dataset))),
      error = function(e) NULL
    )
    if (!is.null(ds_meta)) {
      ct_vars <- intersect(meta$variable[!is.na(meta$codelist)], data_vars)
      for (v in ct_vars) {
        # get_bad_ct() uses non-standard evaluation on `var`; pass the value via
        # do.call so a loop variable resolves to the column name, not "v".
        bad <- tryCatch(
          suppressWarnings(suppressMessages(
            do.call(metatools::get_bad_ct, list(data, ds_meta, v))
          )),
          error = function(e) character(0)
        )
        bad <- unique(bad[!is.na(bad)])
        if (length(bad) > 0) {
          shown <- paste(utils::head(bad, 5), collapse = ", ")
          if (length(bad) > 5) shown <- paste0(shown, ", ...")
          add("Value not in controlled terminology", v, "Error",
              paste0("Unexpected values: ", shown))
        }
      }
    }
  }

  if (length(findings) == 0) {
    return(data.frame(
      Check = "All checks passed", Variable = NA_character_,
      Severity = "Success", Details = "No conformance issues detected",
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, findings)
  sev_rank <- c(Error = 1, Warning = 2, Info = 3, Success = 4)
  out <- out[order(sev_rank[out$Severity], out$Variable), , drop = FALSE]
  rownames(out) <- NULL
  out
}
