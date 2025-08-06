#' Validate Dataset for Analysis
#'
#' Check if a dataset is suitable for various types of analysis
#'
#' @param data A data.frame to validate
#' @return A list with validation results
validate_dataset <- function(data) {
  if (is.null(data)) {
    return(list(
      valid = FALSE,
      message = "No dataset provided",
      issues = "Dataset is NULL"
    ))
  }
  
  if (!is.data.frame(data)) {
    return(list(
      valid = FALSE,
      message = "Invalid data format",
      issues = "Data is not a data.frame"
    ))
  }
  
  if (nrow(data) == 0) {
    return(list(
      valid = FALSE,
      message = "Dataset is empty",
      issues = "No rows in dataset"
    ))
  }
  
  if (ncol(data) == 0) {
    return(list(
      valid = FALSE,
      message = "Dataset has no columns",
      issues = "No columns in dataset"
    ))
  }
  
  # Check for basic data quality issues
  issues <- c()
  
  # Check for all NA columns
  all_na_cols <- sapply(data, function(x) all(is.na(x)))
  if (any(all_na_cols)) {
    issues <- c(issues, paste("Columns with all NA values:", 
                             paste(names(data)[all_na_cols], collapse = ", ")))
  }
  
  # Check for constant columns
  constant_cols <- sapply(data, function(x) length(unique(x[!is.na(x)])) <= 1)
  if (any(constant_cols)) {
    issues <- c(issues, paste("Constant columns:", 
                             paste(names(data)[constant_cols], collapse = ", ")))
  }
  
  # Check for excessive missing data
  missing_pct <- sum(is.na(data)) / (nrow(data) * ncol(data)) * 100
  if (missing_pct > 50) {
    issues <- c(issues, paste("High missing data:", round(missing_pct, 1), "%"))
  }
  
  list(
    valid = TRUE,
    message = "Dataset validation successful",
    issues = if (length(issues) > 0) issues else "No significant issues detected",
    missing_percent = round(missing_pct, 2),
    numeric_vars = sum(sapply(data, is.numeric)),
    character_vars = sum(sapply(data, is.character))
  )
}

#' Safe Column Selection
#'
#' Safely select columns from a dataset with validation
#'
#' @param data A data.frame
#' @param cols Column names or indices
#' @param default_cols Default columns to return if selection fails
#' @return Selected columns as data.frame
safe_select_columns <- function(data, cols, default_cols = names(data)[1:min(5, ncol(data))]) {
  tryCatch({
    if (is.character(cols)) {
      # Check if columns exist
      existing_cols <- cols[cols %in% names(data)]
      if (length(existing_cols) == 0) {
        warning("None of the specified columns exist, using default columns")
        return(data[, default_cols, drop = FALSE])
      }
      return(data[, existing_cols, drop = FALSE])
    } else if (is.numeric(cols)) {
      # Check if indices are valid
      valid_indices <- cols[cols > 0 & cols <= ncol(data)]
      if (length(valid_indices) == 0) {
        warning("No valid column indices, using default columns")
        return(data[, default_cols, drop = FALSE])
      }
      return(data[, valid_indices, drop = FALSE])
    }
  }, error = function(e) {
    warning(paste("Error selecting columns:", e$message, "- using default columns"))
    return(data[, default_cols, drop = FALSE])
  })
}