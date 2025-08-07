#' Read SDTM/ADaM Datasets with Enhanced Features
#'
#' Reads SDTM/ADaM datasets from various formats (XPT, CSV, TXT) with optional
#' row limiting, column selection, and performance optimizations.
#'
#' @param path File path to the dataset
#' @param nrows Maximum number of rows to read (NULL for all rows)
#' @param cols Vector of column names or indices to select (NULL for all columns)
#' @param encoding Character encoding (default: "UTF-8")
#' @param validate_data Logical, whether to perform data validation (default: TRUE)
#' @param progress_callback Function to call with progress updates (optional)
#' @return A data.frame containing the dataset
#' @export
#' @examples
#' \dontrun{
#'   # Read all data
#'   data <- read_sdtm_adam("path/to/dataset.xpt")
#'   
#'   # Read first 1000 rows only
#'   data <- read_sdtm_adam("path/to/large_dataset.csv", nrows = 1000)
#'   
#'   # Read specific columns
#'   data <- read_sdtm_adam("path/to/dataset.xpt", cols = c("USUBJID", "AGE"))
#' }
read_sdtm_adam <- function(path, nrows = NULL, cols = NULL, encoding = "UTF-8", 
                          validate_data = TRUE, progress_callback = NULL) {
  # Simple input validation
  if (missing(path) || is.null(path) || path == "" || !file.exists(path)) {
    stop("Invalid or non-existent file path: ", path)
  }
  
  # Get file extension safely
  ext <- tolower(tools::file_ext(path))
  if (is.na(ext) || length(ext) == 0) ext <- ""
  
  # Get file size
  file_size <- tryCatch(file.size(path), error = function(e) 0)
  
  if (is.function(progress_callback)) {
    progress_callback("Starting file read...")
  }
  
  # Read data based on file type
  data <- tryCatch({
    if (ext == "xpt") {
      # XPT files
      if (is.function(progress_callback)) progress_callback("Reading XPT file...")
      result <- haven::read_xpt(path)
      if (!is.null(nrows) && nrows < nrow(result)) {
        result <- result[1:nrows, ]
      }
      if (!is.null(cols)) {
        cols_exist <- cols[cols %in% names(result)]
        if (length(cols_exist) > 0) {
          result <- result[, cols_exist, drop = FALSE]
        }
      }
      result
    } else {
      # CSV/TXT files - use simple approach
      if (is.function(progress_callback)) progress_callback("Reading delimited file...")
      
      # Determine delimiter
      if (ext == "txt") {
        # Simple check for tab-delimited
        first_line <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) "")
        delim <- if (grepl("\t", first_line)) "\t" else ","
      } else {
        delim <- ","
      }
      
      # Read file
      result <- readr::read_delim(
        path, 
        delim = delim,
        n_max = nrows,
        show_col_types = FALSE,
        locale = readr::locale(encoding = encoding),
        trim_ws = TRUE,
        name_repair = "minimal"
      )
      
      # Select columns if specified
      if (!is.null(cols)) {
        cols_exist <- cols[cols %in% names(result)]
        if (length(cols_exist) > 0) {
          result <- result[, cols_exist, drop = FALSE]
        }
      }
      
      result
    }
  }, error = function(e) {
    stop("Error reading file '", basename(path), "': ", e$message, call. = FALSE)
  })
  
  # Basic validation - only if explicitly requested and data exists
  if (validate_data && !is.null(data) && nrow(data) > 0) {
    tryCatch({
      # Simple validation without complex logic
      if (ncol(data) == 0) {
        warning("Dataset has no columns")
      } else if (nrow(data) == 0) {
        warning("Dataset has no rows")
      }
    }, error = function(e) {
      # Ignore validation errors
    })
  }
  
  # Add basic attributes
  if (!is.null(data)) {
    attr(data, "source_file") <- path
    attr(data, "read_timestamp") <- Sys.time()
    attr(data, "file_size") <- file_size
    attr(data, "nrows_read") <- nrow(data)
    
    if (is.function(progress_callback)) {
      progress_callback(paste("Successfully loaded", nrow(data), "rows,", ncol(data), "columns"))
    }
  }
  
  return(data)
}

#' Get Dataset Information with Enhanced Analysis
#'
#' Extract comprehensive information about a dataset including data quality metrics
#'
#' @param data A data.frame
#' @param detailed Logical, whether to include detailed analysis (default: FALSE)
#' @return A list with dataset information
get_dataset_info <- function(data, detailed = FALSE) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      nrows = 0, ncols = 0, variables = character(0),
      error = "No data available"
    ))
  }
  
  # Basic info (always computed)
  basic_info <- list(
    nrows = nrow(data),
    ncols = ncol(data),
    variables = names(data),
    variable_types = sapply(data, function(x) class(x)[1]),
    missing_counts = sapply(data, function(x) sum(is.na(x))),
    missing_percent = sapply(data, function(x) round(sum(is.na(x))/length(x) * 100, 2))
  )
  
  if (!detailed) {
    return(basic_info)
  }
  
  # Detailed analysis
  numeric_vars <- sapply(data, is.numeric)
  character_vars <- sapply(data, is.character)
  factor_vars <- sapply(data, is.factor)
  date_vars <- sapply(data, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")))
  
  detailed_info <- list(
    # Data type summary
    numeric_count = sum(numeric_vars),
    character_count = sum(character_vars),
    factor_count = sum(factor_vars),
    date_count = sum(date_vars),
    
    # Missing data analysis
    total_missing = sum(basic_info$missing_counts),
    missing_rate = round(sum(basic_info$missing_counts) / (nrow(data) * ncol(data)) * 100, 2),
    
    # Data quality flags
    duplicate_rows = sum(duplicated(data)),
    constant_columns = names(data)[sapply(data, function(x) length(unique(x[!is.na(x)])) <= 1)],
    high_missing_vars = names(data)[basic_info$missing_percent > 50],
    
    # Memory usage
    memory_size = object.size(data),
    
    # Sample statistics for numeric variables
    numeric_summary = if (sum(numeric_vars) > 0) {
      lapply(data[numeric_vars], function(x) {
        if (all(is.na(x))) {
          list(min = NA, max = NA, mean = NA, median = NA, sd = NA)
        } else {
          list(
            min = min(x, na.rm = TRUE),
            max = max(x, na.rm = TRUE), 
            mean = mean(x, na.rm = TRUE),
            median = median(x, na.rm = TRUE),
            sd = sd(x, na.rm = TRUE)
          )
        }
      })
    } else NULL
  )
  
  return(c(basic_info, detailed_info))
}

#' Get File Preview Information
#'
#' Quickly inspect file structure without loading entire dataset
#'
#' @param path File path to inspect
#' @param max_rows Maximum rows to sample (default: 10)
#' @return List with file structure information
#' @export
get_file_preview <- function(path, max_rows = 10) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path)
  }
  
  ext <- tolower(tools::file_ext(path))
  file_info <- file.info(path)
  
  tryCatch({
    preview_data <- read_sdtm_adam(path, nrows = max_rows, validate_data = FALSE)
    
    list(
      file_path = path,
      file_name = basename(path),
      file_size = file_info$size,
      file_extension = ext,
      estimated_rows = if (ext %in% c("csv", "txt")) {
        # Rough estimate based on file size and sample
        round(file_info$size / (object.size(preview_data) / max_rows))
      } else NA,
      sample_data = preview_data,
      column_names = names(preview_data),
      column_types = sapply(preview_data, function(x) class(x)[1]),
      readable = TRUE
    )
  }, error = function(e) {
    list(
      file_path = path,
      file_name = basename(path),
      file_size = file_info$size, 
      file_extension = ext,
      readable = FALSE,
      error_message = e$message
    )
  })
}
