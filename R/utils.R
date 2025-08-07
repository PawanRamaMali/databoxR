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
  
  # Check for all NA columns with error handling
  tryCatch({
    all_na_cols <- sapply(data, function(x) all(is.na(x)))
    all_na_cols[is.na(all_na_cols)] <- FALSE  # Replace NA with FALSE
    if (any(all_na_cols)) {
      issues <- c(issues, paste("Columns with all NA values:", 
                               paste(names(data)[all_na_cols], collapse = ", ")))
    }
  }, error = function(e) {
    # Skip this check if it fails
  })
  
  # Check for constant columns with error handling
  tryCatch({
    constant_cols <- sapply(data, function(x) {
      unique_vals <- unique(x[!is.na(x)])
      length(unique_vals) <= 1
    })
    constant_cols[is.na(constant_cols)] <- FALSE  # Replace NA with FALSE
    if (any(constant_cols)) {
      issues <- c(issues, paste("Constant columns:", 
                               paste(names(data)[constant_cols], collapse = ", ")))
    }
  }, error = function(e) {
    # Skip this check if it fails
  })
  
  # Check for excessive missing data with error handling
  missing_pct <- tryCatch({
    sum(is.na(data)) / (nrow(data) * ncol(data)) * 100
  }, error = function(e) 0)
  
  if (!is.na(missing_pct) && missing_pct > 50) {
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

#' Simple Data Cache for Performance
#'
#' Implements a simple in-memory cache for datasets to improve performance
#'
#' @export
DataCache <- R6::R6Class("DataCache",
  public = list(
    #' @field cache_store Internal cache storage
    cache_store = NULL,
    
    #' @field max_size Maximum number of datasets to cache
    max_size = 10,
    
    #' @field max_memory Maximum memory usage (in bytes)
    max_memory = 500 * 1024^2, # 500MB
    
    #' Initialize the cache
    #' @param max_size Maximum number of cached items
    #' @param max_memory Maximum memory usage in bytes
    initialize = function(max_size = 10, max_memory = 500 * 1024^2) {
      self$max_size <- max_size
      self$max_memory <- max_memory
      self$cache_store <- list()
    },
    
    #' Get cache key for a dataset
    #' @param path File path
    #' @param nrows Number of rows parameter
    #' @param cols Columns parameter
    get_cache_key = function(path, nrows = NULL, cols = NULL) {
      # Include file modification time in key for cache invalidation
      file_mtime <- file.info(path)$mtime
      paste(path, file_mtime, nrows %||% "all", 
            paste(cols %||% "all", collapse = ","), sep = "|")
    },
    
    #' Check if data is cached
    #' @param key Cache key
    has = function(key) {
      key %in% names(self$cache_store)
    },
    
    #' Get cached data
    #' @param key Cache key
    get = function(key) {
      if (self$has(key)) {
        item <- self$cache_store[[key]]
        item$last_accessed <- Sys.time()
        return(item$data)
      }
      NULL
    },
    
    #' Cache data
    #' @param key Cache key
    #' @param data Data to cache
    set = function(key, data) {
      # Check memory constraints
      data_size <- as.numeric(object.size(data))
      
      if (data_size > self$max_memory / 2) {
        message("Data too large for cache (>", round(self$max_memory / 2 / 1024^2, 1), "MB)")
        return(FALSE)
      }
      
      # Remove old items if needed
      self$cleanup_cache()
      
      # Add new item
      self$cache_store[[key]] <- list(
        data = data,
        size = data_size,
        created = Sys.time(),
        last_accessed = Sys.time()
      )
      
      # Cleanup if cache is still too large
      current_memory <- self$get_cache_memory()
      if (current_memory > self$max_memory) {
        self$cleanup_by_memory()
      }
      
      TRUE
    },
    
    #' Get current cache memory usage
    get_cache_memory = function() {
      sum(sapply(self$cache_store, function(x) x$size))
    },
    
    #' Cleanup cache by size limit
    cleanup_cache = function() {
      if (length(self$cache_store) >= self$max_size) {
        # Remove oldest accessed items
        access_times <- sapply(self$cache_store, function(x) x$last_accessed)
        oldest_key <- names(self$cache_store)[which.min(access_times)]
        self$cache_store[[oldest_key]] <- NULL
      }
    },
    
    #' Cleanup cache by memory usage
    cleanup_by_memory = function() {
      while (self$get_cache_memory() > self$max_memory && length(self$cache_store) > 0) {
        access_times <- sapply(self$cache_store, function(x) x$last_accessed)
        oldest_key <- names(self$cache_store)[which.min(access_times)]
        self$cache_store[[oldest_key]] <- NULL
      }
    },
    
    #' Clear all cached data
    clear = function() {
      self$cache_store <- list()
      gc() # Trigger garbage collection
    },
    
    #' Get cache statistics
    get_stats = function() {
      list(
        items = length(self$cache_store),
        memory_mb = round(self$get_cache_memory() / 1024^2, 2),
        max_memory_mb = round(self$max_memory / 1024^2, 2),
        max_items = self$max_size
      )
    }
  )
)

#' Global cache instance
.global_cache <- NULL

#' Get or create global cache instance
#' @return DataCache instance
get_global_cache <- function() {
  if (is.null(.global_cache)) {
    .global_cache <<- DataCache$new()
  }
  .global_cache
}

#' Enhanced file reader with caching
#'
#' Wrapper around read_sdtm_adam with caching support
#'
#' @param path File path
#' @param use_cache Logical, whether to use caching (default: TRUE)
#' @param ... Additional arguments passed to read_sdtm_adam
#' @return data.frame
#' @export
read_with_cache <- function(path, use_cache = TRUE, ...) {
  if (!use_cache) {
    return(read_sdtm_adam(path, ...))
  }
  
  cache <- get_global_cache()
  key <- cache$get_cache_key(path, ...)
  
  # Try to get from cache
  cached_data <- cache$get(key)
  if (!is.null(cached_data)) {
    message("Using cached data for ", basename(path))
    return(cached_data)
  }
  
  # Load and cache
  data <- read_sdtm_adam(path, ...)
  cache$set(key, data)
  
  return(data)
}

#' Null coalescing operator
#' @param x First value
#' @param y Second value (used if x is NULL)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Safely execute function with error handling
#'
#' @param expr Expression to execute
#' @param default Default value to return on error
#' @param error_message Custom error message
#' @return Result of expression or default value
safe_execute <- function(expr, default = NULL, error_message = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      if (!is.null(error_message)) {
        warning(error_message, ": ", e$message)
      } else {
        warning("Error in safe_execute: ", e$message)
      }
      default
    }
  )
}

#' Format file size for display
#'
#' @param size_bytes File size in bytes
#' @return Formatted size string
format_file_size <- function(size_bytes) {
  if (is.na(size_bytes) || size_bytes == 0) return("0 B")
  
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- 1
  size <- size_bytes
  
  while (size >= 1024 && i < length(units)) {
    size <- size / 1024
    i <- i + 1
  }
  
  paste(round(size, 1), units[i])
}