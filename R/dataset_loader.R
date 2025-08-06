#' Read SDTM/ADaM Datasets
#'
#' Reads SDTM/ADaM datasets from various formats (XPT, CSV, TXT)
#'
#' @param path File path to the dataset
#' @return A data.frame containing the dataset
#' @export
#' @examples
#' \dontrun{
#'   data <- read_sdtm_adam("path/to/dataset.xpt")
#' }
read_sdtm_adam <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path)
  }
  
  ext <- tools::file_ext(path)
  
  tryCatch({
    if (ext == "xpt") {
      haven::read_xpt(path)
    } else if (ext %in% c("csv", "txt")) {
      readr::read_csv(path, show_col_types = FALSE)
    } else {
      stop("Unsupported format: ", ext, ". Supported formats: xpt, csv, txt")
    }
  }, error = function(e) {
    stop("Error reading file ", path, ": ", e$message)
  })
}

#' Get Dataset Information
#'
#' Extract basic information about a dataset
#'
#' @param data A data.frame
#' @return A list with dataset information
get_dataset_info <- function(data) {
  list(
    nrows = nrow(data),
    ncols = ncol(data),
    variables = names(data),
    variable_types = sapply(data, class),
    missing_counts = sapply(data, function(x) sum(is.na(x))),
    missing_percent = sapply(data, function(x) round(sum(is.na(x))/length(x) * 100, 2))
  )
}
