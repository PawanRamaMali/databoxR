read_sdtm_adam <- function(path) {
  ext <- tools::file_ext(path)
  if (ext == "xpt") {
    haven::read_xpt(path)
  } else if (ext %in% c("csv", "txt")) {
    readr::read_csv(path)
  } else {
    stop("Unsupported format")
  }
}
