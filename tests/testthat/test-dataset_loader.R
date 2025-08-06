test_that("read_sdtm_adam handles file not found", {
  expect_error(read_sdtm_adam("nonexistent_file.xpt"), "File does not exist")
})

test_that("read_sdtm_adam handles unsupported format", {
  # Create a temporary file with unsupported extension
  temp_file <- tempfile(fileext = ".doc")
  file.create(temp_file)
  on.exit(unlink(temp_file))
  
  expect_error(read_sdtm_adam(temp_file), "Unsupported format")
})

test_that("get_dataset_info returns correct structure", {
  # Create test data
  test_data <- data.frame(
    A = 1:5,
    B = c("a", "b", "c", "d", "e"),
    C = c(1.1, 2.2, NA, 4.4, 5.5)
  )
  
  info <- get_dataset_info(test_data)
  
  expect_equal(info$nrows, 5)
  expect_equal(info$ncols, 3)
  expect_equal(info$variables, c("A", "B", "C"))
  expect_equal(info$missing_counts[["C"]], 1)
  expect_equal(info$missing_percent[["C"]], 20)
})

test_that("read_sdtm_adam works with CSV files", {
  # Create a temporary CSV file
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(USUBJID = c("001", "002"), AGE = c(25, 30))
  write.csv(test_data, temp_csv, row.names = FALSE)
  on.exit(unlink(temp_csv))
  
  result <- read_sdtm_adam(temp_csv)
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
  expect_true("USUBJID" %in% names(result))
})