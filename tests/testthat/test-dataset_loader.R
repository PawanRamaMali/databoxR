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

test_that("read_sdtm_adam handles nrows parameter", {
  # Create a temporary CSV file with more data
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    USUBJID = paste0("S", sprintf("%03d", 1:100)),
    AGE = sample(18:80, 100, replace = TRUE),
    SEX = sample(c("M", "F"), 100, replace = TRUE)
  )
  write.csv(test_data, temp_csv, row.names = FALSE)
  on.exit(unlink(temp_csv))
  
  # Test reading limited rows
  result <- read_sdtm_adam(temp_csv, nrows = 10)
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 3)
})

test_that("read_sdtm_adam handles column selection", {
  # Create a temporary CSV file
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    USUBJID = c("001", "002", "003"),
    AGE = c(25, 30, 35),
    SEX = c("M", "F", "M"),
    WEIGHT = c(70, 65, 80)
  )
  write.csv(test_data, temp_csv, row.names = FALSE)
  on.exit(unlink(temp_csv))
  
  # Test column selection by name
  result <- read_sdtm_adam(temp_csv, cols = c("USUBJID", "AGE"))
  expect_equal(ncol(result), 2)
  expect_true(all(c("USUBJID", "AGE") %in% names(result)))
})

test_that("read_sdtm_adam handles empty path parameter", {
  expect_error(read_sdtm_adam(""), "Path parameter is required")
  expect_error(read_sdtm_adam(NULL), "Path parameter is required")
})

test_that("get_dataset_info handles detailed analysis", {
  test_data <- data.frame(
    A = 1:10,
    B = c(letters[1:5], letters[1:5]),  # Some duplicates
    C = c(1.1, 2.2, NA, 4.4, 5.5, 6.6, 7.7, NA, 9.9, 10.1),
    D = rep("constant", 10)  # Constant column
  )
  
  info <- get_dataset_info(test_data, detailed = TRUE)
  
  expect_equal(info$nrows, 10)
  expect_equal(info$ncols, 4)
  expect_equal(info$numeric_count, 2)  # A and C are numeric
  expect_equal(info$character_count, 2)  # B and D are character
  expect_true("D" %in% info$constant_columns)
  expect_equal(length(info$high_missing_vars), 0)  # No vars > 50% missing
})

test_that("get_dataset_info handles empty data", {
  info <- get_dataset_info(NULL)
  expect_equal(info$nrows, 0)
  expect_equal(info$ncols, 0)
  expect_true("error" %in% names(info))
  
  empty_df <- data.frame()
  info2 <- get_dataset_info(empty_df)
  expect_equal(info2$nrows, 0)
})

test_that("get_file_preview works correctly", {
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    USUBJID = paste0("S", sprintf("%03d", 1:50)),
    AGE = sample(18:80, 50, replace = TRUE)
  )
  write.csv(test_data, temp_csv, row.names = FALSE)
  on.exit(unlink(temp_csv))
  
  preview <- get_file_preview(temp_csv, max_rows = 5)
  
  expect_true(preview$readable)
  expect_equal(nrow(preview$sample_data), 5)
  expect_true("USUBJID" %in% preview$column_names)
  expect_equal(preview$file_extension, "csv")
})