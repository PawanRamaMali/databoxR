test_that("validate_dataset works correctly", {
  # Test with valid data
  valid_data <- data.frame(
    A = 1:5,
    B = letters[1:5],
    C = c(1.1, 2.2, 3.3, 4.4, 5.5)
  )
  
  validation <- validate_dataset(valid_data)
  expect_true(validation$valid)
  expect_equal(validation$missing_percent, 0)
  
  # Test with NULL data
  null_validation <- validate_dataset(NULL)
  expect_false(null_validation$valid)
  expect_equal(null_validation$message, "No dataset provided")
  
  # Test with empty data frame
  empty_df <- data.frame()
  empty_validation <- validate_dataset(empty_df)
  expect_false(empty_validation$valid)
  
  # Test with high missing data
  missing_data <- data.frame(
    A = c(1, NA, NA, NA, NA),
    B = c(NA, NA, NA, NA, 5)
  )
  missing_validation <- validate_dataset(missing_data)
  expect_true(missing_validation$valid)
  expect_true(missing_validation$missing_percent > 50)
})

test_that("safe_select_columns works correctly", {
  test_data <- data.frame(
    A = 1:5,
    B = letters[1:5],
    C = c(1.1, 2.2, 3.3, 4.4, 5.5),
    D = LETTERS[1:5]
  )
  
  # Test selecting existing columns by name
  result1 <- safe_select_columns(test_data, c("A", "C"))
  expect_equal(ncol(result1), 2)
  expect_true(all(c("A", "C") %in% names(result1)))
  
  # Test selecting columns by index
  result2 <- safe_select_columns(test_data, c(1, 3))
  expect_equal(ncol(result2), 2)
  expect_equal(names(result2), c("A", "C"))
  
  # Test selecting non-existent columns (should fall back to default)
  expect_warning(result3 <- safe_select_columns(test_data, c("X", "Y")))
  expect_true(ncol(result3) > 0)  # Should return default columns
  
  # Test invalid indices
  expect_warning(result4 <- safe_select_columns(test_data, c(10, 20)))
  expect_true(ncol(result4) > 0)  # Should return default columns
})

test_that("DataCache works correctly", {
  cache <- DataCache$new(max_size = 3, max_memory = 1024^2)  # 1MB
  
  # Test basic cache operations
  test_data <- data.frame(A = 1:100, B = letters[1:100])
  key1 <- "test_key_1"
  
  # Test set and get
  expect_true(cache$set(key1, test_data))
  expect_true(cache$has(key1))
  cached_data <- cache$get(key1)
  expect_equal(nrow(cached_data), 100)
  
  # Test cache statistics
  stats <- cache$get_stats()
  expect_equal(stats$items, 1)
  expect_true(stats$memory_mb > 0)
  
  # Test clear
  cache$clear()
  expect_false(cache$has(key1))
  expect_equal(cache$get_stats()$items, 0)
})

test_that("format_file_size works correctly", {
  expect_equal(format_file_size(0), "0 B")
  expect_equal(format_file_size(1023), "1023 B")
  expect_equal(format_file_size(1024), "1 KB")
  expect_equal(format_file_size(1024^2), "1 MB")
  expect_equal(format_file_size(1024^3), "1 GB")
  
  # Test with NA
  expect_equal(format_file_size(NA), "0 B")
})

test_that("null coalescing operator works correctly", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% "default", 0)
  expect_equal(FALSE %||% "default", FALSE)
})

test_that("safe_execute works correctly", {
  # Test successful execution
  result1 <- safe_execute(1 + 1, default = 0)
  expect_equal(result1, 2)
  
  # Test error handling
  expect_warning(result2 <- safe_execute(stop("test error"), default = -1))
  expect_equal(result2, -1)
  
  # Test with custom error message
  expect_warning(
    result3 <- safe_execute(stop("test"), default = 0, error_message = "Custom error"),
    "Custom error"
  )
  expect_equal(result3, 0)
})

test_that("read_with_cache works correctly", {
  # Create test file
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    USUBJID = c("001", "002", "003"),
    AGE = c(25, 30, 35)
  )
  write.csv(test_data, temp_csv, row.names = FALSE)
  on.exit(unlink(temp_csv))
  
  # Clear cache first
  cache <- get_global_cache()
  cache$clear()
  
  # First read should load from file
  result1 <- read_with_cache(temp_csv)
  expect_equal(nrow(result1), 3)
  
  # Second read should use cache
  expect_message(result2 <- read_with_cache(temp_csv), "Using cached data")
  expect_equal(nrow(result2), 3)
  
  # Test without cache
  result3 <- read_with_cache(temp_csv, use_cache = FALSE)
  expect_equal(nrow(result3), 3)
})