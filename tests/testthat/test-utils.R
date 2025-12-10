test_that("check_paths throws error for non-existent files", {
  expect_error(check_paths("non_existent_file.txt"), "File not found: non_existent_file.txt")
})
