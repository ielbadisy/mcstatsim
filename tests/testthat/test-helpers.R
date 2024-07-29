library(testthat)

# Test 1: input is not a list
test_that("Error is raised for non-list input", {
  expect_error(combine_df("not a list"), "The input must be a non-empty list of lists containing dataframes.")
})

# Test 2: input is an empty list
test_that("Error is raised for empty list input", {
  expect_error(combine_df(list()), "The input must be a non-empty list of lists containing dataframes.")
})

# Test 3: input contains non-dataframe elements
test_that("Error is raised for non-dataframe elements", {
  incorrect_list <- list(list(data.frame(a = 1), "not a dataframe"))
  expect_error(combine_df(incorrect_list), "Each item in the nested list must be a list of dataframes.")
})

# Test 4: single sublist with multiple data frames
test_that("Single sublist with multiple data frames is processed correctly", {
  df1 <- data.frame(a = 1:3, b = letters[1:3])
  df2 <- data.frame(a = 4:6, b = letters[4:6])
  combined <- combine_df(list(list(df1, df2)))
  expect_true(all(combined$ID == 1))
  expect_equal(nrow(combined), 6)
})

# Test 5: multiple sublists are processed correctly
test_that("Multiple sublists are processed correctly", {
  df1 <- data.frame(a = 1:3, b = letters[1:3])
  df2 <- data.frame(a = 4:6, b = letters[4:6])
  df3 <- data.frame(a = 7:9, b = letters[7:9])
  df4 <- data.frame(a = 10:12, b = letters[10:12])
  combined <- combine_df(list(list(df1, df2), list(df3, df4)))
  expect_equal(unique(combined$ID), c(1, 2))
  expect_equal(nrow(combined), 12)
})

# Test 6: ID column uniqueness and correctness
test_that("ID column is unique and correct", {
  df1 <- data.frame(a = 1:3, b = letters[1:3])
  df2 <- data.frame(a = 4:6, b = letters[4:6])
  df3 <- data.frame(a = 7:9, b = letters[7:9])
  df4 <- data.frame(a = 10:12, b = letters[10:12])
  combined <- combine_df(list(list(df1, df2), list(df3), list(df4)))
  expect_equal(combined$ID, c(rep(1, 6), rep(2, 3), rep(3, 3)))
})
