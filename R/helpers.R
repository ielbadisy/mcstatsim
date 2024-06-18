#' Combine nested lists of dataframes into a single dataframe
#'
#' This function takes a nested list of data frames (a list of lists, where each inner list
#' contains data frames) and combines them into a single data frame. Each data frame within
#' the same sublist is combined row-wise, and an ID column is added to identify the source sublist.
#' The function ensures that all elements of the input are proper lists containing data frames,
#' and it stops with an error message if the input does not meet these criteria.
#'
#' @param nested_list A non-empty list of lists, where each inner list contains one or more
#' data frames. It is expected that all data frames within the same sublist can be row-bound.
#'
#' @return A single data frame that is the row-wise combination of all input data frames,
#' with an additional column `ID` indicating the originating sublist.
#'
#' @examples
#' df1 <- data.frame(a = 1:3, b = letters[1:3])
#' df2 <- data.frame(a = 4:6, b = letters[4:6])
#' df3 <- data.frame(a = 7:9, b = letters[7:9])
#' df4 <- data.frame(a = 10:12, b = letters[10:12])
#'
#' nested_list <- list(list(df1, df2), list(df3, df4))
#' combined_df <- combine_df(nested_list)
#' print(combined_df)
#'
#' @export
combine_df <- function(nested_list) {
  # check the input
  if (!is.list(nested_list) || length(nested_list) < 1) {
    stop("The input must be a non-empty list of lists containing dataframes.")
  }

  # initialize an empty list to store combined data frames
  combined_dfs <- list()

  # loop through each group in the nested list
  for (i in seq_along(nested_list)) {
    # check if the current item in the list is itself a list containing dataframes
    if (!is.list(nested_list[[i]]) || !all(sapply(nested_list[[i]], is.data.frame))) {
      stop("Each item in the nested list must be a list of dataframes.")
    }

    # use lapply to add an ID column to each dataframe and then combine them
    combined_dfs[[i]] <- do.call(rbind, lapply(nested_list[[i]], function(df) {
      df$ID <- i
      return(df)
    }))
  }

  # combine all group dataframes into a single dataframe
  combined_df <- do.call(rbind, combined_dfs)

  # return the combined data frame
  return(combined_df)
}
