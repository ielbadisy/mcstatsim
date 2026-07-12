#' Combine Dataframes in a Nested List
#'
#' This function combines dataframes that are nested within a list of lists into a single dataframe.
#'
#' @param nested_list A list of lists, where each sublist contains dataframes to be combined.
#' @return A combined dataframe where each original dataframe is augmented with an ID column indicating its source list.
#' @details The function first checks if the input is a non-empty list of lists containing dataframes.
#' It then iterates through each sublist, combining the dataframes and adding an ID column to indicate their source.
#' Finally, all combined dataframes are row-bound into a single dataframe.
#' @examples
#' df1 <- data.frame(a = 1:3, b = 4:6)
#' df2 <- data.frame(a = 7:9, b = 10:12)
#' nested_list <- list(list(df1, df2), list(df1, df2))
#' combine_df(nested_list)
#' @export
combine_df <- function(nested_list) {
  if (!is.list(nested_list) || length(nested_list) < 1) {
    stop("The input must be a non-empty list of lists containing dataframes.")
  }

  combined_dfs <- lapply(seq_along(nested_list), function(i) {
    if (!is.list(nested_list[[i]]) || !all(sapply(nested_list[[i]], is.data.frame))) {
      stop("Each item in the nested list must be a list of dataframes.")
    }
    df <- do.call(rbind, nested_list[[i]])
    df$ID <- i
    df
  })

  combined_df <- do.call(rbind, combined_dfs)
  return(combined_df)
}
