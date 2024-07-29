library(dplyr)

#' -----------------------------------------------------------------------------
#' @title: logger
#' -----------------------------------------------------------------------------
#' @description:
#' This function compares two data sets and extract any changes as a log file.
#' Both data frames must have "_uuid" columns.
#'
#' @param: raw_data, data frame
#' @param: clean_data, data frame
#' @param: additional_columns, vector of extra column names, optional
#' @return: logs_df, data frame
#' -----------------------------------------------------------------------------

logger <- function(raw_data, clean_data, additional_columns = NULL) {
  # Convert character columns to numeric where applicable
  df1 <- raw_data %>% mutate(across(where(is.character), ~ ifelse(is.na(.) | grepl("^[0-9.-]+$", .), as.numeric(.), .)))
  df2 <- clean_data %>% mutate(across(where(is.character), ~ ifelse(is.na(.) | grepl("^[0-9.-]+$", .), as.numeric(.), .)))

  # Merge dataframes by _uuid to align matching records
  merged_df <- inner_join(df1, df2, by = "_uuid", suffix = c(".df1", ".df2"))

  # Create an empty dataframe to store the log
  log_df <- data.frame(
    uuid = character(),
    question = character(),
    old = character(),
    new = character(),
    stringsAsFactors = FALSE
  )

  # Identify common columns to compare
  common_columns <- setdiff(intersect(names(df1), names(df2)), "_uuid")

  # Iterate over each common column and compare the values row by row
  for (col in common_columns) {
    col_df1 <- paste0(col, ".df1")
    col_df2 <- paste0(col, ".df2")

    # Check if the column is numeric and compare values accordingly
    if (is.numeric(merged_df[[col_df1]]) && is.numeric(merged_df[[col_df2]])) {
      # Compare numeric values up to five decimal points
      diff_rows <- round(merged_df[[col_df1]], 5) != round(merged_df[[col_df2]], 5)
    } else {
      # Compare non-numeric values directly
      diff_rows <- merged_df[[col_df1]] != merged_df[[col_df2]]
    }

    # Ensure NA values are handled correctly
    diff_rows[is.na(merged_df[[col_df1]]) & !is.na(merged_df[[col_df2]])] <- TRUE
    diff_rows[is.na(merged_df[[col_df2]]) & !is.na(merged_df[[col_df1]])] <- TRUE

    if (any(diff_rows, na.rm = TRUE)) {
      temp_log <- data.frame(
        uuid = merged_df[diff_rows, "_uuid"],
        question = col,
        old = merged_df[diff_rows, col_df1],
        new = merged_df[diff_rows, col_df2],
        stringsAsFactors = FALSE
      )

      if (!is.null(additional_columns)) {
        # Add additional columns to the log
        for (add_col in additional_columns) {
          temp_log[[add_col]] <- merged_df[diff_rows, paste0(add_col, ".df1")]
        }
      }

      log_df <- rbind(log_df, temp_log)
    }
  }
  return(log_df)
}

