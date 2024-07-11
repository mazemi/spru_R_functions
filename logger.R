# this function compare two dataframes and return a log dataframe.
# The aim is comparing KOBO raw data and final clean data.
# Both dataframe should have "_uuid" columns.

library(dplyr)

logger <- function(df1, df2, additional_columns = NULL) {

  df1$`_uuid` <- as.character(df1$`_uuid`)
  df2$`_uuid` <- as.character(df2$`_uuid`)

  # Merge dataframes by _uuid to align matching records
  merged_df <- full_join(df1, df2, by = "_uuid", suffix = c(".df1", ".df2"))

  # Create an empty dataframe to store the log
  log_df <- data.frame(
    `_uuid` = character(),
    question = character(),
    old = character(),
    new = character(),
    stringsAsFactors = FALSE
  )

  # Iterate over each common column and compare the values row by row
  common_columns <- intersect(names(df1), names(df2))
  common_columns <- setdiff(common_columns, "_uuid")

  for (col in common_columns) {
    col_df1 <- paste0(col, ".df1")
    col_df2 <- paste0(col, ".df2")

    # Check for differences and log them
    diff_rows <- merged_df[[col_df1]] != merged_df[[col_df2]] & !is.na(merged_df[[col_df1]]) & !is.na(merged_df[[col_df2]])
    if (any(diff_rows)) {
      temp_log <- data.frame(
        `_uuid` = merged_df[diff_rows, "_uuid"],
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

  log_df <- log_df %>% rename(uuid = X_uuid)
  # write.csv(log_df, "log.csv", row.names = FALSE)
  return(log_df)
}

