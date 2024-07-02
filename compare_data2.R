library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)
library(progress)

#' -----------------------------------------------------------------------------
#' @title: compare_data
#' -----------------------------------------------------------------------------
#' @description:
#' This function compares two data sets and extract any changes as a log file.
#' One of the main use case is comparing raw data and clean data set.
#'
#' @param: raw_df, data frame
#' @param: clean_df, data frame
#' @param: optional, vector of column names for comparing
#' @return: logs_df, data frame
#'
#' If both data frames has "_uuid" columns, the third parameter is not required.
#' Otherwise, the function expects "unique_ids" vector ie. c("id1","id2")
#' -----------------------------------------------------------------------------

compare_data <- function(raw_df, clean_df, columns = NULL) {
  if (missing(raw_df) | missing(clean_df)) {
    message("Please set both data sets.")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  if (!"_uuid" %in% colnames(raw_df)) {
    message("raw dataset dose not have _uuid column.")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  if (!"_uuid" %in% colnames(clean_df)) {
    message("clean dataset dose not have _uuid column.")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  unique_raw_id <- "_uuid"
  unique_clean_id <- "_uuid"

  # convert every thing to character and replacing NA with ''
  raw_df[] <- lapply(raw_df, as.character)
  clean_df[] <- lapply(clean_df, as.character)
  raw_df[is.na(raw_df)] <- ""
  clean_df[is.na(clean_df)] <- ""

  # set a fixed name as a primary key
  colnames(raw_df)[colnames(raw_df) == colnames(raw_df[unique_raw_id])] <- "uuid_raw"
  colnames(clean_df)[colnames(clean_df) == colnames(clean_df[unique_clean_id])] <- "uuid"

  if (missing(columns)) {
    # looking for identical columns in raw and clean data sets
    check_columns <- intersect(colnames(raw_df), colnames(clean_df))
  } else {
    check_columns <- columns
  }

  if (length(check_columns) == 0) {
    cat("There is no identical columns in the provided data sets.")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  # check if unique_ids in both data frame have common values
  check_uuids <- intersect(raw_df$uuid_raw, clean_df$uuid)
  if (length(check_uuids) == 0) {
    cat("There is no identical unique ids in the provided data sets.")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  # add "_clean" extension in the column names of clean data set
  colnames(clean_df) <- paste(colnames(clean_df), "_clean", sep = "")

  # deleted uuids in the clean data set:
  removed_df <- raw_df$uuid_raw[!(raw_df$uuid_raw %in% clean_df$uuid_clean)]
  removed_df <- as.data.frame(removed_df)
  all_data <- raw_df %>% inner_join(clean_df, by = c("uuid_raw" = "uuid_clean"))

  logs_df <- data.frame(
    uuid = character(),
    question = character(),
    old.value = character(),
    new.value = character()
  )

  # progress bar
  pb <- progress_bar$new(
    format = "(:spin) [:bar] :percent",
    total = nrow(all_data), clear = FALSE, width = 80
  )

  # need to improvement, nested for statements:
  for (i in 1:nrow(all_data)) {
    pb$tick()
    for (j in check_columns) {
      if (all_data[i, j] != all_data[i, paste(j, "_clean", sep = "")]) {
        logs_df[nrow(logs_df) + 1, ] <- c(
          all_data$uuid_raw[i], colnames(all_data)[which(colnames(all_data) == j)],
          all_data[i, j], all_data[i, paste(j, "_clean", sep = "")]
        )
      }
    }
  }

  pb$terminate()
  invisible()

  if (nrow(logs_df) == 0) {
    cat("", sep = "\n")
    cat("No change was found.", sep = "\n")
  } else {
    base_name <- "log"
    file_name <- "log.xlsx"
    file_path <- file_name

    # Check if the file already exists and generate a new name if it does
    counter <- 1
    while (file.exists(file_path)) {
      file_name <- paste0(base_name, "_", counter, ".xlsx")
      file_path <- file_name
      counter <- counter + 1
    }

    write.xlsx(logs_df, file_path, overwrite = TRUE)
    cat(paste0("please see the log file: ", file_path), sep = "\n")
    return(logs_df)
  }
}

# loading data:
sheet_no <- 12
rd <- read.xlsx("./01_Raw_data/UGM_6_Provinces__raw_data_2024-05-23.xlsx", sheet = sheet_no)
cd <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_2024-05-28.xlsx", sheet = sheet_no)

res <- compare_data(rd, cd)
res$key  <- paste0(res$uuid, res$question)

clog <- read.xlsx("./02_Cleanning_logs/Master cleaning logs.xlsx", sheet = 3) %>% filter(change == "yes") 
sheets <- unique(clog$sheet_name)
clog <- clog %>% filter(sheet_name == "Food_Sheet")
clog$key <- paste0(clog$uuid, clog$question)

extra_logs <- setdiff(clog$key, res$key)
missed_logs <- setdiff(res$key, clog$key)

ss <- res %>% filter(key %in% missed_logs)
write.csv(ss, "ss.csv")
