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
#' One of the main use case is comparing raw data set with clean data set.
#'
#' @param: raw_df, data frame
#' @param: clean_df, data frame
#' @param: unique_ids, vector with two string, optional
#' @param: columns, vector of column names for comparing, optional
#' @return: logs_df, data frame
#' 
#' If both data frames has "_uuid" columns, the third parameter is not required.
#' Otherwise, the function expects "unique_ids" vector ie. c("id1","id2")
#' -----------------------------------------------------------------------------

compare_data <- function(raw_df,clean_df, unique_ids, columns = NULL){
  
  if(missing(raw_df) | missing(clean_df)) {
    message('Please set both data sets.')
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    stop()
  }
  
  if(missing(unique_ids)) {
    unique_raw_id <- "_uuid"
    unique_clean_id <- "_uuid"
  }else if (length(unique_ids) != 2) {
    message('Please enter unique column names in a vectore.')
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    stop()
  } else{
    unique_raw_id <- unique_ids[1]
    unique_clean_id <- unique_ids[2]
  }
  
  # convert every thing to character and replacing NA with ''
  raw_df[] <- lapply(raw_df, as.character)
  clean_df[] <- lapply(clean_df, as.character)
  raw_df[is.na(raw_df)] <- ""
  clean_df[is.na(clean_df)] <- ""
  
  # set a fixed name as a primary key
  colnames(raw_df)[colnames(raw_df) == colnames(raw_df[unique_raw_id])] = "uuid_raw"
  colnames(clean_df)[colnames(clean_df) == colnames(clean_df[unique_clean_id])] = "uuid"
  
  if(missing(columns)) {
    # looking for identical columns in raw and clean data sets
    check_columns <- intersect(colnames(raw_df),colnames(clean_df))
  }else{
    check_columns <- columns
  }

  if (length(check_columns) == 0){
    cat('There is no identical columns in the provided data sets.')
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    stop()
  }
  
  # check if unique_ids in both data frame have common values
  check_uuids <- intersect(raw_df$uuid_raw, clean_df$uuid)
  if (length(check_uuids) == 0){
    cat('There is no identical unique ids in the provided data sets.')
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    stop()
  }
  
  # add "_clean" extension in the column names of clean data set
  colnames(clean_df) <- paste(colnames(clean_df), "_clean", sep = "")
  
  # deleted uuids in the clean data set:
  removed_df <- raw_df$uuid_raw[!(raw_df$uuid_raw %in% clean_df$uuid_clean)]
  removed_df <- as.data.frame(removed_df)
  all_data <- raw_df %>% inner_join(clean_df, by=c("uuid_raw"="uuid_clean"))
  
  logs_df <- data.frame(uuid = character(),
                        question = character(),
                        old.value = character(),
                        new.value = character())
  
  # progress bar
  pb <- progress_bar$new(
    format = "(:spin) [:bar] :percent",
    total = nrow(all_data), clear = FALSE, width = 80)
  
  # need to improvement, nested for statements:
  for (i in 1:nrow(all_data)) {
    pb$tick()
    for (j in check_columns) {
      if (all_data[i,j]!=all_data[i,paste(j, "_clean", sep = "")]){
        logs_df[nrow(logs_df)+1,] <- c(all_data$uuid_raw[i], colnames(all_data)[which(colnames(all_data)==j)],
                                       all_data[i,j], all_data[i,paste(j, "_clean", sep = "")])
      }
    }
  }
  
  pb$terminate()
  invisible()
  
  if(nrow(logs_df) == 0){
    cat("",sep = "\n")
    cat("No change was found.", sep = "\n")
  }else{
    
    log_path <- paste0("chages_log_",format(Sys.time(), "%d_%b_%Y_%H_%M"),".xlsx")
    write.xlsx(logs_df,log_path , overwrite = TRUE)
    cat(paste0("please see the log file: ",log_path ), sep = "\n")
    return(logs_df)
  }
}

