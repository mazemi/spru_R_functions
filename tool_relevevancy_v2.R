library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(stringi)

relevancy_check <- function(tool_path) {
  #' -----------------------------------------------------------------------------
  #' @title: relevancy_check 
  #' -----------------------------------------------------------------------------
  #' @description:
  #' This function extracts relevancy issues from KOBO xls questionnaire.
  #' This is NOT a syntax error detection tool.
  #' All select expressions need to be in the selected(${var},'choice') format.
  #'
  #' @param: full path of the tools file, string
  #' @return: log_df, data frame
  #' -----------------------------------------------------------------------------
  
  # Check input path
  tryCatch(
    {
      kobo_survey_df <- read_excel(tool_path, sheet = "survey")
      kobo_choices_df <- read_excel(tool_path, sheet = "choices")
    },
    error = function(e) {
      message('Please check the tool path.')
      stop()
    }
  )
  
  # General notes for the user:
  cat("KOBO tool relevancy:\n")
  cat("1) This function is not for checking syntax errors.\n")
  cat("2) All select expressions need to be in the selected(${variable},'choice') format.\n")
  
  kobo_survey_df <- kobo_survey_df %>% select(type, name, relevant)
  kobo_choices_df <- kobo_choices_df %>% select(list_name, name) %>% drop_na()
  
  # Identifying only select_one and select_multiple variables in the survey sheet:
  kobo_survey_df <- kobo_survey_df %>%
    mutate(
      row = row_number() + 1,
      select_var = case_when(
        stri_startswith_fixed(type, "select_multiple_external") | 
          stri_startswith_fixed(type, "select_one_external") ~ NA_character_,
        stri_startswith_fixed(type, "select_one") | 
          stri_startswith_fixed(type, "select_multiple") ~ stri_extract_last_words(type),
        TRUE ~ NA_character_
      ),
      select_external = if_else(
        stri_startswith_fixed(type, "select_multiple_external") | 
          stri_startswith_fixed(type, "select_one_external"), "yes", NA_character_
      )
    )
  
  # Vector of select variables in the survey sheet:
  survey_select_vars <- kobo_survey_df$select_var %>% na.omit() %>% unique() %>% sort()
  
  # Vector of list_name variables in the choices sheet:
  list_name_vars <- kobo_choices_df$list_name %>% na.omit() %>% unique() %>% sort()
  
  # Extracting un-matched variables in the survey sheet and choices sheet:
  extera_in_survey_sheet <- setdiff(survey_select_vars, list_name_vars)
  extera_in_choices_sheet <- setdiff(list_name_vars, survey_select_vars)
  
  # Extracting variable names and choices from relevant column of the kobo_survey_df:
  relevant_df <- kobo_survey_df %>%
    select(row, relevant) %>%
    drop_na() %>%
    mutate(
      relevant = gsub('"', "'", relevant),
      relevant_vars = str_extract_all(relevant, "\\{\\s*(.*?)\\s*\\}"),
      relevant_choices = str_extract_all(relevant, "'(.*?)'"),
      select_expr = str_extract_all(relevant, "selected\\((.*?)\\)"),
      select_count = str_count(relevant, "selected\\((.*?)\\)"),
      has_select = if_else(select_count > 0, "yes", "no")
    )
  
  relevant_df <- relevant_df %>% select(row, relevant_vars, relevant_choices, select_expr, has_select)
  
  selected_df <- relevant_df %>%
    unnest(select_expr) %>%
    select(row, select_expr) %>%
    mutate(
      name = str_extract(select_expr, "\\{\\s*(.*?)\\s*\\}") %>% gsub("\\{|\\}", "", .),
      choice = str_extract(select_expr, "'(.*?)'") %>% gsub("'", "", .) %>% gsub("N/A", "", .)
    )
  
  log_df <- data.frame(row = integer(), parameter = character(), column = character(), note = character())
  
  # Check the relevant column that has selected() expression
  for (i in seq_len(nrow(selected_df))) {
    detect <- which(kobo_survey_df$name == selected_df$name[i])
    if (!length(detect)) {
      log_df <- rbind(log_df, data.frame(
        row = selected_df$row[i], parameter = selected_df$name[i],
        column = "relevant", note = paste0("The variable does not exist in the NAME column, see also the ",
                                           selected_df$select_expr[i], " expression")
      ))
      next
    }
    
    if (is.na(kobo_survey_df$select_var[detect]) & is.na(kobo_survey_df$select_external[detect])) {
      log_df <- rbind(log_df, data.frame(
        row = detect + 1, parameter = selected_df$name[i],
        column = "name", note = paste0("Warning, this is not a SELECT type, also see the relevancy at row ",
                                       selected_df$row[i], " in the ", selected_df$select_expr[i])
      ))
      next
    }
    
    if (is.na(kobo_survey_df$select_var[detect]) & !is.na(kobo_survey_df$select_external[detect])) {
      next
    }
    
    temp_df <- kobo_choices_df %>% filter(list_name == kobo_survey_df$select_var[detect])
    if (nrow(temp_df) == 0) {
      log_df <- rbind(log_df, data.frame(
        row = detect + 1, parameter = kobo_survey_df$select_var[detect],
        column = "type", note = paste0("This SELECT type does not exist in the choices sheet, also see the relevancy at row ",
                                       selected_df$row[i], " in the ", selected_df$select_expr[i])
      ))
      next
    }
    
    detect_choice <- which(temp_df$name == selected_df$choice[i])
    if (!length(detect_choice) & selected_df$choice[i] != "") {
      log_df <- rbind(log_df, data.frame(
        row = selected_df$row[i], parameter = selected_df$choice[i],
        column = "relevant", note = paste0("The choice in the ",
                                           selected_df$select_expr[i], " does not exist in the choice sheet")
      ))
    }
  }
  
  if (nrow(log_df) == 0) {
    cat("No relevancy issues found.\n")
  } else {
    log_df <- log_df %>% arrange(row)
    log_path <- paste0("tool_check_", format(Sys.time(), "%d_%b_%Y_%H_%M"), ".xlsx")
    
    # Styling the log file if any issue has been detected
    wb <- createWorkbook()
    addWorksheet(wb, "Sheet1")
    writeData(wb, "Sheet1", log_df)
    headerStyle <- createStyle(
      fontSize = 12, fontColour = "#FFFFFF", halign = "center",
      fgFill = "#4F81BD"
    )
    addStyle(wb, sheet = 1, headerStyle, rows = 1, cols = 1:4, gridExpand = TRUE)
    setColWidths(wb, sheet = 1, cols = c(1, 2, 3, 4), widths = c(7, "auto"))
    saveWorkbook(wb, log_path, overwrite = TRUE)
    cat("-----------------------------------------------------------\n")
    cat("Relevancy check finished.\n")
    cat(paste0("Please see the log file: ", log_path), "\n")
    return(log_df)
  }
}
