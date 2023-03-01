library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(readxl)
library(openxlsx)
library(lubridate)
library(stringr)
library(stringi)

relevancy_check <- function(tool_path){
  
  #' -----------------------------------------------------------------------------
  #' @title: relevancy_check 
  #' -----------------------------------------------------------------------------
  #' @description:
  #' This function extract relevancy issues from KOBO xls questionnaire.
  #' This is NOT a syntax error detection tool.
  #' All select expressions need to be in the selected(${var},'choice') format.
  #'
  #' @param: full path of the tools file, string
  #' @return: log_df, data frame
  #' -----------------------------------------------------------------------------
  
  # check input path
  tryCatch(
    {
      suppressMessages(kobo_survey_df <- read_excel(tool_path, sheet = "survey"))
      suppressMessages(kobo_choices_df <- read_excel(tool_path, sheet = "choices"))
    },
    error=function(e) {
      message('Please check the tool path.')
      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))
      stop()
    }
  )
  
  # general notes for the user:
  cat("KOBO tool relevancy note: \n") 
  cat("1) This function is not for checking syntax errors. \n")
  cat("2) All select expressions need to be in the selected(${variable},'choice') format. \n")
  kobo_survey_df <-kobo_survey_df %>% select(type, name, relevant)
  kobo_choices_df <-  kobo_choices_df %>% select(list_name, name) %>% na.omit(kobo_choices_df) 
  
  # identifying only select_one and select_multiple variables in the survey sheet:
  kobo_survey_df <- kobo_survey_df %>% mutate(
    row = row_number() + 1,
    select_var = case_when(
      str_sub(type, start = 1,end = 24) == "select_multiple_external" | str_sub(type, start = 1,end = 19) == "select_one_external" 
      ~ as.character(NA),
      str_sub(type, start = 1,end = 10) == "select_one" | str_sub(type, start = 1,end = 15) == "select_multiple" 
      ~ stri_extract_last_words(type),
      TRUE ~ as.character(NA)
    ),
    select_external = case_when(
      str_sub(type, start = 1,end = 24) == "select_multiple_external" | str_sub(type, start = 1,end = 19) == "select_one_external" ~ "yes",
      TRUE ~ as.character(NA)
    )
  )
  
  # vector of select variables in the survey sheet:
  survey_select_vars <- kobo_survey_df$select_var %>% na.omit(survey_select_vars) 
  survey_select_vars <- unique(survey_select_vars) 
  survey_select_vars <- str_sort(survey_select_vars)  
  
  # vector of list_name variables in the choices sheet:
  list_name_vars <- kobo_choices_df$list_name %>% na.omit(list_name_vars)
  list_name_vars <- unique(list_name_vars)
  list_name_vars <- str_sort(list_name_vars)
  
  # extracting un-matched variables in the survey sheet and choices sheet:
  extera_in_survey_sheet <- setdiff(survey_select_vars,list_name_vars)
  extera_in_choices_sheet <- setdiff(list_name_vars,survey_select_vars)
  
  # extracting variable names and choices form relevant column of the kobo_survey_df:
  relevant_df <- data.frame()
  relevant_df <- kobo_survey_df %>% select(row, relevant) %>% na.omit(relevant_df$relevant)
  relevant_df$relevant <- gsub('"',"'",relevant_df$relevant)
  relevant_df <- relevant_df %>% 
    mutate(
      relevant_vars = str_extract_all(relevant_df$relevant, "\\{\\s*(.*?)\\s*\\}"), # regex for getting all string (variable names) between "{" and "}"
      relevant_choices = str_extract_all(relevant_df$relevant, "'(.*?)'"), #  regex for getting all string (choices) between "'" and "'"
      select_expr = str_extract_all(relevant_df$relevant, "selected*(.*?)\\)"), #  regex for getting all selected() statements
      select_count = str_count(relevant_df$relevant, "selected*(.*?)\\)"), #  regex for count of all the selected() statements
      has_select = if_else(select_count > 0, "yes","no"),
    )
  
  relevant_df <- relevant_df %>% select(row, relevant_vars, relevant_choices, select_expr, has_select)
  test1 <-  unnest(relevant_df, relevant_vars)
  test2 <-  unnest(relevant_df, relevant_choices)
  
  selected_df <- data.frame()
  selected_df <-  unnest(relevant_df, select_expr) %>% select(row, select_expr)
  selected_df$name <- unlist(str_extract_all(selected_df$select_expr, "\\{\\s*(.*?)\\s*\\}"))
  selected_df$name <- gsub("\\{|\\}","",selected_df$name)
  selected_df$choice <- unlist(str_extract_all(selected_df$select_expr, "'(.*?)'"))
  selected_df$choice <- gsub("'","",selected_df$choice)
  selected_df$choice <- gsub("N/A","",selected_df$choice)
  
  log_df <- data.frame(row = integer(),
                       parameter = character(),
                       column = character(),
                       note = character())
  
  # check the relevant column that has selected() expression
  for (i in 1:nrow(selected_df)){
    detect = which(kobo_survey_df$name == selected_df$name[i])
    if (!length(detect)){
      log_df[nrow(log_df)+1,] <- c(selected_df$row[i], selected_df$name[i],
                                   "relevant", paste0("The variable dose not exist in the NAME column, see also the ", 
                                                      selected_df$select_expr[i], " expression"))
      next
    }
    
    if (is.na(kobo_survey_df$select_var[detect]) & is.na(kobo_survey_df$select_external[detect])){
      log_df[nrow(log_df)+1,] <- c(detect + 1, selected_df$name[i],
                                   "name", paste0("Warning, this is not a SELECT type, also see the relevancy at row ",
                                                  selected_df$row[i], " in the ", selected_df$select_expr[i]))
      next
    }
    
    if (is.na(kobo_survey_df$select_var[detect]) & !is.na(kobo_survey_df$select_external[detect])){
      next
    }
    
    temp_df <- kobo_choices_df %>% filter(list_name == kobo_survey_df$select_var[detect])
    if (nrow(temp_df) == 0){
      log_df[nrow(log_df)+1,] <- c(detect + 1, kobo_survey_df$select_var[detect],
                                   "type", paste0("This SELECT type dose not exist in the choices sheet,also see the relevancy at row ",
                                                  selected_df$row[i], " in the ", selected_df$select_expr[i]))
      next
    }
    
    detect_chice = which(temp_df$name == selected_df$choice[i])
    if (!length(detect_chice) & selected_df$choice[i] !="") {
      log_df[nrow(log_df)+1,] <- c(selected_df$row[i],selected_df$choice[i],
                                   "relevant", paste0("The choice in the " ,
                                                      selected_df$select_expr[i], " dose not exist in the choice sheet"))
    }
  }
  
  if(nrow(log_df) == 0){
    cat("",sep = "\n")
    cat("No revelancy issue found.", sep = "\n")
  }else{
    log_df[1] <- lapply(log_df[1], as.numeric)
    log_df <- log_df[order(log_df$row),]
    log_path <- paste0("tool_check_",format(Sys.time(), "%d_%b_%Y_%H_%M"),".xlsx")
    
    # styling the log file if any issue has been detected.
    wb <- write.xlsx(log_df, log_path, overwrite = TRUE)
    headerStyle <- createStyle(
      fontSize = 12, fontColour = "#FFFFFF", halign = "center",
      fgFill = "#4F81BD"
    )
    addStyle(wb, sheet = 1, headerStyle, rows = 1, cols = 1:4, gridExpand = TRUE)
    setColWidths(wb, sheet = 1, cols = c(1,2:4), widths = c(7,"auto"))
    saveWorkbook(wb, log_path, overwrite = TRUE)
    cat("-----------------------------------------------------------",sep = "\n")
    cat("Relevancy check finished. \n")
    cat(paste0("please see the log file: ",log_path ), sep = "\n")
    return(log_df)
  }
}

