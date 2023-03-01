library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
library(data.table)
library(glue)

format_datamerge <- function(datamerge, tool_survey, tool_choices, analysis_plan){
  
  #' -----------------------------------------------------------------------------
  #' @title: format data merge
  #' -----------------------------------------------------------------------------
  #' @description:
  #' This function adds question and response labels to the standard data merge.
  #' The result will be an xlsx file in the working directory. necessary
  #' If the datamerge contains any extra field like in script calculation,
  #' all of them needs to be added manually in the tools.
  #' @param: datamerge, df
  #' @param: tool_survey, df
  #' @param: tool_choices, df
  #' @param: analysis_plan, df 
  #' -----------------------------------------------------------------------------
  
  if (missing(datamerge) | missing(tool_survey) | missing(tool_choices) | missing(analysis_plan)){
    message('Please set all the necessary parameters.')
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    stop()
  }
  
  # input files: data merge, tools and analysis plan
  tryCatch(
    {
      datamerge <- datamerge %>% 
        select(-ends_with("_forgraphs"))
    },
    error=function(e) {
      message('Please check the data merge.')
      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))
      stop()
    }
  )
  
  tryCatch(
    {
      tool <- tool_survey %>% 
        select(type, name, label= "label::English")
      choices <- tool_choices %>%
        select(name, label= "label::English")
    },
    error=function(e) {
      message('Please check the tool. Type, name and label::English in the survey sheet and name and label::English in the choices sheet are required.')
      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))
      stop()
    }
  )
  
  tryCatch(
    {
      analysis_plan <- analysis_plan %>%
        select(xi, nomb)
    },
    error=function(e) {
      message('Please check the analysis plan. xi and nomb coulmns are requierd.')
      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))
      stop()
    }
  )
  
  # extracting question name and values from data merge
  dm_cols <- colnames(datamerge) 
  dm_responses <- gsub(".*.\\.\\.value\\.\\.","",dm_cols)
  dm_questions <- gsub("\\.\\.value\\.\\..*.","",dm_cols)
  
  # initial vectors of labels:
  questions_label <- vector()
  responses_label <- vector()
  
  edit_col <- which(dm_cols == "disaggregation")
  samplesize_col <- which(dm_cols == "samplesize")
  level_col <- which(dm_cols == "level")
  
  # question and responses labels vector
  for (i in 1:length(dm_questions)){
    check <- length(tool$label[which(tool$name == dm_questions[i])])
    if (check != 0){
      q_label <- tool$label[which(tool$name == dm_questions[i])]
      is_nubmer <- analysis_plan$nomb[which.max(analysis_plan$xi == dm_questions[i])]
      if (is_nubmer == "no"){
        q_label_type <- paste0(q_label, " (percentage)")
        r_label <- choices$label[which.max(choices$name == dm_responses[i])]
      }else{
        q_label_type <- paste0(q_label, " (average)")
        r_label <- ""
      }
    }else{
      # warning(glue("No label found for {dm_questions[i]}."), sep = "\n")
      q_label_type <- ""
      r_label <- ""
    }
    questions_label <- c(questions_label,q_label_type)
    responses_label <- c(responses_label,r_label)
  }
  
  new_questions <- as.data.frame(t(questions_label))
  new_responses <- as.data.frame(t(responses_label))
  result <- list2DF(Map(c, new_questions, new_responses, datamerge))
  
  result[1,edit_col] <- "Question Label"
  result[2,edit_col] <- "Response Label"
  result[1,samplesize_col] <- "Sample Size"
  result[1,level_col] <- "Level"
  
  # finding similar question labels
  q_temp <- dm_questions
  
  # replacing random numbers for NA values in the temp vector 
  q_temp[is.na(q_temp)] <- sample(9900:9999, size=sum(is.na(q_temp)), replace=F)
  
  # initialization of a data frame with start_col and end_col for merged questions
  merge_cols <- data.frame(start_col = integer(),
                           end_col = integer())
  
  for (i in 1:length(q_temp)) {
    index_values <- which(q_temp %in% q_temp[i])
    merge_cols[nrow(merge_cols)+1,] <- c(index_values[1], index_values[length(index_values)])
  }
  
  merge_cols <- unique(merge_cols)
  merge_cols <- merge_cols %>% filter(start_col != end_col)
  
  # formatted xlsx result
  wb <- write.xlsx(result,"./output/formatted_data.xlsx", colNames = F, overwrite = TRUE)
  setRowHeights(wb, 1, rows = 1, heights = 50)
  setRowHeights(wb, 1, rows = 2, heights = 25)
  
  question_style <- createStyle(
    fontSize = 10, halign = "center", valign = "center", wrapText = T,
    fgFill = "#bcbcbc", border = "TopBottomLeftRight", borderColour = "#000000"
  )

  response_style <- createStyle(
    fontSize = 10, halign = "center", valign = "center", wrapText = T,
    fgFill = "#d8d8d8", border = "TopBottomLeftRight", borderColour = "#000000"
  )  
  # merging similar question labels
  tryCatch(
    {
      for (i in 1:nrow(merge_cols)) {
        mergeCells(wb, 1, cols = merge_cols$start_col[i]:merge_cols$end_col[i], rows = 1)
      }
    },
    error=function(e) {
      message('Please check the datamerge columns. There is incosistency in the column orders.')
      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))
      stop()
    }
  )

  addStyle(wb, sheet = 1, question_style, rows = 1, cols = 1:ncol(result), gridExpand = TRUE)
  addStyle(wb, sheet = 1, response_style, rows = 2, cols = 1:ncol(result), gridExpand = TRUE)
  setColWidths(wb, sheet = 1, cols = 1, widths = 5)
  setColWidths(wb, sheet = 1, cols = edit_col, widths = 20)
  setColWidths(wb, sheet = 1, cols = 3:ncol(result), widths = 15)
  freezePane(wb, 1, firstActiveRow = 3, firstActiveCol = edit_col +1)
  saveWorkbook(wb, paste0("./formatted_data_", format(Sys.time(), "%d_%b_%Y_%H_%M"),".xlsx"), overwrite = TRUE) 
  
  cat("The formatted data merge has been generated.")
}





