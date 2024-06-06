library(dplyr)
library(openxlsx)

# kobo tools
questions <- read.xlsx("input/questionnaire/AFG2303_MFGD_KOBOTool_v8.xlsx", sheet = "survey")
choices <- read.xlsx("input/questionnaire/AFG2303_MFGD_KOBOTool_v8.xlsx", sheet = "choices")

# format numeric results:
fixed_headers <- c("Disaggregation", "Disaggregation Level")

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 10, fontColour = "#222222", fontName = "Calibri")
options("openxlsx.borderColour" = "#222222")
options("openxlsx.borderStyle" = "thin")
gray_style <- createStyle(fontSize = 10, halign = "center", valign = "center", fgFill = "#dddddd", wrapText = TRUE, border = "TopBottomLeftRight")
gray_small_style <- createStyle(fontSize = 9, halign = "center", valign = "center", fgFill = "#dddddd", wrapText = TRUE, border = "TopBottomLeftRight")
font_style <- createStyle(fontSize = 10, border = "TopBottomLeftRight")
# should run twice:
i <- 1
sheet_name <- c("gozar", "iset")
df <- read.xlsx("./results/UGM_numeric_indicators.xlsx", sheet = i + 1)
col.names <- colnames(df)
col.names <- col.names[-c(1, 2)]
string_to_remove <- "_Mean|_Median|_Mode"
col.names <- gsub(string_to_remove, "", col.names)
col.names <- as.vector(col.names)
questions2 <- data.frame(
  name = questions$name,
  label = questions$`label::English`,
  stringsAsFactors = FALSE
)

lookup <- setNames(questions2$label, questions2$name)
col.labels <- lookup[col.names]

# col.labels <- rep(col.labels, times = 3)
mmm.vec <- c("Mean", "Median", "Mode")
mmm.col <- rep(mmm.vec, times = length(col.labels) / 3)

addWorksheet(wb, sheet_name[i])
writeData(wb, sheet_name[i], t(col.labels), colNames = FALSE, startRow = 1, startCol = 3)

# Merge every three cells
for (k in seq(3, length(col.labels) + 1, by = 3)) {
  mergeCells(wb, sheet = sheet_name[i], cols = k:(k + 2), rows = 1)
}
writeData(wb, sheet_name[i], t(fixed_headers), colNames = FALSE, startRow = 2, startCol = 1)
writeData(wb, sheet_name[i], t(mmm.col), colNames = FALSE, startRow = 2, startCol = 3)
writeData(wb, sheet_name[i], df, colNames = FALSE, startRow = 3, startCol = 1)
addStyle(wb, sheet_name[i], style = gray_style, rows = 1:2, cols = 1:(length(col.labels) + 2), gridExpand = TRUE)
setRowHeights(wb, sheet_name[i], rows = 1, heights = 40)
setColWidths(wb, sheet_name[i], cols = 1:2, widths = c(15, 18))
freezePane(wb, sheet_name[i], firstActiveRow = 3, firstActiveCol = 3)

# final save
saveWorkbook(wb, "Formatted_numeric_tabular_data.xlsx", overwrite = TRUE)

# format nominal results:
wb2 <- createWorkbook()

df <- read.csv("./datamerge/8_datamerge_UGM_MFGD_mosque.csv")
sheet_name <- "mosque"

{
cols <- colnames(df)
cols <- cols[-c(1, 2)]
cols <- head(cols,-2)

extract_question <- function(text, pattern) {
  regex_pattern <- paste0("^(.*?)", pattern)
  matches <- regmatches(text, regexec(regex_pattern, text))
  return(matches[[1]][2])
}

extract_choice <- function(text, pattern) {
  regex_pattern <- paste0(pattern, "(.*?)$")
  matches <- regmatches(text, regexec(regex_pattern, text))
  return(matches[[1]][2])
}

q_vec <- c()
o_vec <- c()

for (i in 1:length(cols)){
  q <- extract_question(cols[i], "..value..")
  o <- extract_choice(cols[i], "..value..")
  q_vec <- c(q_vec, q)
  o_vec <- c(o_vec, o)  
}

get_select <- function(df, param) {
  row <- df[df$name == param, ]
  # Split the 'type' column value into words and return the second word
  if (nrow(row) > 0) {
    words <- strsplit(as.character(row$type), " ")[[1]]
    return(words[2])
  } else {
    return(NA)
  }
}

get_question_label <- function(df, name_par) {
  row <- df[df$name == name_par, ]
  
  # Return the remark if a matching row is found
  if (nrow(row) > 0) {
    return(row$`label::English`)
  } else {
    return(NA) 
  }
}

get_option_label <- function(df, select_par, name_par) {
  row <- df[df$list_name == select_par & df$name == name_par, ]
  
  # Return the remark if a matching row is found
  if (nrow(row) > 0) {
    return(row$`label::English`)
  } else {
    return(NA) 
  }
}

xdf <- data.frame(
  name = q_vec,
  option = o_vec
)

xdf <- xdf %>% rowwise() %>% mutate(
  list_name = get_select(questions, name),
  question_label = get_question_label(questions, name),
  option_label = get_option_label(choices, list_name, option)
)

xdf <- xdf %>% rowwise() %>% mutate(
  option_label = get_option_label(choices, list_name, option)
)

labels_df <- xdf %>% select(question_label, option_label)

labels_df2 <- t(labels_df)

# merge info for the first row:
vec_count <- table(factor(xdf$name, levels = unique(xdf$name)))
merge_info <- as.vector(vec_count)

merge_info <- c(1,1,1, merge_info)

# Function to convert numeric values to percentages, excluding specified columns
to_percentage <- function(data, exclude_columns) {
  numeric_columns <- sapply(data, is.numeric)
  numeric_columns[exclude_columns] <- FALSE  # Exclude specified columns
  data[numeric_columns] <- lapply(data[numeric_columns], function(x) ifelse(is.na(x), "", paste0(x, "%")))
  return(data)
}

df2 <- to_percentage(df, exclude_columns = c("X", "samplesize"))

first_three <- c("level", "disaggregation", "samplesize")

# Rearrange the columns of the data frame
rearranged_df <- df2[, c(first_three, setdiff(names(df2), first_three))]
rearranged_df <- rearranged_df %>% select(-X)

fixed_headers2 <- c("Disaggregation", "Disaggregation Level", "sample size")



addWorksheet(wb2, sheet_name)
writeData(wb2, sheet_name, t(fixed_headers2), colNames = FALSE, startRow = 2, startCol = 1)
writeData(wb2, sheet_name, labels_df2, colNames = FALSE, startRow = 1, startCol = 4)
writeData(wb2, sheet_name, rearranged_df, colNames = FALSE, startRow = 3, startCol = 1)
addStyle(wb2, sheet_name, style = font_style, rows = 1:(nrow(rearranged_df) + 2), cols = 1:(ncol(labels_df2) + 3), gridExpand = TRUE)
addStyle(wb2, sheet_name, style = gray_small_style, rows = 1:2, cols = 1:(ncol(labels_df2) + 3), gridExpand = TRUE)
# Initialize the starting column
start_col <- 1

# Loop through the vector to merge cells accordingly
for (merge_count in merge_info) {
  end_col <- start_col + merge_count - 1
  if (merge_count > 1) {
    mergeCells(wb2, sheet_name, cols = start_col:end_col, rows = 1)
  }
  
  start_col <- end_col + 1
}

setRowHeights(wb2, sheet_name, rows = 1:2, heights = 40)
setColWidths(wb2, sheet_name, cols = 1:3, widths = c(15, 18, 7))
setColWidths(wb2, sheet_name, cols = 4:(ncol(labels_df2) + 3), widths = 14)


}


# final save
saveWorkbook(wb2, "Formatted_nominal_tabular_data.xlsx", overwrite = TRUE)

