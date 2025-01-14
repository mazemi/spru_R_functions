
library(readxl)
library(writexl)
library(dplyr)

root_directory <- "input"

# Recursively list all Excel files that start with "District_Median" and end with ".xlsx"
file_list <- list.files(path = root_directory, pattern = "^District_Median.*\\.xlsx$", full.names = TRUE, recursive = TRUE)


combined_price <- data.frame()

for (file in file_list) {

  sheet_data <- read_excel(file, sheet = "Median_prices_AFN")
  path_parts <- strsplit(file, "/")[[1]]
  sheet_data$round <- path_parts[3]
  combined_price <- bind_rows(combined_price, sheet_data)
}

write_xlsx(combined_price, "combined_prices.xlsx")

combined_meb <- data.frame()

for (file in file_list) {
  
  sheet_data <- read_excel(file, sheet = "Median_MEB")
  path_parts <- strsplit(file, "/")[[1]]
  sheet_data$round <- path_parts[3]
  combined_meb <- bind_rows(combined_meb, sheet_data)
}

write_xlsx(combined_meb, "combined_meb.xlsx")