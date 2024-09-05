library(readxl)
library(openxlsx)
library(dplyr)
library(purrr)

# Function to read the "Median_MEB" sheet and add the root folder name
read_and_add_root_folder_name <- function(file_path, base_directory) {
  # Extract the root folder name relative to the base directory
  relative_path <- sub(paste0("^", normalizePath(base_directory), "/"), "", normalizePath(file_path))
  root_folder <- strsplit(relative_path, "/")[[1]][1]
  sheet_data <- read_excel(file_path, sheet = "Median_MEB")
  # Add the root folder name as the first column
  sheet_data <- sheet_data %>% mutate(Root_Folder = root_folder, .before = 1)
  return(sheet_data)
}

remove_cols <- c(
  "Root_Folder", "nfi_basket_AFN","Healthcare_AFN","shelter_AFN","ten_percent_unmet_AFN", "nfi_basket_USD", 
  "Healthcare_USD", "shelter_USD", "ten_percent_unmet_USD", "Fule_Electricity_AFN", 
  "Education_AFN", "Water_AFN", "Communication_AFN", "Transportation_AFN", 
  "Fule_Electricity_USD", "Education_USD", "Water_USD", 
  "Communication_USD", "Transportation_USD", 
  "Wash_hygiene_AFN", "Wash_hygiene_USD"
)


# levels: National Regional Province District
# Function to find Excel files starting with "District_Median"
find_excel_files <- function(directory) {
  list.files(directory, pattern = "^District_Median.*\\.xlsx$", recursive = TRUE, full.names = TRUE)
}

# Set the directory where the folders are located
base_directory <- "./data/"
excel_files <- find_excel_files(base_directory)
combined_data <- map_df(excel_files, ~ read_and_add_root_folder_name(.x, base_directory))
combined_data$jround <- sub(".*\\\\data\\\\(\\d{2}).*", "\\1", combined_data$Root_Folder)

combined_data <- combined_data %>% select(- all_of(remove_cols))
combined_data <- combined_data %>% select(jround, everything())

write.xlsx(combined_data, "./output/prework MEBs/District_meb.xlsx")
