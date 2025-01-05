library(readxl)
library(openxlsx)
library(dplyr)
library(purrr)

# Function to read the "Median_MEB" sheet and add the root folder name
read_and_add_root_folder_name <- function(file_path, base_directory) {
  relative_path <- sub(paste0("^", normalizePath(base_directory), "/"), "", normalizePath(file_path))
  root_folder <- strsplit(relative_path, "/")[[1]][1]
  sheet_data <- read_excel(file_path, sheet = "Median_MEB")
  sheet_data <- sheet_data %>% mutate(Root_Folder = root_folder, .before = 1)
  return(sheet_data)
}

remove_cols <- c(
  "Root_Folder", "nfi_basket_AFN", "Healthcare_AFN", "shelter_AFN", "ten_percent_unmet_AFN", 
  "nfi_basket_USD", "Healthcare_USD", "shelter_USD", "ten_percent_unmet_USD", 
  "Fule_Electricity_AFN", "Education_AFN", "Water_AFN", "Communication_AFN", 
  "Transportation_AFN", "Fule_Electricity_USD", "Education_USD", "Water_USD", 
  "Communication_USD", "Transportation_USD", "Wash_hygiene_AFN", "Wash_hygiene_USD"
)

# Function to process files for a specific level
process_level <- function(level, base_directory, output_directory) {
  # Dynamically set the file prefix based on level
  file_prefix <- paste0("^", level, "_Median")
  # Find Excel files for the current level
  excel_files <- list.files(base_directory, pattern = paste0(file_prefix, ".*\\.xlsx$"), recursive = TRUE, full.names = TRUE)
  
  if (length(excel_files) > 0) {
    # Combine data from all files
    combined_data <- map_df(excel_files, ~ read_and_add_root_folder_name(.x, base_directory))
    combined_data$jround <- sub(".*\\\\data\\\\(\\d{2}).*", "\\1", combined_data$Root_Folder)
    # Remove unnecessary columns and reorder
    combined_data <- combined_data %>% select(-all_of(remove_cols))
    combined_data <- combined_data %>% select(jround, everything())
    # Write to Excel file
    output_file <- file.path(output_directory, paste0(level, "_meb.xlsx"))
    write.xlsx(combined_data, output_file)
    message("Processed and saved data for level: ", level)
  } else {
    message("No files found for level: ", level)
  }
}

# Main loop for all levels
base_directory <- "./data/"
output_directory <- "./output/prework MEBs/"
levels <- c("National", "Regional", "Province", "District")

# Create output directory if it doesn't exist
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

# Process each level
for (level in levels) {
  process_level(level, base_directory, output_directory)
}
