library(readxl)
library(dplyr)
library(purrr)
library(writexl) 

# folder path
folder_path <- "./all_clean_data_after_2022/"
files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Function to read an Excel file and add a column with the file name
read_excel_with_filename <- function(file) {
  df <- read_excel(file, .name_repair = "unique") %>%
    mutate_all(as.character) %>% 
    mutate(Source_File = basename(file))
  return(df)
}

# Read all files and combine them using bind_rows
combined_data <- files %>%
  map_dfr(read_excel_with_filename, .id = NULL)

write_xlsx(combined_data, "combined_data.xlsx")
