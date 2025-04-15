library(readxl)
library(dplyr)
library(writexl)

# Set your root directory
root_dir <- "./data/MEB data"

# File name patterns
patterns <- c("District_mean", "National_mean", "Province_mean", "Regional_mean",
              "District_Median", "National_Median", "Province_Median", "Regional_Median")

# Regex pattern
file_pattern <- paste0("^(", paste(patterns, collapse = "|"), ").*\\.xlsx$")

# Find matching files
files <- list.files(path = root_dir, pattern = file_pattern,
                    full.names = TRUE, recursive = TRUE)

# Desired columns
cols_to_keep <- c("afg_region", "afg_prov", "afg_dist",
                  "food_basket_AFN", "meb_afn", 
                  "food_basket_USD", "meb_usd")

# Read and combine
combined_data <- lapply(files, function(file) {
  # cat(file, "\n")
  sheet_name <- if (grepl("Median", basename(file), ignore.case = TRUE)) "Median_MEB" else "Mean_MEB"
  
  tryCatch({
    df <- read_excel(file, sheet = sheet_name)
    df <- df %>% select(any_of(cols_to_keep))
    df$source_file <- basename(file)
    df$sheet_used <- sheet_name
    df
  }, error = function(e) {
    message(paste("Error in file:", file, "-", e$message))
    NULL
  })
}) %>% bind_rows()

# Save output
write_xlsx(combined_data, "Combined_MEB_Selected_Columns.xlsx")

