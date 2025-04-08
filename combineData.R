library(readxl)
library(openxlsx)
library(dplyr)

# Set the root folder where the search starts
root_dir <- "./"

# List all Excel files starting with "District_mean"
excel_files <- list.files(
  path = root_dir,
  pattern = "^District_mean.*\\.xlsx?$",
  recursive = TRUE,
  full.names = TRUE
)

# Read and combine the "Mean_prices_AFN" sheet from each file
combined_data1 <- bind_rows(
  lapply(excel_files, function(file) {
    tryCatch({
      read_excel(file, sheet = "Mean_prices_AFN") %>%
        mutate(source_file = basename(file))  # Optional: track file origin
    }, error = function(e) {
      message(paste("Error reading file:", file))
      return(NULL)
    })
  })
)

write.xlsx(combined_data1, "mean_combined_data.xlsx")

# Read and combine the "Mean_prices_AFN" sheet from each file
combined_data2 <- bind_rows(
  lapply(excel_files, function(file) {
    tryCatch({
      read_excel(file, sheet = "Mean_MEB") %>%
        mutate(source_file = basename(file))  # Optional: track file origin
    }, error = function(e) {
      message(paste("Error reading file:", file))
      return(NULL)
    })
  })
)

write.xlsx(combined_data2, "mean_meb_combined_data.xlsx")
