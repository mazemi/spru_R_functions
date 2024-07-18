library(readxl)
library(openxlsx)
library(dplyr)

folder_path <- "./data"
excel_files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)
data_list <- list()

# Loop through each Excel file
for (file in excel_files) {

  sheet_data <- read_excel(file, sheet = "MFS")
  
  # Get the first and last columns
  district_code <- sheet_data[[1]]
  mfs <- sheet_data[[ncol(sheet_data)]]
  result_df <- data.frame(district_code, mfs)
  result_df$File_Name <- basename(file)
  data_list[[length(data_list) + 1]] <- result_df
}

# Combine all data frames into one
final_data <- bind_rows(data_list)

final_data$round <- sub("MFS_R([0-9]+).*", "\\1", final_data$File_Name)

pr <- read.xlsx("./xlsx/location_code.xlsx", sheet = "province")
dis <- read.xlsx("./xlsx/location_code.xlsx", sheet = "district")

final_data$province_code <- substr(final_data$district_code, 1, 4)
final_data <- final_data %>%  left_join(pr)
final_data <- final_data %>%  left_join(dis)

final_data <- final_data %>% select(round ,provice_name, district_name, mfs)


# Filter the data for "Full functionality"
top_filtered_data <- final_data %>% filter(mfs == "Full functionality")

top_district_counts <- top_filtered_data %>%
  group_by(provice_name, district_name) %>%
  summarise(full_functionality = n()) %>%
  arrange(desc(full_functionality))

# Get the top ten districts
top_five_districts <- head(top_district_counts, 5)


# Filter the data for "Poor functionality"
low_filtered_data <- final_data %>% filter(mfs == "Poor functionality")

low_district_counts <- low_filtered_data %>%
  group_by(provice_name, district_name) %>%
  summarise(poor_functionality = n()) %>%
  arrange(desc(poor_functionality))

# Get the top ten districts
low_five_districts <- head(low_district_counts, 5)

wb <- createWorkbook()

addWorksheet(wb, "info")
addWorksheet(wb, "top five")
addWorksheet(wb, "bottom five")

# Write data to sheets
writeData(wb, sheet = "info", x = final_data)
writeData(wb, sheet = "top five", x = top_five_districts)
writeData(wb, sheet = "bottom five", x = low_five_districts)

saveWorkbook(wb, file = "MFS combined.xlsx", overwrite = TRUE)

