library(openxlsx)
library(dplyr)
library(stringr)
library(readxl)

# File paths
mfs_file <- "./input/MFS_R54_25_Dec_2024.xlsx"
admin_codes <- read.xlsx("./other/admin_codes.xlsx")
jmmi_turn <- read.xlsx("./other/JMMI_Rounds.xlsx") %>% filter(!is.na(round(turn)))

### convert MFS ### 
mfs_df <- read.xlsx(mfs_file) %>% select(-classification)
j_turn <- as.numeric(str_extract(mfs_file, "(?<=MFS_R)\\d{2}"))
j_year <- jmmi_turn$year[jmmi_turn$turn == j_turn]
j_month <- jmmi_turn$month[jmmi_turn$turn == j_turn]

mfs_df$admin_level_aggregation <- "admin3"
mfs_df$year <- j_year
mfs_df$month <- j_month
mfs_df$country <- "AFG"

# Change column names for MFS
old_cols <- c(
  "availability.[30]",
  "accessibility.[25]",
  "resilience.[20]",
  "affordability.[15]",
  "infrastructure.[10]",
  "MFS"
)

new_cols <- c(
  "mfs_availability_score",
  "mfs_accessibility_score",
  "mfs_resilience_score",
  "mfs_affordability_score",
  "mfs_infrastructure_score",
  "mfs_total_score"
)

colnames(mfs_df)[match(old_cols, colnames(mfs_df))] <- new_cols

# Add admin code and levels
mfs_df <- left_join(mfs_df, admin_codes, by = c("district.code" = "code"))

# Arrange the final mfs data
order_cols <- c(
  "admin_level_aggregation",
  "year",
  "month",
  "country",
  "admin1_code",
  "admin1_label",
  "admin2_code",
  "admin2_label",
  "admin3_code",
  "admin3_label",
  "mfs_accessibility_score",
  "mfs_availability_score",
  "mfs_affordability_score",
  "mfs_resilience_score",
  "mfs_infrastructure_score",
  "mfs_total_score"
)

mfs_df <- mfs_df %>% select(all_of(order_cols))

### convert MEB ### 
median_folder <- "./input/median/"
sheet_name <- "Median_MEB"

# Get the list of Excel files in the folder
files <- list.files(median_folder, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store data frames
dfs <- list()

cols <- c(
  "afg_dist",
  "food_basket_AFN",
  "meb_afn",
  "food_basket_USD",
  "meb_usd",
  "level",
  "afg_prov" ,
  "afg_region"
)

# Loop through all files and read the specified sheet
for (file in files) {
  df <- read_excel(file, sheet = sheet_name)
  df <- df %>% select(any_of(cols))
  dfs <- append(dfs, list(df))
}

meb_df <- bind_rows(dfs)

# Add level and label columns
meb_df <- meb_df %>% 
  mutate(
    level = case_when(
      level == "National" ~ "admin0",
      !is.na(afg_region) ~ "admin1",
      !is.na(afg_prov) ~ "admin2",
      !is.na(afg_dist) ~ "admin3",
    ),
    
    label = case_when(
      level == "admin0" ~ "AFG",
      level == "admin1" ~ afg_region,
      level == "admin2" ~ afg_prov,
      level == "admin3" ~ afg_dist,
    )  
  )

meb_df <- meb_df %>% select(-c(afg_region, afg_prov, afg_dist))

# Add admin codes
meb_df <- left_join(meb_df, admin_codes, by = c("level" = "level", "label" = "label"))

# Add year and month
first_file_name <- files[1]
j_turn <- as.numeric(str_extract(first_file_name, "(?<=JMMI_R)\\d{2}"))
j_year <- jmmi_turn$year[jmmi_turn$turn == j_turn]
j_month <- jmmi_turn$month[jmmi_turn$turn == j_turn]

meb_df$year <- j_year
meb_df$month <- j_month
meb_df$country <- "AFG"

# Change column names for MEB
old_cols <- c(
  "food_basket_AFN",
  "meb_afn",
  "food_basket_USD",
  "meb_usd",
  "level"
)

new_cols <- c(
  "meb_food_local_currency",
  "meb_total_local_currency",
  "meb_food_usd_xrate_official",
  "meb_total_usd_xrate_official",
  "admin_level_aggregation"
)

colnames(meb_df)[match(old_cols, colnames(meb_df))] <- new_cols

# Ordered columns for MEB
order_cols <- c(
  "admin_level_aggregation",
  "year",
  "month",
  "country",
  "admin1_code",
  "admin1_label",
  "admin2_code",
  "admin2_label",
  "admin3_code",
  "admin3_label",
  "meb_food_local_currency",
  "meb_total_local_currency",
  "meb_food_usd_xrate_official",
  "meb_total_usd_xrate_official"
)

meb_df <- meb_df %>% select(all_of(order_cols))

# Create a new Excel workbook and add both sheets
wb <- createWorkbook()

# Add the MFS data to the first sheet
addWorksheet(wb, "MFS Data")
writeData(wb, "MFS Data", mfs_df)

# Add the MEB data to the second sheet
addWorksheet(wb, "MEB Data")
writeData(wb, "MEB Data", meb_df)

# Save the workbook to a single file
output_file <- paste0("./output/Combined_Report_R", j_turn, ".xlsx")
saveWorkbook(wb, output_file, overwrite = TRUE)

# Message to indicate success
cat("Combined Excel file saved as:", output_file, "\n")
