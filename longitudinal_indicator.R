# The aim of this script is to append the latest JMMI indicators to the longitudinal data
source("./R/formatter.R")
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Define file paths
old_data_path <- "./input/old_data/longitudinal_indicators.csv"
current_data_path <- "./input/current_data/datamerge_JMMI_R48_Jun24.csv"
region_province_path <- "./misc_data/region_province.csv"
indicators_list_path <- "./misc_data/indicators_list.xlsx"

questions = read.xlsx("./misc_data/JMMI_Tool_combinded_v3_old_codes.xlsx", sheet = "survey")
choices = read.xlsx("./misc_data/JMMI_Tool_combinded_v3_old_codes.xlsx", sheet = "choices")
db <- read.csv(old_data_path)
dm <- read.csv(current_data_path)
region_province <- read.csv(region_province_path, fileEncoding = "UTF-8-BOM")

# Function to extract the last JMMI round
extract_number <- function(filename) {
  as.numeric(str_extract(filename, "(?<=JMMI_R)\\d+"))
}

# Function to extract date
extract_date <- function(filename) {
  match <- regexpr("([A-Za-z]{3}\\d{2})\\.csv$", filename)
  date_abbreviation <- substr(filename, match, match + attr(match, "match.length") - 5)
  month_year <- parse_date_time(date_abbreviation, orders = "my")
  paste0(month(month_year), "/1/", year(month_year))
}

# Extract the file name for identifying round and date
datamerge_name <- basename(current_data_path)
last_date <- extract_date(datamerge_name) 
last_round <- extract_number(datamerge_name)  

# Read indicator list
indicator_list <- read.xlsx(indicators_list_path, sheet = "required_indicatores")
indicator_vec <- paste0(indicator_list$var, "..value..")

# Create a regular expression to match columns starting with any of the indicator_vec
pattern <- paste0("^(", paste(indicator_vec, collapse = "|"), ")")

# Filter and select relevant columns
dm <- dm %>% 
  filter(level != "afg_dist") %>% 
  select(level, disaggregation, samplesize, matches(pattern))

# Merge with region_province data
dm <- merge(dm, region_province, by.x = "disaggregation", by.y = "province_name", all.x = TRUE)

# Mutate and rearrange columns
dm <- dm %>% 
  mutate(
    round = last_round,
    date = last_date,
    level = case_when(
      level == "No_disagregation" ~ "National",
      level == "afg_region" ~ "Region",
      level == "afg_prov" ~ "Province",
      TRUE ~ level
    ),
    afg_region = case_when(
      level == "Region" ~ disaggregation,
      level == "Province" ~ region_name,
      TRUE ~ NA_character_
    ),
    afg_prov = case_when(
      level == "Province" ~ disaggregation,
      TRUE ~ NA_character_
    )
  ) %>% 
  ungroup() %>% 
  select(round, date, level, afg_region, afg_prov, samplesize, everything(), -c(region_name, disaggregation))

# Combine columns from both dataframes
all_columns <- union(names(db), names(dm))

# Prepare for column reordering
db_vec <- sub("..value...*", "..value..", colnames(db))
db_vec <- unique(db_vec)
ref_col <- data.frame(root.col = db_vec, col.order = 1:length(db_vec))

ref_col_full <- data.frame(col.name = all_columns) %>%
  mutate(root.col = sub("..value...*", "..value..", col.name)) %>%
  left_join(ref_col, by = c("root.col" = "root.col"))

# Assign order value to missing columns
ref_col_full$col.order[is.na(ref_col_full$col.order)] <- length(db_vec) + 1
ref_col_full <- ref_col_full %>% arrange(col.order)

# Ensure both dataframes have all columns
db_filled <- db %>% mutate(across(everything(), ~ .x))
dm_filled <- dm %>% mutate(across(everything(), ~ .x))

# Add missing columns with NA
for (col in all_columns) {
  if (!col %in% names(db_filled)) {
    db_filled[[col]] <- NA
  }
  if (!col %in% names(dm_filled)) {
    dm_filled[[col]] <- NA
  }
}

# Combine dataframes
df_combined <- bind_rows(db_filled, dm_filled)
df_combined[is.na(df_combined)] <- NA

# Reorder columns and write to Excel
df_combined <- df_combined %>% select(all_of(ref_col_full$col.name))

# output
write.csv(df_combined, "./output/csv/longitudinal_indicators.csv", row.names = FALSE)
write.xlsx(df_combined, "./output/xlsx/longitudinal_indicators.xlsx", row.names = FALSE, showNA=FALSE)

# reload the data from csv (for some reason if passing directly the df this generates issues)
df_indicators = read.csv("./output/csv/longitudinal_indicators.csv", na.strings='NA')
df_indicators = format_indicator_data(df_indicators, questions, choices)
split_by_level(df_indicators, "./output/formatted/longitudinal_indicators.xlsx", "indicators")

