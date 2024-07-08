
# Load necessary libraries
source("./R/formatter.R")
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Define file paths
old_data_path <- "./input/old_data/longitudinal_prices.csv"
current_data_folder <- "./input/current_data/"
region_province_path <- "./misc_data/region_province.csv"
indicators_list_path <- "./misc_data/indicators_list.xlsx"

# Read files
questions <- read.xlsx("./misc_data/JMMI_Tool_combinded_v3_old_codes.xlsx", sheet = "survey")
choices <- read.xlsx("./misc_data/JMMI_Tool_combinded_v3_old_codes.xlsx", sheet = "choices")
region_province <- read.csv(region_province_path, fileEncoding = "UTF-8-BOM")
db <- read.csv(old_data_path)

# Function to extract the last JMMI round
extract_number <- function(filename) {
  as.numeric(str_extract(filename, "(?<=JMMI_R)\\d+"))
}

# Function to extract date
extract_date <- function(filename) {
  match <- regexpr("([A-Za-z]{3}\\d{2})\\.xlsx$", filename)
  date_abbreviation <- substr(filename, match, match + attr(match, "match.length") - 5)
  month_year <- parse_date_time(date_abbreviation, orders = "my")
  paste0(month(month_year), "/1/", year(month_year))
}

# Extract the last date and round from file names
file_name <- list.files(current_data_folder, pattern = "^National_Median_JMMI_", full.names = FALSE)
last_date <- extract_date(file_name[1]) 
last_round <- extract_number(file_name[1])  

file_names <- list.files(current_data_folder, pattern = "_Median_JMMI_", full.names = TRUE)

# Read Median prices and MEB sheets from three files
sheet_names <- c("Median_prices_AFN", "Median_prices_USD", "Median_MEB")
prefixes <- c("AFN_", "USD_", "")
excluded_cols <- c(
  "level", "Healthcare_AFN", "shelter_AFN", "Fule_Electricity_AFN", "Education_AFN",
  "Communication_AFN", "Transportation_AFN", "ten_percent_unmet_AFN", "Healthcare_USD",
  "shelter_USD", "Fule_Electricity_USD", "Education_USD", "Communication_USD",
  "Transportation_USD", "ten_percent_unmet_USD", "afg_region", "afg_prov"
)

data_list <- list()

for (file in file_names) {
  cat(file, "\n")
  data <- list()
  for (i in seq_along(sheet_names)) {
    sheet <- sheet_names[i]
    prefix <- prefixes[i]
    
    if (sheet == "Median_MEB") {
      data[[i]] <- read.xlsx(file, sheet = sheet) %>%
        select(-any_of(excluded_cols)) 
    } else {
      data[[i]] <- read.xlsx(file, sheet = sheet)
      colnames(data[[i]]) <- paste0(prefix, colnames(data[[i]]))
    }
  }
  
  level_data <- bind_cols(data)
  
  if (grepl("National", file)) {
    level_data$level <- "National"
  } else if (grepl("Regional", file)) {
    level_data$level <- "Region"
  } else {
    level_data$level <- "Province"
  }
  
  data_list <- append(data_list, list(level_data))
}

price_df <- bind_rows(data_list)
price_df <- price_df %>% ungroup()
price_df$round <- last_round
price_df$date <- last_date

# Merge with region_province refrence data 
price_df <- merge(price_df, region_province, by.x = "AFN_afg_prov", by.y = "province_name", all.x = TRUE)
price_df <- price_df %>% mutate( AFN_afg_region = if_else(level == "Province", region_name, AFN_afg_region))
price_df <- price_df %>% select(round, date, level, afg_region = AFN_afg_region, afg_prov = AFN_afg_prov, everything(),-c(USD_afg_prov, USD_afg_region, region_name))

# calculating food basket as defined in CVWG August 2022 guidance
# food basket = wheat * 89 + rice * 21 + pulses * 9 + oil * 7 + salt * 1
price_df <- price_df %>% mutate(
  AFN_food_basket_recalculated = AFN_wheat_price_cr_median * 89 +
    AFN_rice_price_cr_median * 21 +
    AFN_pulses_merged_price_final_cr_median * 9 +
    AFN_veg_oil_price_cr_median * 7 +
    AFN_salt_price_cr_median,

  USD_food_basket_recalculated = USD_wheat_price_cr_median * 89 +
    USD_rice_price_cr_median * 21 +
    USD_pulses_merged_price_final_cr_median * 9 +
    USD_veg_oil_price_cr_median * 7 +
    USD_salt_price_cr_median
) 
  
price_df <- price_df %>% select(any_of(colnames(db)), everything())

# Combine columns from both dataframes
all_columns <- union(names(db), names(price_df))

# Ensure both dataframes have all columns
db_filled <- db %>% mutate(across(everything(), ~ .x))
price_filled <- price_df %>% mutate(across(everything(), ~ .x))

# Add missing columns with NA
for (col in all_columns) {
  if (!col %in% names(db_filled)) {
    db_filled[[col]] <- NA
  }
  if (!col %in% names(price_filled)) {
    price_filled[[col]] <- NA
  }
}

# Combine dataframes
price_combined <- bind_rows(db_filled, price_filled)
price_combined[is.na(price_combined)] <- NA

# output
write.csv(price_combined, "./output/csv/longitudinal_prices.csv", row.names = FALSE)
write.xlsx(price_combined, "./output/xlsx/longitudinal_prices.xlsx", row.names = FALSE, showNA=FALSE)

# reload the data from csv (for some reason if passing directly the df this generates issues)
df_prices = read.csv("output/csv/longitudinal_prices.csv", na.strings='NA')
df_prices = format_price_data(df_prices)
split_by_level(df_prices, "./output/formatted/longitudinal_prices.xlsx", "prices")






