# Load necessary libraries
library(openxlsx)
library(dplyr)
library(stringr)
library(lubridate)

# Define file paths
old_data_path <- "./input/old_data/longitudinal_indicators.csv"
current_data_path <- "./input/current_data/datamerge_JMMI_R48_Jun24.csv"
region_province_path <- "./input/region_province.csv"
indicators_list_path <- "./input/indicators_list.xlsx"

# Read data files
db <- read.csv(old_data_path)
dm <- read.csv(current_data_path)
region_province <- read.csv(region_province_path)

datamerge_name <- basename(current_data_path)

# Function to get region name based on province name
get_region_name <- function(province, region_province_df) {
  region <- region_province_df$region_name[region_province_df$province_name == province]
  if(length(region) == 0) {
    return(NA)
  } else {
    return(region)
  }
}

# Function to extract the last JMMI round
extract_number <- function(filename) {
  as.numeric(str_extract(filename, "(?<=JMMI_R)\\d+"))
}

# Function to extract date
extract_date <- function(filename) {
  match <- regexpr("([A-Za-z]{3}\\d{2})\\.csv$", filename)
  date_abbreviation <- substr(filename, match, match + attr(match, "match.length") - 5)
  month_year <- parse_date_time(date_abbreviation, orders = "my")
  month <- month(month_year)
  year <- year(month_year)
  return(paste0("1/", month, "/", year))
}

# Extract the last date and round from file names
last_date <- extract_date(datamerge_name) 
last_round <- extract_number(datamerge_name)  

# Read indicator list
indicator_list <- read.xlsx(indicators_list_path, sheet = "required_indicatores")
indicator_vec <- paste0(indicator_list$var, "..value..")

# Create a regular expression to match columns starting with any of the indicator_vec
pattern <- paste0("^(", paste(indicator_vec, collapse = "|"), ")")

dm <- dm %>% 
  filter(level != "afg_dist") %>% 
  select(level, disaggregation, samplesize, matches(pattern))
  
dm <- merge(dm, region_province, by.x = "disaggregation", by.y = "province_name", all.x = TRUE)

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
    afg_province = case_when(
      level == "Province" ~ disaggregation,
      TRUE ~ NA_character_
    )
  ) %>% 
  select(round, date, level, afg_region, afg_province, samplesize, everything(), -region_name)

