
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)

source("./R/formatter.R")

old_data_path <- "./input/old_data/longitudinal_indicators.csv"
region_province_path <- "./misc_data/region_province.csv"
indicators_list_path <- "./misc_data/indicators_list.xlsx"
output_csv_path <- "./output/csv/longitudinal_indicators.csv"
output_xlsx_path <- "./output/xlsx/longitudinal_indicators.xlsx"
formatted_output_path <- "./output/formatted/longitudinal_indicators.xlsx"

# core function
process_jmmi_indicators <- function(current_datamerge_path, tool_path) {
  questions = read.xlsx(tool_path, sheet = "survey")
  choices = read.xlsx(tool_path, sheet = "choices")
  db <- read.csv(old_data_path)
  dm <- read.csv(current_datamerge_path)
  # rename some of column names
  dm <- correct_header(dm)
  
  region_province <- read.csv(region_province_path, fileEncoding = "UTF-8-BOM")
  
  datamerge_name <- basename(current_datamerge_path)
  last_date <- extract_date(datamerge_name) 
  last_round <- extract_number(datamerge_name)  
  
  indicator_list <- read.xlsx(indicators_list_path, sheet = "required_indicatores")
  indicator_vec <- paste0(indicator_list$var, "..value..")
  pattern <- paste0("^(", paste(indicator_vec, collapse = "|"), ")")
  
  dm <- dm %>% 
    filter(level != "afg_dist") %>% 
    select(level, disaggregation, samplesize, matches(pattern))
  
  dm <- merge(dm, region_province, by.x = "disaggregation", by.y = "province_name", all.x = TRUE)
  
  cat("merging current data with old data ...", "\n")
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
  
  all_columns <- union(names(db), names(dm))
  
  db_vec <- sub("..value...*", "..value..", colnames(db))
  db_vec <- unique(db_vec)
  ref_col <- data.frame(root.col = db_vec, col.order = 1:length(db_vec))
  
  ref_col_full <- data.frame(col.name = all_columns) %>%
    mutate(root.col = sub("..value...*", "..value..", col.name)) %>%
    left_join(ref_col, by = c("root.col" = "root.col"))
  
  ref_col_full$col.order[is.na(ref_col_full$col.order)] <- length(db_vec) + 1
  ref_col_full <- ref_col_full %>% arrange(col.order)
  
  db_filled <- db %>% mutate(across(everything(), ~ .x))
  dm_filled <- dm %>% mutate(across(everything(), ~ .x))
  
  for (col in all_columns) {
    if (!col %in% names(db_filled)) {
      db_filled[[col]] <- NA
    }
    if (!col %in% names(dm_filled)) {
      dm_filled[[col]] <- NA
    }
  }
  
  df_combined <- bind_rows(db_filled, dm_filled)
  df_combined[is.na(df_combined)] <- NA
  
  df_combined <- df_combined %>% select(all_of(ref_col_full$col.name))
  
  write.csv(df_combined, output_csv_path, row.names = FALSE)
  write.xlsx(df_combined, output_xlsx_path, row.names = FALSE, showNA = FALSE)
  
  df_indicators = read.csv(output_csv_path, na.strings = 'NA')
  df_indicators = format_indicator_data(df_indicators, questions, choices)
  cat("generating final longitudinal data ...", "\n")
  split_by_level(df_indicators, formatted_output_path, "indicators")
  cat("longitudinal indicators has been generated.", "\n")
}

# get JMMI round
extract_number <- function(filename) {
  as.numeric(str_extract(filename, "(?<=JMMI_R)\\d+"))
}

# get JMMI date
extract_date <- function(filename) {
  match <- regexpr("([A-Za-z]{3}\\d{2})\\.csv$", filename)
  date_abbreviation <- substr(filename, match, match + attr(match, "match.length") - 5)
  month_year <- parse_date_time(date_abbreviation, orders = "my")
  paste0(month(month_year), "/1/", year(month_year))
}


# to match current datamerge with old data, some column names need to be changed
correct_header <- function(df) {
  name_mapping <- data.frame(
    OriginalName = c(
      "items_available_marketplace_wheat..value..Completely_unavailable",
      "items_available_marketplace_wheat..value..Limited_availability",
      "items_available_marketplace_wheat..value..Prefer_not_to_answer",
      "items_available_marketplace_wheat..value..Widely_available",
      "items_available_marketplace_rice..value..Completely_unavailable",
      "items_available_marketplace_rice..value..Limited_availability",
      "items_available_marketplace_rice..value..Prefer_not_to_answer",
      "items_available_marketplace_rice..value..Widely_available",
      "items_available_marketplace_veg_oil..value..Completely_unavailable",
      "items_available_marketplace_veg_oil..value..Limited_availability",
      "items_available_marketplace_veg_oil..value..Prefer_not_to_answer",
      "items_available_marketplace_veg_oil..value..Widely_available",
      "items_available_marketplace_pulses..value..Completely_unavailable",
      "items_available_marketplace_pulses..value..Limited_availability",
      "items_available_marketplace_pulses..value..Prefer_not_to_answer",
      "items_available_marketplace_pulses..value..Widely_available",
      "items_available_marketplace_salt..value..Limited_availability",
      "items_available_marketplace_salt..value..Prefer_not_to_answer",
      "items_available_marketplace_salt..value..Widely_available",
      "items_available_marketplace_sugar..value..Limited_availability",
      "items_available_marketplace_sugar..value..Prefer_not_to_answer",
      "items_available_marketplace_sugar..value..Widely_available",
      "items_available_marketplace_cotton_cloth..value..Completely_unavailable",
      "items_available_marketplace_cotton_cloth..value..dk",
      "items_available_marketplace_cotton_cloth..value..Limited_availability",
      "items_available_marketplace_cotton_cloth..value..Prefer_not_to_answer",
      "items_available_marketplace_cotton_cloth..value..Widely_available",
      "items_available_marketplace_toothbrush_adult..value..Completely_unavailable",
      "items_available_marketplace_toothbrush_adult..value..dk",
      "items_available_marketplace_toothbrush_adult..value..Limited_availability",
      "items_available_marketplace_toothbrush_adult..value..Prefer_not_to_answer",
      "items_available_marketplace_toothbrush_adult..value..Widely_available",
      "items_available_marketplace_toothpaste..value..Completely_unavailable",
      "items_available_marketplace_toothpaste..value..dk",
      "items_available_marketplace_toothpaste..value..Limited_availability",
      "items_available_marketplace_toothpaste..value..Prefer_not_to_answer",
      "items_available_marketplace_toothpaste..value..Widely_available",
      "items_available_marketplace_sanitary_pad..value..Completely_unavailable",
      "items_available_marketplace_sanitary_pad..value..dk",
      "items_available_marketplace_sanitary_pad..value..Limited_availability",
      "items_available_marketplace_sanitary_pad..value..Prefer_not_to_answer",
      "items_available_marketplace_sanitary_pad..value..Widely_available",
      "items_available_marketplace_soap..value..Completely_unavailable",
      "items_available_marketplace_soap..value..dk",
      "items_available_marketplace_soap..value..Limited_availability",
      "items_available_marketplace_soap..value..Prefer_not_to_answer",
      "items_available_marketplace_soap..value..Widely_available",
      "items_available_marketplace_pen..value..Completely_unavailable",
      "items_available_marketplace_pen..value..dk",
      "items_available_marketplace_pen..value..Limited_availability",
      "items_available_marketplace_pen..value..Prefer_not_to_answer",
      "items_available_marketplace_pen..value..Widely_available",
      "items_available_marketplace_notebook..value..Completely_unavailable",
      "items_available_marketplace_notebook..value..dk",
      "items_available_marketplace_notebook..value..Limited_availability",
      "items_available_marketplace_notebook..value..Prefer_not_to_answer",
      "items_available_marketplace_notebook..value..Widely_available",
      "items_available_marketplace_safe_water..value..Completely_unavailable",
      "items_available_marketplace_safe_water..value..dk",
      "items_available_marketplace_safe_water..value..Limited_availability",
      "items_available_marketplace_safe_water..value..Prefer_not_to_answer",
      "items_available_marketplace_safe_water..value..Widely_available",
      "items_available_marketplace_lpg..value..Completely_unavailable",
      "items_available_marketplace_lpg..value..dk",
      "items_available_marketplace_lpg..value..Limited_availability",
      "items_available_marketplace_lpg..value..Prefer_not_to_answer",
      "items_available_marketplace_lpg..value..Widely_available",
      "items_available_marketplace_diesel..value..Completely_unavailable",
      "items_available_marketplace_diesel..value..dk",
      "items_available_marketplace_diesel..value..Limited_availability",
      "items_available_marketplace_diesel..value..Prefer_not_to_answer",
      "items_available_marketplace_diesel..value..Widely_available",
      "items_available_marketplace_petrol..value..Completely_unavailable",
      "items_available_marketplace_petrol..value..dk",
      "items_available_marketplace_petrol..value..Limited_availability",
      "items_available_marketplace_petrol..value..Prefer_not_to_answer",
      "items_available_marketplace_petrol..value..Widely_available",
      "items_available_marketplace_cooking_pot..value..Completely_unavailable",
      "items_available_marketplace_cooking_pot..value..dk",
      "items_available_marketplace_cooking_pot..value..Limited_availability",
      "items_available_marketplace_cooking_pot..value..Prefer_not_to_answer",
      "items_available_marketplace_cooking_pot..value..Widely_available",
      "items_available_marketplace_water_container..value..Completely_unavailable",
      "items_available_marketplace_water_container..value..dk",
      "items_available_marketplace_water_container..value..Limited_availability",
      "items_available_marketplace_water_container..value..Prefer_not_to_answer",
      "items_available_marketplace_water_container..value..Widely_available"
    ),
    NewName = c(
      "items_available_marketplace_wheat..value..completely_unavailable",
      "items_available_marketplace_wheat..value..limited_availability",
      "items_available_marketplace_wheat..value..prefer_not_to_answer",
      "items_available_marketplace_wheat..value..widely_available",
      "items_available_marketplace_rice..value..completely_unavailable",
      "items_available_marketplace_rice..value..limited_availability",
      "items_available_marketplace_rice..value..prefer_not_to_answer",
      "items_available_marketplace_rice..value..widely_available",
      "items_available_marketplace_veg_oil..value..completely_unavailable",
      "items_available_marketplace_veg_oil..value..limited_availability",
      "items_available_marketplace_veg_oil..value..prefer_not_to_answer",
      "items_available_marketplace_veg_oil..value..widely_available",
      "items_available_marketplace_pulses..value..completely_unavailable",
      "items_available_marketplace_pulses..value..limited_availability",
      "items_available_marketplace_pulses..value..prefer_not_to_answer",
      "items_available_marketplace_pulses..value..widely_available",
      "items_available_marketplace_salt..value..limited_availability",
      "items_available_marketplace_salt..value..prefer_not_to_answer",
      "items_available_marketplace_salt..value..widely_available",
      "items_available_marketplace_sugar..value..limited_availability",
      "items_available_marketplace_sugar..value..prefer_not_to_answer",
      "items_available_marketplace_sugar..value..widely_available",
      "items_available_marketplace_cotton_cloth..value..completely_unavailable",
      "items_available_marketplace_cotton_cloth..value..dk",
      "items_available_marketplace_cotton_cloth..value..limited_availability",
      "items_available_marketplace_cotton_cloth..value..prefer_not_to_answer",
      "items_available_marketplace_cotton_cloth..value..widely_available",
      "items_available_marketplace_toothbrush_adult..value..completely_unavailable",
      "items_available_marketplace_toothbrush_adult..value..dk",
      "items_available_marketplace_toothbrush_adult..value..limited_availability",
      "items_available_marketplace_toothbrush_adult..value..prefer_not_to_answer",
      "items_available_marketplace_toothbrush_adult..value..widely_available",
      "items_available_marketplace_toothpaste..value..completely_unavailable",
      "items_available_marketplace_toothpaste..value..dk",
      "items_available_marketplace_toothpaste..value..limited_availability",
      "items_available_marketplace_toothpaste..value..prefer_not_to_answer",
      "items_available_marketplace_toothpaste..value..widely_available",
      "items_available_marketplace_sanitary_pad..value..completely_unavailable",
      "items_available_marketplace_sanitary_pad..value..dk",
      "items_available_marketplace_sanitary_pad..value..limited_availability",
      "items_available_marketplace_sanitary_pad..value..prefer_not_to_answer",
      "items_available_marketplace_sanitary_pad..value..widely_available",
      "items_available_marketplace_soap..value..completely_unavailable",
      "items_available_marketplace_soap..value..dk",
      "items_available_marketplace_soap..value..limited_availability",
      "items_available_marketplace_soap..value..prefer_not_to_answer",
      "items_available_marketplace_soap..value..widely_available",
      "items_available_marketplace_pen..value..completely_unavailable",
      "items_available_marketplace_pen..value..dk",
      "items_available_marketplace_pen..value..limited_availability",
      "items_available_marketplace_pen..value..prefer_not_to_answer",
      "items_available_marketplace_pen..value..widely_available",
      "items_available_marketplace_notebook..value..completely_unavailable",
      "items_available_marketplace_notebook..value..dk",
      "items_available_marketplace_notebook..value..limited_availability",
      "items_available_marketplace_notebook..value..prefer_not_to_answer",
      "items_available_marketplace_notebook..value..widely_available",
      "items_available_marketplace_safe_water..value..completely_unavailable",
      "items_available_marketplace_safe_water..value..dk",
      "items_available_marketplace_safe_water..value..limited_availability",
      "items_available_marketplace_safe_water..value..prefer_not_to_answer",
      "items_available_marketplace_safe_water..value..widely_available",
      "items_available_marketplace_lpg..value..completely_unavailable",
      "items_available_marketplace_lpg..value..dk",
      "items_available_marketplace_lpg..value..limited_availability",
      "items_available_marketplace_lpg..value..prefer_not_to_answer",
      "items_available_marketplace_lpg..value..widely_available",
      "items_available_marketplace_diesel..value..completely_unavailable",
      "items_available_marketplace_diesel..value..dk",
      "items_available_marketplace_diesel..value..limited_availability",
      "items_available_marketplace_diesel..value..prefer_not_to_answer",
      "items_available_marketplace_diesel..value..widely_available",
      "items_available_marketplace_petrol..value..completely_unavailable",
      "items_available_marketplace_petrol..value..dk",
      "items_available_marketplace_petrol..value..limited_availability",
      "items_available_marketplace_petrol..value..prefer_not_to_answer",
      "items_available_marketplace_petrol..value..widely_available",
      "items_available_marketplace_cooking_pot..value..completely_unavailable",
      "items_available_marketplace_cooking_pot..value..dk",
      "items_available_marketplace_cooking_pot..value..limited_availability",
      "items_available_marketplace_cooking_pot..value..prefer_not_to_answer",
      "items_available_marketplace_cooking_pot..value..widely_available",
      "items_available_marketplace_water_container..value..completely_unavailable",
      "items_available_marketplace_water_container..value..dk",
      "items_available_marketplace_water_container..value..limited_availability",
      "items_available_marketplace_water_container..value..prefer_not_to_answer",
      "items_available_marketplace_water_container..value..widely_available"
    )
  )
  
  # Create a named vector for renaming
  rename_vector <- setNames(name_mapping$NewName, name_mapping$OriginalName)
  
  # Ensure names match existing columns
  rename_vector <- rename_vector[names(rename_vector) %in% names(df)]
  
  # Rename the columns
  df_renamed <- df %>% rename_with(~ rename_vector[.x], .cols = names(rename_vector))
  
  return(df_renamed)
}
