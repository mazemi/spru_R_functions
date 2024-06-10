

{ 

library(dplyr)
library(readxl)
library(openxlsx)

# -----------------------
# dimension     max score
# -----------------------
# availability:     30
# accessibility:    25
# resilience:       20
# affordability:    15
# infrastructure:   10
# -----------------------

# Winter months: Oct, Nov, Dec, Jan, Feb, Mar
winterization <- FALSE
JMMI_round <- "R47"

df_all <- read.csv("./input/AFG2002_JMMI_R47_May24_recoded.csv")
district_all <- read.xlsx("./input/AFG_Districts.xlsx")

# districts with less than four records:
small_data_districts <- df_all %>%
  group_by(afg_dist_code) %>%
  summarise(survey_count = n(),) %>%
  filter(survey_count < 4)

# exclude districts with less than four records from the data:
df <- df_all %>% filter(!(afg_dist_code %in% small_data_districts$afg_dist_code))

# adjustment data: 10, Jun, 2024
CONVERSION.CONFIG <- list(
  availability = 0.937, 
  accessibility_with_winterization = 1.470,
  resilience_with_winterization = 0.289,
  resilience_without_winterization = 0.350,
  affordability = 1.666,
  infrastructure = 1  
)

}

#------------------------- part1: availability   ------------------------#

{

# constant availability configuration indicator
# format: c(wide availability score, limited availability score)
AVAILABILITY.RECODE<- list(
  wheat = c(4, 2),
  rice = c(4, 2),
  pulses = c(2, 1),
  veg_oil = c(4, 2),
  sugar = c(1.5, 1),
  salt = c(1.5, 1),
  soap = c(3, 2),
  toothbrush_adult = c(1, 0.5),
  toothpaste = c(1, 0.5),
  sanitary_pad = c(1, 0.5),
  pen = c(1, 0.5),
  notebook = c(1, 0.5),
  diesel = c(2, 1),
  petrol = c(2, 1),
  lpg = c(3, 2)
)

# re-code availability mark for each record
recode_availability <- function(df, config) {
  # list of variables for calculation of availability:
  items_available_marketplace <- paste0("items_available_marketplace_", names(config))

  items_availability_df <- df %>% select(afg_dist_code, all_of(items_available_marketplace))
  
  # Initialize an empty data frame to store the results
  recoded_df <- data.frame(afg_dist_code = items_availability_df$afg_dist_code)
  
  for (item in names(config)) {
    col_name <- paste0(item, "_availability_mark")
    recoded_df[[col_name]] <- case_when(
      items_availability_df[[paste0("items_available_marketplace_", item)]] == "Widely_available" ~ config[[item]][1],
      items_availability_df[[paste0("items_available_marketplace_", item)]] == "Limited_availability" ~ config[[item]][2],
      items_availability_df[[paste0("items_available_marketplace_", item)]] == "Completely_unavailable" ~ 0,
      TRUE ~ NA_real_
    )
  }
  return(recoded_df)
}

# mode function for the calculation of availability indicator. we are applying an optimistic approach 
# for the availability indicator in case of having multiple mode values
getmode <- function(score) {
  score <- score[!is.na(score)]
  score <- sort(score, decreasing = TRUE)
  unique_score <- unique(score)
  unique_score[which.max(tabulate(match(score, unique_score)))]
}

calculate_availability <- function(df) {
    avail_df <- df %>%
    group_by(afg_dist_code) %>%
    summarise(
      survey_count = n(),
      across(ends_with("_mark"), ~ getmode(.x))
    )
    
    avail_df <- avail_df %>% 
      mutate(
        availability_mark = rowSums(across(ends_with("_mark")), na.rm = TRUE),
        # mapping score to maximum score
        availability_score = availability_mark * CONVERSION.CONFIG$availability
      )
    avail_df <- avail_df %>% 
      mutate_if(~ is.numeric(.), ~ round(., 1))
    
  return(avail_df)
}

# final results:
recoded_availability <- recode_availability(df, AVAILABILITY.RECODE)
availability_info_full <- calculate_availability(recoded_availability) 
availability_info <- availability_info_full %>% select(afg_dist_code, availability_score)
}

#------------------------- part2: Affordability  ------------------------#

{
food_nfi_item_prices <- c(
  # food items
  "wheat_price",
  "rice_price",
  "veg_oil_price",
  "pulses_lentils_price",
  "pulses_beans_price",
  "pulses_split_peas_price",
  "salt_price",
  "sugar_price",
  # NFI
  "sanitary_pad_price",
  "cotton_cloth_price",
  "toothbrush_adult_price",
  "toothpaste_price",
  "soap_price",
  "pen_price",
  "notebook_price",
  "safe_water_price",
  "lpg_price",
  "diesel_price",
  "petrol_price",
  "cooking_pot_price",
  "water_container_price"
)

winterization_item_prices <- c(
  "firewood_price",
  "coal_price",
  "blanket_price",
  "winter_jacket_price"
)

# Define a function to calculate median prices for each districts
calculate_median_prices <- function(df, item_prices) {
  df %>%
    select(afg_dist_code, all_of(item_prices)) %>%
    group_by(afg_dist_code) %>%
    summarise(
      survey_count = n(),
      across(everything(), ~ median(as.numeric(.x), na.rm = TRUE))
    )
}

# Define a function to calculate national median prices 
calculate_national_median_prices <- function(df, item_prices) {
  df %>%
    select(all_of(item_prices)) %>%
    summarise(
      across(everything(), ~ median(as.numeric(.x), na.rm = TRUE))
    )
}

# Calculate median prices for food/NFI items
district_median_prices <- calculate_median_prices(df, c(food_nfi_item_prices, winterization_item_prices))
national_median_prices <- calculate_national_median_prices(df, c(food_nfi_item_prices, winterization_item_prices))

# Price point modifier function:
price_point_modifier <- function(item_median, national_prices_df) {
  item <- substitute(item_median)
  national_price <- national_prices_df[[deparse(item)]] # Fetching the national price
  case_when(
    item_median <= national_price * 0.5 ~ 2,
    item_median > national_price * 0.5 & item_median <= national_price * 0.75 ~ 1.5,
    item_median > national_price * 0.75 & item_median <= national_price * 0.90 ~ 1,
    item_median > national_price * 0.90 & item_median <= national_price * 1.1 ~ 0,
    item_median > national_price * 1.1 & item_median <= national_price * 1.25 ~ -1,
    item_median > national_price * 1.25 & item_median <= national_price * 1.5 ~ -1.5,
    item_median > national_price * 1.5 ~ -2, 
    TRUE ~ NA_real_
  )
}

# Adding price points for all districts:
district_price_modified <- district_median_prices %>%
  mutate(
    across(
      c(food_nfi_item_prices, winterization_item_prices),
      ~ price_point_modifier(., national_median_prices),
      .names = "{.col}_point"
    )
  ) 

district_price_modified <- district_price_modified %>%
  mutate(sum_point = rowSums(select(., ends_with("_point")), na.rm = TRUE)) %>%
  select(afg_dist_code, sum_point)

# Financial Access:
financial_info <- df %>%
  select(
    afg_dist_code,
    can_not_afford = financial_access.can_not_afford,
    cannot_pay = financial_access.cannot_pay,
    expensive_transportation = financial_access.expensive_transportation,
    expensive_fuel = financial_access.expensive_fuel
  ) %>%
  mutate(
    financial_access_problem = ifelse(
      can_not_afford | cannot_pay | expensive_transportation | expensive_fuel, 1, 0
    )
  ) %>%
  group_by(afg_dist_code) %>%
  summarise(
    survey_count = n(),
    financial_access_problem_count = sum(financial_access_problem),
    financial_access_problem_percentage = financial_access_problem_count / survey_count * 100,
    financial_access_mark = case_when(
      financial_access_problem_percentage <= 10 ~ 9,
      financial_access_problem_percentage > 10 & financial_access_problem_percentage <= 25 ~ 6,
      financial_access_problem_percentage > 25 & financial_access_problem_percentage <= 50 ~ 3,
      financial_access_problem_percentage > 50 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  select(afg_dist_code, financial_access_mark)

# combining affordability financial_info with district_prices_point
affordability_info <- financial_info %>%
  left_join(district_price_modified, by = c("afg_dist_code" = "afg_dist_code")) %>% 
  mutate(
    affordability_mark = case_when(
      financial_access_mark + sum_point <= 9 & financial_access_mark + sum_point >= 0 ~ financial_access_mark + sum_point,
      financial_access_mark + sum_point > 9 ~ 9,
      financial_access_mark + sum_point < 0 ~ 0 
    ),
    
    # mapping score to maximum score
    affordability_score = affordability_mark * CONVERSION.CONFIG$affordability
  ) %>% 
  mutate_if(~ is.numeric(.), ~ round(., 1))

}

#------------------------- part3: Resilience     ------------------------#

{
# 3.1 Supply chain disruption
# list of all records that only have supply chain issue:
supply_chain_disruption_df <- df %>%
  select(difficulty_supply_items_reasons, "X_uuid") %>%
  filter(!is.na(difficulty_supply_items_reasons)) %>%
  filter(difficulty_supply_items_reasons != "prefer_not_to_answer")
supply_chain_disruption_df$disruption <- 1

# list of all records if there is any supply chain issue, the disruption column will be 1 otherwise 0.
all_supply_chain_df <- df %>%
  select(afg_dist_code, "X_uuid") %>%
  left_join(supply_chain_disruption_df, by = c("X_uuid" = "X_uuid")) %>%
  select(afg_dist_code, disruption)

# supply chain score for all districts:
district_supply_chain <- all_supply_chain_df %>%
  group_by(afg_dist_code) %>%
  summarise(
    survey_count = n(),
    disruption = sum(disruption, na.rm = TRUE),
    disruption_percentage = disruption / survey_count * 100
  ) %>%
  mutate(
    disruption_mark = case_when(
      disruption_percentage <= 5 ~ 12,
      disruption_percentage > 5 & disruption_percentage <= 10 ~ 9,
      disruption_percentage > 10 & disruption_percentage <= 25 ~ 6,
      disruption_percentage > 25 & disruption_percentage <= 50 ~ 3,
      disruption_percentage > 50 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  select(afg_dist_code, disruption_mark)

# 3.2 Supplier Diversity for Food
district_one_food_supplier <- df %>%
  select(afg_dist_code, food_supplier_count) %>%
  group_by(afg_dist_code) %>%
  summarise(
    survey_count = n(),
    one_food_supplier = sum(food_supplier_count[food_supplier_count == 1]),
    one_food_supplier_percentage = one_food_supplier / survey_count * 100
  ) %>%
  mutate(
    one_food_supplier_mark = case_when(
      one_food_supplier_percentage <= 20 ~ 3,
      one_food_supplier_percentage > 20 & one_food_supplier_percentage <= 30 ~ 2,
      one_food_supplier_percentage > 30 & one_food_supplier_percentage <= 40 ~ 1,
      one_food_supplier_percentage > 40 ~ 0, # maybe un-necessary
      TRUE ~ 0 
    )
  ) %>%
  select(afg_dist_code, one_food_supplier_mark)

# 3.3 Supplier Diversity for NFI
district_one_nfi_supplier <- df %>%
  select(afg_dist_code, nfi_supplier_count) %>%
  group_by(afg_dist_code) %>%
  summarise(
    survey_count = n(),
    one_nfi_supplier = sum(nfi_supplier_count[nfi_supplier_count == 1]),
    one_nfi_supplier_percentage = one_nfi_supplier / survey_count * 100
  ) %>%
  mutate(
    one_nfi_supplier_mark = case_when(
      one_nfi_supplier_percentage <= 20 ~ 3,
      one_nfi_supplier_percentage > 20 & one_nfi_supplier_percentage <= 30 ~ 2,
      one_nfi_supplier_percentage > 30 & one_nfi_supplier_percentage <= 40 ~ 1,
      one_nfi_supplier_percentage > 40 ~ 0, # maybe un-necessary
      TRUE ~ 0 
    )
  ) %>%
  select(afg_dist_code, one_nfi_supplier_mark)

# 3.4 Restocking 
district_restock <- df %>% 
  select(afg_dist_code, starts_with("diff_stock_restock")) %>%
  group_by(afg_dist_code) %>%
  summarise(across(starts_with("diff_stock_restock"),
                   ~ median(as.numeric(.), na.rm = TRUE),
                   .names = "restock_{col}_median"))

calculate_restock_mark <- function(restock_median) {
  case_when(
    restock_median >= 3 ~ 3,
    restock_median > 0 & restock_median < 3 ~ 2,
    restock_median == 0 ~ 1,
    restock_median < 0 ~ 0,
    TRUE ~ NA_real_
  )
}

district_restock <- district_restock %>%
  mutate(across(starts_with("restock_diff"), ~ calculate_restock_mark(.), .names = "{col}_mark"))

district_restock <- district_restock %>%
  mutate(restock_mark = rowSums(select(., ends_with("_mark")), na.rm = TRUE))

# combine resilience sections:
resilience_info <- district_supply_chain %>%
  inner_join(district_one_food_supplier, by = c("afg_dist_code" = "afg_dist_code")) %>%
  inner_join(district_one_nfi_supplier, by = c("afg_dist_code" = "afg_dist_code")) %>%
  inner_join(district_restock, by = c("afg_dist_code" = "afg_dist_code"))

resilience_info <- district_supply_chain %>%
  inner_join(district_one_food_supplier, by = "afg_dist_code") %>%
  inner_join(district_one_nfi_supplier, by = "afg_dist_code") %>%
  inner_join(district_restock, by = "afg_dist_code") %>%
  mutate(
    resilience_mark = disruption_mark + one_food_supplier_mark + one_nfi_supplier_mark + restock_mark,
    resilience_score = resilience_mark * if_else(
      winterization == TRUE, 
      CONVERSION.CONFIG$resilience_with_winterization, 
      CONVERSION.CONFIG$resilience_without_winterization
    )
  ) %>%
  mutate(across(where(is.numeric), round, 1)) %>% 
  select(- starts_with("restock_diff"))

}

#------------------------- part3: Resilience     ------------------------#