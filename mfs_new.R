

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
  accessibility = 1.470,
  resilience_with_winterization = 0.289,
  resilience_without_winterization = 0.350,
  affordability = 1.666,
  infrastructure = 1  
)

}

#------------------------- part1: Availability ---------------------------#

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

#------------------------- part2: Affordability --------------------------#

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

#------------------------- part3: Resilience -----------------------------#

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

#------------------------- part4: Infrastructure -------------------------#

{
# 4.1 Adequacy of and damage to facilities
damaged_facilities_df <- df %>% select(afg_dist_code, physical_access.hazardous_building)
district_damaged_facilities_df <- damaged_facilities_df %>%
  group_by(afg_dist_code) %>%
  summarise(
    survey_count = n(),
    damaged_facilities = sum(physical_access.hazardous_building),
    damaged_facilities_percentage = damaged_facilities / survey_count * 100,
  ) %>%
  mutate(
    damaged_facilities_mark = case_when(
      damaged_facilities_percentage <= 5 ~ 4,
      damaged_facilities_percentage > 5 & damaged_facilities_percentage <= 10 ~ 3,
      damaged_facilities_percentage > 10 & damaged_facilities_percentage <= 25 ~ 2,
      damaged_facilities_percentage > 25 & damaged_facilities_percentage <= 50 ~ 1,
      damaged_facilities_percentage > 50 ~ 0,
      TRUE ~ 0
    )
  ) %>%
  select(afg_dist_code, damaged_facilities_mark)

# 4.2 Storage
storage_df <- df %>% select(afg_dist_code, storage)
storage_df <- storage_df %>% mutate(
  no_storage = if_else(storage == "yes_my_own" | storage == "yes_elsewhere", 0, 1)
)

district_storage_df <- storage_df %>%
  group_by(afg_dist_code) %>%
  summarise(
    survey_count = n(),
    no_storage_sum = sum(no_storage),
    no_storage_percentage = no_storage_sum / survey_count * 100,
  ) %>%
  mutate(
    no_storage_mark = case_when(
      no_storage_percentage <= 10 ~ 3,
      no_storage_percentage > 10 & no_storage_percentage <= 25 ~ 2,
      no_storage_percentage > 25 & no_storage_percentage <= 50 ~ 1,
      no_storage_percentage > 50 ~ 0, # maybe un-necessary
      TRUE ~ 0
    )
  ) %>%
  select(afg_dist_code, no_storage_mark)

# 4.3 Payment Moralities
payment_df <- df %>%
  select(
    afg_dist_code,
    modality.mobile_money,
    modality.credit,
    modality.barter,
    modality.voucher,
    modality.other
  ) %>%
  rowwise() %>%
  mutate(
    # this field will be 1 if the KI says we have multiple payment moralities along with cash types modalities.
    modality_ok = if_else(
      sum(c_across(starts_with("modality."))) >= 1, 1, 0
    )
  )

district_payment_df <- payment_df %>%
  group_by(afg_dist_code) %>%
  summarise(
    survey_count = n(),
    modality_sum = sum(modality_ok[modality_ok >= 1]),
    modality_percetage = modality_sum / survey_count * 100,
    modality_mark = case_when(
      modality_percetage >= 75 ~ 3,
      modality_percetage >= 50 & modality_percetage < 75 ~ 2,
      modality_percetage >= 25 & modality_percetage < 50 ~ 1,
      modality_percetage < 25 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Final Infrastructure Index
infrastructure_info <- district_damaged_facilities_df %>%
  inner_join(district_storage_df, by = "afg_dist_code") %>%
  inner_join(district_payment_df, by = "afg_dist_code") %>%
  mutate(
    infrastructure_mark = damaged_facilities_mark + no_storage_mark + modality_mark,
    infrastructure_score = infrastructure_mark * CONVERSION.CONFIG$infrastructure
  ) %>%
  mutate(across(where(is.numeric), round, 1)) %>% 
  select(-c(modality_sum, modality_percetage))
}

#------------------------- part5: Accessibility --------------------------#

{
physical_access_df <- df %>%
  select(
    afg_dist_code,
    physical_access.hazardous_building,
    physical_access.movement_restrictions,
    physical_access.ongoing_fighting,
    physical_access.inadequate_facilities,
    physical_access.limited_transportation,
    physical_access.vendors_difficult,
    physical_access.operates_limited_times,
    physical_access.customers_not_safe,
    physical_access.distance_market,
  ) %>%
  rowwise() %>%
  mutate(
    physical_access_issue = if_else(
      sum(c_across(physical_access.hazardous_building:physical_access.distance_market)) >= 1, 1, 0
    )
  ) %>%
  select(afg_dist_code, physical_access_issue)

district_physical_access_df <- physical_access_df %>%
  group_by(afg_dist_code) %>%
  summarise(
    survey_count = n(),
    physical_access_issue = sum(physical_access_issue),
    physical_access_issue_percentage = physical_access_issue / survey_count * 100,
  ) %>%
  mutate(
    physical_access_issue_mark = case_when(
      physical_access_issue_percentage <= 5 ~ 8,
      physical_access_issue_percentage > 5 & physical_access_issue_percentage <= 10 ~ 6,
      physical_access_issue_percentage > 10 & physical_access_issue_percentage <= 25 ~ 4,
      physical_access_issue_percentage > 25 & physical_access_issue_percentage <= 50 ~ 2,
      physical_access_issue_percentage > 50 ~ 0,
      TRUE ~ NA_real_
    )
  )

# 5.2 Adequacy of and damage to facilities
hazards_roads_df <- df %>% select(afg_dist_code, physical_access.hazards_roads)

district_hazards_roads_df <- hazards_roads_df %>%
  group_by(afg_dist_code) %>%
  summarise(
    survey_count = n(),
    hazards_roads = sum(physical_access.hazards_roads, na.rm = TRUE), # 16/4/2023: na.rm = TRUE has been added.
    hazards_roads_percentage = hazards_roads / survey_count * 100,
  ) %>%
  mutate(
    hazards_roads_mark = case_when(
      hazards_roads_percentage <= 5 ~ 4,
      hazards_roads_percentage > 5 & hazards_roads_percentage <= 10 ~ 3,
      hazards_roads_percentage > 10 & hazards_roads_percentage <= 25 ~ 2,
      hazards_roads_percentage > 25 & hazards_roads_percentage <= 50 ~ 1,
      hazards_roads_percentage > 50 ~ 0,
      TRUE ~ 0
    )
  )

# 5.3 Safety and security
security_df <- df %>% select(afg_dist_code, access_security)

district_security_df <- security_df %>%
  group_by(afg_dist_code) %>%
  summarise(
    survey_count = n(),
    access_security = sum(access_security == "yes"),
    access_security_percentage = access_security / survey_count * 100,
  ) %>%
  mutate(
    access_security_mark = case_when(
      access_security_percentage <= 5 ~ 3,
      access_security_percentage > 5 & access_security_percentage <= 10 ~ 2,
      access_security_percentage > 10 & access_security_percentage <= 20 ~ 1,
      access_security_percentage > 20 ~ 0,
      TRUE ~ NA_real_
    )
  )

# 5.4 Social access
social_df <- df %>%
  select(
    afg_dist_code,
    social_access.yes_ethnicity,
    social_access.yes_religion
  ) %>%
  mutate(
    social_access_issue = if_else(social_access.yes_ethnicity == 1 | social_access.yes_religion == 1, 1, 0)
  )

district_social_df <- social_df %>%
  group_by(afg_dist_code) %>%
  summarise(
    social_access_issue = sum(social_access_issue),
  ) %>%
  mutate(
    social_access_mark = case_when(
      social_access_issue == 0 ~ 2,
      TRUE ~ 0
    )
  )

# final accessibility info:
accessibility_info <- district_physical_access_df %>%
  inner_join(district_hazards_roads_df, by = "afg_dist_code") %>%
  inner_join(district_security_df, by = "afg_dist_code") %>%
  inner_join(district_social_df, by = "afg_dist_code") %>%
  mutate(
    accessibility_mark = physical_access_issue_mark  + hazards_roads_mark  +
      access_security_mark  + social_access_mark ,
    accessibility_score = accessibility_mark* CONVERSION.CONFIG$accessibility
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>% 
  select(afg_dist_code, survey_count, ends_with("_mark"), accessibility_score)
    
}

#------------------------- part6: Combining all MFS parts ----------------#

mfs <- district_physical_access_df %>%
  inner_join(availability_info, by = c("afg_dist_code" = "afg_dist_code")) %>%
  inner_join(accessibility_info, by = c("afg_dist_code" = "afg_dist_code")) %>%
  inner_join(resilience_info, by = c("afg_dist_code" = "afg_dist_code")) %>%
  inner_join(affordability_info, by = c("afg_dist_code" = "afg_dist_code")) %>%
  inner_join(infrastructure_info, by = c("afg_dist_code" = "afg_dist_code")) %>%
  select(afg_dist_code, ends_with("score"))

mfs$MFS <- mfs$availability_score + mfs$affordability_score + mfs$resilience_score +
  mfs$infrastructure_score + mfs$accessibility_score

# add all district labels including districts with less than four records:
district_lables <- df_all %>%
  select(afg_dist_code, afg_dist) %>%
  distinct()
mfs <- district_lables %>% left_join(mfs, by = c("afg_dist_code" = "afg_dist_code"))

# district classification
mfs <- mfs %>%
  mutate(
    low_accessibility = if_else(mfs$accessibility_score < 12.5, 1, 0),
    low_availability = if_else(mfs$availability_score < 15, 1, 0),
    low_affordability = if_else(mfs$affordability_score < 7.5, 1, 0),
    low_resilience = if_else(mfs$resilience_score < 10, 1, 0),
    low_infrastructure = if_else(mfs$infrastructure_score < 5, 1, 0),
    
    # number of poor indicators less than 50% of their max scores
    low_dimension = low_accessibility + low_availability + low_affordability +
      low_resilience + low_infrastructure,
    classification = case_when(
      mfs$MFS >= 80 & low_dimension == 0 ~ "Full functionality",
      mfs$MFS >= 60 | low_dimension <= 1 ~ "Limited functionality",
      (mfs$MFS >= 25 & mfs$MFS < 60) | low_dimension <= 2 ~ "Poor functionality",
      mfs$MFS < 25 | low_dimension >= 3 ~ "Severe issues",
      TRUE ~ "Insufficient data"
    ),
    sorted = case_when(
      classification == "Full functionality" ~ 5,
      classification == "Limited functionality" ~ 4,
      classification == "Poor functionality" ~ 3,
      classification == "Severe issues" ~ 2,
      classification == "Insufficient data" ~ 1
    )
  ) %>%
  select(
    afg_dist_code, afg_dist, availability_score, accessibility_score, resilience_score,
    affordability_score, infrastructure_score, MFS, classification, sorted
  ) %>%
  arrange(desc(MFS)) %>%
  arrange(desc(sorted)) %>%
  select(-sorted)

mfs <- mfs %>% mutate_if(~ is.numeric(.), ~ round(., 1))

# rename MFS column names
colnames(mfs)[colnames(mfs) == "afg_dist_code"] <- "district code"
colnames(mfs)[colnames(mfs) == "afg_dist"] <- "district"
colnames(mfs)[colnames(mfs) == "availability_score"] <- "availability [30]"
colnames(mfs)[colnames(mfs) == "accessibility_score"] <- "accessibility [25]"
colnames(mfs)[colnames(mfs) == "resilience_score"] <- "resilience [20]"
colnames(mfs)[colnames(mfs) == "affordability_score"] <- "affordability [15]"
colnames(mfs)[colnames(mfs) == "infrastructure_score"] <- "infrastructure [10]"

merged_mfs <- left_join(district_all, mfs, by = c("Dstrct_Cod" = "district code"))
dashboard_mfs <- merged_mfs %>% mutate(mfi_map = case_when(
  classification == "Full functionality" ~ "Full functionality",
  classification == "Insufficient data" ~ "Insufficient data",
  classification == "Limited functionality" ~ "Limited functionality",
  classification == "Poor functionality" ~ "Poor functionality",
  is.null(classification) | is.na(classification) ~ "Not covered",
  TRUE ~ "Not covered"
))

# MFS output as an MS Excel file
mfs_file <- createWorkbook()
addWorksheet(mfs_file, "MFS")
addWorksheet(mfs_file, "availability")
addWorksheet(mfs_file, "accessibility")
addWorksheet(mfs_file, "resilience")
addWorksheet(mfs_file, "affordability")
addWorksheet(mfs_file, "infrastructure")

# basic style for the output file:
modifyBaseFont(mfs_file, fontSize = 10)

header_style <- createStyle(
  halign = "center", valign = "center", wrapText = T,
  fgFill = "#bcbcbc", border = "TopBottomLeftRight", borderColour = "#000000"
)

# red style if the the dimension is less than half of it's max value
low_value_style <- createStyle(fontColour = "#f50000")

addStyle(mfs_file, sheet = 1, header_style, rows = 1, cols = 1:9, gridExpand = TRUE)
conditionalFormatting(mfs_file,
  sheet = 1, cols = 3,
  rows = 1:nrow(mfs) + 1, rule = " < 15", type = "expression", style = low_value_style
)

conditionalFormatting(mfs_file,
  sheet = 1, cols = 4,
  rows = 1:nrow(mfs) + 1, rule = " < 12.5", type = "expression", style = low_value_style
)

conditionalFormatting(mfs_file,
  sheet = 1, cols = 5,
  rows = 1:nrow(mfs) + 1, rule = " < 10", type = "expression", style = low_value_style
)

conditionalFormatting(mfs_file,
  sheet = 1, cols = 6,
  rows = 1:nrow(mfs) + 1, rule = " < 7.5", type = "expression", style = low_value_style
)

conditionalFormatting(mfs_file,
  sheet = 1, cols = 7,
  rows = 1:nrow(mfs) + 1, rule = " < 5", type = "expression", style = low_value_style
)

setColWidths(mfs_file, sheet = 1, cols = 1:9, widths = 18)
setColWidths(mfs_file, sheet = 2, cols = 1:ncol(availability_info_full), widths = "auto")
setColWidths(mfs_file, sheet = 3, cols = 1:ncol(accessibility_info), widths = "auto")
setColWidths(mfs_file, sheet = 4, cols = 1:ncol(resilience_info), widths = "auto")
setColWidths(mfs_file, sheet = 5, cols = 1:ncol(affordability_info), widths = "auto")
setColWidths(mfs_file, sheet = 6, cols = 1:ncol(infrastructure_info), widths = "auto")

writeData(mfs_file, sheet = "MFS", x = mfs)
writeData(mfs_file, sheet = "availability", x = availability_info_full)
writeData(mfs_file, sheet = "accessibility", x = accessibility_info)
writeData(mfs_file, sheet = "resilience", x = resilience_info)
writeData(mfs_file, sheet = "affordability", x = affordability_info)
writeData(mfs_file, sheet = "infrastructure", x = infrastructure_info)

saveWorkbook(mfs_file, paste0("./output/MFS_", JMMI_round, "_", format(Sys.time(), "%d_%b_%Y"), ".xlsx"), overwrite = T)
write.csv(dashboard_mfs,"./output/MFI attributes.csv")

    