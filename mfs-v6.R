library(dplyr)
library(readxl)
library(openxlsx)

JMMI_round <- "R36"

# import recoded data:
df_all <- read.csv("./input/AFG2002_JMMI_R36_JUN23_recoded.csv")

# importing previous round food basket data:
# sheet number changed to 3 from 5, because the excel file has only three main sheets,updated on the 25/06/2023
previous_food_basket <- read_xlsx("./input/District_Median_JMMI_R35_MAY23.xlsx", sheet = 3) %>% 
  select(afg_district, previous_food_basket = food_basket_AFN) # need to change to Food_basket_AFN

# districts with less than four records:
small_data_districts <- df_all %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
  ) %>% 
  filter(survey_count < 4)

# exclude districts with less than four records from the data:
df <- df_all %>% filter(!(afg_dist_code %in% small_data_districts$afg_dist_code))

# winterization constant:
include_winterization <- FALSE

# constant availability configuration indicator:
AVALABILITY.CONFIG <- list(
  wheat_widely_available = 4,
  wheat_limited_available = 2,
  
  rice_widely_available = 4,
  rice_limited_available = 2,
  
  pulses_lentils_widely_available = 2,
  pulses_lentils_limited_available = 1,
  
  pulses_beans_widely_available = 2,
  pulses_beans_limited_available = 1,
  
  pulses_split_peas_widely_available = 2,
  pulses_split_peas_limited_available = 1,
  
  tomatoes_widely_available = 4,
  tomatoes_limited_available = 2,
  
  vegetable_oil_widely_available = 4,
  vegetable_oil_limited_available = 2,
  
  sugar_widely_available = 1.5,
  sugar_limited_available = 1,
  
  salt_widely_available = 1.5,
  salt_limited_available = 1,
  
  soap_widely_available = 3,
  soap_limited_available = 2,
  
  toothbrush_adult_widely_available = 1,
  toothbrush_adult_limited_available = .5,
  
  toothpaste_widely_available = 1,
  toothpaste_limited_available = .5,
  
  sanitary_pad_widely_available = 1,
  sanitary_pad_limited_available = .5,
  
  pen_widely_available = 1, 
  pen_limited_available = .5,
  
  rubber_widely_available = 1, 
  rubber_limited_available = .5,
  
  notebook_widely_available = 1, 
  notebook_limited_available = .5, 
  
  diesel_widely_available = 2,
  diesel_limited_available = 1,
  
  petrol_widely_available = 2,
  petrol_limited_available = 1,
  
  lpg_widely_available = 3, 
  lpg_limited_available = 2, 
  
  firewood_widely_available = 2,
  firewood_limited_available = 1,
  
  blanket_widely_available = 3,
  blanket_limited_available = 2,
  
  winter_jacket_widely_available = 3,
  winter_jacket_limited_available = 2
)

# make the availability configuration as a constant:
# lockBinding("AVALABILITY.CONFIG", globalenv())
# lockBinding("include_winterization", globalenv())

# list of variables for calculation of availability:
items_available_marketplace <- c(
  "items_available_marketplace_wheat_local",
  "items_available_marketplace_wheat_imported",
  "items_available_marketplace_local_rice",
  "items_available_marketplace_veg_oil",
  "items_available_marketplace_pulses_lentils",
  "items_available_marketplace_pulses_beans",
  "items_available_marketplace_pulses_split_peas",
  "items_available_marketplace_salt",
  "items_available_marketplace_sugar",
  "items_available_marketplace_tomatoes",
  "items_available_marketplace_toothbrush_adult",
  "items_available_marketplace_toothpaste",
  "items_available_marketplace_sanitary_pad",
  "items_available_marketplace_soap",
  "items_available_marketplace_pen",
  "items_available_marketplace_notebook",
  "items_available_marketplace_rubber",
  "items_available_marketplace_firewood",
  "items_available_marketplace_lpg",
  "items_available_marketplace_diesel",
  "items_available_marketplace_petrol",
  "items_available_marketplace_blanket",
  "items_available_marketplace_winter_jacket" 
  
  # excluded items
  # "items_available_marketplace_cotton_cloth",
  # "items_available_marketplace_safe_water",
  # "items_available_marketplace_coal", 
  # "items_available_marketplace_cooking_pot",
  # "items_available_marketplace_water_container",
)

# 1 Availability index ***********************************
items_availability_df <- df %>% select(afg_dist_code, all_of(items_available_marketplace))

items_availability_df <- items_availability_df %>% mutate(
  wheat_local_availability_score = case_when(
    items_available_marketplace_wheat_local == "Widely_available" ~ AVALABILITY.CONFIG$wheat_widely_available,
    items_available_marketplace_wheat_local == "Limited_availability" ~ AVALABILITY.CONFIG$wheat_limited_available,
    items_available_marketplace_wheat_local == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  wheat_imported_availability_score = case_when(
    items_available_marketplace_wheat_imported == "Widely_available" ~ AVALABILITY.CONFIG$wheat_widely_available,
    items_available_marketplace_wheat_imported == "Limited_availability" ~ AVALABILITY.CONFIG$wheat_limited_available,
    items_available_marketplace_wheat_imported == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  rice_availability_score = case_when(
    items_available_marketplace_local_rice == "Widely_available" ~ AVALABILITY.CONFIG$rice_widely_available,
    items_available_marketplace_local_rice == "Limited_availability" ~ AVALABILITY.CONFIG$rice_limited_available,
    items_available_marketplace_local_rice == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  veg_oil_availability_score = case_when(
    items_available_marketplace_veg_oil == "Widely_available" ~ AVALABILITY.CONFIG$vegetable_oil_widely_available,
    items_available_marketplace_veg_oil == "Limited_availability" ~ AVALABILITY.CONFIG$vegetable_oil_limited_available,
    items_available_marketplace_veg_oil == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  pulses_lentils_availability_score = case_when(
    items_available_marketplace_pulses_lentils == "Widely_available" ~ AVALABILITY.CONFIG$pulses_lentils_widely_available,
    items_available_marketplace_pulses_lentils == "Limited_availability" ~ AVALABILITY.CONFIG$pulses_lentils_limited_available,
    items_available_marketplace_pulses_lentils == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  pulses_beans_availability_score = case_when(
    items_available_marketplace_pulses_beans == "Widely_available" ~ AVALABILITY.CONFIG$pulses_beans_widely_available,
    items_available_marketplace_pulses_beans == "Limited_availability" ~ AVALABILITY.CONFIG$pulses_beans_limited_available,
    items_available_marketplace_pulses_beans == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  pulses_split_peas_availability_score = case_when(
    items_available_marketplace_pulses_split_peas == "Widely_available" ~ AVALABILITY.CONFIG$pulses_split_peas_widely_available,
    items_available_marketplace_pulses_split_peas == "Limited_availability" ~ AVALABILITY.CONFIG$pulses_lentils_limited_available,
    items_available_marketplace_pulses_split_peas == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  salt_availability_score = case_when(
    items_available_marketplace_salt == "Widely_available" ~ AVALABILITY.CONFIG$salt_widely_available,
    items_available_marketplace_salt == "Limited_availability" ~ AVALABILITY.CONFIG$salt_limited_available,
    items_available_marketplace_salt == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  sugar_availability_score = case_when(
    items_available_marketplace_sugar == "Widely_available" ~ AVALABILITY.CONFIG$sugar_widely_available,
    items_available_marketplace_sugar == "Limited_availability" ~ AVALABILITY.CONFIG$sugar_limited_available,
    items_available_marketplace_sugar == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
 tomatoes_availability_score = case_when(
    items_available_marketplace_tomatoes == "Widely_available" ~ AVALABILITY.CONFIG$tomatoes_widely_available,
    items_available_marketplace_tomatoes == "Limited_availability" ~ AVALABILITY.CONFIG$tomatoes_limited_available,
    items_available_marketplace_tomatoes == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  toothbrush_adult_availability_score = case_when(
    items_available_marketplace_toothbrush_adult == "Widely_available" ~ AVALABILITY.CONFIG$toothbrush_adult_widely_available,
    items_available_marketplace_toothbrush_adult == "Limited_availability" ~ AVALABILITY.CONFIG$toothbrush_adult_limited_available,
    items_available_marketplace_toothbrush_adult == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  toothpaste_availability_score = case_when(
    items_available_marketplace_toothpaste == "Widely_available" ~ AVALABILITY.CONFIG$toothpaste_widely_available,
    items_available_marketplace_toothpaste == "Limited_availability" ~ AVALABILITY.CONFIG$toothpaste_limited_available,
    items_available_marketplace_toothpaste == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  sanitary_pad_availability_score = case_when(
    items_available_marketplace_sanitary_pad == "Widely_available" ~ AVALABILITY.CONFIG$sanitary_pad_widely_available,
    items_available_marketplace_sanitary_pad == "Limited_availability" ~ AVALABILITY.CONFIG$sanitary_pad_limited_available,
    items_available_marketplace_sanitary_pad == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  soap_availability_score = case_when(
    items_available_marketplace_soap == "Widely_available" ~ AVALABILITY.CONFIG$soap_widely_available,
    items_available_marketplace_soap == "Limited_availability" ~ AVALABILITY.CONFIG$soap_limited_available,
    items_available_marketplace_soap == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_ 
  ),
  
  pen_availability_score = case_when(
    items_available_marketplace_pen == "Widely_available" ~ AVALABILITY.CONFIG$pen_widely_available,
    items_available_marketplace_pen == "Limited_availability" ~ AVALABILITY.CONFIG$pen_limited_available,
    items_available_marketplace_pen == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_ 
  ),
  
  notebook_availability_score = case_when(
    items_available_marketplace_notebook == "Widely_available" ~ AVALABILITY.CONFIG$notebook_widely_available,
    items_available_marketplace_notebook == "Limited_availability" ~ AVALABILITY.CONFIG$notebook_limited_available,
    items_available_marketplace_notebook == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_ 
  ),
  
  rubber_availability_score = case_when(
    items_available_marketplace_rubber == "Widely_available" ~ AVALABILITY.CONFIG$rubber_widely_available,
    items_available_marketplace_rubber == "Limited_availability" ~ AVALABILITY.CONFIG$rubber_limited_available,
    items_available_marketplace_rubber == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_ 
  ),
  
  firewood_availability_score = case_when(
    items_available_marketplace_firewood == "Widely_available" ~ AVALABILITY.CONFIG$firewood_widely_available,
    items_available_marketplace_firewood == "Limited_availability" ~ AVALABILITY.CONFIG$firewood_limited_available,
    items_available_marketplace_firewood == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  lpg_availability_score = case_when(
    items_available_marketplace_lpg == "Widely_available" ~ AVALABILITY.CONFIG$lpg_widely_available,
    items_available_marketplace_lpg == "Limited_availability" ~ AVALABILITY.CONFIG$lpg_limited_available,
    items_available_marketplace_lpg == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  diesel_availability_score = case_when(
    items_available_marketplace_diesel == "Widely_available" ~ AVALABILITY.CONFIG$diesel_widely_available,
    items_available_marketplace_diesel == "Limited_availability" ~ AVALABILITY.CONFIG$diesel_limited_available,
    items_available_marketplace_diesel == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  petrol_availability_score = case_when(
    items_available_marketplace_petrol == "Widely_available" ~ AVALABILITY.CONFIG$petrol_widely_available,
    items_available_marketplace_petrol == "Limited_availability" ~ AVALABILITY.CONFIG$petrol_limited_available,
    items_available_marketplace_petrol == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  blanket_availability_score = case_when(
    items_available_marketplace_blanket == "Widely_available" ~ AVALABILITY.CONFIG$blanket_widely_available,
    items_available_marketplace_blanket == "Limited_availability" ~ AVALABILITY.CONFIG$blanket_limited_available,
    items_available_marketplace_blanket == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  ),
  
  winter_jacket_availability_score = case_when(
    items_available_marketplace_winter_jacket == "Widely_available" ~ AVALABILITY.CONFIG$winter_jacket_widely_available,
    items_available_marketplace_winter_jacket == "Limited_availability" ~ AVALABILITY.CONFIG$winter_jacket_limited_available,
    items_available_marketplace_winter_jacket == "Completely_unavailable" ~ 0,
    TRUE ~ NA_real_
  )
)

# mode function for the calculation of availability indicator.
# we are applying an optimistic approach for the availability indicator in case of having multiple mode values)
### Comment from PV: I would explain what the optimistic approach is in clear language for assessment team to understand
getmode <- function(score) {
  # filter the desired responses by removing all NA scores:
  score <- score[!is.na(score)]
  
  # sorting the response in desecrating order (this is essential to take the highest weight,
  score <- sort(score, decreasing = TRUE)
  unique_score <- unique(score)
  unique_score[which.max(tabulate(match(score, unique_score)))]
}

# dissagreagation of clean data by district:
district_items_availability_df <- items_availability_df %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count= n(),
    # mode of food items:
    wheat_local_availability = getmode(wheat_local_availability_score),
    wheat_imported_availability = getmode(wheat_imported_availability_score),
    rice_availability = getmode(rice_availability_score),
    pulses_lentils_availability = getmode(pulses_lentils_availability_score),
    pulses_beans_availability = getmode(pulses_beans_availability_score),
    pulses_split_peas_availability = getmode(pulses_split_peas_availability_score),
    tomatoes_availability = getmode(tomatoes_availability_score),
    vegetable_oil_availability = getmode(veg_oil_availability_score),
    sugar_availability = getmode(sugar_availability_score),
    salt_availability = getmode(salt_availability_score),
    
    # mode of NFIs:
    soap_availability = getmode(soap_availability_score),
    # safe_water_availability = getmode(safe_water_availability_score),
    toothbrush_adult_availability = getmode(toothbrush_adult_availability_score),
    toothpaste_availability = getmode(toothpaste_availability_score),
    sanitary_pad_availability = getmode(sanitary_pad_availability_score),
    pen_availability = getmode(pen_availability_score),
    rubber_availability = getmode(rubber_availability_score),
    notebook_availability = getmode(notebook_availability_score), 
    diesel_availability = getmode(diesel_availability_score),
    petrol_availability = getmode(petrol_availability_score),
    lpg_availability = getmode(lpg_availability_score), 
    
    # mode of winterization items:
    firewood_availability = getmode(firewood_availability_score),
    # coal_availability = getmode(coal_availability_score), 
    blanket_availability = getmode(blanket_availability_score),
    winter_jacket_availability = getmode(winter_jacket_availability_score)
  )

# getting the highest mode of local and imported wheat
district_items_availability_df <- district_items_availability_df %>% rowwise() %>%
  mutate(wheat_availibility = max(wheat_local_availability, wheat_imported_availability)) %>% 
  ungroup() %>% 
  select(afg_dist_code, survey_count, wheat_availibility, everything(), - c(wheat_local_availability, wheat_imported_availability))

# final calculation of availability index:
availability_info_full <- district_items_availability_df %>% 
  mutate(
    availability_score = case_when(
      include_winterization == TRUE ~ rowSums(.[3:24], na.rm=TRUE),
      TRUE ~ rowSums(.[3:21], na.rm=TRUE)
    ),
    availability_index = availability_score / ifelse(include_winterization, 1.633, 1.367)  # mapping score to maximum 30 (convert scores to index)
  )

availability_info <- availability_info_full %>%select(afg_dist_code, availability_index)

# round all numeric values:
availability_info_full <- availability_info_full %>% mutate_if(~is.numeric(.), ~round(.,2))
availability_info <- availability_info %>% mutate_if(~is.numeric(.), ~round(.,2))

# 2 Affordability index ***********************************

# 2.1 Price point modifier [Food and NFIs]:
# after processing the clean data the normalized price per unit will be the recoded data, 
food_nfi_item_prices <- c("wheat_local_price",
                          "wheat_imported_price",
                          "local_rice_price",
                          "veg_oil_price",
                          "pulses_lentils_price",
                          "pulses_beans_price",
                          "pulses_split_peas_price",
                          "salt_price",
                          "sugar_price",
                          "tomatoes_price",
                          "sanitary_pad_price",
                          "cotton_cloth_price",
                          "toothbrush_adult_price",
                          "toothpaste_price",
                          "soap_price", 
                          "pen_price",
                          "notebook_price",
                          "rubber_price",
                          "safe_water_price",
                          "lpg_price",
                          "diesel_price",
                          "petrol_price",
                          "cooking_pot_price",
                          "water_container_price")

winterization_item_prices <- c("firewood_price",
                               "coal_price",
                               "blanket_price",
                               "winter_jacket_price")

# median prices in districts:
district_food_nfi_prices_df <- df %>% select(afg_dist_code, all_of(food_nfi_item_prices)) %>% 
  group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    wheat_local_median = median(as.numeric(wheat_local_price), na.rm = TRUE),
    wheat_imported_median = median(as.numeric(wheat_imported_price), na.rm = TRUE),
    local_rice_median = median(as.numeric(local_rice_price), na.rm = TRUE),
    veg_oil_median = median(as.numeric(veg_oil_price), na.rm = TRUE),
    pulses_lentils_median = median(as.numeric(pulses_lentils_price), na.rm = TRUE),
    pulses_beans_median = median(as.numeric(pulses_beans_price), na.rm = TRUE),
    pulses_split_peas_median = median(as.numeric(pulses_split_peas_price), na.rm = TRUE),
    salt_median = median(as.numeric(salt_price), na.rm = TRUE),
    sugar_median = median(as.numeric(sugar_price), na.rm = TRUE),
    tomatoes_median = median(as.numeric(tomatoes_price), na.rm = TRUE),
    sanitary_pad_median = median(as.numeric(sanitary_pad_price), na.rm = TRUE),
    cotton_cloth_median = median(as.numeric(cotton_cloth_price), na.rm = TRUE),
    toothbrush_adult_median = median(as.numeric(toothbrush_adult_price), na.rm = TRUE),
    toothpaste_median = median(as.numeric(toothpaste_price), na.rm = TRUE),
    soap_median = median(as.numeric(soap_price), na.rm = TRUE),
    pen_median = median(as.numeric(pen_price), na.rm = TRUE),
    notebook_median = median(as.numeric(notebook_price), na.rm = TRUE),
    rubber_median = median(as.numeric(rubber_price), na.rm = TRUE),
    safe_water_median = median(as.numeric(safe_water_price), na.rm = TRUE),
    lpg_median = median(as.numeric(lpg_price), na.rm = TRUE),
    diesel_median = median(as.numeric(diesel_price), na.rm = TRUE),
    petrol_median = median(as.numeric(petrol_price), na.rm = TRUE),
    cooking_pot_median = median(as.numeric(cooking_pot_price), na.rm = TRUE),
    water_container_median = median(as.numeric(water_container_price), na.rm = TRUE)
  )

# median of prices in national level:
national_prices_df <- df %>% select(all_of(food_nfi_item_prices), all_of(winterization_item_prices)) %>% 
  summarise(
    survey_count = n(),
    wheat_local_median = median(as.numeric(wheat_local_price), na.rm = TRUE),
    wheat_imported_median = median(as.numeric(wheat_imported_price), na.rm = TRUE),
    local_rice_median = median(as.numeric(local_rice_price), na.rm = TRUE),
    veg_oil_median = median(as.numeric(veg_oil_price), na.rm = TRUE),
    pulses_lentils_median = median(as.numeric(pulses_lentils_price), na.rm = TRUE),
    pulses_beans_median = median(as.numeric(pulses_beans_price), na.rm = TRUE),
    pulses_split_peas_median = median(as.numeric(pulses_split_peas_price), na.rm = TRUE),
    salt_median = median(as.numeric(salt_price), na.rm = TRUE),
    sugar_median = median(as.numeric(sugar_price), na.rm = TRUE),
    tomatoes_median = median(as.numeric(tomatoes_price), na.rm = TRUE),
    sanitary_pad_median = median(as.numeric(sanitary_pad_price), na.rm = TRUE),
    cotton_cloth_median = median(as.numeric(cotton_cloth_price), na.rm = TRUE),
    toothbrush_adult_median = median(as.numeric(toothbrush_adult_price), na.rm = TRUE),
    toothpaste_median = median(as.numeric(toothpaste_price), na.rm = TRUE),
    soap_median = median(as.numeric(soap_price), na.rm = TRUE),  # was missed in the draft version and has been added via HQ comment
    pen_median = median(as.numeric(pen_price), na.rm = TRUE),
    notebook_median = median(as.numeric(notebook_price), na.rm = TRUE),
    rubber_median = median(as.numeric(rubber_price), na.rm = TRUE),
    safe_water_median = median(as.numeric(safe_water_price), na.rm = TRUE),
    lpg_median = median(as.numeric(lpg_price), na.rm = TRUE),
    diesel_median = median(as.numeric(diesel_price), na.rm = TRUE),
    petrol_median = median(as.numeric(petrol_price), na.rm = TRUE),
    cooking_pot_median = median(as.numeric(cooking_pot_price), na.rm = TRUE),
    water_container_median = median(as.numeric(water_container_price), na.rm = TRUE),
    firewood_median = median(as.numeric(firewood_price), na.rm = TRUE),
    coal_median = median(as.numeric(coal_price), na.rm = TRUE),
    blanket_median = median(as.numeric(blanket_price), na.rm = TRUE),
    winter_jacket_median = median(as.numeric(winter_jacket_price), na.rm = TRUE)
  )

# price point modifier function:
# This function compare national level prices with district level prices and assign score based on the price comparison.
price_point_modifier <- function(item_median){
  item <- deparse(substitute(item_median))
  result = case_when(
    {{item_median}} <= national_prices_df[[item]] * .5 ~ 2,
    {{item_median}} > national_prices_df[[item]] * .5 & {{item_median}} <= national_prices_df[[item]] * .75 ~ 1.5,
    {{item_median}} > national_prices_df[[item]] * .75 & {{item_median}} <= national_prices_df[[item]] * .90 ~ 1,
    {{item_median}} > national_prices_df[[item]] * .90 & {{item_median}} <= national_prices_df[[item]] * 1.1 ~ 0,
    {{item_median}} > national_prices_df[[item]] * 1.1 & {{item_median}} <= national_prices_df[[item]] * 1.25 ~ -1,
    {{item_median}} > national_prices_df[[item]] * 1.25 & {{item_median}} <= national_prices_df[[item]] * 1.5 ~ -1.5,
    {{item_median}} > national_prices_df[[item]] * 1.5 ~ -2, # corrected based on HQ comment
    TRUE ~ NA_real_
  )
  return (result)
}

# adding price points for all districts:
district_food_nfi_prices_df <- district_food_nfi_prices_df %>% 
  mutate(
    wheat_local_point = price_point_modifier(wheat_local_median),
    wheat_imported_point = price_point_modifier(wheat_imported_median ),
    local_rice_point = price_point_modifier(local_rice_median),
    veg_oil_point = price_point_modifier(veg_oil_median),
    pulses_lentils_point = price_point_modifier(pulses_lentils_median),
    pulses_beans_point = price_point_modifier(pulses_beans_median),
    pulses_split_peas_point = price_point_modifier(pulses_split_peas_median),
    salt_point = price_point_modifier(salt_median),
    sugar_point = price_point_modifier(sugar_median),
    tomatoes_point = price_point_modifier(tomatoes_median),
    sanitary_pad_point = price_point_modifier(sanitary_pad_median),
    cotton_cloth_point = price_point_modifier(cotton_cloth_median),
    toothbrush_adult_point = price_point_modifier(toothbrush_adult_median),
    toothpaste_point = price_point_modifier(toothpaste_median),
    soap_point = price_point_modifier(soap_median),
    pen_point = price_point_modifier(pen_median),
    notebook_point = price_point_modifier(notebook_median),
    rubber_point = price_point_modifier(rubber_median),
    safe_water_point = price_point_modifier(safe_water_median),
    lpg_point = price_point_modifier(lpg_median),
    diesel_point = price_point_modifier(diesel_median),
    petrol_point = price_point_modifier(petrol_median),
    cooking_pot_point = price_point_modifier(cooking_pot_median),
    water_container_point = price_point_modifier(water_container_median)
  )

# count of item prices in each district:
district_food_nfi_prices_df$price_count <- rowSums(!is.na(district_food_nfi_prices_df[, 3:26]))

# sum of all the points in every districts:
food_nfi_points <- district_food_nfi_prices_df %>%
  mutate(
    # below case_when has been added to set the maximum point
    food_nfi_point = case_when(
      rowSums(select(., ends_with("point")) , na.rm = TRUE) <= 40 ~ rowSums(select(., ends_with("point")) , na.rm = TRUE),
      TRUE ~ 40
    )
  ) %>% 
  select(afg_dist_code, food_nfi_point)

# 2.2 Price point modifier [Winterization]:
# median of prices in districts:
district_winterization_prices_df <- df %>% select(afg_dist_code, all_of(winterization_item_prices)) %>% 
  group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    firewood_median = median(as.numeric(firewood_price), na.rm = TRUE),
    coal_median = median(as.numeric(coal_price), na.rm = TRUE),
    blanket_median = median(as.numeric(blanket_price), na.rm = TRUE),
    winter_jacket_median = median(as.numeric(winter_jacket_price), na.rm = TRUE)
  )
    
# adding price points for all districts:
district_winterization_prices_df <- district_winterization_prices_df %>% 
  mutate(
    firewood_point = price_point_modifier(firewood_median),
    coal_point = price_point_modifier(coal_median),
    blanket_point = price_point_modifier(blanket_median),
    winter_jacket_point = price_point_modifier(winter_jacket_median),
    
  )

# count of item prices in each district:
district_winterization_prices_df$price_count <- rowSums(!is.na(district_winterization_prices_df[, 3:6]))

# sum of all the points in every districts:
winterization_points <- district_winterization_prices_df %>%
  mutate(
    winterization_point = rowSums(select(., ends_with("point")) , na.rm = TRUE)
  ) %>% 
  select(afg_dist_code, winterization_point)

# 2.3 Financial Access:
financial_df <- df %>% select(afg_dist_code, 
                                  financial_access.can_not_afford,
                                  financial_access.cannot_pay,
                                  financial_access.expensive_transportation,
                                  financial_access.expensive_fuel,
                                  financial_access.other)

# creating new column and populate that with 1 if there is at least one financial access problem: 
financial_df <- financial_df %>% mutate(
  financial_access_problem = case_when(
    financial_access.can_not_afford == 1 | financial_access.cannot_pay == 1 | financial_access.expensive_transportation == 1 | 
    financial_access.expensive_fuel == 1 | financial_access.other == 1 ~ 1,
    TRUE ~ 0
  )
)

# disaggregation the financial_df based on district and calculate financial_access_score for each districts:
financial_info <- financial_df %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    financial_access_problem_count = sum(financial_access_problem),
    financial_access_problem_percentage = financial_access_problem_count / survey_count * 100,
    financial_access_score = case_when(
      financial_access_problem_percentage <= 10 ~ 9,
      financial_access_problem_percentage > 10 & financial_access_problem_percentage <= 25 ~ 6,
      financial_access_problem_percentage > 25 & financial_access_problem_percentage <= 50 ~ 3,
      financial_access_problem_percentage > 50 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% select(afg_dist_code, financial_access_score)

# 2.4 Indebtedness change:
indebtedness_df <- df %>% select(afg_dist_code, purchase_credit)

indebtedness_info <- indebtedness_df %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    indebtedness_problem = sum(purchase_credit=="increase"),
    indebtedness_problem_percentage = indebtedness_problem / survey_count * 100,
    indebtedness_problem_score = case_when(
      indebtedness_problem_percentage <= 10 ~ 5,
      indebtedness_problem_percentage > 10 & indebtedness_problem_percentage <= 20 ~ 2,
      indebtedness_problem_percentage > 20 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% select(afg_dist_code, indebtedness_problem_score)

# 2.5 Food basket stability:
# required data for calculation of the food basket:
food_basket_df <- df %>% select(afg_dist_code, wheat_imported_calculation, rice_price_calculation,
                                veg_oil_calculation, pulses_lentils_calculation,
                                pulses_beans_calculation, pulses_split_peas_calculation,
                                salt_calculation)

# getting the minimum price from all three pulses:
food_basket_df <- food_basket_df %>%
  rowwise() %>% 
  # median parameters has been set to a single vector. updated on the 24the June 2023
  mutate(pulses_min_calculation = median(c(pulses_lentils_calculation, pulses_beans_calculation, pulses_split_peas_calculation), na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(everything(), - c(pulses_lentils_calculation, pulses_beans_calculation, pulses_split_peas_calculation))

# replace inf values in the pulses_min_calculation to NA (edited on 1th May 2023)
food_basket_df[sapply(food_basket_df, is.infinite)] <- NA

# disaggregated the data by districts:
food_basket_district_df <- food_basket_df %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    wheat_imported_price_median = median(as.numeric(wheat_imported_calculation), na.rm = T),
    local_rice_price_median = median(as.numeric(rice_price_calculation), na.rm = T),
    veg_oil_price_median = median(as.numeric(veg_oil_calculation), na.rm = T),
    pulses_min_price_median = median(as.numeric(pulses_min_calculation), na.rm = T),
    salt_price_median = median(as.numeric(salt_calculation), na.rm = T),
    
    # food basket calculation:
    food_basket = (wheat_imported_price_median * 89) +
      (local_rice_price_median * 21) +
      (veg_oil_price_median * 7) +
      (pulses_min_price_median * 9) +
      (salt_price_median * 1)
  ) %>% 
  select(afg_dist_code, survey_count, food_basket)

# merging current food basket with previous one:
food_basket_stability_df <- food_basket_district_df %>% 
  left_join(previous_food_basket, by= c("afg_dist_code"="afg_district"))

# comparing current food basket with previous one:
food_basket_stability_df <- food_basket_stability_df %>% mutate(
  food_basket_stability_score = case_when(
    # edited based on HQ comment:
    abs(((food_basket - previous_food_basket)/previous_food_basket * 100)) <= 5 ~ 5,
    abs(((food_basket - previous_food_basket)/previous_food_basket * 100)) > 5 &
      abs(((food_basket - previous_food_basket)/previous_food_basket * 100)) <= 10 ~ 3,
    abs(((food_basket - previous_food_basket)/previous_food_basket * 100)) > 10 ~ 0,
    TRUE ~ NA_real_
  )
)

# final affordability index:
affordability_info <- financial_info %>% 
  inner_join(indebtedness_info, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(food_basket_stability_df, by= c("afg_dist_code"="afg_dist_code")) %>%
  inner_join(food_nfi_points, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(winterization_points, by= c("afg_dist_code"="afg_dist_code")) %>% mutate(
    
    # updated on 4th May 2023:
    max_score = case_when(
      include_winterization == TRUE & !is.na(food_basket_stability_score) ~ 27,
      include_winterization == TRUE & is.na(food_basket_stability_score) ~ 22,
      include_winterization == FALSE & !is.na(food_basket_stability_score) ~ 19,
      TRUE ~ 14
    ),
    
    affordability_score = case_when(
      !is.na(food_basket_stability_score) ~ financial_access_score + indebtedness_problem_score + 
        food_basket_stability_score + food_nfi_point + winterization_point,
      TRUE ~ financial_access_score + indebtedness_problem_score + food_nfi_point + winterization_point,
    ),
    affordability_final_score = case_when(
      affordability_score >= 0 & affordability_score <= max_score ~ affordability_score, 
      affordability_score < 0 ~ 0, # affordability_score can be a negative value, so in such cases the final score will be converted to zero.
      affordability_score > max_score ~ max_score
    ),
    
    # map final score to 15
    affordability_index = case_when(
      max_score == 14 ~ affordability_final_score * 1.071428571,
      max_score == 19 ~ affordability_final_score * 0.789473684,
      max_score == 22 ~ affordability_final_score * 0.681818182,
      max_score == 27 ~ affordability_final_score * 0.555555556,
    )
  )

# round all numeric values:
affordability_info <- affordability_info %>% mutate_if(~is.numeric(.), ~round(.,2))
  
# 3 Resilience index ***********************************
# 3.1 Supply chain disruption

# list of all records that only have supply chain issue:
supply_chain_disruption_df <- df %>%
  select(difficulty_supply_items_reasons, "X_uuid") %>% 
  filter(!is.na(difficulty_supply_items_reasons)) %>% 
  filter(difficulty_supply_items_reasons != "prefer_not_to_answer")
supply_chain_disruption_df$disruption <- 1

# list of all records if there is any supply chain issue, the disruption column will be 1 otherwise 0.
all_supply_chain_df <- df %>% select(afg_dist_code, "X_uuid") %>% 
  left_join(supply_chain_disruption_df, by=c("X_uuid"="X_uuid")) %>% 
  select(afg_dist_code, disruption)

# supply chain score for all districts:
district_supply_chain_df <- all_supply_chain_df %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    disruption = sum(disruption, na.rm = TRUE),
    disruption_percentage = disruption / survey_count * 100
  ) %>% mutate(
    disruption_score = case_when(
      disruption_percentage <= 5 ~ 12,
      disruption_percentage > 5 & disruption_percentage <= 10 ~ 9,
      disruption_percentage > 10 & disruption_percentage <= 25 ~ 6,
      disruption_percentage > 25 & disruption_percentage <= 50 ~ 3,
      disruption_percentage > 50 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% select(afg_dist_code, disruption_score)
  
# 3.2 Supplier Diversity for Food 
district_one_food_supplier_df <- df %>% select(afg_dist_code, food_supplier_count) %>% 
group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    one_food_supplier = sum(food_supplier_count[food_supplier_count == 1]),
    one_food_supplier_percentage = one_food_supplier / survey_count * 100
  ) %>% mutate(
    one_food_supplier_score = case_when(
      one_food_supplier_percentage <= 20 ~ 3,
      one_food_supplier_percentage > 20 & one_food_supplier_percentage <= 30 ~ 2,
      one_food_supplier_percentage > 30 & one_food_supplier_percentage <= 40 ~ 1,
      one_food_supplier_percentage > 40 ~ 0,
      TRUE ~ 0 # assign 0 for no data 
    )
  ) %>% 
  select(afg_dist_code, one_food_supplier_score)

# 3.3 Supplier Diversity for NFI 
district_one_nfi_supplier_df <- df %>% select(afg_dist_code, nfi_supplier_count) %>% 
  group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    one_nfi_supplier = sum(nfi_supplier_count[nfi_supplier_count == 1]),
    one_nfi_supplier_percentage = one_nfi_supplier / survey_count * 100
  ) %>% mutate(
    one_nfi_supplier_score = case_when(
      one_nfi_supplier_percentage <= 20 ~ 3,
      one_nfi_supplier_percentage > 20 & one_nfi_supplier_percentage <= 30 ~ 2,
      one_nfi_supplier_percentage > 30 & one_nfi_supplier_percentage <= 40 ~ 1,
      one_nfi_supplier_percentage > 40 ~ 0,
      TRUE ~ 0 # assign 0 for no data 
    )
  ) %>% 
  select(afg_dist_code, one_nfi_supplier_score)

# 3.4 Restocking [Food and NFIs]
restock_df <- df %>% select(afg_dist_code,
                            diff_stock_restock_wheat_local,
                            diff_stock_restock_wheat_imported,
                            diff_stock_restock_local_rice,
                            diff_stock_restock_veg_oil,
                            diff_stock_restock_pulses_beans,
                            diff_stock_restock_tomatoes,
                            diff_stock_restock_cotton_cloth,
                            diff_stock_restock_soap,
                            diff_stock_restock_pen,
                            diff_stock_restock_safe_water,
                            diff_stock_restock_lpg,
                            diff_stock_restock_diesel,
                            diff_stock_restock_petrol,
                            diff_stock_restock_cooking_pot,
                            diff_stock_restock_water_container
                          )

district_restock_df <- restock_df %>% group_by(afg_dist_code) %>% 
  # corrected on 2th may 2023:
  summarise(
    restock_wheat_local_median = median(as.numeric( diff_stock_restock_wheat_local), na.rm = T),
    restock_wheat_imported_median = median(as.numeric( diff_stock_restock_wheat_imported), na.rm = T),
    restock_local_rice_median = median(as.numeric( diff_stock_restock_local_rice), na.rm = T),
    restock_veg_oil_median = median(as.numeric( diff_stock_restock_veg_oil), na.rm = T),
    restock_pulses_beans_median = median(as.numeric( diff_stock_restock_pulses_beans), na.rm = T),
    restock_tomatoes_median = median(as.numeric( diff_stock_restock_tomatoes), na.rm = T),
    restock_cotton_cloth_median = median(as.numeric( diff_stock_restock_cotton_cloth), na.rm = T),
    restock_soap_median = median(as.numeric( diff_stock_restock_soap), na.rm = T),
    restock_pen_median = median(as.numeric( diff_stock_restock_pen), na.rm = T),
    restock_safe_water_median = median(as.numeric( diff_stock_restock_safe_water), na.rm = T),
    restock_lpg_median = median(as.numeric( diff_stock_restock_lpg), na.rm = T),
    restock_diesel_median = median(as.numeric( diff_stock_restock_diesel), na.rm = T),
    restock_petrol_median = median(as.numeric( diff_stock_restock_petrol), na.rm = T),
    restock_cooking_pot_median = median(as.numeric( diff_stock_restock_cooking_pot), na.rm = T),
    restock_water_container_median = median(as.numeric( diff_stock_restock_water_container), na.rm = T)
  ) 

# restock score function:
restock_score <- function(restock_median){
  result = case_when(
    {{restock_median}} >= 3 ~ 1.5,
    {{restock_median}} > 0 & {{restock_median}} < 3 ~ 1,
    {{restock_median}} == 0 ~ .5,
    {{restock_median}} < 0 ~ 0,
    TRUE ~ NA_real_
  )
  return (result)
}

# assigning scores based on the restock values for all items:
district_restock_df <- district_restock_df %>% mutate(
  restock_wheat_local_score = restock_score(restock_wheat_local_median),
  restock_wheat_imported_score = restock_score(restock_wheat_imported_median),
  restock_local_rice_score = restock_score(restock_local_rice_median),
  restock_veg_oil_score = restock_score(restock_veg_oil_median),
  restock_pulses_beans_score = restock_score(restock_pulses_beans_median),
  restock_tomatoes_score = restock_score(restock_tomatoes_median),
  restock_cotton_cloth_score = restock_score(restock_cotton_cloth_median),
  restock_soap_score = restock_score(restock_soap_median),
  restock_pen_score = restock_score(restock_pen_median),
  restock_safe_water_score = restock_score(restock_safe_water_median),
  restock_lpg_score = restock_score(restock_lpg_median),
  restock_diesel_score = restock_score(restock_diesel_median),
  restock_petrol_score = restock_score(restock_petrol_median),
  restock_cooking_pot_score = restock_score(restock_cooking_pot_median),
  restock_water_container_score = restock_score(restock_water_container_median)
)

# assigning sum up scores for each district: 
district_restock_df2 <- district_restock_df %>% mutate(
  restock_score = rowSums(select(., ends_with("score")) , na.rm = TRUE)
) %>% select(afg_dist_code, restock_score)

# 3.5 Restocking [Winterization]
winterization_restock_df <- df %>% select(afg_dist_code, 
                                          diff_stock_restock_firewood,
                                          diff_stock_restock_coal,
                                          diff_stock_restock_blanket,
                                          diff_stock_restock_winter_jacket
                                        )
district_winterization_restock_df <- winterization_restock_df %>% group_by(afg_dist_code) %>% 
  summarise(
    restock_firewood_median = median(as.numeric( diff_stock_restock_firewood), na.rm = T),
    restock_coal_median = median(as.numeric( diff_stock_restock_coal), na.rm = T),
    restock_blanket_median = median(as.numeric( diff_stock_restock_blanket), na.rm = T),
    restock_jacket_median = median(as.numeric( diff_stock_restock_winter_jacket), na.rm = T)
  )

# assigning scores based on the restock values for all winterization items:
district_winterization_restock_df <- district_winterization_restock_df %>% mutate(
  restock_firewood_score = restock_score(restock_firewood_median),
  restock_coal_score = restock_score(restock_coal_median),
  restock_blanket_score = restock_score(restock_blanket_median),
  restock_jacket_score = restock_score(restock_jacket_median)
) 

# assigning sum up scores for each district: 
district_winterization_restock_df2 <- district_winterization_restock_df %>% mutate(
  winterization_restock_score = rowSums(select(., ends_with("score")) , na.rm = TRUE)
) %>% select(afg_dist_code, winterization_restock_score)

# final Resilience index:
resilience_info <- district_supply_chain_df %>% 
  inner_join(district_one_food_supplier_df, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(district_one_nfi_supplier_df, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(district_restock_df2, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(district_winterization_restock_df2, by= c("afg_dist_code"="afg_dist_code"))

resilience_info <- resilience_info %>% mutate(
  resilience_score = case_when(
    include_winterization == TRUE ~ disruption_score + one_food_supplier_score + one_nfi_supplier_score + 
      restock_score + winterization_restock_score,
    TRUE ~ disruption_score + one_food_supplier_score + one_nfi_supplier_score + 
      restock_score
  ), 
  # map resilience score to 20
  resilience_index = case_when(
    include_winterization == TRUE ~ resilience_score * 0.430107527,
      TRUE ~ resilience_score * 0.49382716
  )
)  

# round all numeric values:
resilience_info <- resilience_info %>% mutate_if(~is.numeric(.), ~round(.,2))

# 4 Infrastructure index ***********************************
# 4.1 Adequacy of and damage to facilities
damaged_facilities_df <- df %>% select(afg_dist_code, physical_access.hazardous_building)
district_damaged_facilities_df <- damaged_facilities_df %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    damaged_facilities = sum(physical_access.hazardous_building),
    damaged_facilities_percentage = damaged_facilities / survey_count * 100,
  ) %>% mutate(
    damaged_facilities_score = case_when(
      damaged_facilities_percentage <= 10 ~ 5,
      damaged_facilities_percentage > 10 & damaged_facilities_percentage <= 20 ~ 3,
      damaged_facilities_percentage > 20 ~ 0,
      TRUE ~ 0 
    )
  ) %>%  
  select(afg_dist_code, damaged_facilities_score)

# 4.2 Storage
storage_df <- df %>% select(afg_dist_code, storage)
storage_df <- storage_df %>% mutate(
  no_storage = if_else(storage == "yes_my_own" | storage == "yes_elsewhere", 0, 1) 
)

district_storage_df <- storage_df %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(), 
    no_storage_sum = sum(no_storage),
    no_storage_percentage = no_storage_sum / survey_count * 100,
  ) %>% mutate(
    no_storage_score = case_when(
      no_storage_percentage <= 10 ~ 5,
      no_storage_percentage > 10 & no_storage_percentage <= 20 ~ 3,
      no_storage_percentage > 20 ~ 0,
      TRUE ~ 0 
    )
  ) %>%  
  select(afg_dist_code, no_storage_score)

# 4.3 Financial Services
financial_services_df <- df %>% select(afg_dist_code,
                                       financial_services.banks,
                                       financial_services.mobile_money_agents,
                                       financial_services.formal_transfer_services,
                                       financial_services.hawala,
                                       financial_services.microfinance_institutions,
                                       financial_services.village_banks,
                                       financial_services.credit_unions,
                                       financial_services.local_businesses,
                                       financial_services.members_community,
                                       financial_services.other)


district_financial_services_df <- financial_services_df %>% group_by(afg_dist_code) %>% 
  summarise(
    n(),
    banks = if_else(sum(financial_services.banks)>=1, 1,0),
    mobile_money_agents = if_else(sum(financial_services.mobile_money_agents)>=1, 1,0),
    formal_transfer_services = if_else(sum(financial_services.formal_transfer_services)>=1, 1,0),
    hawala = if_else(sum(financial_services.hawala)>=1, 1,0),
    microfinance_institutions = if_else(sum(financial_services.microfinance_institutions)>=1, 1,0),
    village_banks = if_else(sum(financial_services.village_banks)>=1, 1,0),
    credit_unions = if_else(sum(financial_services.credit_unions)>=1, 1,0),
    local_businesses = if_else(sum(financial_services.local_businesses)>=1, 1,0),
    members_community = if_else(sum(financial_services.members_community)>=1, 1,0),
    other = if_else(sum(financial_services.other)>=1, 1,0),
  ) %>% rowwise() %>% 
  mutate(
    fsp_types_count = sum(banks, mobile_money_agents, formal_transfer_services,
                          hawala, microfinance_institutions, village_banks, 
                          credit_unions, local_businesses, members_community,
                          other)
  )

district_financial_services_df2 <- district_financial_services_df %>% mutate(
  fsp_types_score = case_when(
    fsp_types_count > 3 ~ 5,
    fsp_types_count >= 1 & fsp_types_count <= 3 ~ 3,
    fsp_types_count == 0 ~ 0,
    TRUE ~ NA_real_
  )
) %>% select(afg_dist_code, fsp_types_score)

# 4.4 Payment Moralities
payment_df <- df %>% select(afg_dist_code, 
                            modality.mobile_money,
                            modality.credit,
                            modality.barter,
                            modality.voucher,
                            modality.other
                            ) %>% 
  rowwise() %>% 
  mutate(
    modality = sum(modality.mobile_money, modality.credit,
                   modality.barter, modality.voucher, modality.other),
    modality_ok = if_else(modality>=1, 1,0) # this field will be 1 if the KI says we have multiple payment moralities along with cash types modalities.
  ) %>% 
  ungroup()

district_payment_df <- payment_df %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    modality_sum = sum(modality_ok[modality_ok >= 1]),
    modality_percetage = modality_sum / survey_count * 100,
    modality_score = case_when(
      modality_percetage >= 75 ~ 3, 
      modality_percetage >= 50 & modality_percetage < 75 ~ 2, 
      modality_percetage >= 25 & modality_percetage < 50 ~ 1, 
      modality_percetage < 25 ~ 0, 
      TRUE ~ NA_real_
    )
   )
 
# final infrastructure index:
infrastructure_info <- district_damaged_facilities_df %>% 
  inner_join(district_storage_df, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(district_financial_services_df2, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(district_payment_df, by= c("afg_dist_code"="afg_dist_code"))

# corrected infrastructure_score:
infrastructure_info$infrastructure_score <- infrastructure_info$damaged_facilities_score +
  infrastructure_info$no_storage_score + infrastructure_info$fsp_types_score + infrastructure_info$modality_score

# map infrastructure score to 10:
infrastructure_info$infrastructure_index <- infrastructure_info$infrastructure_score * .5555 

# round all numeric values:
infrastructure_info <- infrastructure_info %>% mutate_if(~is.numeric(.), ~round(.,2))

# 5 Accessibility index ***********************************
# 5.1 Physical access

physical_access_df <- df %>% select(afg_dist_code, 
                                     physical_access.hazards_roads, 
                                     physical_access.hazardous_building,
                                     physical_access.movement_restrictions,
                                     physical_access.ongoing_fighting,
                                     physical_access.inadequate_facilities,
                                     physical_access.limited_transportation,
                                     physical_access.vendors_difficult,
                                     physical_access.operates_limited_times,
                                     physical_access.customers_not_safe,
                                     physical_access.distance_market,
                                     physical_access.other
                                    ) %>% mutate(
                                      physical_access_issue = case_when(
                                        (physical_access.hazardous_building == 1 |
                                        physical_access.movement_restrictions == 1 |
                                        physical_access.ongoing_fighting == 1 |
                                        physical_access.inadequate_facilities == 1 |
                                        physical_access.limited_transportation == 1 |
                                        physical_access.vendors_difficult == 1 |
                                        physical_access.operates_limited_times == 1 |
                                        physical_access.customers_not_safe == 1 |
                                        physical_access.distance_market == 1 |
                                        physical_access.other == 1 ) & physical_access.hazards_roads == 0 ~ 1,
                                        TRUE ~ 0
                                      ) 
                                    ) %>% select(afg_dist_code, physical_access_issue)



district_physical_access_df <- physical_access_df %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    physical_access_issue = sum(physical_access_issue),
    physical_access_issue_percentage = physical_access_issue / survey_count * 100,
  ) %>% mutate(
    physical_access_issue_score = case_when(
      physical_access_issue_percentage <= 10 ~ 5,
      physical_access_issue_percentage > 10 & physical_access_issue_percentage <= 20 ~ 3,
      physical_access_issue_percentage > 20 ~ 0,
      TRUE ~ NA_real_
    )
  )

# 5.2 Adequacy of and damage to facilities
hazards_roads_df <- df %>% 
  select(afg_dist_code, physical_access.hazards_roads)

district_hazards_roads_df <- hazards_roads_df %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    hazards_roads = sum(physical_access.hazards_roads, na.rm = TRUE), # 16/4/2023: na.rm = TRUE has been added.
    hazards_roads_percentage = hazards_roads / survey_count * 100,
  ) %>% mutate(
    hazards_roads_score = case_when(
      hazards_roads_percentage <= 10 ~ 3,
      hazards_roads_percentage > 10 & hazards_roads_percentage <= 20 ~ 2,
      hazards_roads_percentage > 20 ~ 0,
      TRUE ~ 0
    )
  )

# 5.3 Safety and security
security_df <- df %>% 
  select(afg_dist_code, access_security)

district_security_df <- security_df %>% group_by(afg_dist_code) %>% 
  summarise(
    survey_count = n(),
    access_security = sum(access_security == "yes"),
    access_security_percentage = access_security / survey_count * 100,
  ) %>% mutate(
    access_security_score = case_when(
      access_security_percentage <= 10 ~ 6,
      access_security_percentage > 10 & access_security_percentage <= 20 ~ 3,
      access_security_percentage > 20 ~ 0,
      TRUE ~ NA_real_
    )
  )

# 5.4 Social access
social_df <- df %>% 
  select(afg_dist_code, 
         social_access.yes_ethnicity,
         social_access.yes_religion,
         social_access.yes
         ) %>% mutate(
           social_access_issue = if_else(social_access.yes_ethnicity == 1 | 
                                           social_access.yes_religion == 1 |
                                           social_access.yes == 1, 1, 0)
         )

district_social_df <- social_df %>% group_by(afg_dist_code) %>% 
  summarise(
    # survey_count = n(), # commented on 16/4/2023 
    social_access_issue = sum(social_access_issue),
    # social_access_issue_percentage = social_access_issue / survey_count * 100,
  ) %>% mutate(
    social_access_score = case_when(
      # 16/4/2023: the calculation has been reversed by Chris comment:
      social_access_issue == 0 ~ 3, 
      TRUE ~ 3
    )
  )

# 5.5 Women's access to the marketplace
women_access_df <- df %>% 
  select(afg_dist_code, women_access) %>% 
  mutate(
    women_access_score = case_when(
      women_access == "yes_unaccopmanied" ~ 4,
      women_access == "yes_accompanied" ~ 2,
      women_access == "no_not_at_all" ~ 0,
      TRUE ~ NA_real_
    )
  )

district_women_access_df <- women_access_df %>% group_by(afg_dist_code) %>% 
  summarise(
    # survey_count= n(), # commented on 16/4/2023 
    women_access_mode = getmode(women_access_score)
  )

# final accessibility info:
accessibility_info <- district_physical_access_df %>% 
  inner_join(district_hazards_roads_df, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(district_security_df, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(district_social_df, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(district_women_access_df, by= c("afg_dist_code"="afg_dist_code"))

# 16/4/2023, based on HQ comments, social_access_score will be 0 when the women access is 0 or 2  
accessibility_info$social_access_score2 <- if_else(accessibility_info$women_access_mode < 4 , 0, accessibility_info$social_access_score)
accessibility_info <- accessibility_info %>% relocate(social_access_score2, .after = social_access_score)

accessibility_info$accessibility_score <- accessibility_info$physical_access_issue_score + 
  accessibility_info$hazards_roads_score + 
  accessibility_info$access_security_score + accessibility_info$social_access_score2 + 
  accessibility_info$women_access_mode

# map score to 25 
accessibility_info$accessibility_index <- accessibility_info$accessibility_score * 1.19047619

# round all numeric values:
accessibility_info <- accessibility_info%>% mutate_if(~is.numeric(.), ~round(.,2))

# overall MFS: *******************************************************************
mfs <- district_physical_access_df %>% 
  inner_join(availability_info, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(accessibility_info, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(resilience_info, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(affordability_info, by= c("afg_dist_code"="afg_dist_code")) %>% 
  inner_join(infrastructure_info, by= c("afg_dist_code"="afg_dist_code")) %>%
  select(afg_dist_code, ends_with("index"))

mfs$MFS <- mfs$availability_index + mfs$affordability_index + mfs$resilience_index + 
  mfs$infrastructure_index + mfs$accessibility_index

# add all district labels including districts with less than four records: 
district_lables <- df_all %>% select(afg_dist_code, afg_dist) %>% distinct()
mfs <- district_lables %>% left_join(mfs, by= c("afg_dist_code"="afg_dist_code"))

# district classification
mfs <- mfs %>% mutate(
  low_accessibility = if_else(mfs$accessibility_index < 12.5, 1, 0),
  low_availability = if_else(mfs$availability_index < 15, 1, 0),
  low_affordability = if_else(mfs$affordability_index < 7.5, 1, 0),
  low_resilience = if_else(mfs$resilience_index < 10, 1, 0),
  low_infrastructure = if_else(mfs$infrastructure_index < 5, 1, 0),
  
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
  select(afg_dist_code, afg_dist, availability_index, accessibility_index, resilience_index,
         affordability_index, infrastructure_index, MFS, low_dimension, classification, sorted ) %>% 
  arrange(desc(MFS)) %>% 
  arrange(desc(sorted)) %>% 
  select(- sorted)

# round all numeric values:
mfs <- mfs %>% mutate_if(~is.numeric(.), ~round(.,2))

# MFS output as an MS Excel file
# arrange output sheets based on the weights of the dimensions:
# -----------------------
# dimension       weight
# -----------------------
# availability:     30
# accessibility:    25
# resilience:       20
# affordability:    15
# infrastructure:   10
# -----------------------

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
  fontSize = 9, halign = "center", valign = "center", wrapText = T,
  fgFill = "#bcbcbc", border = "TopBottomLeftRight", borderColour = "#000000"
)

addStyle(mfs_file, sheet = 1, header_style, rows = 1, cols = 1:10, gridExpand = TRUE)
setColWidths(mfs_file, sheet = 1, cols =1:10, widths = 20)

setColWidths(mfs_file, sheet = 2, cols = 1:ncol(availability_info_full), widths = "auto")
setColWidths(mfs_file, sheet = 3, cols = 1:ncol(accessibility_info), widths = "auto")
setColWidths(mfs_file, sheet = 4, cols = 1:ncol(resilience_info), widths = "auto")
setColWidths(mfs_file, sheet = 5, cols = 1:ncol(affordability_info), widths = "auto")
setColWidths(mfs_file, sheet = 6, cols = 1:ncol(infrastructure_info), widths = "auto")

writeData(mfs_file, sheet = "MFS", x = mfs)
writeData(mfs_file, sheet = "availability", x = availability_info_full)
writeData(mfs_file, sheet = "accessibility", x = accessibility_info )
writeData(mfs_file, sheet = "resilience", x = resilience_info)
writeData(mfs_file, sheet = "affordability", x = affordability_info)
writeData(mfs_file, sheet = "infrastructure", x = infrastructure_info)

saveWorkbook(mfs_file, paste0("./output/MFS_", JMMI_round, "_", format(Sys.time(), "%d_%b_%Y"),".xlsx"), overwrite = T)

