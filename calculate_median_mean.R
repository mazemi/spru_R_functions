library(readxl)
library(survey)
library(openxlsx)
library(tidyr)
library(dplyr)
library(tidyverse)

source("./R/functions/JMMI_admin_label.R")
`%notin%` <- Negate(`%in%`) 

usd_rate <- 86.85
session <-"JMMI_R35_MAY23"

# fixed price items - AFN
# updated on 17Th May 2023, base on the MEB revision on the April 2023
Healthcare_basket <- 49.39 * usd_rate
Shelter_basket <- 34 * usd_rate
Education_basket <- 4.86 * usd_rate
Fuel_Electricity_basket <- 18 * usd_rate
Communication_basket <- 1.94 * usd_rate
Transportation_basket <- 6.82 * usd_rate
Under_wear <- 0.57 * usd_rate

# Current Round Data
curr_data <- read_excel("input/data/clean/curr_data/JMMI_R35_clean.xlsx", na =  c("", "NA","N/A"))  %>% type.convert(as.is = T)

# Previous Round Data
prev_data <- read.csv("input/data/clean/prev_data/AFG2002_JMMI_R34_APR23_recoded.csv", na.strings = c("", "NA","N/A"), stringsAsFactors = F)

# reading kobo tool
kobotool.choices <- read_excel("input/questionnaire/JMMI_35_final.xlsx", sheet = "choices" )

# previous six month:
ditricts_median_pr6 <- read_excel("input/data/median-pr6/District_Median_JMMI_R29_OCT22.xlsx", sheet = "Median_prices_AFN" )
provinces_median_pr6 <- read_excel("input/data/median-pr6/Province_Median_JMMI_R29_OCT22.xlsx", sheet = "Median_prices_AFN" )
regionals_median_pr6 <- read_excel("input/data/median-pr6/Regional_Median_JMMI_R29_OCT22.xlsx", sheet = "Median_prices_AFN" )
national_median_pr6 <- read_excel("input/data/median-pr6/National_Median_JMMI_R29_OCT22.xlsx", sheet = "Median_prices_AFN" )

# adding admin labels
curr_data <- JMMI_admin_label(curr_data, region = "afg_region", province = "afg_prov", district = "afg_dist")

oldnames = c("afg_region", "afg_prov", "afg_dist", "region_name", "province_name", "district_name")
newnames = c("afg_region_code","afg_prov_code", "afg_dist_code", "afg_region", "afg_prov", "afg_dist")
curr_data <- curr_data %>% rename_at(vars(all_of(oldnames)), ~ all_of(newnames))

curr_data <- curr_data %>% mutate_at(vars(ends_with("_price")), as.numeric)
curr_data <- curr_data %>% mutate_at(vars(ends_with("_unit_specify")), as.numeric)

# median price of pulses (updated on 23 May 2023)
prev_data <- prev_data %>% 
  rowwise() %>%
  mutate(pulses_merged_price_final = median(pulses_beans_price, pulses_lentils_price, pulses_split_peas_price, na.rm = TRUE)) %>% 
  # mutate(pulses_merged_price_final = suppressWarnings(min(pulses_beans_price, pulses_lentils_price, pulses_split_peas_price, na.rm = TRUE))) %>% 
  # mutate(pulses_merged_price_final = ifelse(is.infinite(pulses_merged_price_final), NA, pulses_merged_price_final)) %>%
  ungroup()

## Normalize prices
#cotton price for one square meter:
curr_data$cotton_cloth_price <- curr_data$cotton_cloth_price 
curr_data$firewood_price <- curr_data$firewood_price / curr_data$firewood_unit_specify
# Safe Water-20L Jugs
curr_data$safe_water_price <- curr_data$safe_water_price
curr_data$coal_price <- curr_data$coal_price / curr_data$coal_unit_specify
# Sanitary pad - problematic 
curr_data$sanitary_pad_price <- curr_data$sanitary_pad_price / curr_data$sanitary_pad_unit_specify * 10 # adjusted price for one package of 10 sanitary pad pieces
curr_data$salt_price <- curr_data$salt_price / curr_data$salt_unit_specify
curr_data$pulses_split_peas_price <- curr_data$pulses_split_peas_price / curr_data$pulses_split_peas_unit_specify
curr_data$pulses_beans_price <- curr_data$pulses_beans_price / curr_data$pulses_beans_unit_specify
curr_data$pulses_lentils_price <- curr_data$pulses_lentils_price / curr_data$pulses_lentils_unit_specify
curr_data$local_rice_price <- curr_data$local_rice_price / curr_data$local_rice_unit_specify
curr_data$wheat_local_price <- curr_data$wheat_local_price / curr_data$wheat_local_unit_specify
curr_data$wheat_imported_price <- curr_data$wheat_imported_price / curr_data$wheat_imported_unit_specify
curr_data$veg_oil_price <- curr_data$veg_oil_price / curr_data$veg_oil_unit_specify
# items stock and restock
curr_data$pulses_lentils_stock_last <- curr_data$pulses_beans_stock_last
curr_data$pulses_split_peas_stock_last <- curr_data$pulses_beans_stock_last
curr_data$re_stock_pulses_lentils <- curr_data$re_stock_pulses_beans
curr_data$re_stock_pulses_split_peas <- curr_data$re_stock_pulses_beans
curr_data$salt_stock_last <- curr_data$veg_oil_stock_last
curr_data$re_stock_salt <- curr_data$re_stock_veg_oil
curr_data$sugar_stock_last <- curr_data$veg_oil_stock_last
curr_data$re_stock_sugar <- curr_data$re_stock_veg_oil
curr_data$toothbrush_adult_stock_last <- curr_data$soap_stock_last
curr_data$re_stock_toothbrush_adult <- curr_data$re_stock_soap
curr_data$toothpaste_stock_last <- curr_data$soap_stock_last
curr_data$re_stock_toothpaste <- curr_data$re_stock_soap
curr_data$sanitary_pad_stock_last <- curr_data$soap_stock_last
curr_data$re_stock_sanitary_pad<- curr_data$re_stock_soap
curr_data$notebook_stock_last <- curr_data$pen_stock_last
curr_data$re_stock_notebook <- curr_data$re_stock_pen
curr_data$rubber_stock_last <- curr_data$pen_stock_last
curr_data$re_stock_rubber <- curr_data$re_stock_pen

# need to be check if we can remove them or not?
# curr_data$diff_stock_restock_pulses_lentils <- curr_data$diff_stock_restock_pulses_beans
# curr_data$diff_stock_restock_pulses_split_peas <- curr_data$diff_stock_restock_pulses_beans
# curr_data$diff_stock_restock_salt <- curr_data$diff_stock_restock_veg_oil
# curr_data$diff_stock_restock_sugar <- curr_data$diff_stock_restock_veg_oil
# curr_data$diff_stock_restock_toothbrush_adult <- curr_data$diff_stock_restock_soap
# curr_data$diff_stock_restock_toothpaste <- curr_data$diff_stock_restock_soap
# curr_data$diff_stock_restock_sanitary_pad <- curr_data$diff_stock_restock_soap
# curr_data$diff_stock_restock_notebook <- curr_data$diff_stock_restock_pen
# curr_data$diff_stock_restock_rubber <- curr_data$diff_stock_restock_pen
# curr_data$bordercrossing_nfi_s <- curr_data$bordercrossing_food_s
# curr_data$women_access_continued <- curr_data$women_access

# Constructing new variables
curr_data$financial_services_sum <- curr_data$financial_services.banks + curr_data$financial_services.mobile_money_agents +
  curr_data$financial_services.formal_transfer_services + curr_data$financial_services.hawala +
  curr_data$financial_services.microfinance_institutions + curr_data$financial_services.village_banks +
  curr_data$financial_services.credit_unions +curr_data$financial_services.local_businesses +
  curr_data$financial_services.members_community + curr_data$financial_services.other

curr_data <- curr_data %>% mutate(
  financial_services_cat = case_when(
    financial_services_sum == 0 ~ "none",
    financial_services_sum == 1 ~ "one",
    financial_services_sum > 1 ~ "two_or_more",
  ), 
  
  difficulty_roads_comp = case_when(
    difficulty_roads.no_none == 1 ~ "no",
    difficulty_roads.no_none != 1 ~ "yes_prior_last_30_days",
  )
)

# median price of pulses (updated on 23 May 2023)
curr_data <- curr_data %>% 
  rowwise() %>%
  mutate(pulses_merged_price_final = median(pulses_beans_price, pulses_lentils_price, pulses_split_peas_price, na.rm = TRUE)) %>% 
  # mutate(pulses_merged_price_final = suppressWarnings(min(pulses_beans_price, pulses_lentils_price, pulses_split_peas_price, na.rm = TRUE))) %>% 
  # mutate(pulses_merged_price_final = ifelse(is.infinite(pulses_merged_price_final), NA, pulses_merged_price_final)) %>%
  ungroup()

# convert prices to USD
curr_data$wheat_local_price_USD <- curr_data$wheat_local_price / usd_rate
curr_data$wheat_imported_price_USD <- curr_data$wheat_imported_price / usd_rate
curr_data$local_rice_price_USD <- curr_data$local_rice_price / usd_rate
curr_data$veg_oil_price_USD <- curr_data$veg_oil_price / usd_rate
curr_data$pulses_merged_price_final_USD <- curr_data$pulses_merged_price_final / usd_rate
curr_data$salt_price_USD <- curr_data$salt_price / usd_rate
curr_data$sugar_price_USD <- curr_data$sugar_price / usd_rate
curr_data$tomatoes_price_USD <- curr_data$tomatoes_price / usd_rate
curr_data$pen_price_USD <- curr_data$pen_price / usd_rate
curr_data$notebook_price_USD <- curr_data$notebook_price / usd_rate
curr_data$rubber_price_USD <- curr_data$rubber_price / usd_rate
curr_data$cotton_cloth_price_USD <- curr_data$cotton_cloth_price / usd_rate
curr_data$toothbrush_adult_price_USD <- curr_data$toothbrush_adult_price / usd_rate
curr_data$toothpaste_price_USD <- curr_data$toothpaste_price / usd_rate
curr_data$sanitary_pad_price_USD <- curr_data$sanitary_pad_price / usd_rate
curr_data$coal_price_price_USD <- curr_data$coal_price / usd_rate
curr_data$soap_price_USD <- curr_data$soap_price / usd_rate
curr_data$lpg_price_USD <- curr_data$lpg_price / usd_rate
# diesel and petrol
curr_data$diesel_price_USD <- curr_data$diesel_price / usd_rate
curr_data$petrol_price_USD <- curr_data$petrol_price / usd_rate
curr_data$safe_water_price_USD <- curr_data$safe_water_price / usd_rate
curr_data$firewood_price_USD <- curr_data$firewood_price / usd_rate
# WINTERIZATION
curr_data$winter_jacket_price_USD <-  curr_data$winter_jacket_price / usd_rate
curr_data$water_container_price_USD <- curr_data$water_container_price / usd_rate
curr_data$cooking_pot_price_USD <- curr_data$cooking_pot_price / usd_rate
curr_data$blanket_price_USD <- curr_data$blanket_price / usd_rate

############ MEDIAN Calculations #################
# This function calculate the median of prices
# option: NULL, "prev_data", "USD"
calculate_median <- function(df, option = NULL){
  if (missing(option)) {
    df %>% 
      summarise(
        wheat_local_price_cr_median = median(wheat_local_price, na.rm = T),
        wheat_imported_price_cr_median = median(wheat_imported_price, na.rm = T),
        local_rice_price_cr_median = median(local_rice_price, na.rm = T),
        veg_oil_price_cr_median = median(veg_oil_price, na.rm = T),
        pulses_merged_price_final_cr_median = median(pulses_merged_price_final, na.rm = T),
        salt_price_cr_median = median(salt_price, na.rm = T),
        sugar_price_cr_median = median(sugar_price, na.rm = T),
        tomatoes_price_cr_median = median(tomatoes_price, na.rm = T),
        pen_price_cr_median = median(pen_price, na.rm = T),
        notebook_price_cr_median = median(notebook_price, na.rm = T),
        rubber_price_cr_median = median(rubber_price, na.rm = T),
        cotton_cloth_price_cr_median = median(cotton_cloth_price, na.rm = T),
        toothbrush_adult_price_cr_median = median(toothbrush_adult_price, na.rm = T),
        toothpaste_price_cr_median = median(toothpaste_price, na.rm = T),
        sanitary_pad_price_cr_median = median(sanitary_pad_price, na.rm = T),
        coal_price_cr_median = median(coal_price, na.rm = T),
        soap_price_cr_median = median(soap_price, na.rm = T),
        lpg_price_cr_median = median(lpg_price, na.rm = T),
        # diesel and petrol
        diesel_price_cr_median = median(diesel_price, na.rm = T),
        petrol_price_cr_median = median(petrol_price, na.rm = T),
        # USD buy and sell rate
        buy_rate_cr_median = median(buy_rate, na.rm = T),
        sell_rate_cr_median = median(sell_rate, na.rm = T),
        
        safe_water_price_cr_median = median(safe_water_price, na.rm = T),
        firewood_price_cr_median = median(firewood_price, na.rm = T),
        pulses_lentils_price_cr_median = median(pulses_lentils_price, na.rm = T),
        pulses_beans_price_cr_median = median(pulses_beans_price, na.rm = T),
        pulses_split_peas_price_cr_median = median(pulses_split_peas_price, na.rm = T),
        
        # WINTERIZATION 
        winter_jacket_price_cr_median = median(winter_jacket_price, na.rm = T),
        water_container_price_cr_median =  median(water_container_price, na.rm = T),
        cooking_pot_price_cr_median =  median(cooking_pot_price, na.rm = T),
        blanket_price_cr_median = median(blanket_price, na.rm = T)
      )
  } else if (option == "prev_data"){
    df %>% 
      summarise(
        wheat_local_price_pr_median = median(wheat_local_price, na.rm = T),
        wheat_imported_price_pr_median = median(wheat_imported_price, na.rm = T),
        local_rice_price_pr_median = median(local_rice_price, na.rm = T),
        veg_oil_price_pr_median = median(veg_oil_price, na.rm = T),
        pulses_merged_price_final_pr_median = median(pulses_merged_price_final, na.rm = T),
        salt_price_pr_median = median(salt_price, na.rm = T),
        sugar_price_pr_median = median(sugar_price, na.rm = T),
        tomatoes_price_pr_median = median(tomatoes_price, na.rm = T),
        pen_price_pr_median = median(pen_price, na.rm = T),
        notebook_price_pr_median = median(notebook_price, na.rm = T),
        rubber_price_pr_median = median(rubber_price, na.rm = T),
        cotton_cloth_price_pr_median = median(cotton_cloth_price, na.rm = T),
        toothbrush_adult_price_pr_median = median(toothbrush_adult_price, na.rm = T),
        toothpaste_price_pr_median = median(toothpaste_price, na.rm = T),
        sanitary_pad_price_pr_median = median(sanitary_pad_price, na.rm = T),
        coal_price_pr_median = median(coal_price, na.rm = T),
        soap_price_pr_median = median(soap_price, na.rm = T),
        lpg_price_pr_median = median(lpg_price, na.rm = T),
        # diesel and petrol
        diesel_price_pr_median = median(diesel_price, na.rm = T),
        petrol_price_pr_median = median(petrol_price, na.rm = T),
        # USD buy and sell rate
        buy_rate_pr_median = median(buy_rate, na.rm = T),
        sell_rate_pr_median = median(sell_rate, na.rm = T),
        
        safe_water_price_pr_median = median(safe_water_price, na.rm = T),
        firewood_price_pr_median = median(firewood_price, na.rm = T),
        pulses_lentils_price_pr_median = median(pulses_lentils_price, na.rm = T),
        pulses_beans_price_pr_median = median(pulses_beans_price, na.rm = T),
        pulses_split_peas_price_pr_median = median(pulses_split_peas_price, na.rm = T),
        
        # WINTERIZATION 
        winter_jacket_price_pr_median = median(winter_jacket_price, na.rm = T),
        water_container_price_pr_median =  median(water_container_price, na.rm = T),
        cooking_pot_price_pr_median =  median(cooking_pot_price, na.rm = T),
        blanket_price_pr_median = median(blanket_price, na.rm = T)
      )
  } else if (option == "USD"){
    df %>% 
      summarise(
        wheat_local_price_cr_median = median(wheat_local_price_USD, na.rm = T),
        wheat_imported_price_cr_median = median(wheat_imported_price_USD, na.rm = T),
        local_rice_price_cr_median = median(local_rice_price_USD, na.rm = T),
        veg_oil_price_cr_median = median(veg_oil_price_USD, na.rm = T),
        pulses_merged_price_final_cr_median = median(pulses_merged_price_final_USD, na.rm = T),
        salt_price_cr_median = median(salt_price_USD, na.rm = T),
        sugar_price_cr_median = median(sugar_price_USD, na.rm = T),
        tomatoes_price_cr_median = median(tomatoes_price_USD, na.rm = T),
        pen_price_cr_median = median(pen_price_USD, na.rm = T),
        notebook_price_cr_median = median(notebook_price_USD, na.rm = T),
        rubber_price_cr_median = median(rubber_price_USD, na.rm = T),
        cotton_cloth_price_cr_median = median(cotton_cloth_price_USD, na.rm = T),
        toothbrush_adult_price_cr_median = median(toothbrush_adult_price_USD, na.rm = T),
        toothpaste_price_cr_median = median(toothpaste_price_USD, na.rm = T),
        sanitary_pad_price_cr_median = median(sanitary_pad_price_USD, na.rm = T),
        soap_price_cr_median = median(soap_price_USD, na.rm = T),
        coal_price_cr_median = median(coal_price_price_USD, na.rm = T),
        safe_water_price_cr_median = median(safe_water_price_USD, na.rm = T),
        firewood_price_cr_median = median(firewood_price_USD, na.rm = T),
        lpg_price_cr_median = median(lpg_price_USD, na.rm = T),
        # diesel and petrol
        diesel_price_cr_median = median(diesel_price_USD, na.rm = T),
        petrol_price_cr_median = median(petrol_price_USD, na.rm = T),
        # WINTERIZATION
        winter_jacket_price_cr_median = median(winter_jacket_price_USD, na.rm = T),
        water_container_price_cr_median =  median(water_container_price_USD, na.rm = T),
        cooking_pot_price_cr_median =  median(cooking_pot_price_USD, na.rm = T),
        blanket_price_cr_median = median(blanket_price_USD, na.rm = T)
      )
  }
}

# This function calculate the mean of prices
# option: NULL, "prev_data", "USD"
calculate_mean <- function(df, option = NULL){
  if (missing(option)) {
    df %>% 
      summarise(
        wheat_local_price_cr_mean = mean(wheat_local_price, na.rm = T),
        wheat_imported_price_cr_mean = mean(wheat_imported_price, na.rm = T),
        local_rice_price_cr_mean = mean(local_rice_price, na.rm = T),
        veg_oil_price_cr_mean = mean(veg_oil_price, na.rm = T),
        pulses_merged_price_final_cr_mean = mean(pulses_merged_price_final, na.rm = T),
        salt_price_cr_mean = mean(salt_price, na.rm = T),
        sugar_price_cr_mean = mean(sugar_price, na.rm = T),
        tomatoes_price_cr_mean = mean(tomatoes_price, na.rm = T),
        pen_price_cr_mean = mean(pen_price, na.rm = T),
        notebook_price_cr_mean = mean(notebook_price, na.rm = T),
        rubber_price_cr_mean = mean(rubber_price, na.rm = T),
        cotton_cloth_price_cr_mean = mean(cotton_cloth_price, na.rm = T),
        toothbrush_adult_price_cr_mean = mean(toothbrush_adult_price, na.rm = T),
        toothpaste_price_cr_mean = mean(toothpaste_price, na.rm = T),
        sanitary_pad_price_cr_mean = mean(sanitary_pad_price, na.rm = T),
        coal_price_cr_mean = mean(coal_price, na.rm = T),
        soap_price_cr_mean = mean(soap_price, na.rm = T),
        lpg_price_cr_mean = mean(lpg_price, na.rm = T),
        # diesel and petrol
        diesel_price_cr_mean = mean(diesel_price, na.rm = T),
        petrol_price_cr_mean = mean(petrol_price, na.rm = T),
        # USD buy and sell rate
        buy_rate_cr_mean = mean(buy_rate, na.rm = T),
        sell_rate_cr_mean = mean(sell_rate, na.rm = T),
        
        safe_water_price_cr_mean = mean(safe_water_price, na.rm = T),
        firewood_price_cr_mean = mean(firewood_price, na.rm = T),
        pulses_lentils_price_cr_mean = mean(pulses_lentils_price, na.rm = T),
        pulses_beans_price_cr_mean = mean(pulses_beans_price, na.rm = T),
        pulses_split_peas_price_cr_mean = mean(pulses_split_peas_price, na.rm = T),
        
        # WINTERIZATION 
        winter_jacket_price_cr_mean = mean(winter_jacket_price, na.rm = T),
        water_container_price_cr_mean =  mean(water_container_price, na.rm = T),
        cooking_pot_price_cr_mean =  mean(cooking_pot_price, na.rm = T),
        blanket_price_cr_mean = mean(blanket_price, na.rm = T)
      )
  } else if (option == "prev_data"){
    df %>% 
      summarise(
        wheat_local_price_pr_mean = mean(wheat_local_price, na.rm = T),
        wheat_imported_price_pr_mean = mean(wheat_imported_price, na.rm = T),
        local_rice_price_pr_mean = mean(local_rice_price, na.rm = T),
        veg_oil_price_pr_mean = mean(veg_oil_price, na.rm = T),
        pulses_merged_price_final_pr_mean = mean(pulses_merged_price_final, na.rm = T),
        salt_price_pr_mean = mean(salt_price, na.rm = T),
        sugar_price_pr_mean = mean(sugar_price, na.rm = T),
        tomatoes_price_pr_mean = mean(tomatoes_price, na.rm = T),
        pen_price_pr_mean = mean(pen_price, na.rm = T),
        notebook_price_pr_mean = mean(notebook_price, na.rm = T),
        rubber_price_pr_mean = mean(rubber_price, na.rm = T),
        cotton_cloth_price_pr_mean = mean(cotton_cloth_price, na.rm = T),
        toothbrush_adult_price_pr_mean = mean(toothbrush_adult_price, na.rm = T),
        toothpaste_price_pr_mean = mean(toothpaste_price, na.rm = T),
        sanitary_pad_price_pr_mean = mean(sanitary_pad_price, na.rm = T),
        coal_price_pr_mean = mean(coal_price, na.rm = T),
        soap_price_pr_mean = mean(soap_price, na.rm = T),
        lpg_price_pr_mean = mean(lpg_price, na.rm = T),
        # diesel and petrol
        diesel_price_pr_mean = mean(diesel_price, na.rm = T),
        petrol_price_pr_mean = mean(petrol_price, na.rm = T),
        # USD buy and sell rate
        buy_rate_pr_mean = mean(buy_rate, na.rm = T),
        sell_rate_pr_mean = mean(sell_rate, na.rm = T),
        
        safe_water_price_pr_mean = mean(safe_water_price, na.rm = T),
        firewood_price_pr_mean = mean(firewood_price, na.rm = T),
        pulses_lentils_price_pr_mean = mean(pulses_lentils_price, na.rm = T),
        pulses_beans_price_pr_mean = mean(pulses_beans_price, na.rm = T),
        pulses_split_peas_price_pr_mean = mean(pulses_split_peas_price, na.rm = T),
        
        # WINTERIZATION 
        winter_jacket_price_pr_mean = mean(winter_jacket_price, na.rm = T),
        water_container_price_pr_mean =  mean(water_container_price, na.rm = T),
        cooking_pot_price_pr_mean =  mean(cooking_pot_price, na.rm = T),
        blanket_price_pr_mean = mean(blanket_price, na.rm = T)
      )
  } else if (option == "USD"){
    df %>% 
      summarise(
        wheat_local_price_cr_mean = mean(wheat_local_price_USD, na.rm = T),
        wheat_imported_price_cr_mean = mean(wheat_imported_price_USD, na.rm = T),
        local_rice_price_cr_mean = mean(local_rice_price_USD, na.rm = T),
        veg_oil_price_cr_mean = mean(veg_oil_price_USD, na.rm = T),
        pulses_merged_price_final_cr_mean = mean(pulses_merged_price_final_USD, na.rm = T),
        salt_price_cr_mean = mean(salt_price_USD, na.rm = T),
        sugar_price_cr_mean = mean(sugar_price_USD, na.rm = T),
        tomatoes_price_cr_mean = mean(tomatoes_price_USD, na.rm = T),
        pen_price_cr_mean = mean(pen_price_USD, na.rm = T),
        notebook_price_cr_mean = mean(notebook_price_USD, na.rm = T),
        rubber_price_cr_mean = mean(rubber_price_USD, na.rm = T),
        cotton_cloth_price_cr_mean = mean(cotton_cloth_price_USD, na.rm = T),
        toothbrush_adult_price_cr_mean = mean(toothbrush_adult_price_USD, na.rm = T),
        toothpaste_price_cr_mean = mean(toothpaste_price_USD, na.rm = T),
        sanitary_pad_price_cr_mean = mean(sanitary_pad_price_USD, na.rm = T),
        soap_price_cr_mean = mean(soap_price_USD, na.rm = T),
        coal_price_cr_mean = mean(coal_price_price_USD, na.rm = T),
        safe_water_price_cr_mean = mean(safe_water_price_USD, na.rm = T),
        firewood_price_cr_mean = mean(firewood_price_USD, na.rm = T),
        lpg_price_cr_mean = mean(lpg_price_USD, na.rm = T),
        # diesel and petrol
        diesel_price_cr_mean = mean(diesel_price_USD, na.rm = T),
        petrol_price_cr_mean = mean(petrol_price_USD, na.rm = T),
        # WINTERIZATION
        winter_jacket_price_cr_mean = mean(winter_jacket_price_USD, na.rm = T),
        water_container_price_cr_mean =  mean(water_container_price_USD, na.rm = T),
        cooking_pot_price_cr_mean =  mean(cooking_pot_price_USD, na.rm = T),
        blanket_price_cr_mean = mean(blanket_price_USD, na.rm = T)
      )
  }
}

# this function calculate the changes of current rout and the previous round
calculate_median_difference <- function(df, option = FALSE ){
  
  if (missing(option)) {
    df %>% 
      mutate(
        wheat_local_price_diff_perc = (wheat_local_price_cr_median - wheat_local_price_pr_median) / wheat_local_price_pr_median,
        wheat_imported_price_diff_perc = (wheat_imported_price_cr_median - wheat_imported_price_pr_median) / wheat_imported_price_pr_median,
        local_rice_price_diff_perc = (local_rice_price_cr_median - local_rice_price_pr_median ) / local_rice_price_pr_median,
        veg_oil_price_diff_perc = (veg_oil_price_cr_median - veg_oil_price_pr_median ) / veg_oil_price_pr_median, 
        pulses_merged_price_final_diff_perc = (pulses_merged_price_final_cr_median - pulses_merged_price_final_pr_median  ) / pulses_merged_price_final_pr_median, 
        salt_price_diff_perc = (salt_price_cr_median - salt_price_pr_median) / salt_price_pr_median, 
        sugar_price_diff_perc = (sugar_price_cr_median - sugar_price_pr_median ) / sugar_price_pr_median, 
        tomatoes_price_diff_perc = (tomatoes_price_cr_median - tomatoes_price_pr_median ) / tomatoes_price_pr_median, 
        cotton_cloth_price_diff_perc = (cotton_cloth_price_cr_median - cotton_cloth_price_pr_median ) / cotton_cloth_price_pr_median, 
        toothbrush_adult_price_diff_perc = (toothbrush_adult_price_cr_median - toothbrush_adult_price_pr_median ) / toothbrush_adult_price_pr_median, 
        toothpaste_price_diff_perc = (toothpaste_price_cr_median - toothpaste_price_pr_median  ) / toothpaste_price_pr_median, 
        sanitary_pad_price_diff_perc = (sanitary_pad_price_cr_median - sanitary_pad_price_pr_median) / sanitary_pad_price_pr_median, 
        soap_price_diff_perc = (soap_price_cr_median - soap_price_pr_median) / soap_price_pr_median, 
        safe_water_price_diff_perc = (safe_water_price_cr_median - safe_water_price_pr_median ) / safe_water_price_pr_median, 
        firewood_price_diff_perc = (firewood_price_cr_median - firewood_price_pr_median ) / firewood_price_pr_median, 
        pulses_lentils_price_diff_perc = (pulses_lentils_price_cr_median - pulses_lentils_price_pr_median) / pulses_lentils_price_pr_median,
        pulses_beans_price_diff_perc = (pulses_beans_price_cr_median - pulses_beans_price_pr_median) / pulses_beans_price_pr_median,
        pulses_split_peas_price_diff_perc = (pulses_split_peas_price_cr_median - pulses_split_peas_price_pr_median) / pulses_split_peas_price_pr_median,
        pen_price_diff_perc = (pen_price_cr_median - pen_price_pr_median) / pen_price_pr_median,
        notebook_price_diff_perc = (notebook_price_cr_median - notebook_price_pr_median) / notebook_price_pr_median,
        rubber_price_diff_perc = (rubber_price_cr_median - rubber_price_pr_median) / rubber_price_pr_median,
        coal_price_diff_perc = (coal_price_cr_median - coal_price_pr_median) / coal_price_pr_median,
        # diesel and petrol
        diesel_price_diff_perc = (diesel_price_cr_median - diesel_price_pr_median) / diesel_price_pr_median,
        petrol_price_diff_perc = (petrol_price_cr_median - petrol_price_pr_median) / petrol_price_pr_median,
        buy_rate_diff_perc = (buy_rate_cr_median - buy_rate_pr_median) / buy_rate_pr_median,
        sell_rate_diff_perc = (sell_rate_cr_median - sell_rate_pr_median) / sell_rate_pr_median,
        lpg_price_diff_perc = (lpg_price_cr_median - lpg_price_pr_median) / lpg_price_pr_median,
        # WINTERIZATION
        winter_jacket_price_diff_perc = (winter_jacket_price_cr_median - winter_jacket_price_pr_median) / winter_jacket_price_pr_median,
        water_container_price_diff_perc = (water_container_price_cr_median - water_container_price_pr_median) / water_container_price_pr_median,
        cooking_pot_price_diff_perc = (cooking_pot_price_cr_median - cooking_pot_price_pr_median) / cooking_pot_price_pr_median,
        blanket_price_diff_perc = (blanket_price_cr_median - blanket_price_pr_median) / blanket_price_pr_median
      ) %>%
      mutate(across(ends_with("_diff_perc"), ~ round(. * 100, 2)))
  } else if (option == TRUE){
    df %>% 
      mutate(
        wheat_local_price_diff_perc = (wheat_local_price_cr_median - wheat_local_price_pr6_median) / wheat_local_price_pr6_median,
        wheat_imported_price_diff_perc = (wheat_imported_price_cr_median - wheat_imported_price_pr6_median) / wheat_imported_price_pr6_median,
        local_rice_price_diff_perc = (local_rice_price_cr_median - local_rice_price_pr6_median ) / local_rice_price_pr6_median,
        veg_oil_price_diff_perc = (veg_oil_price_cr_median - veg_oil_price_pr6_median ) / veg_oil_price_pr6_median, 
        pulses_merged_price_final_diff_perc = (pulses_merged_price_final_cr_median - pulses_merged_price_final_pr6_median  ) / pulses_merged_price_final_pr6_median, 
        salt_price_diff_perc = (salt_price_cr_median - salt_price_pr6_median) / salt_price_pr6_median, 
        sugar_price_diff_perc = (sugar_price_cr_median - sugar_price_pr6_median ) / sugar_price_pr6_median, 
        tomatoes_price_diff_perc = (tomatoes_price_cr_median - tomatoes_price_pr6_median ) / tomatoes_price_pr6_median, 
        cotton_cloth_price_diff_perc = (cotton_cloth_price_cr_median - cotton_cloth_price_pr6_median ) / cotton_cloth_price_pr6_median, 
        toothbrush_adult_price_diff_perc = (toothbrush_adult_price_cr_median - toothbrush_adult_price_pr6_median ) / toothbrush_adult_price_pr6_median, 
        toothpaste_price_diff_perc = (toothpaste_price_cr_median - toothpaste_price_pr6_median  ) / toothpaste_price_pr6_median, 
        sanitary_pad_price_diff_perc = (sanitary_pad_price_cr_median - sanitary_pad_price_pr6_median) / sanitary_pad_price_pr6_median, 
        soap_price_diff_perc = (soap_price_cr_median - soap_price_pr6_median) / soap_price_pr6_median, 
        safe_water_price_diff_perc = (safe_water_price_cr_median - safe_water_price_pr6_median ) / safe_water_price_pr6_median,
        firewood_price_diff_perc = (firewood_price_cr_median - firewood_price_pr6_median ) / firewood_price_pr6_median, 
        pulses_lentils_price_diff_perc = (pulses_lentils_price_cr_median - pulses_lentils_price_pr6_median) / pulses_lentils_price_pr6_median,
        pulses_beans_price_diff_perc = (pulses_beans_price_cr_median - pulses_beans_price_pr6_median) / pulses_beans_price_pr6_median,
        pulses_split_peas_price_diff_perc = (pulses_split_peas_price_cr_median - pulses_split_peas_price_pr6_median) / pulses_split_peas_price_pr6_median,
        pen_price_diff_perc = (pen_price_cr_median - pen_price_pr6_median) / pen_price_pr6_median,
        notebook_price_diff_perc = (notebook_price_cr_median - notebook_price_pr6_median) / notebook_price_pr6_median,
        rubber_price_diff_perc = (rubber_price_cr_median - rubber_price_pr6_median) / rubber_price_pr6_median,
        coal_price_diff_perc = (coal_price_cr_median - coal_price_pr6_median) / coal_price_pr6_median,
        # diesel and petrol
        diesel_price_diff_perc = (diesel_price_cr_median - diesel_price_pr6_median) / diesel_price_pr6_median,
        petrol_price_diff_perc = (petrol_price_cr_median - petrol_price_pr6_median) / petrol_price_pr6_median,
        buy_rate_diff_perc = (buy_rate_cr_median - buy_rate_pr6_median) / buy_rate_pr6_median,
        sell_rate_diff_perc = (sell_rate_cr_median - sell_rate_pr6_median) / sell_rate_pr6_median,
        lpg_price_diff_perc = (lpg_price_cr_median - lpg_price_pr6_median) / lpg_price_pr6_median,
        
        # WINTERIZATION
        winter_jacket_price_diff_perc = (winter_jacket_price_cr_median - winter_jacket_price_pr6_median) / winter_jacket_price_pr6_median,
        water_container_price_diff_perc = (water_container_price_cr_median - water_container_price_pr6_median) / water_container_price_pr6_median,
        cooking_pot_price_diff_perc = (cooking_pot_price_cr_median - cooking_pot_price_pr6_median) / cooking_pot_price_pr6_median,
        blanket_price_diff_perc = (blanket_price_cr_median - blanket_price_pr6_median) / blanket_price_pr6_median
      ) %>%
      mutate(across(ends_with("_diff_perc"), ~ round(. * 100, 2)))
  }
}


# this function calculate the changes of current rout and the previous round
calculate_mean_difference <- function(df, option = FALSE ){
  
  if (missing(option)) {
    df %>% 
      mutate(
        wheat_local_price_diff_perc = (wheat_local_price_cr_mean - wheat_local_price_pr_mean) / wheat_local_price_pr_mean,
        wheat_imported_price_diff_perc = (wheat_imported_price_cr_mean - wheat_imported_price_pr_mean) / wheat_imported_price_pr_mean,
        local_rice_price_diff_perc = (local_rice_price_cr_mean - local_rice_price_pr_mean ) / local_rice_price_pr_mean,
        veg_oil_price_diff_perc = (veg_oil_price_cr_mean - veg_oil_price_pr_mean ) / veg_oil_price_pr_mean, 
        pulses_merged_price_final_diff_perc = (pulses_merged_price_final_cr_mean - pulses_merged_price_final_pr_mean  ) / pulses_merged_price_final_pr_mean, 
        salt_price_diff_perc = (salt_price_cr_mean - salt_price_pr_mean) / salt_price_pr_mean, 
        sugar_price_diff_perc = (sugar_price_cr_mean - sugar_price_pr_mean ) / sugar_price_pr_mean, 
        tomatoes_price_diff_perc = (tomatoes_price_cr_mean - tomatoes_price_pr_mean ) / tomatoes_price_pr_mean, 
        cotton_cloth_price_diff_perc = (cotton_cloth_price_cr_mean - cotton_cloth_price_pr_mean ) / cotton_cloth_price_pr_mean, 
        toothbrush_adult_price_diff_perc = (toothbrush_adult_price_cr_mean - toothbrush_adult_price_pr_mean ) / toothbrush_adult_price_pr_mean, 
        toothpaste_price_diff_perc = (toothpaste_price_cr_mean - toothpaste_price_pr_mean  ) / toothpaste_price_pr_mean, 
        sanitary_pad_price_diff_perc = (sanitary_pad_price_cr_mean - sanitary_pad_price_pr_mean) / sanitary_pad_price_pr_mean, 
        soap_price_diff_perc = (soap_price_cr_mean - soap_price_pr_mean) / soap_price_pr_mean, 
        safe_water_price_diff_perc = (safe_water_price_cr_mean - safe_water_price_pr_mean ) / safe_water_price_pr_mean, 
        firewood_price_diff_perc = (firewood_price_cr_mean - firewood_price_pr_mean ) / firewood_price_pr_mean, 
        pulses_lentils_price_diff_perc = (pulses_lentils_price_cr_mean - pulses_lentils_price_pr_mean) / pulses_lentils_price_pr_mean,
        pulses_beans_price_diff_perc = (pulses_beans_price_cr_mean - pulses_beans_price_pr_mean) / pulses_beans_price_pr_mean,
        pulses_split_peas_price_diff_perc = (pulses_split_peas_price_cr_mean - pulses_split_peas_price_pr_mean) / pulses_split_peas_price_pr_mean,
        pen_price_diff_perc = (pen_price_cr_mean - pen_price_pr_mean) / pen_price_pr_mean,
        notebook_price_diff_perc = (notebook_price_cr_mean - notebook_price_pr_mean) / notebook_price_pr_mean,
        rubber_price_diff_perc = (rubber_price_cr_mean - rubber_price_pr_mean) / rubber_price_pr_mean,
        coal_price_diff_perc = (coal_price_cr_mean - coal_price_pr_mean) / coal_price_pr_mean,
        # diesel and petrol
        diesel_price_diff_perc = (diesel_price_cr_mean - diesel_price_pr_mean) / diesel_price_pr_mean,
        petrol_price_diff_perc = (petrol_price_cr_mean - petrol_price_pr_mean) / petrol_price_pr_mean,
        buy_rate_diff_perc = (buy_rate_cr_mean - buy_rate_pr_mean) / buy_rate_pr_mean,
        sell_rate_diff_perc = (sell_rate_cr_mean - sell_rate_pr_mean) / sell_rate_pr_mean,
        lpg_price_diff_perc = (lpg_price_cr_mean - lpg_price_pr_mean) / lpg_price_pr_mean,
        # WINTERIZATION
        winter_jacket_price_diff_perc = (winter_jacket_price_cr_mean - winter_jacket_price_pr_mean) / winter_jacket_price_pr_mean,
        water_container_price_diff_perc = (water_container_price_cr_mean - water_container_price_pr_mean) / water_container_price_pr_mean,
        cooking_pot_price_diff_perc = (cooking_pot_price_cr_mean - cooking_pot_price_pr_mean) / cooking_pot_price_pr_mean,
        blanket_price_diff_perc = (blanket_price_cr_mean - blanket_price_pr_mean) / blanket_price_pr_mean
      ) %>%
      mutate(across(ends_with("_diff_perc"), ~ round(. * 100, 2)))
  } else if (option == TRUE){
    df %>% 
      mutate(
        wheat_local_price_diff_perc = (wheat_local_price_cr_mean - wheat_local_price_pr6_mean) / wheat_local_price_pr6_mean,
        wheat_imported_price_diff_perc = (wheat_imported_price_cr_mean - wheat_imported_price_pr6_mean) / wheat_imported_price_pr6_mean,
        local_rice_price_diff_perc = (local_rice_price_cr_mean - local_rice_price_pr6_mean ) / local_rice_price_pr6_mean,
        veg_oil_price_diff_perc = (veg_oil_price_cr_mean - veg_oil_price_pr6_mean ) / veg_oil_price_pr6_mean, 
        pulses_merged_price_final_diff_perc = (pulses_merged_price_final_cr_mean - pulses_merged_price_final_pr6_mean  ) / pulses_merged_price_final_pr6_mean, 
        salt_price_diff_perc = (salt_price_cr_mean - salt_price_pr6_mean) / salt_price_pr6_mean, 
        sugar_price_diff_perc = (sugar_price_cr_mean - sugar_price_pr6_mean ) / sugar_price_pr6_mean, 
        tomatoes_price_diff_perc = (tomatoes_price_cr_mean - tomatoes_price_pr6_mean ) / tomatoes_price_pr6_mean, 
        cotton_cloth_price_diff_perc = (cotton_cloth_price_cr_mean - cotton_cloth_price_pr6_mean ) / cotton_cloth_price_pr6_mean, 
        toothbrush_adult_price_diff_perc = (toothbrush_adult_price_cr_mean - toothbrush_adult_price_pr6_mean ) / toothbrush_adult_price_pr6_mean, 
        toothpaste_price_diff_perc = (toothpaste_price_cr_mean - toothpaste_price_pr6_mean  ) / toothpaste_price_pr6_mean, 
        sanitary_pad_price_diff_perc = (sanitary_pad_price_cr_mean - sanitary_pad_price_pr6_mean) / sanitary_pad_price_pr6_mean, 
        soap_price_diff_perc = (soap_price_cr_mean - soap_price_pr6_mean) / soap_price_pr6_mean, 
        safe_water_price_diff_perc = (safe_water_price_cr_mean - safe_water_price_pr6_mean ) / safe_water_price_pr6_mean,
        firewood_price_diff_perc = (firewood_price_cr_mean - firewood_price_pr6_mean ) / firewood_price_pr6_mean, 
        pulses_lentils_price_diff_perc = (pulses_lentils_price_cr_mean - pulses_lentils_price_pr6_mean) / pulses_lentils_price_pr6_mean,
        pulses_beans_price_diff_perc = (pulses_beans_price_cr_mean - pulses_beans_price_pr6_mean) / pulses_beans_price_pr6_mean,
        pulses_split_peas_price_diff_perc = (pulses_split_peas_price_cr_mean - pulses_split_peas_price_pr6_mean) / pulses_split_peas_price_pr6_mean,
        pen_price_diff_perc = (pen_price_cr_mean - pen_price_pr6_mean) / pen_price_pr6_mean,
        notebook_price_diff_perc = (notebook_price_cr_mean - notebook_price_pr6_mean) / notebook_price_pr6_mean,
        rubber_price_diff_perc = (rubber_price_cr_mean - rubber_price_pr6_mean) / rubber_price_pr6_mean,
        coal_price_diff_perc = (coal_price_cr_mean - coal_price_pr6_mean) / coal_price_pr6_mean,
        # diesel and petrol
        diesel_price_diff_perc = (diesel_price_cr_mean - diesel_price_pr6_mean) / diesel_price_pr6_mean,
        petrol_price_diff_perc = (petrol_price_cr_mean - petrol_price_pr6_mean) / petrol_price_pr6_mean,
        buy_rate_diff_perc = (buy_rate_cr_mean - buy_rate_pr6_mean) / buy_rate_pr6_mean,
        sell_rate_diff_perc = (sell_rate_cr_mean - sell_rate_pr6_mean) / sell_rate_pr6_mean,
        lpg_price_diff_perc = (lpg_price_cr_mean - lpg_price_pr6_mean) / lpg_price_pr6_mean,
        
        # WINTERIZATION
        winter_jacket_price_diff_perc = (winter_jacket_price_cr_mean - winter_jacket_price_pr6_mean) / winter_jacket_price_pr6_mean,
        water_container_price_diff_perc = (water_container_price_cr_mean - water_container_price_pr6_mean) / water_container_price_pr6_mean,
        cooking_pot_price_diff_perc = (cooking_pot_price_cr_mean - cooking_pot_price_pr6_mean) / cooking_pot_price_pr6_mean,
        blanket_price_diff_perc = (blanket_price_cr_mean - blanket_price_pr6_mean) / blanket_price_pr6_mean
      ) %>%
      mutate(across(ends_with("_diff_perc"), ~ round(. * 100, 2)))
  }
}

### National ###
curr_data_national_median <- calculate_median(curr_data)
prev_data_national_median <- calculate_median(prev_data, option = "prev_data")
national_median_merged <- cbind(curr_data_national_median, prev_data_national_median)
national_median_merged_price_diff <- calculate_median_difference(national_median_merged)
curr_data_national_median_USD <- calculate_median(curr_data, option = "USD")

curr_data_national_mean <- calculate_mean(curr_data)
prev_data_national_mean <- calculate_mean(prev_data, option = "prev_data")
national_mean_merged <- cbind(curr_data_national_mean, prev_data_national_mean)
national_mean_merged_price_diff <- calculate_mean_difference(national_mean_merged)
curr_data_national_mean_USD <- calculate_mean(curr_data, option = "USD")

### Region ###
curr_data_region_median <- curr_data %>% group_by(afg_region)
curr_data_region_median <- calculate_median(curr_data_region_median)
prev_data_region_median <- prev_data %>% group_by(afg_region)
prev_data_region_median <- calculate_median(prev_data_region_median, option = "prev_data")
region_median_merged <- curr_data_region_median %>% left_join(prev_data_region_median, by = "afg_region")
region_median_merged_price_diff <- calculate_median_difference(region_median_merged)
curr_data_region_median_USD <- curr_data %>% group_by(afg_region)
curr_data_region_median_USD <- calculate_median(curr_data_region_median_USD, option = "USD")

curr_data_region_mean <- curr_data %>% group_by(afg_region)
curr_data_region_mean <- calculate_mean(curr_data_region_mean)
prev_data_region_mean <- prev_data %>% group_by(afg_region)
prev_data_region_mean <- calculate_mean(prev_data_region_mean, option = "prev_data")
region_mean_merged <- curr_data_region_mean %>% left_join(prev_data_region_mean, by = "afg_region")
region_mean_merged_price_diff <- calculate_mean_difference(region_mean_merged)
curr_data_region_mean_USD <- curr_data %>% group_by(afg_region)
curr_data_region_mean_USD <- calculate_mean(curr_data_region_mean_USD, option = "USD")

### Province ###

curr_data_provice_median <- curr_data %>% group_by(afg_prov)
curr_data_provice_median <- calculate_median(curr_data_provice_median)
prev_data_provice_median <- prev_data %>% group_by(afg_prov)
prev_data_provice_median <- calculate_median(prev_data_provice_median, option = "prev_data")
province_median_merged <- curr_data_provice_median %>% left_join(prev_data_provice_median, by = "afg_prov")
province_median_merged_price_diff <- calculate_median_difference(province_median_merged)
curr_data_province_median_USD <- curr_data %>% group_by(afg_prov)
curr_data_province_median_USD <- calculate_median(curr_data_province_median_USD, option = "USD")

curr_data_provice_mean <- curr_data %>% group_by(afg_prov)
curr_data_provice_mean <- calculate_mean(curr_data_provice_mean)
prev_data_provice_mean <- prev_data %>% group_by(afg_prov)
prev_data_provice_mean <- calculate_mean(prev_data_provice_mean, option = "prev_data")
province_mean_merged <- curr_data_provice_mean %>% left_join(prev_data_provice_mean, by = "afg_prov")
province_mean_merged_price_diff <- calculate_mean_difference(province_mean_merged)
curr_data_province_mean_USD <- curr_data %>% group_by(afg_prov)
curr_data_province_mean_USD <- calculate_mean(curr_data_province_mean_USD, option = "USD")

### District Level ###
curr_data_district_median <- curr_data %>% group_by(afg_dist)
curr_data_district_median <- calculate_median(curr_data_district_median)
prev_data_district_median <- prev_data %>% group_by(afg_dist)
prev_data_district_median <- calculate_median(prev_data_district_median, option = "prev_data")
district_median_merged <- curr_data_district_median %>% left_join(prev_data_district_median, by = "afg_dist")
district_median_merged_price_diff <- calculate_median_difference(district_median_merged)
curr_data_district_median_USD <- curr_data %>% group_by(afg_dist)
curr_data_district_median_USD <- calculate_median(curr_data_district_median_USD, option = "USD")

curr_data_district_mean <- curr_data %>% group_by(afg_dist)
curr_data_district_mean <- calculate_mean(curr_data_district_mean)
prev_data_district_mean <- prev_data %>% group_by(afg_dist)
prev_data_district_mean <- calculate_mean(prev_data_district_mean, option = "prev_data")
district_mean_merged <- curr_data_district_mean %>% left_join(prev_data_district_mean, by = "afg_dist")
district_mean_merged_price_diff <- calculate_mean_difference(district_mean_merged)
curr_data_district_mean_USD <- curr_data %>% group_by(afg_dist)
curr_data_district_mean_USD <- calculate_mean(curr_data_district_mean_USD, option = "USD")

### MEB Calculation ###
# This function calculate the MEB:
# measurement has tow values: median and mean
calculate_meb <- function(input_df, group, measurement){
  if (!missing(group)){
    group_list <- as.list(substitute(group))
    if(length(group_list)> 1) group_list <- group_list[-1]
    groups <- purrr::map_chr(group_list, rlang::as_string)
    
    result <- input_df %>% group_by(across(all_of(groups)))
  }else{
    result <- input_df
  }
  
  if (measurement == "median"){
    result <- result %>%   
      summarise(
        # food items: 
        wheat= median(as.numeric(wheat_imported_price) , na.rm = TRUE) * 89,
        rice= median(as.numeric(local_rice_price) , na.rm = TRUE) * 21,
        veg_oil= median(as.numeric(veg_oil_price) , na.rm = TRUE) * 7,
        salt= median(as.numeric(salt_price) , na.rm = TRUE) * 1,
        pulses= median(as.numeric(pulses_merged_price_final) , na.rm = TRUE) * 9,
        
        # WASH items: 
        soap= median(as.numeric(soap_price) , na.rm = TRUE) * 23,
        sanitary_pad= median(as.numeric(sanitary_pad_price) , na.rm = TRUE) * 4,
        cotton= median(as.numeric(cotton_cloth_price) , na.rm = TRUE) * 4,
        toothpaste= median(as.numeric(toothpaste_price) , na.rm = TRUE) * 2,
        safe_water= median(as.numeric(safe_water_price) , na.rm = TRUE) * 5.83,
        under_wear = Under_wear * 6
      )
  }else if (measurement == "mean"){
    result <- result %>%   
      summarise(
        # food items: 
        wheat= mean(as.numeric(wheat_imported_price) , na.rm = TRUE) * 89,
        rice= mean(as.numeric(local_rice_price) , na.rm = TRUE) * 21,
        veg_oil= mean(as.numeric(veg_oil_price) , na.rm = TRUE) * 7,
        salt= mean(as.numeric(salt_price) , na.rm = TRUE) * 1,
        pulses= mean(as.numeric(pulses_merged_price_final) , na.rm = TRUE) * 9,
        
        # WASH items: 
        soap= mean(as.numeric(soap_price) , na.rm = TRUE) * 23,
        sanitary_pad= mean(as.numeric(sanitary_pad_price) , na.rm = TRUE) * 4,
        cotton= mean(as.numeric(cotton_cloth_price) , na.rm = TRUE) * 4,
        toothpaste= mean(as.numeric(toothpaste_price) , na.rm = TRUE) * 2,
        safe_water= mean(as.numeric(safe_water_price) , na.rm = TRUE) * 5.83,
        under_wear = Under_wear * 6
      )
  }
    result <- result %>%   
      mutate(
      # calculation of food basket and WASH basket:
      Food_basket_AFN = wheat + rice + veg_oil + salt + pulses,
      Wash_hygiene_AFN = cotton + toothpaste + sanitary_pad + soap + safe_water + under_wear,
      
      # fixed values in the MEB
      Healthcare_AFN =   Healthcare_basket,
      Shelter_AFN   = Shelter_basket,
      Fuel_Electricity_AFN =  Fuel_Electricity_basket,
      Education_AFN =  Education_basket,
      Communication_AFN =  Communication_basket,
      Transportation_AFN =  Transportation_basket,
      
      # extra ten percent of all baskets:
      Ten_percent_unmet_AFN = (Food_basket_AFN + Wash_hygiene_AFN + Healthcare_basket + Shelter_basket + 
                                 Fuel_Electricity_basket + Education_basket + Communication_basket + Transportation_basket) * 0.1, 
      
      # final MEB:
      MEB_AFN = Food_basket_AFN + Wash_hygiene_AFN + Healthcare_basket + Shelter_basket + 
        Fuel_Electricity_basket + Education_basket + Communication_basket + Transportation_basket + Ten_percent_unmet_AFN,
      
      #USD
      food_basket_USD = Food_basket_AFN / usd_rate,
      Wash_hygiene_USD = Wash_hygiene_AFN / usd_rate,
      Healthcare_USD = Healthcare_AFN / usd_rate,
      Shelter_USD = Shelter_AFN / usd_rate,
      Fuel_Electricity_USD = Fuel_Electricity_AFN / usd_rate,
      Education_USD = Education_AFN / usd_rate,
      Communication_USD = Communication_AFN / usd_rate,
      Transportation_USD = Transportation_AFN / usd_rate,
      Ten_percent_unmet_USD = Ten_percent_unmet_AFN / usd_rate,
      MEB_USD = MEB_AFN / usd_rate
    )
  
  # only JMMI-related values:
  result <- result %>% select(starts_with("afg"), ends_with("AFN"), ends_with("USD"))
  return(result)
}

meb_median_national <- calculate_meb(curr_data, measurement = "median")
meb_median_regional <- calculate_meb(curr_data, afg_region, measurement = "median")
meb_median_provincial  <- calculate_meb(curr_data, afg_prov, measurement = "median")
meb_median_district <- calculate_meb(curr_data, afg_dist, measurement = "median")

meb_mean_national <- calculate_meb(curr_data, measurement = "mean")
meb_mean_regional <- calculate_meb(curr_data, afg_region, measurement = "mean")
meb_mean_provincial  <- calculate_meb(curr_data, afg_prov, measurement = "mean")
meb_mean_district <- calculate_meb(curr_data, afg_dist, measurement = "mean")

meb_median_national$level <- "National"
meb_mean_national$level <- "National"
# Rename column names 
ditricts_old_names <- colnames(ditricts_median_pr6)
ditricts_new_names <- gsub("cr", "pr6", ditricts_old_names)
colnames(ditricts_median_pr6) <- ditricts_new_names

provinces_old_names <- colnames(provinces_median_pr6)
provinces_new_names <- gsub("cr", "pr6", provinces_old_names)
colnames(provinces_median_pr6) <- provinces_new_names

regionals_old_names <- colnames(regionals_median_pr6)
regionals_new_names <- gsub("cr", "pr6", regionals_old_names)
colnames(regionals_median_pr6) <- regionals_new_names

national_old_names <- colnames(national_median_pr6)
national_new_names <- gsub("cr", "pr6", national_old_names)
colnames(national_median_pr6) <- national_new_names

# Merge current Median with previous six months medians
district_median_price_pr6_diff <- curr_data_district_median %>% left_join(ditricts_median_pr6, by='afg_dist')
provice_median_price_pr6_diff <- curr_data_provice_median %>% left_join(provinces_median_pr6, by='afg_prov')
region_median_price_pr6_diff <- curr_data_region_median %>% left_join(regionals_median_pr6, by='afg_region') 
national_median_price_pr6_diff <- merge(curr_data_national_median, national_median_pr6)

# Price median difference District level - median with previous six months
district_median_price_pr6_diff <- calculate_median_difference(district_median_price_pr6_diff, option = TRUE)
provice_median_price_pr6_diff <- calculate_median_difference(provice_median_price_pr6_diff, option = TRUE)
region_median_price_pr6_diff <- calculate_median_difference(region_median_price_pr6_diff, option = TRUE)
national_median_price_pr6_diff <- calculate_median_difference(national_median_price_pr6_diff, option = TRUE)

# National median
national_median <- list(
  Median_prices_AFN = curr_data_national_median,
  Median_price_diff = national_median_merged_price_diff,
  Median_price_6months_diff = national_median_price_pr6_diff,
  Median_prices_USD = curr_data_national_median_USD,
  Median_MEB = meb_median_national
)

# National mean
national_mean <- list(
  Mean_prices_AFN = curr_data_national_mean,
  Mean_price_diff = national_mean_merged_price_diff,
  Mean_prices_USD = curr_data_national_mean_USD,
  Mean_MEB = meb_mean_national
)

# Regional median
regional_median <- list(
  Median_prices_AFN = curr_data_region_median,
  Median_price_diff = region_median_merged_price_diff,
  Median_price_6months_diff = region_median_price_pr6_diff,
  Median_prices_USD = curr_data_region_median_USD,
  Median_MEB = meb_median_regional
)

# Regional mean
regional_mean <- list(
  Mean_prices_AFN = curr_data_region_mean,
  Mean_price_diff = region_mean_merged_price_diff,
  Mean_prices_USD = curr_data_region_mean_USD,
  Mean_MEB = meb_mean_regional
)

# Province median
province_median <- list(
  Median_prices_AFN = curr_data_provice_median,
  Median_price_diff = province_median_merged_price_diff,
  Median_price_6months_diff = provice_median_price_pr6_diff,
  Median_prices_USD = curr_data_province_median_USD,
  Median_MEB = meb_median_provincial
)

# Province mean
province_mean <- list(
  Mean_prices_AFN = curr_data_provice_mean,
  Mean_price_diff = province_mean_merged_price_diff,
  Mean_prices_USD = curr_data_province_mean_USD,
  Mean_MEB = meb_mean_provincial
)

# District median
district_median <- list(
  Median_prices_AFN = curr_data_district_median,
  Median_price_diff = district_median_merged_price_diff,
  Median_price_6months_diff = district_median_price_pr6_diff,
  Median_prices_USD = curr_data_district_median_USD,
  Median_MEB = meb_median_district
)

# District mean
district_mean <- list(
  Mean_prices_AFN = curr_data_district_mean,
  Mean_price_diff = district_mean_merged_price_diff,
  Mean_prices_USD = curr_data_district_mean_USD,
  Mean_MEB = meb_mean_district
)

#### Export Data ####
write.csv(curr_data, sprintf("input/data/recoded/AFG2002_%s_recoded5.csv", session), row.names = F)

write.xlsx(national_median, sprintf("results/median/National_Median5_%s.xlsx", session)) 
write.xlsx(regional_median, sprintf("results/median/Regional_Median5_%s.xlsx", session))
write.xlsx(province_median, sprintf("results/median/Province_Median5_%s.xlsx", session)) 
write.xlsx(district_median, sprintf("results/median/District_Median5_%s.xlsx", session)) 

write.xlsx(national_mean, sprintf("results/mean/National_mean5_%s.xlsx", session)) 
write.xlsx(regional_mean, sprintf("results/mean/Regional_mean5_%s.xlsx", session))
write.xlsx(province_mean, sprintf("results/mean/Province_mean5_%s.xlsx", session)) 
write.xlsx(district_mean, sprintf("results/mean/District_mean5_%s.xlsx", session)) 


