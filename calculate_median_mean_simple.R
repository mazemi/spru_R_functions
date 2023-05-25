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

# reading kobo tool
kobotool.choices <- read_excel("input/questionnaire/JMMI_35_final.xlsx", sheet = "choices" )

# adding admin labels
curr_data <- JMMI_admin_label(curr_data, region = "afg_region", province = "afg_prov", district = "afg_dist")

oldnames = c("afg_region", "afg_prov", "afg_dist", "region_name", "province_name", "district_name")
newnames = c("afg_region_code","afg_prov_code", "afg_dist_code", "afg_region", "afg_prov", "afg_dist")
curr_data <- curr_data %>% rename_at(vars(all_of(oldnames)), ~ all_of(newnames))

curr_data <- curr_data %>% mutate_at(vars(ends_with("_price")), as.numeric)
curr_data <- curr_data %>% mutate_at(vars(ends_with("_unit_specify")), as.numeric)

## Normalize prices
#cotton price for one square meter:
curr_data$cotton_cloth_price <- curr_data$cotton_cloth_price 
curr_data$firewood_price <- curr_data$firewood_price / curr_data$firewood_unit_specify
# Safe Water-20L Jugs
curr_data$safe_water_price <- curr_data$safe_water_price
curr_data$coal_price <- curr_data$coal_price / curr_data$coal_unit_specify
# Sanitary pad, adjusted price for one package of 10 pieces
curr_data$sanitary_pad_price <- curr_data$sanitary_pad_price / curr_data$sanitary_pad_unit_specify * 10 
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
# option: "AFN", "USD"
calculate_median <- function(df, option = "AFN"){
  if (option == "AFN") {
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
# option: "AFN", "USD"
calculate_mean <- function(input_df, option = "AFN"){
  if (option == "AFN") {
    df <- input_df %>% 
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
  } else if (option == "USD"){
    df <- input_df %>% 
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
  
  df <- df
  df[df == "NaN"] <- NA
  return(df)
}


### National ###
curr_data_national_median <- calculate_median(curr_data)
curr_data_national_median_USD <- calculate_median(curr_data, option = "USD")

curr_data_national_mean <- calculate_mean(curr_data)
curr_data_national_mean_USD <- calculate_mean(curr_data, option = "USD")

### Region ###
curr_data_region_median <- curr_data %>% group_by(afg_region)
curr_data_region_median <- calculate_median(curr_data_region_median)
curr_data_region_median_USD <- curr_data %>% group_by(afg_region)
curr_data_region_median_USD <- calculate_median(curr_data_region_median_USD, option = "USD")

curr_data_region_mean <- curr_data %>% group_by(afg_region)
curr_data_region_mean <- calculate_mean(curr_data_region_mean)
curr_data_region_mean_USD <- curr_data %>% group_by(afg_region)
curr_data_region_mean_USD <- calculate_mean(curr_data_region_mean_USD, option = "USD")

### Province ###
curr_data_provice_median <- curr_data %>% group_by(afg_prov)
curr_data_provice_median <- calculate_median(curr_data_provice_median)
curr_data_province_median_USD <- curr_data %>% group_by(afg_prov)
curr_data_province_median_USD <- calculate_median(curr_data_province_median_USD, option = "USD")

curr_data_provice_mean <- curr_data %>% group_by(afg_prov)
curr_data_provice_mean <- calculate_mean(curr_data_provice_mean)
curr_data_province_mean_USD <- curr_data %>% group_by(afg_prov)
curr_data_province_mean_USD <- calculate_mean(curr_data_province_mean_USD, option = "USD")

### District Level ###
curr_data_district_median <- curr_data %>% group_by(afg_dist)
curr_data_district_median <- calculate_median(curr_data_district_median)
curr_data_district_median_USD <- curr_data %>% group_by(afg_dist)
curr_data_district_median_USD <- calculate_median(curr_data_district_median_USD, option = "USD")

curr_data_district_mean <- curr_data %>% group_by(afg_dist)
curr_data_district_mean <- calculate_mean(curr_data_district_mean)
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
  result[result == "NaN"] <- NA
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

# National median
national_median <- list(
  Median_prices_AFN = curr_data_national_median,
  Median_prices_USD = curr_data_national_median_USD,
  Median_MEB = meb_median_national
)

# National mean
national_mean <- list(
  Mean_prices_AFN = curr_data_national_mean,
  Mean_prices_USD = curr_data_national_mean_USD,
  Mean_MEB = meb_mean_national
)

# Regional median
regional_median <- list(
  Median_prices_AFN = curr_data_region_median,
  Median_prices_USD = curr_data_region_median_USD,
  Median_MEB = meb_median_regional
)

# Regional mean
regional_mean <- list(
  Mean_prices_AFN = curr_data_region_mean,
  Mean_prices_USD = curr_data_region_mean_USD,
  Mean_MEB = meb_mean_regional
)

# Province median
province_median <- list(
  Median_prices_AFN = curr_data_provice_median,
  Median_prices_USD = curr_data_province_median_USD,
  Median_MEB = meb_median_provincial
)

# Province mean
province_mean <- list(
  Mean_prices_AFN = curr_data_provice_mean,
  Mean_prices_USD = curr_data_province_mean_USD,
  Mean_MEB = meb_mean_provincial
)

# District median
district_median <- list(
  Median_prices_AFN = curr_data_district_median,
  Median_prices_USD = curr_data_district_median_USD,
  Median_MEB = meb_median_district
)

# District mean
district_mean <- list(
  Mean_prices_AFN = curr_data_district_mean,
  Mean_prices_USD = curr_data_district_mean_USD,
  Mean_MEB = meb_mean_district
)

#### Export Data ####
write.csv(curr_data, sprintf("input/data/recoded/AFG2002_%s_recoded6.csv", session), row.names = F)

write.xlsx(national_median, sprintf("results/median/National_Median6_%s.xlsx", session)) 
write.xlsx(regional_median, sprintf("results/median/Regional_Median6_%s.xlsx", session))
write.xlsx(province_median, sprintf("results/median/Province_Median6_%s.xlsx", session)) 
write.xlsx(district_median, sprintf("results/median/District_Median6_%s.xlsx", session)) 

write.xlsx(national_mean, sprintf("results/mean/National_mean6_%s.xlsx", session)) 
write.xlsx(regional_mean, sprintf("results/mean/Regional_mean6_%s.xlsx", session))
write.xlsx(province_mean, sprintf("results/mean/Province_mean6_%s.xlsx", session)) 
write.xlsx(district_mean, sprintf("results/mean/District_mean6_%s.xlsx", session)) 


