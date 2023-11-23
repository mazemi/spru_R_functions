library(readxl)
library(survey)
library(openxlsx)
library(tidyr)
library(dplyr)
library(tidyverse)

source("./R/functions/JMMI_admin_label.R")
`%notin%` <- Negate(`%in%`)

usd_rate <- 74.46 # based on UN treasury at 25-10-2023
session <- "Herat_EQ_R2"

# fixed price items - AFN
# updated on 17Th May 2023, base on the MEB revision on the April 2023
Healthcare_basket <- 49.39 * usd_rate
Shelter_basket <- 34 * usd_rate
Education_basket <- 4.86 * usd_rate
Fuel_Electricity_basket <- 18 * usd_rate
Communication_basket <- 1.94 * usd_rate
Transportation_basket <- 6.82 * usd_rate
Under_wear <- 0.57 * usd_rate

curr_data <- read_excel("input/cleaned_data/clean_data.xlsx",na = c("", "NA", "N/A")) %>% type.convert(as.is = T)
kobotool.choices <- read_excel("input/tool/Herat_Earthquake_Market_Assessment_tool_v8_sv.xlsx", sheet = "choices")

# adding admin labels
curr_data <- JMMI_admin_label(curr_data, district = "afg_dist")

oldnames <- c("afg_dist", "district_name")
newnames <- c("afg_dist_code", "afg_dist")
curr_data <- curr_data %>% rename_at(vars(all_of(oldnames)), ~ all_of(newnames))

curr_data <- curr_data %>% mutate_at(vars(ends_with("_price")), as.numeric)
curr_data <- curr_data %>% mutate_at(vars(ends_with("_unit_specify")), as.numeric)

## Normalize prices
unit_values <- colnames(curr_data)

# getting only items in the vector that ends with "unit_specify"
unit_values <- unit_values[grepl("unit_specify$", unit_values)]
price_values <- sub("unit_specify", "price", unit_values)
curr_data[price_values] <- curr_data[price_values] / curr_data[unit_values]

# adjusted price for "one package" of 10 pieces of the sanitary pad
curr_data$sanitary_pad_price <- curr_data$sanitary_pad_price * 10

# add a merged price of all pulses
curr_data <- curr_data %>%
  rowwise() %>%
  mutate(pulses_merged_price_final = median(
    c(
      pulses_beans_price,
      pulses_lentils_price,
      pulses_split_peas_price,
      na.rm = TRUE
    )
  )) %>%
  ungroup()

## convert AFN prices to USD
afn_price_values <- colnames(curr_data)
afn_price_values <- afn_price_values[grepl("price$", afn_price_values)]
usd_price_values <- sub("price", "price_USD", afn_price_values)
curr_data[usd_price_values] <- curr_data[afn_price_values] / usd_rate
curr_data$pulses_merged_price_final_USD <- curr_data$pulses_merged_price_final / usd_rate

# items stock and restock
# Stock and restock of some items is not included in toot, calculation is done based on similar items
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
curr_data$re_stock_sanitary_pad <- curr_data$re_stock_soap
curr_data$notebook_stock_last <- curr_data$pen_stock_last
curr_data$re_stock_notebook <- curr_data$re_stock_pen
curr_data$rubber_stock_last <- curr_data$pen_stock_last
curr_data$re_stock_rubber <- curr_data$re_stock_pen

curr_data <- curr_data %>% mutate(
  financial_services_sum = select(., starts_with("financial_services.")) %>% rowSums(),
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

############ MEDIAN Calculations #################
# This function calculate the median of prices
calculate_median <- function(df, option = "AFN") {
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
        diesel_price_cr_median = median(diesel_price, na.rm = T),
        petrol_price_cr_median = median(petrol_price, na.rm = T),
        safe_water_price_cr_median = median(safe_water_price, na.rm = T),
        firewood_price_cr_median = median(firewood_price, na.rm = T),
        pulses_lentils_price_cr_median = median(pulses_lentils_price, na.rm = T),
        pulses_beans_price_cr_median = median(pulses_beans_price, na.rm = T),
        pulses_split_peas_price_cr_median = median(pulses_split_peas_price, na.rm = T),

        # WINTERIZATION
        winter_jacket_price_cr_median = median(winter_jacket_price, na.rm = T),
        water_container_price_cr_median = median(water_container_price, na.rm = T),
        cooking_pot_price_cr_median = median(cooking_pot_price, na.rm = T),
        blanket_price_cr_median = median(blanket_price, na.rm = T),

        # CONSTRUCTION
        tent_price_cr_median = median(tent_price, na.rm = T),
        tarpaulin_price_cr_median = median(tarpaulin_price, na.rm = T),
        rope_price_cr_median = median(rope_price, na.rm = T),
        shovel_price_cr_median = median(shovel_price, na.rm = T),
        plastic_sheeting_price_cr_median = median(plastic_sheeting_price, na.rm = T),
        assorted_nails_price_cr_median = median(assorted_nails_price, na.rm = T),

        # USD buy and sell rate
        buy_rate_cr_median = median(buy_rate, na.rm = T),
        sell_rate_cr_median = median(sell_rate, na.rm = T),
      )
  } else if (option == "USD") {
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
        coal_price_cr_median = median(coal_price_USD, na.rm = T),
        safe_water_price_cr_median = median(safe_water_price_USD, na.rm = T),
        firewood_price_cr_median = median(firewood_price_USD, na.rm = T),
        lpg_price_cr_median = median(lpg_price_USD, na.rm = T),
        diesel_price_cr_median = median(diesel_price_USD, na.rm = T),
        petrol_price_cr_median = median(petrol_price_USD, na.rm = T),
        safe_water_price_cr_median = median(safe_water_price_USD, na.rm = T),
        firewood_price_cr_median = median(firewood_price_USD, na.rm = T),
        pulses_lentils_price_cr_median = median(pulses_lentils_price_USD, na.rm = T),
        pulses_beans_price_cr_median = median(pulses_beans_price_USD, na.rm = T),
        pulses_split_peas_price_cr_median = median(pulses_split_peas_price_USD, na.rm = T),

        # WINTERIZATION
        winter_jacket_price_cr_median = median(winter_jacket_price_USD, na.rm = T),
        water_container_price_cr_median = median(water_container_price_USD, na.rm = T),
        cooking_pot_price_cr_median = median(cooking_pot_price_USD, na.rm = T),
        blanket_price_cr_median = median(blanket_price_USD, na.rm = T),

        # CONSTRUCTION
        tent_price_cr_median = median(tent_price_USD, na.rm = T),
        tarpaulin_price_cr_median = median(tarpaulin_price_USD, na.rm = T),
        rope_price_cr_median = median(rope_price_USD, na.rm = T),
        shovel_price_cr_median = median(shovel_price_USD, na.rm = T),
        plastic_sheeting_price_cr_median = median(plastic_sheeting_price_USD, na.rm = T),
        assorted_nails_price_cr_median = median(assorted_nails_price_USD, na.rm = T)
      )
  }
}

# This function calculate the mean of prices
calculate_mean <- function(input_df, option = "AFN") {
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
        diesel_price_cr_mean = mean(diesel_price, na.rm = T),
        petrol_price_cr_mean = mean(petrol_price, na.rm = T),
        safe_water_price_cr_mean = mean(safe_water_price, na.rm = T),
        firewood_price_cr_mean = mean(firewood_price, na.rm = T),
        pulses_lentils_price_cr_mean = mean(pulses_lentils_price, na.rm = T),
        pulses_beans_price_cr_mean = mean(pulses_beans_price, na.rm = T),
        pulses_split_peas_price_cr_mean = mean(pulses_split_peas_price, na.rm = T),

        # WINTERIZATION
        winter_jacket_price_cr_mean = mean(winter_jacket_price, na.rm = T),
        water_container_price_cr_mean = mean(water_container_price, na.rm = T),
        cooking_pot_price_cr_mean = mean(cooking_pot_price, na.rm = T),
        blanket_price_cr_mean = mean(blanket_price, na.rm = T),

        # CONSTRUCTION
        tent_price_cr_median = mean(tent_price, na.rm = T),
        tarpaulin_price_cr_median = mean(tarpaulin_price, na.rm = T),
        rope_price_cr_median = mean(rope_price, na.rm = T),
        shovel_price_cr_median = mean(shovel_price, na.rm = T),
        plastic_sheeting_price_cr_median = mean(plastic_sheeting_price, na.rm = T),
        assorted_nails_price_cr_median = mean(assorted_nails_price, na.rm = T),

        # USD buy and sell rate
        buy_rate_cr_mean = mean(buy_rate, na.rm = T),
        sell_rate_cr_mean = mean(sell_rate, na.rm = T),
      )
  } else if (option == "USD") {
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
        coal_price_cr_mean = mean(coal_price_USD, na.rm = T),
        safe_water_price_cr_mean = mean(safe_water_price_USD, na.rm = T),
        firewood_price_cr_mean = mean(firewood_price_USD, na.rm = T),
        lpg_price_cr_mean = mean(lpg_price_USD, na.rm = T),
        diesel_price_cr_mean = mean(diesel_price_USD, na.rm = T),
        petrol_price_cr_mean = mean(petrol_price_USD, na.rm = T),
        safe_water_price_cr_mean = mean(safe_water_price_USD, na.rm = T),
        firewood_price_cr_mean = mean(firewood_price_USD, na.rm = T),
        pulses_lentils_price_cr_mean = mean(pulses_lentils_price_USD, na.rm = T),
        pulses_beans_price_cr_mean = mean(pulses_beans_price_USD, na.rm = T),
        pulses_split_peas_price_cr_mean = mean(pulses_split_peas_price_USD, na.rm = T),

        # WINTERIZATION
        winter_jacket_price_cr_mean = mean(winter_jacket_price_USD, na.rm = T),
        water_container_price_cr_mean = mean(water_container_price_USD, na.rm = T),
        cooking_pot_price_cr_mean = mean(cooking_pot_price_USD, na.rm = T),
        blanket_price_cr_mean = mean(blanket_price_USD, na.rm = T),

        # CONSTRUCTION
        tent_price_cr_median = mean(tent_price_USD, na.rm = T),
        tarpaulin_price_cr_median = mean(tarpaulin_price_USD, na.rm = T),
        rope_price_cr_median = mean(rope_price_USD, na.rm = T),
        shovel_price_cr_median = mean(shovel_price_USD, na.rm = T),
        plastic_sheeting_price_cr_median = mean(plastic_sheeting_price_USD, na.rm = T),
        assorted_nails_price_cr_median = mean(assorted_nails_price_USD, na.rm = T)
      )
  }

  df <- df
  df[df == "NaN"] <- NA
  return(df)
}

### Province ###
curr_data$afg_prov <- "Herat"
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
calculate_meb <- function(input_df, group, measurement) {
  if (!missing(group)) {
    group_list <- as.list(substitute(group))
    if (length(group_list) > 1) group_list <- group_list[-1]
    groups <- purrr::map_chr(group_list, rlang::as_string)

    result <- input_df %>% group_by(across(all_of(groups)))
  } else {
    result <- input_df
  }

  if (measurement == "median") {
    result <- result %>%
      summarise(
        # food items:
        wheat = median(as.numeric(wheat_imported_price), na.rm = TRUE) * 89,
        rice = median(as.numeric(local_rice_price), na.rm = TRUE) * 21,
        veg_oil = median(as.numeric(veg_oil_price), na.rm = TRUE) * 7,
        salt = median(as.numeric(salt_price), na.rm = TRUE) * 1,
        pulses = median(as.numeric(pulses_merged_price_final), na.rm = TRUE) * 9,

        # WASH items:
        soap = median(as.numeric(soap_price), na.rm = TRUE) * 23,
        sanitary_pad = median(as.numeric(sanitary_pad_price), na.rm = TRUE) * 4,
        cotton = median(as.numeric(cotton_cloth_price), na.rm = TRUE) * 4,
        toothpaste = median(as.numeric(toothpaste_price), na.rm = TRUE) * 2,
        safe_water = median(as.numeric(safe_water_price), na.rm = TRUE) * 5.83,
        under_wear = Under_wear * 6
      )
  } else if (measurement == "mean") {
    result <- result %>%
      summarise(
        # food items:
        wheat = mean(as.numeric(wheat_imported_price), na.rm = TRUE) * 89,
        rice = mean(as.numeric(local_rice_price), na.rm = TRUE) * 21,
        veg_oil = mean(as.numeric(veg_oil_price), na.rm = TRUE) * 7,
        salt = mean(as.numeric(salt_price), na.rm = TRUE) * 1,
        pulses = mean(as.numeric(pulses_merged_price_final), na.rm = TRUE) * 9,

        # WASH items:
        soap = mean(as.numeric(soap_price), na.rm = TRUE) * 23,
        sanitary_pad = mean(as.numeric(sanitary_pad_price), na.rm = TRUE) * 4,
        cotton = mean(as.numeric(cotton_cloth_price), na.rm = TRUE) * 4,
        toothpaste = mean(as.numeric(toothpaste_price), na.rm = TRUE) * 2,
        safe_water = mean(as.numeric(safe_water_price), na.rm = TRUE) * 5.83,
        under_wear = Under_wear * 6
      )
  }
  result <- result %>%
    mutate(
      # calculation of food basket and WASH basket:
      food_basket_AFN = wheat + rice + veg_oil + salt + pulses,
      Wash_hygiene_AFN = cotton + toothpaste + sanitary_pad + soap + safe_water + under_wear,

      # fixed values in the MEB
      Healthcare_AFN = Healthcare_basket,
      shelter_AFN = Shelter_basket,
      Fule_Electricity_AFN = Fuel_Electricity_basket,
      Education_AFN = Education_basket,
      Communication_AFN = Communication_basket,
      Transportation_AFN = Transportation_basket,

      # extra ten percent of all baskets:
      ten_percent_unmet_AFN = (food_basket_AFN + Wash_hygiene_AFN + Healthcare_basket + Shelter_basket +
        Fule_Electricity_AFN + Education_basket + Communication_basket + Transportation_basket) * 0.1,

      # final MEB:
      meb_afn = food_basket_AFN + Wash_hygiene_AFN + Healthcare_basket + Shelter_basket +
        Fuel_Electricity_basket + Education_basket + Communication_basket + Transportation_basket + ten_percent_unmet_AFN,

      # USD
      food_basket_USD = food_basket_AFN / usd_rate,
      Wash_hygiene_USD = Wash_hygiene_AFN / usd_rate,
      Healthcare_USD = Healthcare_AFN / usd_rate,
      shelter_USD = shelter_AFN / usd_rate,
      Fule_Electricity_USD = Fule_Electricity_AFN / usd_rate,
      Education_USD = Education_AFN / usd_rate,
      Communication_USD = Communication_AFN / usd_rate,
      Transportation_USD = Transportation_AFN / usd_rate,
      ten_percent_unmet_USD = ten_percent_unmet_AFN / usd_rate,
      meb_usd = meb_afn / usd_rate
    )

  # only JMMI-related values:
  result <- result %>% select(starts_with("afg"), ends_with("AFN"), ends_with("USD"))
  result[result == "NaN"] <- NA
  return(result)
}

meb_median_provincial <- calculate_meb(curr_data, afg_prov, measurement = "median")
meb_median_district <- calculate_meb(curr_data, afg_dist, measurement = "median")

meb_mean_provincial <- calculate_meb(curr_data, afg_prov, measurement = "mean")
meb_mean_district <- calculate_meb(curr_data, afg_dist, measurement = "mean")

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
write.xlsx(province_median, sprintf("results/median/Province_Median_%s.xlsx", session))
write.xlsx(district_median, sprintf("results/median/District_Median_%s.xlsx", session))
write.xlsx(province_mean, sprintf("results/mean/Province_mean_%s.xlsx", session))
write.xlsx(district_mean, sprintf("results/mean/District_mean_%s.xlsx", session))

