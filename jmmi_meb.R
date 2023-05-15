library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# MEB fixed values in AFN (based on US exchange rate 89.11):
Healthcare_basket =	4401.1429
Shelter_basket	= 3020.829
Fule_Electricity_basket =	989.121
Education_basket	= 980.21
Communication_basket	= 312.7761
Transportation_basket	= 760.1083

# load clean data:
df <- read.csv("./AFG2002_JMMI_R32_FEB23_recoded.csv")
df <- df %>% select(afg_prov_code,afg_dist_code,afg_prov,afg_dist,trader_type, ends_with("price"), pulses_merged_price_final) %>% filter(afg_prov %in% c("Balkh","Ghazni") )

# simple function for data aggregation:
calculation_fun <- function(input_df, group){
  group_list <- as.list(substitute(group))
  if(length(group_list)> 1) group_list <- group_list[-1]
  groups <- purrr::map_chr(group_list, rlang::as_string)
  
  result <- input_df %>% group_by(across(all_of(groups))) %>% 
    summarise(
      # food items: 
      wheat= median(as.numeric(wheat_imported_price) , na.rm = TRUE) * 89,
      rice= median(as.numeric(local_rice_price) , na.rm = TRUE) * 21,
      veg_oil= median(as.numeric(veg_oil_price) , na.rm = TRUE) * 7,
      salt= median(as.numeric(salt_price) , na.rm = TRUE) * 1,
      pulses= median(as.numeric(pulses_merged_price_final) , na.rm = TRUE) * 9,
      
      # WASH items: 
      cotton= median(as.numeric(cotton_cloth_price) , na.rm = TRUE) * 2,
      toothpaste= median(as.numeric(toothpaste_price) , na.rm = TRUE) * 2,
      sanitary_pad= median(as.numeric(sanitary_pad_price) , na.rm = TRUE) * 2,
      soap= median(as.numeric(soap_price) , na.rm = TRUE) * 14,
      safe_water= median(as.numeric(safe_water_price) , na.rm = TRUE) * 5.83,
      under_wear = 213.864, # fixed value
      
      # calculation of food basket and WASH basket:
      Food_basket = wheat + rice + veg_oil + salt + pulses,
      WASH_basket = cotton + toothpaste + sanitary_pad + soap + safe_water + under_wear,
      
      # extra ten percent of all baskets:
      Ten_percent = (Food_basket + WASH_basket + Healthcare_basket + Shelter_basket + 
                       Fule_Electricity_basket + Education_basket + Communication_basket + Transportation_basket) * .1, 
      
      # final MEB:
      MEB = Food_basket + WASH_basket + Healthcare_basket + Shelter_basket + 
        Fule_Electricity_basket + Education_basket + Communication_basket + Transportation_basket + Ten_percent
    ) 
  
  # only JMMI-related values:
  result <- result %>% select(starts_with("afg"), Food_basket, WASH_basket, Ten_percent ,MEB)
  
  return(result)
}

province_level_MEB <- calculation_fun(df, afg_prov)
district_level_MEB <- calculation_fun(df, c(afg_prov, afg_dist))

                    