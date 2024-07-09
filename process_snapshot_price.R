library(purrr)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)

# core function for calculating price snapshot
generate_snapshot <- function(){
  # extract national level prices from longitudinal prices
  price_df = read.csv("output/csv/longitudinal_prices.csv", na.strings='NA') %>% 
    filter(level == "National")
  
  # We only require data from the most recent round, as well as from three and six months ago.
  last_round <-max(price_df$round)
  price_df <- price_df %>% filter(round %in% c(last_round, last_round - 1, last_round - 6))
  price_df <- price_df %>% arrange(round)
  
  item_list <- c(
    "wheat_local",
    "wheat_imported",
    "wheat",
    "_rice",
    "veg_oil",
    "pulses_merged_price_final",
    "salt",
    "sugar",
    "tomatoes",
    "pen",
    "notebook",
    "rubber",
    "cotton_cloth",
    "toothbrush_adult",
    "toothpaste",
    "sanitary_pad",
    "soap",
    "coal",
    "safe_water",
    "firewood",
    "lpg",
    "diesel",
    "petrol",
    "winter_jacket",
    "water_container",
    "cooking_pot",
    "blanket"
  )
  
  price_df <- price_df %>% select( 
    round,
    contains(item_list),
    -c(
      "AFN_safe_water_price_include_jerrycan_cr_median",
      "USD_safe_water_price_include_jerrycan_cr_median"
    )
  )
  
  t_price <- as.data.frame(t(price_df))
  colnames(t_price) <- c("six", "one", "last")
  
  # calculation of changes in percentage
  t_price$perc_change1 <- ((t_price$last - t_price$one) / t_price$one) * 100
  t_price$perc_change6 <- ((t_price$last - t_price$six) / t_price$six) * 100
  
  t_price <- t_price %>% mutate_if(is.numeric, ~ round(., 2))
  t_price <- t_price %>% select(-c(one, six))
  t_price$item <- rownames(t_price)
  t_price <- t_price %>% filter(item != "round")
  rownames(t_price) <- NULL
  
  # remove "l_price_cr_median" from the data
  t_price$item <- gsub("_price_cr_median", "", t_price$item)
  
  # divide USD and AFN data
  usd_data <- t_price %>% filter(str_starts(item, "USD"))
  afn_data <- t_price %>% filter(str_starts(item, "AFN"))
  
  usd_data$item <- gsub("USD_", "", usd_data$item)
  colnames(usd_data) <- paste0("usd_", colnames(usd_data))
  afn_data$item <- gsub("AFN_", "", afn_data$item)
  colnames(afn_data) <- paste0("afn_", colnames(afn_data))
  
  merged_price <- usd_data %>% inner_join(afn_data, by = c("usd_item"="afn_item"))
  merged_price <- merged_price %>% select(usd_item, usd_last, afn_last, 
                                          afn_perc_change1, usd_perc_change1, 
                                          afn_perc_change6, usd_perc_change6)
  
  # convert item column to a user friendly values
  merged_price$usd_item <- gsub("pulses_merged_price_final_cr_median", "pulses_merged", merged_price$usd_item)
  merged_price <- merged_price %>% mutate(usd_item = str_to_title(str_replace_all(usd_item, "_", " ")))
  
  # final column name in the output
  column_name <- c("Item", "Price (USD)", "Price (AFN)", "Monthly change AFN", 
                   "Monthly change USD", "6 Month change AFN", "6 Month change USD")
  
  colnames(merged_price) <- column_name
  write.csv(merged_price, "./output/items_price_changes_overview.csv", row.names = TRUE)
  
  cat("Price snapshot dataset has been generated.  ", "\n")
}





