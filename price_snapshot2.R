library(purrr)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(lubridate)

# extract national level prices from longitudinal prices
price_df = read.csv("output/csv/longitudinal_prices.csv", na.strings='NA') %>% filter(level == "National")

last_round <- max(price_df$round)
previous_round <- last_round - 1
six_previous_round <- last_round - 6

remark <- c("last","previous","six_previous")

# extract only last round, previous round and six round ago
price_df <- price_df %>% 
  filter(round %in% c(
    last_round,
    previous_round, 
    six_previous_round
    )
  )

item_list <- c(
  "wheat_local",
  "wheat_imported",
  "local_rice",
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

price_df <- price_df %>% select(round, contains(item_list),
                                -c("AFN_safe_water_price_include_jerrycan_cr_median",
                                   "USD_safe_water_price_include_jerrycan_cr_median"))

price_df <- price_df %>% mutate(
  remark = case_when(
    round == last_round ~ "last",
    round == previous_round ~ "previous",
    TRUE ~ "six_previous"
  )
) 

# this function remove prefix and suffix from column name and add the currency_month field
clean_name <- function(df, currency_label){
  colnames(df) <- sub("_price.*", "", colnames(df))
  colnames(df) <- sub(paste(currency_label,"_", sep = ""), "", colnames(df))
  df$title <- paste(df$remark, "_", currency_label, sep = "")
  return(df)
}

afn_df <- price_df %>% select(remark, starts_with("AFN"))
afn_df <- clean_name(afn_df,"AFN")

usd_df <- price_df %>% select(remark, starts_with("USD"))
usd_df <- clean_name(usd_df, "USD")

merge_price <- rbind(usd_df, afn_df)

#transpose data frame
merge_price_t <- transpose(merge_price)

rownames(merge_price_t) <- colnames(merge_price)
colnames(merge_price_t) <- merge_price_t["title", ]

# remove "title" row
merge_price_t <- merge_price_t[!(row.names(merge_price_t) %in% c("remark", "title")), ] 

# convert character to numeric value
merge_price_t[] <- lapply(merge_price_t, function(x) as.numeric(as.character(x)))

# function for rounding all numeric fields 
rounding <- function(df, digits) {
  numeric_columns <- sapply(df, mode) == 'numeric'
  df[numeric_columns] <-  round(df[numeric_columns], digits)
  df
}

merge_price_t <- rounding(merge_price_t, digits = 2)

# add items column 
merge_price_t$item <- rownames(merge_price_t)

merge_price_final <- merge_price_t %>% select(item, last_USD, last_AFN, starts_with("previous"), everything())

rownames(merge_price_final) <- NULL

# calculate the percentage of price changes:
calculate_change <- function( col){
  col <- deparse(substitute(col))
  if(str_sub(col, start= -3) == "USD"){
    result = round((merge_price_final$last_USD - merge_price_final[[col]]) / merge_price_final[[col]],2) * 100
    return(result)
  }else{
    result = round((merge_price_final$last_AFN - merge_price_final[[col]]) / merge_price_final[[col]],2) * 100
    return(result)
  }
}

merge_price_final <- merge_price_final %>% mutate(
  monthly_change_USD = calculate_change(previous_USD),
  monthly_change_AFN = calculate_change(previous_AFN),
  six_month_change_USD = calculate_change(six_previous_USD),
  six_month_change_AFN = calculate_change(six_previous_AFN)
)

merge_price_final <- merge_price_final %>% select(item, last_USD, last_AFN, starts_with("monthly"), starts_with("six_month_change"))

# friendly item names:
merge_price_final$item <- gsub("_", " ", merge_price_final$item)
merge_price_final$item <- str_to_title(merge_price_final$item)

column_name <- c("Item", "Price (USD)", "Price (AFN)", "Monthly change AFN", "Monthly change USD", "6 Month change AFN", "6 Month change USD")
colnames(merge_price_final) <- column_name

write.csv(merge_price_final, "./output/items_price_changes_overview.csv", row.names = TRUE)

