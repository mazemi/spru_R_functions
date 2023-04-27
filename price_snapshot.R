library(purrr)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(lubridate)

# extract national level prices from longitudinal prices
price_df = read.csv("output/csv/longitudinal_prices - Copy.csv", na.strings='NA') %>% filter(level == "National")

price_df$date <- as.Date(price_df$date ,format="%m/%d/%Y")
# price_df$date <- as.Date(price_df$date ,format="%Y-%m-%d")

last_month <- max(price_df$date)
previous_month <- floor_date(last_month - months(1), "month")
six_previous_month <- floor_date(last_month - months(6), "month")

dates_df <- data.frame(date = c(last_month, previous_month, six_previous_month),
                        remark= c("last","previous","six_previous"))

# extract only last month, previous month and six months ago
price_df <- price_df %>% 
  filter(date %in% c(
    max(date),
    previous_month, 
    six_previous_month
    )
  )

price_df <- dates_df %>% inner_join(price_df)

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

price_df <- price_df %>% select(remark, contains(item_list),
                                -c("AFN_safe_water_price_include_jerrycan_cr_median",
                                   "USD_safe_water_price_include_jerrycan_cr_median"))
                                
# this function remove prefix and suffix from column name and add the currency_month field
clean_name <- function(df, currency_label){
  for ( col in 1:ncol(df)){
    colnames(df)[col] <-  sub("_price.*", "", colnames(df)[col])
    colnames(df)[col] <-  sub(paste(currency_label,"_", sep = ""), "", colnames(df)[col])
  }
  df$title <- paste(df$remark, "_", currency_label, sep = "")
  return(df)
}

afn_df <- price_df %>% select(remark, starts_with("AFN"))
afn_df <- clean_name(afn_df,"AFN")

usd_df <- price_df %>% select(remark, starts_with("USD"))
usd_df <- clean_name(usd_df, "USD")

merge_price <- rbind(usd_df, afn_df)
merge_price <- merge_price %>% select(-remark)

#transpose data frame
merge_price_t <- transpose(merge_price)

rownames(merge_price_t) <- colnames(merge_price)
colnames(merge_price_t) <- merge_price_t["title",]

# remove "title" row
merge_price_t <- merge_price_t[!(row.names(merge_price_t) %in% "title"), ] 

# convert character to numeric value
merge_price_t[] <- lapply(merge_price_t, function(x) as.numeric(as.character(x)))

# function for rounding all numeric fields 
rounding <- function(df, digits) {
  numeric_columns <- sapply(df, mode) == 'numeric'
  df[numeric_columns] <-  round(df[numeric_columns], digits)
  df
}

merge_price_t <- rounding(merge_price_t, digits = 2)

# add row number sequence
merge_price_t <- merge_price_t %>% mutate(id = row_number())

# add items column 
merge_price_t$item <- rownames(merge_price_t)

# add null value if previous data is not available
if(!"previous_USD" %in% colnames(merge_price_t)){
  merge_price_t$previous_USD <- NA_real_
  merge_price_t$previous_AFN <- NA_real_
}

# add null value if six month ago data is not available
if(!"six_previous_USD" %in% colnames(merge_price_t)){
  merge_price_t$previous_USD <- NA_real_
  merge_price_t$previous_AFN <- NA_real_
}

merge_price_final <- merge_price_t %>% select(id, item, last_USD, last_AFN, starts_with("previous"), everything())

rownames(merge_price_final) <- NULL

calculate_change <- function( col){
  col <- deparse(substitute(col))

  if(str_sub(col, start= -3) == "USD"){
     tryCatch(
        {
          result = round((merge_price_final$last_USD - merge_price_final[[col]]) / merge_price_final[[col]],2)
          return(result)
        },

        error=function(e) {
          return(NA_real_)
        }
      )
  }else{
    tryCatch(
      {
        result = round((merge_price_final$last_AFN - merge_price_final[[col]]) / merge_price_final[[col]],2)
        return(result)
      },
      
      error=function(e) {
        return(NA_real_)
      }
    )
  }
}

merge_price_final <- merge_price_final %>% mutate(
  monthly_change_USD = calculate_change(previous_USD),
  monthly_change_AFN = calculate_change(previous_AFN),
  six_month_change_USD = calculate_change(six_previous_USD),
  six_month_change_AFN = calculate_change(six_previous_AFN)
)

merge_price_final <- merge_price_final %>% select(id, item, last_USD, last_AFN, starts_with("monthly"), starts_with("six_month_change"))





