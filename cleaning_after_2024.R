library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# the aim of this script is create a time series of JMMI data after 2024 with in a standard format
df <- read.xlsx("./data/after_2024_messy.xlsx")

# merging local_rice and rice columns
df$items_available_marketplace_rice <- ifelse(
  !is.na(df$items_available_marketplace_local_rice),
  df$items_available_marketplace_local_rice,
  df$items_available_marketplace_rice
)
df$items_available_food.rice <- ifelse(!is.na(df$items_available_food.local_rice), df$items_available_food.local_rice, df$items_available_food.rice)
df$rice_unit_specify         <- ifelse(!is.na(df$local_rice_unit_specify), df$local_rice_unit_specify, df$rice_unit_specify)
df$rice_price                <- ifelse(!is.na(df$local_rice_price), df$local_rice_price, df$rice_price)
df$rice_stock_last           <- ifelse(!is.na(df$local_rice_stock_last), df$local_rice_stock_last, df$rice_stock_last)
df$re_stock_rice             <- ifelse(!is.na(df$re_stock_local_rice), df$re_stock_local_rice, df$re_stock_rice)

df$items_available_food <- gsub("local_rice", "rice", df$items_available_food)

# un necessary column like prices in packages and their units (we need only price of items per unit) 
un_necessary_cols <- c(
  "interview_duration",
  "time_validity",
  "CHECK_traders_open_change",
  "CHECK_duration",
  "CHECK_coping_income",
  "items_available_marketplace_local_rice",
  "diff_stock_restock_local_rice",
  "CHECK_monthly_income_change",
  "CHECK_financial_access",
  "selected_items_count",
  "enumerator_note",
  "items_available_food.local_rice",
  "local_rice_unit_specify",
  "local_rice_price",
  "local_rice_stock_last",
  "re_stock_local_rice",
  "veg_oil_unit_specify",
  "veg_oil_price",
  "pulses_lentils_unit_specify",
  "pulses_lentils_price",
  "pulses_beans_unit_specify",
  "pulses_beans_price",
  "pulses_split_peas_unit_specify",
  "pulses_split_peas_price",
  "salt_unit_specify",
  "salt_price",
  "sanitary_pad_unit_specify",
  "sanitary_pad_price",
  "firewood_unit_specify",
  "firewood_price",
  "coal_unit_specify",
  "coal_price",
  "wheat_unit_specify",
  "wheat_price",
  "rice_unit_specify",
  "rice_price",
  "rice_calculation",
  "_index"
)

# data with no empty columns
df <- df %>% select(-all_of(un_necessary_cols))

# rename various price labels into a standard format:
old_names <- c(
  "wheat_calculation",
  "rice_price_calculation",
  "pulses_lentils_calculation",
  "pulses_beans_calculation",
  "pulses_split_peas_calculation",
  "veg_oil_calculation",
  "sugar_price",
  "salt_calculation",
  "cotton_cloth_price",
  "toothbrush_adult_price",
  "toothpaste_price",
  "soap_price",
  "sanitary_pad_calculation",
  "pen_price",
  "notebook_price",
  "safe_water_price",
  "lpg_price",
  "diesel_price",
  "petrol_price",
  "cooking_pot_price",
  "water_container_price",
  "firewood_calculation",
  "coal_calculation",
  "blanket_price",
  "winter_jacket_price"
)

new_names <- c(
  "wheat_unit_price",
  "rice_unit_price",
  "pulses_lentils_unit_price",
  "pulses_beans_unit_price",
  "pulses_split_peas_unit_price",
  "veg_oil_unit_price",
  "sugar_unit_price",
  "salt_unit_price",
  "cotton_cloth_unit_price",
  "toothbrush_adult_unit_price",
  "toothpaste_unit_price",
  "soap_unit_price",
  "sanitary_pad_unit_price",
  "pen_unit_price",
  "notebook_unit_price",
  "safe_water_unit_price",
  "lpg_unit_price",
  "diesel_unit_price",
  "petrol_unit_price",
  "cooking_pot_unit_price",
  "water_container_unit_price",
  "firewood_unit_price",
  "coal_unit_price",
  "blanket_unit_price",
  "winter_jacket_unit_price"
)
# Apply new names
names(df)[match(old_names, names(df))] <- new_names

# convert the unit prices to numeric values.
df <- df %>% mutate(across(all_of(new_names), ~ round(as.numeric(.), 2)))

# Reorder columns of unit prices
cols <- names(df)
insert_after <- which(cols == "items_available_nfi.none")
remaining <- setdiff(cols, new_names)
new_order <- append(remaining, new_names, after = insert_after)

# Apply new order
df <- df[, new_order]

# Reorder combined data frame based on our recent JMMI data set columns
r57 <- read.xlsx("./data/data_R57.xlsx")

extra_cols <- setdiff(names(df), names(r57))
extra_df <- df %>% select(all_of(extra_cols),"_uuid")
df <- df[, names(r57)[names(r57) %in% names(df)]]
df <- inner_join(extra_df, df, by= "_uuid")
df <- df[, c(setdiff(names(df), "_uuid"), "_uuid")]

write.xlsx(df, "./data/final_after_2024.xlsx", overwrite = T)



