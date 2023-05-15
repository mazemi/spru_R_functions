library(openxlsx)
library(readxl)
library(stringr)
library(dplyr)


df <- read_xlsx("./data/AFG2002_JMMI_Round34.xlsx")


################ check empty columns in the JMMI data ################ 

empty_cols <- sapply(df, function(x) all(is.na(x)))

# list of all empty columns:
empty_cols_names <- names(df)[empty_cols]

# list of empty columns that are OK
ignore_cols <- "note|other|thankyou|_validation_status|_tags"

has_issue <- empty_cols_names[grep(ignore_cols, empty_cols_names, invert = TRUE)]

# show warning massage if there is any issue:
if (length(has_issue)>0) {
  cat("CHECK THIS COLUMNS:", sep = "\n")
  for (i in has_issue) {
    cat(i, sep = "\n")
  }
}


################ check outliers in the JMMI data ################ 

# list of desired numeric columns:
var_list <- c(
  "wheat_local_unit_specify",
  "wheat_local_price",
  "re_stock_wheat_local",
  "wheat_imported_unit_specify",
  "wheat_imported_price",
  "wheat_imported_stock_last",
  "re_stock_wheat_imported",
  "local_rice_unit_specify",
  "local_rice_price",
  "re_stock_local_rice",
  "veg_oil_unit_specify",
  "veg_oil_price",
  "veg_oil_stock_last",
  "re_stock_veg_oil",
  "pulses_lentils_price",
  "pulses_beans_unit_specify",
  "pulses_beans_price",
  "pulses_beans_stock_last",
  "re_stock_pulses_beans",
  "pulses_split_peas_unit_specify",
  "pulses_split_peas_price",
  "salt_unit_specify",
  "salt_price",
  "sugar_price",
  "tomatoes_price",
  "tomatoes_stock_last",
  "re_stock_tomatoes",
  "cotton_cloth_price",
  "re_stock_cotton_cloth",
  "toothbrush_adult_price",
  "toothpaste_price",
  "sanitary_pad_unit_specify",
  "soap_price",
  "re_stock_soap",
  "pen_price",
  "pen_stock_last",
  "re_stock_pen",
  "safe_water_stock_last",
  "re_stock_safe_water",
  "firewood_unit_specify",
  "firewood_price",
  "re_stock_firewood",
  "coal_unit_specify",
  "coal_price",
  "re_stock_coal",
  "lpg_price",
  "lpg_stock_last",
  "re_stock_lpg",
  "diesel_price",
  "diesel_stock_last",
  "re_stock_diesel",
  "petrol_price",
  "petrol_stock_last",
  "re_stock_petrol",
  "blanket_price",
  "blanket_stock_last",
  "re_stock_blanket",
  "cooking_pot_price",
  "cooking_pot_stock_last",
  "re_stock_cooking_pot",
  "water_container_price",
  "re_stock_water_container",
  "winter_jacket_price",
  "winter_jacket_stock_last",
  "re_stock_winter_jacket",
  "food_supplier_count",
  "nfi_supplier_count",
  "buy_rate",
  "sell_rate"
)

numeric_data <- df %>% select(var_list, "_uuid")

numeric_data[var_list] <- sapply(numeric_data[var_list], as.numeric)

log_df <- data.frame(outliers = numeric(),
                     uuid = character(),
                     question = character(),
                     issue = character()
                     )

for (i in 1:length(var_list)) {
  q1 <- quantile(numeric_data[[var_list[i]]], .25,  na.rm = TRUE)
  q3 <- quantile(numeric_data[[var_list[i]]], .75,  na.rm = TRUE)
  iqr <- IQR(numeric_data[[var_list[i]]], na.rm = TRUE)
  
  min_val <- q1 - (1.5 * iqr)
  max_val <- q3 + (1.5 * iqr)
  
  outliers <- numeric_data[[var_list[i]]][which(numeric_data[[var_list[i]]] < min_val | numeric_data[[var_list[i]]] > max_val)]
  
  if (length(outliers) > 0) {
    uuid <- numeric_data %>% filter(numeric_data[[var_list[i]]] %in% outliers)
    uuid <- uuid$"_uuid"
    
    temp_df <- data.frame()
    temp_df <- data.frame(outliers, uuid)
    
    temp_df$question <- var_list[i]
    
    temp_df <- temp_df %>% mutate(
      issue = case_when(
        outliers < min_val ~ "low value",
        TRUE ~ "high value"
      )
    )
    
    log_df <- rbind(log_df, temp_df)
  }
}
