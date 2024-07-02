library(dplyr)
library(openxlsx)

main.data <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated_ref.xlsx", sheet = 1)

# these are a costume function for creating MFGD_CODE
right <- function(text, num_chars) {
  substr(text, nchar(text) - num_chars + 1, nchar(text))
}

remove_txt <- function(text) {
  substr(text, 1, nchar(text) - 4)
}

capitalize_first <- function(text) {
  paste0(toupper(substr(text, 1, 1)), substr(text, 2, nchar(text)))
}

main.data <- main.data %>%
  mutate(
    MFGD_CODE = paste0(
      province_name,
      "_PD ",
      right(nahia, 2),
      "_",
      capitalize_first(remove_txt(mfgd_group)),
      if_else(!is.na(part_number),paste0("_", as.character(part_number)),"")
    )
  )

# define the function to add all references columns
add_reff_columns <- function(data, reference_data) {
  data <- data %>%
    left_join(reference_data, by = c("_submission__uuid"="_uuid"))
  
  return(data)
}

ref_data <- main.data %>% select("_uuid", province_name, nahia, part_number, mfgd_group, MFGD_CODE)

gozar <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated2.xlsx", sheet = 2)
iset <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated2.xlsx", sheet = 3)
water_point <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated2.xlsx", sheet = 4)
health <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated2.xlsx", sheet = 5)
school <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated2.xlsx", sheet = 6)
bazar <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated2.xlsx", sheet = 7)
mosque <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated2.xlsx", sheet = 8)
cc <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated2.xlsx", sheet = 9)
park <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated2.xlsx", sheet = 10)
wfp <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated2.xlsx", sheet = 11)
food <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces__cleaned_data_updated2.xlsx", sheet = 12)

gozar <- add_reff_columns(gozar, ref_data)
iset <- add_reff_columns(iset, ref_data)
water_point <- add_reff_columns(water_point, ref_data)
health <- add_reff_columns(health, ref_data)
school <- add_reff_columns(school, ref_data)
bazar <- add_reff_columns(bazar, ref_data)
mosque <- add_reff_columns(mosque, ref_data)
cc <- add_reff_columns(cc, ref_data)
park <- add_reff_columns(park, ref_data)
wfp <- add_reff_columns(wfp, ref_data)
food <- add_reff_columns(food, ref_data)

# final clean data as an MS Excel file
wb <- createWorkbook()
addWorksheet(wb, "Main_Sheet")
addWorksheet(wb, "gozar")
addWorksheet(wb, "iset")
addWorksheet(wb, "water_point")
addWorksheet(wb, "health")
addWorksheet(wb, "school")
addWorksheet(wb, "bazar")
addWorksheet(wb, "mosque")
addWorksheet(wb, "cc")
addWorksheet(wb, "park")
addWorksheet(wb, "wfp")
addWorksheet(wb, "food")

writeData(wb, sheet = "Main_Sheet", x = main.data)
writeData(wb, sheet = "gozar", x = gozar)
writeData(wb, sheet = "iset", x = iset)
writeData(wb, sheet = "water_point", x = water_point)
writeData(wb, sheet = "health", x = health)
writeData(wb, sheet = "school", x = school)
writeData(wb, sheet = "bazar", x = bazar)
writeData(wb, sheet = "mosque", x = mosque)
writeData(wb, sheet = "cc", x = cc)
writeData(wb, sheet = "park", x = park)
writeData(wb, sheet = "wfp", x = wfp)
writeData(wb, sheet = "food", x = food)

saveWorkbook(wb, "./03_Cleaned_data/UGM_6_Provinces_cleaned_data.xlsx", overwrite = T)

