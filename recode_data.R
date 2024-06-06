library(dplyr)
library(readxl)
library(openxlsx)
library(survey)
library(spatstat)
library(stringr)

source("assist.R")
cleaned_data_path <- "input/data/clean/cleaned_data_2024-04-04.xlsx"

# note: in the Faryab and Jawzjan data we don't have any community centers, so all part related to CCs have been commented.
# calculation column which has been done in the excel data
total_hh_in_gozar <- c(
  "gozar_host_no",
  "gozar_recent_idp_no",
  "gozar_prolonged_idp_no",
  "gozar_protracted_idp_no",
  "gozar_crossborder_returnee_no",
  "gozar_idp_returnee_no",
  "gozar_economic_migrant_no",
  "gozar_refugee_no",
  "gozar_nomad_no"
)

sheet_names <- c(
  "main",
  "gozar_details_loop",
  "iset_details_loop",
  "water_point_loop",
  "health_centre_loop",
  "school_loop",
  "bazar_loop",
  # "cc_loop",
  "mosque_loop"
)

# keep only Faryab/Maymana data
ciry <-  "Maymana"

main_df <- read_excel(cleaned_data_path, sheet = sheet_names[1]) %>% select(all_of(main_vars)) %>% filter(grepl(paste0("^", ciry), new_nahia))
gozar_df <- read_excel(cleaned_data_path, sheet = sheet_names[2]) %>% select(all_of(gozar_vars)) %>% filter(grepl(paste0("^", ciry), new_nahia))
iset_df <- read_excel(cleaned_data_path, sheet = sheet_names[3]) %>% select(all_of(iset_vars)) %>% filter(grepl(paste0("^", ciry), new_nahia))
water_point_df <- read_excel(cleaned_data_path, sheet = sheet_names[4]) %>% select(all_of(water_point_vars)) %>% filter(grepl(paste0("^", ciry), new_nahia))
health_df <- read_excel(cleaned_data_path, sheet = sheet_names[5]) %>% select(all_of(health_vars)) %>% filter(grepl(paste0("^", ciry), new_nahia))
school_df <- read_excel(cleaned_data_path, sheet = sheet_names[6]) %>% select(all_of(school_vars)) %>% filter(grepl(paste0("^", ciry), new_nahia))
bazar_df <- read_excel(cleaned_data_path, sheet = sheet_names[7]) %>% select(all_of(bazar_vars)) %>% filter(grepl(paste0("^", ciry), new_nahia))
# comm_df <- read_excel(cleaned_data_path, sheet = sheet_names[8]) %>% select(all_of(comm_vars))
mosque_df <- read_excel(cleaned_data_path, sheet = sheet_names[8]) %>% select(all_of(mosque_vars)) %>% filter(grepl(paste0("^", ciry), new_nahia))

write.xlsx(main_df, "./input/data/recoded/xlsx/main.xlsx", overwrite = TRUE)
write.xlsx(gozar_df, "./input/data/recoded/xlsx/gozar.xlsx", overwrite = TRUE)
write.xlsx(iset_df, "./input/data/recoded/xlsx/iset.xlsx", overwrite = TRUE)
write.xlsx(water_point_df, "./input/data/recoded/xlsx/water_point.xlsx", overwrite = TRUE)
write.xlsx(health_df, "./input/data/recoded/xlsx/health_center.xlsx", overwrite = TRUE)
write.xlsx(school_df, "./input/data/recoded/xlsx/education.xlsx", overwrite = TRUE)
write.xlsx(bazar_df, "./input/data/recoded/xlsx/bazar.xlsx", overwrite = TRUE)
# write.xlsx(comm_df, "./input/data/recoded/xlsx/comm.xlsx", overwrite = TRUE)
write.xlsx(mosque_df, "./input/data/recoded/xlsx/mosque.xlsx", overwrite = TRUE)

write.csv(main_df, "./input/data/recoded/csv/main.csv")
write.csv(gozar_df, "./input/data/recoded/csv/gozar.csv")
write.csv(iset_df, "./input/data/recoded/csv/iset.csv")
write.csv(water_point_df, "./input/data/recoded/csv/water_point.csv")
write.csv(health_df, "./input/data/recoded/csv/health_center.csv")
write.csv(school_df, "./input/data/recoded/csv/education.csv")
write.csv(bazar_df, "./input/data/recoded/csv/bazar.csv")
# write.csv(comm_df, "./input/data/recoded/csv/comm.csv")
write.csv(mosque_df, "./input/data/recoded/csv/mosque.csv")

