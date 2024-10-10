library(dplyr)
library(openxlsx)

# The aim of this analysis is extract the count of each responses based various disaggregations

file_path <- "./input/Cleaned Data/Distribution_Catchment_Areas_cleaned_data.xlsx"

# Read data
df <- read.xlsx(file_path)
village_df <- read.xlsx(file_path, sheet = "village_rep")
wfpd_df <- read.xlsx(file_path, sheet = "wfp_distribution_points")
wfpc_df <- read.xlsx(file_path, sheet = "wfp_points_catchment")
water_df <- read.xlsx(file_path, sheet = "water_points")
health_df <- read.xlsx(file_path, sheet = "healthcare_points")
education_df <- read.xlsx(file_path, sheet = "education_points")

main_df <- df %>% select(province, district, gender, grep("\\.", names(df)))
main_cols <- names(main_df)[4:ncol(main_df)]

# function for summarizing by district and gender
summarize_main_data <- function(data, group_vars, main_cols) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      MFGD_count = n(),
      across(all_of(main_cols), ~ sum(. == 1, na.rm = TRUE)),
      .groups = 'drop'
    )
}

# function for summarizing infrastructures by district and gender
summarize_infrastructures <- function(data, group_vars, new_col_name) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(!!new_col_name := n(), .groups = 'drop')
}


# part1: analysis of main sheet
aggregated_overall <- summarize_main_data(main_df, group_vars = NULL, main_cols)
aggregated_dist <- summarize_main_data(main_df, group_vars = "district", main_cols)
aggregated_gender <- summarize_main_data(main_df, group_vars = c("gender", "district"), main_cols)

aggregated_overall$disaggregation <- "overall"
aggregated_dist$disaggregation <- "district"
aggregated_gender$disaggregation <- "gender"

aggregated_main_data <- bind_rows(
  aggregated_overall, 
  aggregated_dist,   
  aggregated_gender  
)

aggregated_main_data <- aggregated_main_data %>% select(disaggregation, district, gender, everything())


# part2: analysis of infrastructures sheets
village_overall_summary <- summarize_infrastructures(village_df, group_vars = NULL, "village_count")
village_district_summary <- summarize_infrastructures(village_df, group_vars = "district", "village_count")
village_gender_summary <- summarize_infrastructures(village_df, group_vars = c("gender", "district"), "village_count")

wfpd_overall_summary <- summarize_infrastructures(wfpd_df, group_vars = NULL, "wfpd_count")
wfpd_district_summary <- summarize_infrastructures(wfpd_df, group_vars = "district", "wfpd_count")
wfpd_gender_summary <- summarize_infrastructures(wfpd_df, group_vars = c("gender", "district"), "wfpd_count")

wfpc_overall_summary <- summarize_infrastructures(wfpc_df, group_vars = NULL, "wfpc_count")
wfpc_district_summary <- summarize_infrastructures(wfpc_df, group_vars = "district", "wfpc_count")
wfpc_gender_summary <- summarize_infrastructures(wfpc_df, group_vars = c("gender", "district"), "wfpc_count")

water_overall_summary <- summarize_infrastructures(water_df, group_vars = NULL, "water_count")
water_district_summary <- summarize_infrastructures(water_df, group_vars = "district", "water_count")
water_gender_summary <- summarize_infrastructures(water_df, group_vars = c("gender", "district"), "water_count")

health_overall_summary <- summarize_infrastructures(health_df, group_vars = NULL, "health_count")
health_district_summary <- summarize_infrastructures(health_df, group_vars = "district", "health_count")
health_gender_summary <- summarize_infrastructures(health_df, group_vars = c("gender", "district"), "health_count")

education_overall_summary <- summarize_infrastructures(education_df, group_vars = NULL, "education_count")
education_district_summary <- summarize_infrastructures(education_df, group_vars = "district", "education_count")
education_gender_summary <- summarize_infrastructures(education_df, group_vars = c("gender", "district"), "education_count")

village_overall_summary$disaggregation <- "overall"
village_district_summary$disaggregation <- "district"
village_gender_summary$disaggregation <- "gender"

wfpd_overall_summary$disaggregation <- "overall"
wfpd_district_summary$disaggregation <- "district"
wfpd_gender_summary$disaggregation <- "gender"

wfpc_overall_summary$disaggregation <- "overall"
wfpc_district_summary$disaggregation <- "district"
wfpc_gender_summary$disaggregation <- "gender"

water_overall_summary$disaggregation <- "overall"
water_district_summary$disaggregation <- "district"
water_gender_summary$disaggregation <- "gender"

health_overall_summary$disaggregation <- "overall"
health_district_summary$disaggregation <- "district"
health_gender_summary$disaggregation <- "gender"

education_overall_summary$disaggregation <- "overall"
education_district_summary$disaggregation <- "district"
education_gender_summary$disaggregation <- "gender"

aggregated_village <- bind_rows(
  village_overall_summary, 
  village_district_summary,   
  village_gender_summary  
)

aggregated_wfpd <- bind_rows(
  wfpd_overall_summary, 
  wfpd_district_summary,   
  wfpd_gender_summary  
)

aggregated_wfpc <- bind_rows(
  wfpc_overall_summary, 
  wfpc_district_summary,   
  wfpc_gender_summary  
)

aggregated_water <- bind_rows(
  water_overall_summary, 
  water_district_summary,   
  water_gender_summary  
)

aggregated_health <- bind_rows(
  health_overall_summary, 
  health_district_summary,   
  health_gender_summary  
)

aggregated_education <- bind_rows(
  education_overall_summary, 
  education_district_summary,   
  education_gender_summary  
)

infrastructures_summary <- aggregated_village %>%
  left_join(aggregated_wfpd, by = c("disaggregation" ,"district", "gender")) %>%
  left_join(aggregated_wfpc, by = c("disaggregation" ,"district", "gender")) %>%
  left_join(aggregated_water, by = c("disaggregation" ,"district", "gender")) %>%
  left_join(aggregated_health, by = c("disaggregation" ,"district", "gender")) %>%
  left_join(aggregated_education, by = c("disaggregation" ,"district", "gender"))

infrastructures_summary <- infrastructures_summary %>% 
  select(disaggregation, district, gender, everything())


# combining part1 and part2 analysis
final_summary <- infrastructures_summary %>% 
  inner_join(aggregated_main_data, by = c("disaggregation" ,"district", "gender"))

final_summary <- final_summary %>% select(c("disaggregation" ,"district", "gender", "MFGD_count"), everything())
final_summary <- final_summary %>% rename(WFP_distribution_points = wfpd_count)
final_summary <- final_summary %>% rename(WFP_chatchment_area = wfpc_count)

write.xlsx(final_summary, "./output/DCA_Analysis.xlsx")
