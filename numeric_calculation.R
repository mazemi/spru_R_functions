library(dplyr)
library(readxl)
library(openxlsx)
library(survey)
library(spatstat)
library(stringr)

source("assist.R")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

main_df <- read_xlsx("./input/data/recoded/xlsx/main.xlsx")
gozar_df <- read_xlsx("./input/data/recoded/xlsx/gozar.xlsx")
iset_df <- read_xlsx("./input/data/recoded/xlsx/iset.xlsx")
water_point_df <- read_xlsx("./input/data/recoded/xlsx/water_point.xlsx")
health_df <- read_xlsx("./input/data/recoded/xlsx/health_center.xlsx")
school_df <- read_xlsx("./input/data/recoded/xlsx/education.xlsx")
bazar_df <- read_xlsx("./input/data/recoded/xlsx/bazar.xlsx")
# comm_df <- read_xlsx("./input/data/recoded/xlsx/comm.xlsx")
mosque_df <- read_xlsx("./input/data/recoded/xlsx/mosque.xlsx")

# 1.main_sheet mean, median, mode
main_analysis_overall <- main_df %>%
  summarize(across(
    all_of(main_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
main_analysis_overall$level <- "all"
main_analysis_overall$disaggregation <- "all"

main_analysis_nahia <- main_df %>%
  group_by(disaggregation = new_nahia) %>%
  summarize(across(
    all_of(main_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
main_analysis_nahia$level <- "new_nahia"

main_analysis_mfgd <- main_df %>%
  group_by(disaggregation = new_mfgd_group) %>%
  summarize(across(
    all_of(main_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
main_analysis_mfgd$level <- "new_mfgd"


main_analysis <- do.call("rbind", list(main_analysis_overall, main_analysis_nahia,
                                       main_analysis_mfgd))
main_analysis <- main_analysis %>% select(level,disaggregation,everything())
main_analysis <- main_analysis %>% mutate_if(is.numeric, round, 2)

# 2.gozar_sheet mean, median, mode
gozar_df$level_temp <- "all"
gozar_analysis_overall <- gozar_df %>%
  group_by(disaggregation = level_temp) %>%
  summarize(across(
    all_of(gozar_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
gozar_analysis_overall$level <- "all"
gozar_analysis_overall$disaggregation <- "all"

gozar_analysis_nahia <- gozar_df %>%
  group_by(disaggregation = new_nahia) %>%
  summarize(across(
    all_of(gozar_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
gozar_analysis_nahia$level <- "new_nahia"

gozar_analysis_mfgd <- gozar_df %>%
  group_by(disaggregation = new_mfgd_group) %>%
  summarize(across(
    all_of(gozar_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
gozar_analysis_mfgd$level <- "new_mfgd"

gozar_analysis <- do.call("rbind", list(gozar_analysis_overall, gozar_analysis_nahia,
                                        gozar_analysis_mfgd))
gozar_analysis <- gozar_analysis %>% select(level,disaggregation,everything())
gozar_analysis <- gozar_analysis %>% mutate_if(is.numeric, round, 2)

# 3.iset_sheet mean, median, mode
iset_df$level_temp <- "all"
iset_analysis_overall <- iset_df %>%
  group_by(disaggregation = level_temp) %>%
  summarize(across(
    all_of(iset_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
iset_analysis_overall$level <- "all"
iset_analysis_overall$disaggregation <- "all"

iset_analysis_nahia <- iset_df %>%
  group_by(disaggregation = new_nahia) %>%
  summarize(across(
    all_of(iset_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
iset_analysis_nahia$level <- "new_nahia"

iset_analysis_mfgd <- iset_df %>%
  group_by(disaggregation = new_mfgd_group) %>%
  summarize(across(
    all_of(iset_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
iset_analysis_mfgd$level <- "new_mfgd"

iset_analysis <- do.call("rbind", list(iset_analysis_overall, iset_analysis_nahia,
                                       iset_analysis_mfgd))
iset_analysis <- iset_analysis %>% select(level,disaggregation,everything())
iset_analysis <- iset_analysis %>% mutate_if(is.numeric, round, 2)

# 4.water_point_sheet mean, median, mode
water_point_df$level_temp <- "all"
water_point_analysis_overall <- water_point_df %>%
  group_by(disaggregation = level_temp) %>%
  summarize(across(
    all_of(water_point_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
water_point_analysis_overall$level <- "all"
water_point_analysis_overall$disaggregation <- "all"

water_point_analysis_nahia <- water_point_df %>%
  group_by(disaggregation = new_nahia) %>%
  summarize(across(
    all_of(water_point_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
water_point_analysis_nahia$level <- "new_nahia"

water_point_analysis_mfgd <- water_point_df %>%
  group_by(disaggregation = new_mfgd_group) %>%
  summarize(across(
    all_of(water_point_num_vars),
    list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Mode = ~ Mode(.)
    )
  ), .groups = "drop")
water_point_analysis_mfgd$level <- "new_mfgd"

water_point_analysis <- do.call("rbind", list(water_point_analysis_overall, water_point_analysis_nahia,
                                              water_point_analysis_mfgd))
water_point_analysis <- water_point_analysis %>% select(level,disaggregation,everything())
water_point_analysis <- water_point_analysis %>% mutate_if(is.numeric, round, 2)

result <- list(
  "main" = main_analysis,
  "gozar" = gozar_analysis,
  "iset" = iset_analysis,
  "water_point" = water_point_analysis
)

write.xlsx(result, "results/UGM_numeric_indicators.xlsx", overwrite = T)

