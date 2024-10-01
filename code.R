library(dplyr)
library(openxlsx)

pr <- read.xlsx("./AFG2002_JMMI_R50_Aug24_recoded.xlsx", sheet = "pr")
dis <- read.xlsx("./AFG2002_JMMI_R50_Aug24_recoded.xlsx", sheet = "dis")

median_df1 <- read.xlsx("./median/National_Median_JMMI_R50_Aug24.xlsx")
median_df2 <- read.xlsx("./median/Province_Median_JMMI_R50_Aug24.xlsx")
median_df3 <- read.xlsx("./median/District_Median_JMMI_R50_Aug24.xlsx")

mean_df1 <- read.xlsx("./mean/National_mean_JMMI_R50_Aug24.xlsx")
mean_df2 <- read.xlsx("./mean/Province_mean_JMMI_R50_Aug24.xlsx")
mean_df3 <- read.xlsx("./mean/District_mean_JMMI_R50_Aug24.xlsx")

median_df_p <- rbind(median_df1, median_df2)
median_df_d <- rbind(median_df1, median_df3)

mean_df_p <- rbind(mean_df1, mean_df2)
mean_df_d <- rbind(mean_df1, mean_df3)

median_df_p <- median_df_p %>% rename_with(~ paste0("median_", .), -admin)
mean_df_p <- mean_df_p %>% rename_with(~ paste0("mean_", .), -admin)

median_df_d <- median_df_d %>% rename_with(~ paste0("median_", .), -admin)
mean_df_d <- mean_df_d %>% rename_with(~ paste0("mean_", .), -admin)

# combining data 
pr2 <- pr %>% inner_join(median_df_p, by = "admin") %>% inner_join(mean_df_p, by = "admin")
dis2 <- dis %>% inner_join(median_df_d, by = "admin") %>% inner_join(mean_df_d, by = "admin")

write.xlsx(pr2, "province.xlsx")
write.xlsx(dis2, "district.xlsx")



