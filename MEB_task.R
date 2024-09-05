library(openxlsx)
library(dplyr)

nat_df <- read.xlsx("./output/simple MEBs/National_meb.xlsx")
reg_df <- read.xlsx("./output/simple MEBs/Regional_meb.xlsx")
pro_df <- read.xlsx("./output/simple MEBs/Province_meb.xlsx")
dis_df <- read.xlsx("./output/simple MEBs/District_meb.xlsx")

combined_data <- rbind(nat_df, reg_df, pro_df, dis_df)

admin_df <- read.xlsx("./admin codes/admin_codes.xlsx")
date_df <- read.xlsx("./JMMI_Rounds.xlsx")

combined_data <- left_join(combined_data, admin_df, by = c("level", "label"))
combined_data <- left_join(combined_data, date_df, by = "jround")

write.xlsx(combined_data, "MEB_combined.xlsx")
