library(openxlsx)
library(dplyr)

mfs_df <- read.xlsx("./MSF/msf data.xlsx")

admin_df <- read.xlsx("./admin codes/admin_codes.xlsx")

combined_data <- left_join(mfs_df, admin_df, by = c("district.code" = "code"))

write.xlsx(combined_data, "MFS_combined.xlsx")
