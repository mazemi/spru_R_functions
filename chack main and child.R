
library(dplyr)
library(openxlsx)
library(rlang)
main.data <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_final_cleaned_data.xlsx", sheet = 1)
strct <- c(
  "gozar_no","iset_no", "water_point_no", "health_centre_no", "school_no", 
  "bazar_no", "cc_no", "mosque_no", "park_no", "wfp_no", "fp_no"
)

i = 11

{
cat(paste(strct[i], "\n"))
st.data <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_final_cleaned_data.xlsx", sheet = i+1)
main.summ <- main.data %>%
  select(MFGD_CODE, !!sym(strct[i])) %>% 
  group_by(MFGD_CODE) %>% 
  summarise(n = sum(!!sym(strct[i]), na.rm = TRUE))

st.summ <- st.data %>% 
  group_by(MFGD_CODE) %>% 
  summarise(nn = n())

dt <- main.summ %>% inner_join(st.summ, by = join_by(MFGD_CODE))
dt$dif <- dt$nn -dt$n 

dt <- dt %>% filter(dif != 0)
cat(paste(nrow(dt), "\n"))
}



# ############## deleted logs:

full.log <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(full.log) <- colnames(log)

strct <- c(
  "gozar_no","iset_no", "water_point_no", "health_centre_no", "school_no", 
  "bazar_no", "cc_no", "mosque_no", "park_no", "wfp_no", "fp_no"
)



i = 11
{
log <- read.xlsx("./02_Cleanning_logs/Master cleaning logs.xlsx", sheet = 4) %>% select(uuid = `_uuid._.index`, enumerator, deviceid, Issue )
st.data <- read.xlsx("./01_Raw_data/UGM_6_Provinces__raw_data_2024-05-23.xlsx", sheet = i + 1)

st.data <- st.data %>% rename(uuid = `_submission__uuid`)

log <- log %>% 
  inner_join(st.data ,by = c("uuid"="uuid")) %>% 
  select(1:4, "_index")

sheet_name <- substr(strct[i] , 1, nchar(strct[i]) - 3)
log$sheet.name <- sheet_name 

full.log <- rbind(full.log, log)
}


write.csv(full.log, "removed logs.csv")
# ############## end of deleted logs


gozar <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_cleaned_data2.xlsx", sheet = 2)
iset <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_cleaned_data2.xlsx", sheet = 3)
water_point <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_cleaned_data2.xlsx", sheet = 4)
health <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_cleaned_data2.xlsx", sheet = 5)
school <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_cleaned_data2.xlsx", sheet = 6)
bazar <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_cleaned_data2.xlsx", sheet = 7)
mosque <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_cleaned_data2.xlsx", sheet = 8)
cc <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_cleaned_data2.xlsx", sheet = 9)
park <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_cleaned_data2.xlsx", sheet = 10)
wfp <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_cleaned_data2.xlsx", sheet = 11)
food <- read.xlsx("./03_Cleaned_data/UGM_6_Provinces_cleaned_data2.xlsx", sheet = 12)









removed_uuids <- c(
  "897fe93f-42d5-4ebe-aa30-634f5fe64540",
  "c44ad590-5118-4499-83c5-03f057bfe998"
)

main.data <- main.data %>% filter(!(`_uuid` %in% removed_uuids))
gozar <- gozar %>% filter(!(`_submission__uuid` %in% removed_uuids))
iset <- iset %>% filter(!(`_submission__uuid` %in% removed_uuids))
water_point <- water_point %>% filter(!(`_submission__uuid` %in% removed_uuids))
health <- health %>% filter(!(`_submission__uuid` %in% removed_uuids))
school <- school %>% filter(!(`_submission__uuid` %in% removed_uuids))
mosque <- mosque %>% filter(!(`_submission__uuid` %in% removed_uuids))
cc <- cc %>% filter(!(`_submission__uuid` %in% removed_uuids))
park <- park %>% filter(!(`_submission__uuid` %in% removed_uuids))
wfp <- wfp %>% filter(!(`_submission__uuid` %in% removed_uuids))
food <- food %>% filter(!(`_submission__uuid` %in% removed_uuids))


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

saveWorkbook(wb, "./03_Cleaned_data/UGM_6_Provinces_cleaned_data3.xlsx", overwrite = T)


















