library(dplyr)
library(openxlsx)
source("./logger_function.R")

clean <- read.xlsx("./clean_data.xlsx", sheet = 1)
raw <- read.xlsx("./AFG2002_JMMI_raw_data.xlsx", sheet = 1)

res <- logger(raw,clean)

# write.xlsx(res, "log.xlsx")
# add extra column to the log book for JMMI
log_df <- inner_join(res, raw, by=c("uuid"="_uuid"))
log_df <- log_df %>% select(1:4,deviceid, partner, enumerator)

log_df <- log_df %>% mutate(
  enumeratorid = paste0(partner, "_", enumerator),
  
  issues = case_when(
    question %in% c("location_city", "market_other") ~ "Translation",
    grepl("_calculation$", question) ~ "calculation",
    TRUE ~ "unrealistic price corresponding to this unit"
  ),
  
  typeIssue = case_when(
    question %in% c("location_city", "market_other") ~ "Translation",
    grepl("_calculation$", question) ~ "calculation", 
    TRUE ~ "quality issue"
  ),
  
  feedback = "checked",
  changed = "yes"
)

log_df <- log_df %>% select(uuid,enumeratorid,deviceid,question,issues,typeIssue,feedback,changed,old,new)

# Extract deleted in the raw dataframe
deleted_df <- anti_join(raw, clean, by = "_uuid")
deleted_df <- deleted_df %>% mutate(
  enumeratorid = paste0(partner, "_", enumerator),
  issues = "Time Check",
  typeIssue = "Time Check",
  feedback= ""
  )
deleted_df <- deleted_df %>% select("_uuid",enumeratorid,deviceid,issues,typeIssue,feedback)

extarct_df <- clean %>% mutate(enumeratorid = paste0(partner, "_", enumerator))
extarct_df <- extarct_df %>% select("_uuid",enumeratorid)

# Create full log
wb <- createWorkbook()

addWorksheet(wb, "data_extract")
addWorksheet(wb, "log")
addWorksheet(wb, "deleted")

writeData(wb, "data_extract", extarct_df)
writeData(wb, "log", log_df)
writeData(wb, "deleted", deleted_df)

saveWorkbook(wb, "full_log.xlsx", overwrite = TRUE)


