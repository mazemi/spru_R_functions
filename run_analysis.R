source("./R/functions/tabulator.R")
source("R/functions/small_function.r")
source("R/functions/surveymodule.r")
source("R/functions/graph_function.R")
source("R/functions/to_alphanumeric_lowercase.R")

session <-"JMMI_R35_MAY23"
lang="English"

library(dplyr)
library(scales)
library(readxl)
library(srvyr)
library(openxlsx)
library(purrr)
library(hash)
library(stringr)
library(stringi)

####################################################################
# get the data
data <-read.csv("input/data/recoded/AFG2002_JMMI_R35_MAY23_recoded2.csv",check.names=F, na.strings = c("","NA","N/A"))
names(data) <- gsub(x = names(data), pattern = "\\.", replacement = "/")
names(data) <- tolower(names(data))

# Load Analysis Plan
log<-read_excel("input/analysisplans/analysis_plan_Round_35.xlsx")
log$xi<-to_alphanumeric_lowercase(log$xi)
log$analysis_key<-paste0(log$xi,"/",log$yi)

# kobo
questions <- read_excel("input/questionnaire/JMMI_35_final.xlsx", sheet = "survey")
choices <- read_excel("input/questionnaire/JMMI_35_final.xlsx", sheet = "choices" )

# load the parameters
analysis.params<-parametres(data,log,questions=questions,choices=choices,lang=lang)
check_param(analysis.params)

# process the results
results<-lapply(seq_along(analysis.params$xi),boom_rmd,params=analysis.params)
check_create_directory("RDS")
saveRDS(results,sprintf("RDS/results_%s_%s.RDS",lang,session))

# extract the data results
data_results<- lapply(results, function(x){x$data}) %>% do.call(rbind.fill,.)
write.csv(data_results,sprintf("results/AnalysisResults3_%s.csv",session))

####################################################################
# create data merge
source("R/functions/function_data_merge.R")
check_create_directory("datamerge")

mergelevel<-unique(log$yi)

lapply(mergelevel,function(mergelevel,data_results){
  torank <- NULL
  res<-data_results[data_results$yi==mergelevel,]
  datamerge<-create_the_data_merge(results=res,label="variable",vartorank=torank)
  datamerge$level<-mergelevel
  return(datamerge)
},data_results=data_results) %>% bind_rows -> datamerge

datamerge <- datamerge %>% select(-c(ends_with("_forgraphs")))
write.csv(datamerge,sprintf("datamerge/datamerge3_%s.csv",session))

####################################################################
# JMMI dashboard data set:
datamerge <- datamerge %>% select(-c(ends_with("_forgraphs")))

temp_data <- data
temp_data <- temp_data %>% select(partner, afg_dist_code)
temp_data <- left_join(temp_data, choices, by=c("afg_dist_code"="name"))
temp_data <- temp_data %>% select(partner,`label::English`) 

datamergefinal <- left_join(datamerge, temp_data, by=c("disaggregation"="label::English"))

########### add absent columns with NA value ########### 

tools <- questions %>% select(type, name)
choices <- choices %>% select(list_name, name)
var_list <- read_xlsx("./input/jmmi_dashboard_vars.xlsx")

tools <- tools %>% inner_join(var_list, by = c("name"="var_name"))

# identifying only select_one and select_multiple variables in the survey sheet:
tools <- tools %>% mutate(
  select_var = case_when(
    str_sub(type, start = 1,end = 10) == "select_one" | str_sub(type, start = 1,end = 15) == "select_multiple" 
    ~ stri_extract_last_words(type),
    TRUE ~ as.character(NA)
  )
)

select_vars <- tools %>% filter(!is.na(select_var))
numeric_vars <- tools %>% filter(is.na(select_var))

numeric_vars_formated <- numeric_vars %>% select(order, name)
numeric_vars_formated$name_value <- paste0(numeric_vars_formated$name, "- value -", numeric_vars_formated$name)
numeric_vars_formated <- numeric_vars_formated %>% select(- name)

# get unique select var name from the tools > survey sheet
unique_selects <- unique(select_vars$select_var)

# filter the choices data frame based on unique select vars
choices <- choices %>% filter(list_name %in% unique_selects)

select_vars_formated <- choices %>% inner_join(select_vars,  by = c("list_name"="select_var"))
select_vars_formated <- select_vars_formated %>% select(order, choice = name.x, name = name.y)
select_vars_formated$name_value <- paste0(select_vars_formated$name, "- value -", select_vars_formated$choice)
select_vars_formated <- select_vars_formated %>% select(order, name_value)

all_var_value <- rbind(numeric_vars_formated, select_vars_formated) 
all_var_value <- all_var_value %>% 
  arrange(name_value) %>% 
  arrange(order)

# vector of column names of the datamerge:
all_vars_vector <- as.vector(all_var_value$name_value)
all_vars_vector <- append(all_vars_vector, "disaggregation", 0)
Vall_vars_vector <- append(all_vars_vector, c("samplesize", "level", "partner"))

# list of all column name based on the current round:
dm_col_names <- colnames(datamergefinal)

ignore_col_names <- c("disaggregation", "samplesize", "level", "partner")
dm_col_names <- dm_col_names[!dm_col_names %in% ignore_col_names]

# list of all missed columns:
missed_cols <- all_var_value %>% filter(!name_value %in% dm_col_names)
missed_cols_vector <- missed_cols$name_value

# add NA columns to the datamerge:
for (i in 1:length(missed_cols_vector)) {
  datamergefinal[, missed_cols_vector[i]] <- NA
}

# relocate some columns to the end of data merge:
datamergefinal <- datamergefinal %>% relocate(samplesize, level, partner, .after = last_col())

# arrange the datamerge columns:
datamergefinal <- datamergefinal[ ,Vall_vars_vector]

write.csv(datamergefinal,"./dashboard/latest_datamerge3.csv")

####################################################################
# Public data set:

YEAR <- "2023"
MONTH <- "MAY"
ROUND <- "35"

# # load data
data <- get_data(YEAR, MONTH, ROUND)
 
# # process and aggregate data
df <- gen_data(data$merge_df, data$dist_df, data$prov_df, data$dist_prov_reg_df, data$item_df)
 
# # format data and write to excel
wb <- gen_excel(df)
 
saveWorkbook(wb, paste("public_dataset/output_data/DRAFT_REACH_AFG_CVWG_JMMI_Dataset_", MONTH, YEAR, ".xlsx", sep = ""), overwrite = TRUE)

