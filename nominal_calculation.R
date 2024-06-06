source("R/functions/small_function.r")
source("R/functions/surveymodule.r")
source("R/functions/to_alphanumeric_lowercase.R")

library(dplyr)
library(scales)
library(readxl)
library(srvyr)
library(surveyweights)

lang <- "English"
# note: in the Faryab and Jawzjan data we don't have any community centers, so all part related to CCs have been commented.
sessions <- c(
  "UGM_MFGD_main",
  "UGM_MFGD_gozar",
  "UGM_MFGD_iset",
  "UGM_MFGD_water_point",
  "UGM_MFGD_health_center",
  "UGM_MFGD_education",
  "UGM_MFGD_bazar",
  # "UGM_MFGD_comm",
  "UGM_MFGD_mosque"
)

all_data <- c(
  "main.csv",
  "gozar.csv",
  "iset.csv",
  "water_point.csv",
  "health_center.csv",
  "education.csv",
  "bazar.csv",
  # "comm.csv",
  "mosque.csv"
)

analysis_palns <- c(
  "Analysis_MainSheet.xlsx",
  "Analysis_GozarSheet.xlsx",
  "Analysis_isetSheet.xlsx",
  "Analysis_WaterpointSheet.xlsx",
  "Analysis_HealthCenterSheet.xlsx",
  "Analysis_EducationSheet.xlsx",
  "Analysis_BazarSheet.xlsx",
  # "Analysis_CommunityCentreSheet.xlsx",
  "Analysis_MosqueSheet.xlsx"
)

# i is 1:8 for analysis all files
i <- 1

{
data <- read.csv(paste0("input/data/recoded/csv/", all_data[i]),
  check.names = F, na.strings = c("", "NA", "N/A")
)

# names(data) <- tolower(names(data))
names(data) <- gsub(x = names(data), pattern = "\\.", replacement = "/")
names(data) <- tolower(names(data))

# Load Analysis Plan
analysis <- read_excel(paste0("input/analysisplan/", analysis_palns[i]))
analysis <- analysis %>% filter(remove == "no")
analysis <- analysis %>% filter(xi != yi)
analysis$xi <- to_alphanumeric_lowercase(analysis$xi)
analysis$analysis_key <- paste0(analysis$xi, "/", analysis$yi)

# kobo
questions <- read_excel("input/questionnaire/AFG2303_MFGD_KOBOTool_v8.xlsx", sheet = "survey")
choices <- read_excel("input/questionnaire/AFG2303_MFGD_KOBOTool_v8.xlsx", sheet = "choices")

analysis.params <- parametres(data, analysis, questions = questions, choices = choices, lang = lang)
check_param(analysis.params)


# process the results
results <- lapply(seq_along(analysis.params$xi), boom_rmd, params = analysis.params)
check_create_directory("RDS")
saveRDS(results, sprintf("RDS/results_%s_%s.RDS", lang, sessions[i]))

data_results <- lapply(results, function(x) {
  x$data
}) %>% do.call(rbind.fill, .)

write.csv(data_results, sprintf("results/AnalysisResult_%s.csv", sessions[i]))

# create data merge
source("R/functions/function_data_merge.R")
check_create_directory("datamerge")

mergelevel <- unique(analysis$yi)

lapply(mergelevel, function(mergelevel, data_results) {
  torank <- NULL
  res <- data_results[data_results$yi == mergelevel, ]
  datamerge <- create_the_data_merge(results = res, label = "variable", vartorank = torank)
  datamerge$level <- mergelevel
  return(datamerge)
}, data_results = data_results) %>% bind_rows() -> datamerge

datamerge <- datamerge %>% select(-c(ends_with("_forgraphs")))

write.csv(datamerge, sprintf(paste0("datamerge/", i ,"_datamerge_%s.csv"), sessions[i]))
}
