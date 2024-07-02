
library(dplyr)
library(openxlsx)

main <- read.xlsx("./raw data.xlsx", sheet = 1)
uuids <- read.xlsx("./deleted_uuids.xlsx")


uuids <- uuids %>% inner_join(main, by = c("uuid"="_uuid")) %>% select(uuid, deviceid, notetaker_id, moderators_id)

gozar <- read.xlsx("./raw data.xlsx", sheet = 2)
iset <- read.xlsx("./raw data.xlsx", sheet = 3)
water_point <- read.xlsx("./raw data.xlsx", sheet = 4)
health <- read.xlsx("./raw data.xlsx", sheet = 5)
school <- read.xlsx("./raw data.xlsx", sheet = 6)
bazar <- read.xlsx("./raw data.xlsx", sheet = 7)
mosque <- read.xlsx("./raw data.xlsx", sheet = 8)
cc <- read.xlsx("./raw data.xlsx", sheet = 9)
park <- read.xlsx("./raw data.xlsx", sheet = 10)
wfp <- read.xlsx("./raw data.xlsx", sheet = 11)
food <- read.xlsx("./raw data.xlsx", sheet = 12)

gozar <- gozar %>% inner_join(uuids, by = c("_submission__uuid"="uuid")) %>% 
  select("_submission__uuid", "_index", deviceid, notetaker_id, moderators_id) %>% 
  mutate(
    uuid = paste0(`_submission__uuid`, "_", `_index`),
    enum = paste0(notetaker_id, "_", moderators_id),
    sheet = "gozar"
  ) %>% 
  select(- c("_index",notetaker_id, moderators_id ))

iset <- iset %>% inner_join(uuids, by = c("_submission__uuid"="uuid")) %>% 
  select("_submission__uuid", "_index", deviceid, notetaker_id, moderators_id) %>% 
  mutate(
  uuid = paste0(`_submission__uuid`, "_", `_index`),
  enum = paste0(notetaker_id, "_", moderators_id),
  sheet = "gozar"
) %>% 
  select(- c("_index",notetaker_id, moderators_id ))

water_point <- water_point %>% inner_join(uuids, by = c("_submission__uuid"="uuid")) %>% 
  select("_submission__uuid", "_index", deviceid, notetaker_id, moderators_id) %>% 
  mutate(
    uuid = paste0(`_submission__uuid`, "_", `_index`),
    enum = paste0(notetaker_id, "_", moderators_id),
    sheet = "gozar"
  ) %>% 
  select(- c("_index",notetaker_id, moderators_id ))

health <- health %>% inner_join(uuids, by = c("_submission__uuid"="uuid")) %>% 
  select("_submission__uuid", "_index", deviceid, notetaker_id, moderators_id) %>% 
  mutate(
    uuid = paste0(`_submission__uuid`, "_", `_index`),
    enum = paste0(notetaker_id, "_", moderators_id),
    sheet = "gozar"
  ) %>% 
  select(- c("_index",notetaker_id, moderators_id ))

school <- school %>% inner_join(uuids, by = c("_submission__uuid"="uuid")) %>% 
  select("_submission__uuid", "_index", deviceid, notetaker_id, moderators_id) %>% 
  mutate(
    uuid = paste0(`_submission__uuid`, "_", `_index`),
    enum = paste0(notetaker_id, "_", moderators_id),
    sheet = "gozar"
  ) %>% 
  select(- c("_index",notetaker_id, moderators_id ))

bazar <- bazar %>% inner_join(uuids, by = c("_submission__uuid"="uuid")) %>% 
  select("_submission__uuid", "_index", deviceid, notetaker_id, moderators_id) %>% 
  mutate(
    uuid = paste0(`_submission__uuid`, "_", `_index`),
    enum = paste0(notetaker_id, "_", moderators_id),
    sheet = "gozar"
  ) %>% 
  select(- c("_index",notetaker_id, moderators_id ))

mosque <- mosque %>% inner_join(uuids, by = c("_submission__uuid"="uuid")) %>% 
  select("_submission__uuid", "_index", deviceid, notetaker_id, moderators_id) %>% 
  mutate(
    uuid = paste0(`_submission__uuid`, "_", `_index`),
    enum = paste0(notetaker_id, "_", moderators_id),
    sheet = "gozar"
  ) %>% 
  select(- c("_index",notetaker_id, moderators_id ))

cc <- cc %>% inner_join(uuids, by = c("_submission__uuid"="uuid")) %>% 
  select("_submission__uuid", "_index", deviceid, notetaker_id, moderators_id) %>% 
  mutate(
    uuid = paste0(`_submission__uuid`, "_", `_index`),
    enum = paste0(notetaker_id, "_", moderators_id),
    sheet = "gozar"
  ) %>% 
  select(- c("_index",notetaker_id, moderators_id ))

park <- park %>% inner_join(uuids, by = c("_submission__uuid"="uuid")) %>% 
  select("_submission__uuid", "_index", deviceid, notetaker_id, moderators_id) %>% 
  mutate(
    uuid = paste0(`_submission__uuid`, "_", `_index`),
    enum = paste0(notetaker_id, "_", moderators_id),
    sheet = "gozar"
  ) %>% 
  select(- c("_index",notetaker_id, moderators_id ))

wfp <- wfp %>% inner_join(uuids, by = c("_submission__uuid"="uuid")) %>% 
  select("_submission__uuid", "_index", deviceid, notetaker_id, moderators_id) %>% 
  mutate(
    uuid = paste0(`_submission__uuid`, "_", `_index`),
    enum = paste0(notetaker_id, "_", moderators_id),
    sheet = "gozar"
  ) %>% 
  select(- c("_index",notetaker_id, moderators_id ))

food <- food %>% inner_join(uuids, by = c("_submission__uuid"="uuid")) %>% 
  select("_submission__uuid", "_index", deviceid, notetaker_id, moderators_id) %>% 
  mutate(
    uuid = paste0(`_submission__uuid`, "_", `_index`),
    enum = paste0(notetaker_id, "_", moderators_id),
    sheet = "gozar"
  ) %>% 
  select(- c("_index",notetaker_id, moderators_id ))

gozar <- read.xlsx("./raw data.xlsx", sheet = 2)
iset <- read.xlsx("./raw data.xlsx", sheet = 3)
water_point <- read.xlsx("./raw data.xlsx", sheet = 4)
health <- read.xlsx("./raw data.xlsx", sheet = 5)
school <- read.xlsx("./raw data.xlsx", sheet = 6)
bazar <- read.xlsx("./raw data.xlsx", sheet = 7)
mosque <- read.xlsx("./raw data.xlsx", sheet = 8)
cc <- read.xlsx("./raw data.xlsx", sheet = 9)
park <- read.xlsx("./raw data.xlsx", sheet = 10)
wfp <- read.xlsx("./raw data.xlsx", sheet = 11)
food <- read.xlsx("./raw data.xlsx", sheet = 12)

final_deleted <- rbind(
  gozar,
  iset,
  water_point,
  health,
  school,
  bazar,
  mosque,
  cc,
  park,
  wfp,
  food
)


write.csv(final_deleted, "finaldel.csv")
