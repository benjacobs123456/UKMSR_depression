# load packages
suppressPackageStartupMessages(library(tidyverse))

# set wd
setwd("/jacobsb/Documents/datathon_project/outputs/")

# read in data 
data = readRDS("../datasets/all_datasets.rds")
demographics = readRDS("../datasets/demographics_cleaned.rds")
hads = readRDS("../datasets/hads_cleaned.rds")

# get EDSS data
edss = data$`/Portal/webEDSS`
edss_clinical = data$`/Clinical/EDSS_Clinical`

# add in linker IDs 
linker_ids = data$`/Clinical/StudyId_Clinical`
portal_linker_ids = data$`/Portal/StudyId_PRO`
edss_clinical = edss_clinical %>%
  filter(ClinicalId %in% linker_ids$ClinicalId) %>%
  left_join(linker_ids,by="ClinicalId") %>%
  dplyr::select(StudyId,EDSS,visit_date) %>%
  left_join(portal_linker_ids,by="StudyId")

  

# restrict to individuals in cleaned demographics table
edss = edss %>%
  filter(UserId %in% demographics$UserId)

edss_clinical = edss_clinical %>% 
  filter(UserId %in% demographics$UserId)

# format completion date 
edss = edss %>%
  mutate(date_completed = as.Date(CompletedDate_webEDSS,format = "%Y-%m-%d"))

edss_clinical = edss_clinical %>%
  mutate(date_completed = as.Date(visit_date,format = "%Y-%m-%d"))

# count NAs
count_missing(edss,"CompletedDate_webEDSS")
count_missing(edss,"webEDSS")

count_missing(edss_clinical,"EDSS")
count_missing(edss_clinical,"date_completed")

# filter out nonsense readings (must be between 0 and 10)
edss = edss %>%
  mutate(webEDSS = as.numeric(webEDSS)) %>%
  mutate(webEDSS = ifelse(webEDSS <0 | webEDSS>10,NA,webEDSS))

edss = filter_na(edss,"webEDSS") 

edss_clinical = edss_clinical %>%
  mutate(EDSS = as.numeric(EDSS)) %>%
  mutate(EDSS = ifelse(EDSS <0 | EDSS>10,NA,EDSS))

edss_clinical = filter_na(edss_clinical,"EDSS") 



# histogram 
make_hist(edss,"webEDSS")
make_hist(edss_clinical,"EDSS")

# label edss sources 
edss$edss_source = "webEDSS"
edss_clinical$edss_source = "clinical"

# combine edss sources 
edss = edss_clinical %>%
  dplyr::select(UserId,EDSS,date_completed,edss_source) %>%
  bind_rows(
    edss %>%
      dplyr::select(UserId,webEDSS,date_completed,edss_source) %>%
      dplyr::rename("EDSS" = webEDSS)
  )

# bring in demographics 
# calculate time from date of diagnosis to date of edss
edss = edss %>%
  left_join(demographics,by="UserId") %>%
  mutate(time_from_dx_to_edss = delta_dates_years(date_completed,date_of_dx))

summary(edss$time_from_dx_to_edss)
make_hist(edss,"time_from_dx_to_edss")
message("People with EDSS readings")
nrow(edss %>% distinct(UserId))

# sense-check plot - see how EDSS changes with age 
ggplot(edss,
       aes(time_from_dx_to_edss + age_at_dx, EDSS))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Age at EDSS")+
  theme_minimal()

# see how many people have >1 reading 
multiple_edss = edss %>% 
  dplyr::count(UserId) %>%
  filter(n>1)

# filter to these people 
edss = edss %>%
  filter(UserId %in% multiple_edss$UserId)
message("People with >1 EDSS reading")
nrow(edss %>% distinct(UserId))

# baseline edss readings
make_hist(edss,"time_from_dx_to_edss")
edss_within_5y_baseline = edss %>% 
  filter(time_from_dx_to_edss<5) %>%
  distinct(UserId)

# restrict to people with an EDSS within 5 years of dx
edss = edss %>%
  filter(UserId %in% edss_within_5y_baseline$UserId) 
message("People with >1 EDSS reading within 5 years of dx")
nrow(edss %>% distinct(UserId))

# number edss readings 
gaps_in_edss = edss %>%
  group_by(UserId) %>%
  arrange(date_completed) %>%
  mutate(edss_reading = row_number())

id_edss_intervals = list()
for(i in c(1:length(unique(gaps_in_edss$UserId)))){
  message(i)
  id = unique(gaps_in_edss$UserId)[i]
  
  dat = gaps_in_edss %>%
    filter(UserId == id)
  
  dat = dat %>%
    mutate(diff = delta_dates_years(dat$date_completed[1], date_completed))   
  
  id_edss_intervals[[length(id_edss_intervals)+1]] = dat
}
id_edss_intervals = do.call("bind_rows",id_edss_intervals) 

# filter to those with a second reading >=1 year after first
id_edss_intervals = id_edss_intervals %>%
  filter(edss_reading != 1) %>%
  filter(abs(diff) >= 0.5) %>%
  distinct(UserId)

edss = edss %>%
  filter(UserId %in% id_edss_intervals$UserId)
# save cleaned output 
saveRDS(edss,"../datasets/edss_cleaned.rds")


