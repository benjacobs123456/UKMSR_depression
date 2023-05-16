# load packages
suppressPackageStartupMessages(library(tidyverse))

# set wd
setwd("/jacobsb/Documents/datathon_project/outputs/")
source("../scripts/functions.R")
# read in data 
data = readRDS("../datasets/all_datasets.rds")

# get portal demographics table 
demographics_portal = data$`/Portal/Demographics_PRO`
nrow(demographics_portal %>% distinct(UserId))

# count NAs
count_missing(demographics_portal,"Gender")
count_missing(demographics_portal,"YearOfBirth")
count_missing(demographics_portal,"Ethnicity")
count_missing(demographics_portal,"DiagnosisYear")

# calculate current age & age at dx 
# set unrealistic dates to NA 
demographics_portal = demographics_portal %>%
  mutate(DiagnosisYear = ifelse(DiagnosisYear < 1900,NA, DiagnosisYear)) %>%
  mutate(SymptomsYear = ifelse(SymptomsYear < 1900,NA, SymptomsYear))

data_extract_date = as.Date("01-06-2022",format = "%d-%m-%Y")
demographics_portal = demographics_portal %>%
  mutate(dob = date_from_year(YearOfBirth)) %>%
  mutate(date_of_dx = date_from_year(DiagnosisYear)) %>%
  mutate(date_of_sx = date_from_year(SymptomsYear)) %>%
  mutate(age_at_dx = delta_dates_years(date_of_dx,dob)) %>%
  mutate(age_at_sx = delta_dates_years(date_of_sx,dob)) %>%
  mutate(age_at_data_extract = delta_dates_years(data_extract_date,dob)) %>%
  mutate(disease_duration_at_extract = age_at_data_extract - age_at_sx)

  

# set genders other than M or F to missing 
demographics_portal = demographics_portal %>%
  mutate(Gender = ifelse(Gender %in% c("MALE","FEMALE"), Gender,NA))

# filter NAs
demographics_portal = filter_na(demographics_portal,"Gender")
demographics_portal = filter_na(demographics_portal,"DiagnosisYear")
demographics_portal = filter_na(demographics_portal,"YearOfBirth")
demographics_portal = filter_na(demographics_portal,"MSAtDiagnosis")

# make histograms
message("basic demographic plots")
make_hist(demographics_portal,colname = "DiagnosisYear")
make_hist(demographics_portal,colname = "age_at_dx")
make_hist(demographics_portal,colname = "age_at_data_extract")
make_barchart(demographics_portal,colname = "Gender")
make_barchart(demographics_portal,colname = "MSAtDiagnosis")

# select key columns 
demographics_portal = demographics_portal %>%
  dplyr::select(UserId,Gender,Ethnicity,MSAtDiagnosis,DiagnosisYear,dob,date_of_dx,age_at_dx,age_at_data_extract,v3_education,age_at_sx,date_of_sx,disease_duration_at_extract)

# save cleaned demographics file 
saveRDS(demographics_portal,"../datasets/demographics_cleaned.rds")

