# load packages
suppressPackageStartupMessages(library(tidyverse))

# set wd
setwd("/jacobsb/Documents/datathon_project/outputs/")

# read in data 
data = readRDS("../datasets/all_datasets.rds")
demographics = readRDS("../datasets/demographics_cleaned.rds")

# get HADS data
hads = data$`/Portal/HADS_Sums`

# restrict to individuals in cleaned demographics table
hads = hads %>%
  filter(UserId %in% demographics$UserId)

# format completion date 
hads = hads %>%
  mutate(date_completed = as.Date(CompletedDate,format = "%Y-%m-%d"))

# count NAs
count_missing(hads,"CompletedDate")
count_missing(hads,"anxiety_sums")
count_missing(hads,"depression_sums")

# get rid of NAs 
hads = filter_na(hads,"anxiety_sums") 
hads = filter_na(hads,"depression_sums") 

# filter out nonsense readings (must be between 0 and 21)
hads = hads %>%
  mutate(anxiety_sums = as.numeric(anxiety_sums)) %>%
  mutate(depression_sums = as.numeric(depression_sums)) %>%
  mutate(anxiety_sums = ifelse(anxiety_sums <0 | anxiety_sums>21,NA,anxiety_sums)) %>%
  mutate(depression_sums = ifelse(depression_sums <0 | depression_sums>21,NA,depression_sums))

hads = filter_na(hads,"anxiety_sums") 
hads = filter_na(hads,"depression_sums") 


# define anxious and depressed binary traits
hads = hads %>%
  mutate(anxious = ifelse(anxiety_sums >7,"anxious","not_anxious")) %>%
  mutate(depressed = ifelse(depression_sums >7,"depressed","not_depressed"))

# make histograms 
make_hist(hads,"anxiety_sums")
make_hist(hads,"depression_sums")
make_barchart(hads,"anxious")
make_barchart(hads,"depressed")

# bring in demographics 
# calculate time from date of diagnosis to date of hads
hads = hads %>%
  left_join(demographics,by="UserId") %>%
  mutate(time_from_dx_to_hads = delta_dates_years(date_completed,date_of_dx))


# basic stats
summary(hads$time_from_dx_to_hads)
make_hist(hads,"time_from_dx_to_hads")
get_prop(hads,"anxious")
get_prop(hads,"depressed")
hads %>%
  distinct(UserId) %>%
  nrow()

# over time 

more_than_one = hads %>% 
  dplyr::count(UserId) %>%
  filter(n>1)
more_than_one_hads = hads %>% filter(UserId %in% more_than_one$UserId)
more_than_one_hads = more_than_one_hads %>%
  arrange(UserId,date_completed) %>%
  group_by(UserId) %>%
  mutate(index = row_number())
first_hads = more_than_one_hads %>% filter(index == 1)
for(i in c(1:nrow(first_hads))){
  this_user = first_hads[i,]
  
    more_than_one_hads %>%
    filter(index >1) %>%
    filter(UserId == first_hads$UserId[i]) %>%
      mutate(time_from_first = as.numeric(date_completed - this_user$date_completed) / 365.25) %>%
      filter(time_from_first>1) %>%
      head(1) %>%
      dplyr::select(UserId,depressed)
    
}
# save cleaned exposure data 
saveRDS(hads,"../datasets/hads_cleaned.rds")

