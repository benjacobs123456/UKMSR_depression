# load packages
suppressPackageStartupMessages(library(tidyverse))

# read filelist 
files_in = list.files("S:/Datathon - UK MS Register Datathon/DataIn/",
                      recursive = T,
                      full.names = T,
                        pattern = "csv")

# read in files
data = purrr::map(files_in, function(x){
  message("Reading in ", x)
  y = readr::read_csv(x,
                      col_types = cols(.default="c"))
})

# shorten names of files
short_names = files_in %>% 
  str_remove("S:/Datathon - UK MS Register Datathon/DataIn/") %>% 
  str_remove(".csv")

names(data) = short_names

# save 
saveRDS(data,"/jacobsb/Documents/datathon_project/datasets/all_datasets.rds")
