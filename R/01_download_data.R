
# install.packages(c("here", "tidyverse", "googledrive", "readxl"))
library(here)
library(tidyverse)
library(googledrive)

files_per_experiment <- 5

files <- tibble(
  experiment = c(rep("#1", files_per_experiment), rep("#2", files_per_experiment)),
  gsheets_name = c("AD_BQ.csv", "AD_NV.csv", "AD_A1.csv", "BA_H9.csv", "BA_MO.csv",
                   "BA_BQ.csv", "BA_NV.csv", "BA_A1.csv", "AD_H9.csv", "AD_MO.csv")
)


gfolder_url <- "https://drive.google.com/open?id=11oqV_vZqRDkbMdQ8m2KmAeL3-mOPKClh"
gdata_path <- drive_get(as_id(gfolder_url))

# Download files corresponding to 'Experiment #1'
files_exp <- files %>%
  filter(experiment =="#1") %>%
  select(gsheets_name)

for (f in 1:files_per_experiment) {
  gfile_name <-files_exp$gsheets_name[f]
  gdata_file <- drive_ls(path = gdata_path$name, pattern = gfile_name)

  data_path <- here::here("data-raw", gdata_file$name) # local file
  drive_download(file = as_id(gdata_file$id), path = data_path, overwrite = TRUE, verbose = TRUE)
}

# Download files corresponding to 'Experiment #2'
files_exp <- files %>%
  filter(experiment =="#2") %>%
  select(gsheets_name)

for (f in 1:files_per_experiment) {
  gfile_name <-files_exp$gsheets_name[f]
  gdata_file <- drive_ls(path = gdata_path$name, pattern = gfile_name)
  
  data_path <- here::here("data-raw", gdata_file$name) # local file
  drive_download(file = as_id(gdata_file$id), path = data_path, overwrite = TRUE, verbose = TRUE)
}

drive_deauth()


