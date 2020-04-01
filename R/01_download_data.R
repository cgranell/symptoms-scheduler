
# install.packages(c("here", "tidyverse", "googledrive", "readxl"))
library(here)
library(tidyverse)
library(googledrive)
library(readxl)

#TODO: to propose naming convention
files <- c("BQ" = "AD_BQ.csv",
           "NV" = "AD_NV.csv",
           "A1" = "AD_A1.csv",
           "H9" = "BA_H9.csv",
           "MO" = "BA_MO.csv")

gfolder_url <- "https://drive.google.com/open?id=11oqV_vZqRDkbMdQ8m2KmAeL3-mOPKClh"
gdata_path <- drive_get(as_id(gfolder_url))

for (f in 1:length(files)) {
  gfile_name <- files[[f]]
  gdata_file <- drive_ls(path = gdata_path$name, pattern = gfile_name)

  data_path <- here::here("data-raw", gdata_file$name) # local file
  drive_download(file = as_id(gdata_file$id), path = data_path, overwrite = TRUE, verbose = TRUE)
}

drive_deauth()


