
# install.packages(c("here", "tidyverse", "googledrive", "readxl"))
library(here)
library(tidyverse)
library(googledrive)
library(readxl)

#TODO: to retrieve list of csv files from same folder.
#TODO: to propose naming convention
gfile_name <- "Dummy1minA1_30-03-2020T12h23.csv"
gfolder_url <- "https://drive.google.com/open?id=1Isz9zzE7g8P4DR6i4QGwTHM29MUDKDwA"
gdata_path <- drive_get(as_id(gfolder_url))

gdata_file <- drive_ls(path = gdata_path$name, pattern = gfile_name)

data_path <- here::here("data-raw", gdata_file$name) # local file
drive_download(file = as_id(gdata_file$id), path = data_path, overwrite = TRUE, verbose = TRUE)
drive_deauth()


