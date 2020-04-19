
# install.packages(c("here", "tidyverse", "googledrive", "readxl"))
library(here)
library(tidyverse)
library(googledrive)

files_per_experiment <- 5

files <- tibble(
  experiment = c(rep("#1", files_per_experiment), rep("#2", files_per_experiment)),
  gsheets_name = c("AD_BQ.csv", "AD_NV.csv", "AD_A1.csv", "BA_H9.csv", "BA_MO.csv",
                   "BA_BQ.csv", "BA_NV.csv", "BA_A1.csv", "AD_H9.csv", "AD_MO.csv"),
  gsheets_link = c("https://drive.google.com/open?id=1fP9r0S8ORa689yHRtfCtYQothLuGcCw3",
                   "https://drive.google.com/open?id=1l-vnaT-Smy0SegArIz5A108-IQ2_jmVV",
                   "https://drive.google.com/open?id=1gzk1ezN5t5yBv2RnbznUqvalDx97eu3o",
                   "https://drive.google.com/open?id=1BLXO4Rvz6ppWFJe5oN9Y0hKh__ypofxk",
                   "https://drive.google.com/open?id=1RizGjKR8QLOdJna7qGPP7dYX9WWAysO4",
                   "https://drive.google.com/open?id=1rfaooLZ0Up0gNx1SCA92TV1JFxcLU0sk",
                   "https://drive.google.com/open?id=1_KIXX5FGXv7MO37QbUDnd_KGqiGihiMI",
                   "https://drive.google.com/open?id=1LezrtyB9i4K7cQsmI78E0v_KaOyhZcXI",
                   "https://drive.google.com/open?id=1ISjsfSLZt6n9miryQtDr-tsZV9bfbCDT",
                   "https://drive.google.com/open?id=1bx2RVNz05qVKGr-32GI-ZIOKQWlqKBPz")
)


for (f in 1:10) {
  gfile_name <-files$gsheets_name[f]
  gfile_id <- googledrive::as_id(files$gsheets_link[f])
  
  drive_download(file = gfile_id, 
                 path = here::here("data-raw", gfile_name), 
                 overwrite = TRUE, verbose = TRUE)

}

drive_deauth()


