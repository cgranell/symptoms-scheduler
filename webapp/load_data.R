
# Device data
file_path <- here::here("data", "data.csv")

data <- read_csv(file_path, col_names = TRUE)

devices_lbl <- c("Advanced - BQ Aquaris V" = "BQ",
                 "Advanced - Nvidia Shield Tablet" = "NV",
                 "Advanced - Xiaomi Mi A1" = "A1",
                 "Basic - Honor 9" = "H9",
                 "Basic - Motorola Moto G" = "MO")

start_date = as_date(min(data$plan_date))
end_date = as_date(max(data$plan_date))
min_date = start_date 
max_date = ymd(end_date) + days(1)
  
# Summary table

means <- 
  data %>% 
  group_by(device_id, device_desc) %>%
  summarise(mean = round(mean(delay), 3),
            sd = round(sd(delay), 3),
            lo = round(mean - 2*sd, 3),
            hi = round(mean + 2*sd, 3)) %>%
  select(`device ID`= device_id,
         `Description`= device_desc,
         `Delay mean (mn)` = mean,
         `Standard deviation (sd)` = sd,
         `Lower limit (mean + 2*sd)` = lo,
         `Upper limit (mean + 2*st)`= hi)
 

