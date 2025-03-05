library(dplyr)
install.packages("lubridate")
library(lubridate)
library(readr)
data <- read.csv("E:/UON/Spring/Data Visualization Project/Coursework/Data information/mc1-reports-data.csv")
View(data)
data <- data %>%
mutate(time = ymd_hms(time),time_min = floor_date(time,unit = "minute"))
data_5min <- data %>%
mutate(time_5min = floor_date(time, unit = "5 minutes")) %>%
group_by(time_5min) %>%
summarise(
count = n(),
avg_shake_intensity = coalesce(mean(shake_intensity, na.rm = TRUE), 0),
avg_sewer_water = coalesce(mean(sewer_and_water, na.rm = TRUE), 0),
avg_power = coalesce(mean(power, na.rm = TRUE), 0),
avg_roads_bridges = coalesce(mean(roads_and_bridges, na.rm = TRUE), 0),
avg_medical = coalesce(mean(medical, na.rm = TRUE), 0),
avg_buildings = coalesce(mean(buildings, na.rm = TRUE), 0)
)
View(data_5min)
write_csv(data_5min,"E:/UON/Spring/Data Visualization Project/Coursework/data_total_trend.csv")
data_region <- data %>%
mutate(time_5min = floor_date(time, unit = "5 minutes")) %>%
group_by(time_5min, location) %>%
summarise(
avg_shake_intensity = coalesce(mean(shake_intensity, na.rm = TRUE), 0),
avg_sewer_water = coalesce(mean(sewer_and_water, na.rm = TRUE), 0),
avg_power = coalesce(mean(power, na.rm = TRUE), 0),
avg_roads_bridges = coalesce(mean(roads_and_bridges, na.rm = TRUE), 0),
avg_medical = coalesce(mean(medical, na.rm = TRUE), 0),
avg_buildings = coalesce(mean(buildings, na.rm = TRUE), 0),
count = n(),
.groups = "drop"
)
View(data_region)
all_times <- seq(min(data$time), max(data$time), by = "5 min")
locations <- unique(data$location)
grid <- expand_grid(time_5min = all_times, location = locations)
data_filled_region_heatmap <- grid %>%
left_join(data_region, by = c("time_5min", "location")) %>%
arrange(time_5min, location)
View(data_filled_region_heatmap)
write_csv(data_filled_region_heatmap,"E:/UON/Spring/Data Visualization Project/Coursework/data_region_heatmap.csv")
data_sd <- data %>%
mutate(time_5min = floor_date(time, unit = "5 minutes")) %>%
group_by(location, time_5min) %>%
summarise(
sd_shake_intensity = coalesce(sd(shake_intensity, na.rm = TRUE), 0),
sd_sewer_water = coalesce(sd(sewer_and_water, na.rm = TRUE), 0),
sd_power = coalesce(sd(power, na.rm = TRUE), 0),
sd_roads_bridges = coalesce(sd(roads_and_bridges, na.rm = TRUE), 0),
sd_medical = coalesce(sd(medical, na.rm = TRUE), 0),
sd_buildings = coalesce(sd(buildings, na.rm = TRUE), 0),
.groups = "drop"
)
View(data_sd)
install.packages("tidyr")
library(tidyr)
data_filled_region_sd <- data_sd %>%
complete(location, time_5min = all_times)
View(data_filled_region_sd)
write_csv(data_filled_region_sd, "E:/UON/Spring/Data Visualization Project/Coursework/data_region_sd.csv")
