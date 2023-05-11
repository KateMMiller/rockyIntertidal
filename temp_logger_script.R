#---------------------------------------
# Preliminary code to start working with temperature logger data
#---------------------------------------

# Once I figure out how to extract the high tide temperatures, and how best to deal with the replicates,
# or lost loggers, I'll build this out to a function.

# The temp data will also need to be consistently named and organized from year to year, so easier to pull
# in and bind together in package function.

library(noaaoceans)
library(lubridate)
library(tidyverse)
library(data.table)

path = "../data/rocky/temp_data/collected_2022/"

# Get tide data from noaaoceans


tide_dat <- query_coops_data(station_id = "8413320", start_date = "20150101", end_date = "20221231",
                             data_product = 'predictions', interval = 'hilo', datum = 'MLLW',
                             time_zone = 'lst_ldt') # local daylight savings time

tide_dat$timestamp <- as.POSIXct(tide_dat$t, format = "%Y-%m-%d %H:%M", tz = "America/New_York") #Sys.timezone() also works

high_tide <- tide_dat |> filter(type == "H") |> select(timestamp, v) |>
  mutate(timestamp_tide = timestamp) # backup for data.table join

# Pull in hobo logger data
temp15 <- list.files("../data/rocky/temp_data/collected_2015", pattern = ".csv")
temp16 <- list.files("../data/rocky/temp_data/collected_2016", pattern = ".csv")
temp17 <- list.files("../data/rocky/temp_data/collected_2017", pattern = ".csv")
temp18 <- list.files("../data/rocky/temp_data/collected_2018", pattern = ".csv")
temp19 <- list.files("../data/rocky/temp_data/collected_2019", pattern = ".csv")
temp20 <- list.files("../data/rocky/temp_data/collected_2020", pattern = ".csv")
temp21 <- list.files("../data/rocky/temp_data/collected_2021", pattern = ".csv")
temp22 <- list.files("../data/rocky/temp_data/collected_2022", pattern = ".csv")

prep_tempdata <- function(path, filename){
  dat <- read.table(paste0(path, filename), skip = 1, sep = ",", header = T)[1:3]
  colnames(dat) <- c("row", "timestamp", "Degrees_F")
  dat$timestamp <- as.POSIXct(dat$timestamp,
                              format = "%m/%d/%y %I:%M:%S %p",
                              tz = "America/New_York") #Sys.timezone() also works
  dat$timestamp_temp <- dat$timestamp # backup for dt merge
  return(dat)

}

# Streamline this
BASHAR_T1 <- rbind(
  prep_tempdata(path = "../data/rocky/temp_data/collected_2015/",
                filename = temp15[grepl("BASHAR.TEMP1", temp15)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2016/",
                filename = temp16[grepl("BASHAR.TEMP1", temp16)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2017/",
                filename = temp17[grepl("BASHAR.TEMP1", temp17)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2018/",
                filename = temp18[grepl("BASHAR.TEMP1", temp18)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2019/",
                filename = temp19[grepl("BASHAR.TEMP1", temp19)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2020/",
                filename = temp20[grepl("BASHAR.TEMP1", temp20)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2021/",
                filename = temp21[grepl("BASHAR.TEMP1", temp21)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2022/",
                filename = temp22[grepl("BASHAR.TEMP1", temp22)])
  )

setDT(high_tide)
setDT(BASHAR_T1)

# Rolling join will join the nearest temp timestamp with the nearest high tide timestamp.
# Then have to delete records that differ by more than an hour.
ht_temp <- high_tide[BASHAR_T1, on = 'timestamp', roll = "nearest"]

ht_temp <- ht_temp |> mutate(time_diff = difftime(timestamp_tide, timestamp_temp, units = 'hours')) |>
  filter(abs(time_diff) <= lubridate::hours(2)) # pull in temps within 2 hours of high tide

head(ht_temp)

ggplot(ht_temp, aes(x = timestamp_temp, y = Degrees_F)) +
  geom_line() + rockyIntertidal::theme_rocky() +
  scale_x_datetime(breaks = scales::breaks_width("3 months"), date_labels = "%m/%y") +
  labs(x = NULL, y = "High Tide Water Temp (F) BASHAR T1")+
  theme(axis.text.x = element_text(angle = 90))

ggsave("./testing_scripts/BASHAR_T1_high_tide_water_level_F.jpg")

