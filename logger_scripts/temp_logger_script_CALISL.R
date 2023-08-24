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

# Get tide data from noaaoceans
# ACAD gauge 8413320; BOHA gauge:8443970

tide_dat <- rbind(
  query_coops_data(station_id = "8443970", start_date = "20100101", end_date = "20121231",
                   data_product = 'predictions', interval = 'hilo', datum = 'MLLW',
                   time_zone = 'lst_ldt'), # local daylight savings time
  query_coops_data(station_id = "8443970", start_date = "20130101", end_date = "20221231",
                   data_product = 'predictions', interval = 'hilo', datum = 'MLLW',
                   time_zone = 'lst_ldt') # local daylight savings time
)

tide_dat$timestamp <- as.POSIXct(tide_dat$t,
                                 format = "%Y-%m-%d %H:%M", tz = "America/New_York") #Sys.timezone() also works

high_tide <- tide_dat |> filter(type == "H") |> select(timestamp, v) |>
  mutate(timestamp_tide = timestamp) # backup for data.table join

# Convert 2015-2017 Degrees C to F
path = "../data/rocky/temp_data/collected_2017/"
filename = "CALISL_TEMP3_2017_10404526.csv"

cal17 <- read.table(paste0(path, filename), skip = 1, sep = ",", header = T)[1:3]
colnames(cal17) <- c("row", "timestamp", "Degrees_C")
cal17$timestamp <- as.POSIXct(cal17$timestamp,
                              format = "%m/%d/%y %I:%M:%S %p",
                              tz = "America/New_York") #Sys.timezone() also works
cal17$timestamp_temp <- cal17$timestamp
cal17$Degrees_F <- cal17$Degrees_C * (9/5) + 32

# Pull in hobo logger data
temp11 <- list.files("../data/rocky/temp_data/collected_2011", pattern = ".csv")
temp12 <- list.files("../data/rocky/temp_data/collected_2012", pattern = ".csv")
temp13 <- list.files("../data/rocky/temp_data/collected_2013", pattern = ".csv")
temp14 <- list.files("../data/rocky/temp_data/collected_2014", pattern = ".csv")
temp15 <- list.files("../data/rocky/temp_data/collected_2015", pattern = ".csv")
temp16 <- list.files("../data/rocky/temp_data/collected_2016", pattern = ".csv")
temp17 <- list.files("../data/rocky/temp_data/collected_2017", pattern = ".csv")[2:8]# drops uncor BASHAR T1
temp18 <- list.files("../data/rocky/temp_data/collected_2018", pattern = ".csv")
temp19 <- list.files("../data/rocky/temp_data/collected_2019", pattern = ".csv")
temp20 <- list.files("../data/rocky/temp_data/collected_2020", pattern = ".csv")
temp21 <- list.files("../data/rocky/temp_data/collected_2021", pattern = ".csv")
temp22 <- list.files("../data/rocky/temp_data/collected_2022", pattern = ".csv")


# Function to prepare temp data for rbind
prep_tempdata <- function(path, filename){
  dat <- read.table(paste0(path, filename), skip = 1, sep = ",", header = T)[1:3]
  colnames(dat) <- c("row", "timestamp", "Degrees_F")
  dat$timestamp <- as.POSIXct(dat$timestamp,
                              format = "%m/%d/%y %I:%M:%S %p",
                              tz = "America/New_York") #Sys.timezone() also works
  dat$timestamp_temp <- dat$timestamp # backup for dt merge
  return(dat)

}
names(cal17)

# Streamline this
CALISL <- rbind(
  prep_tempdata(path = "../data/rocky/temp_data/collected_2011/",
                filename = temp11[grepl("CALISL.TEMP1", temp11, ignore.case = TRUE)]),
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2012/",
  # #               filename = temp12[grepl("CALISL.TEMP1", temp12, ignore.case = TRUE)]), # No 2012 data
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2013/",
  #               filename = temp13[grepl("CALISL.TEMP1", temp13, ignore.case = TRUE)]), # No 2013 data
  prep_tempdata(path = "../data/rocky/temp_data/collected_2014/",
                filename = temp14[grepl("CALISL.Temp1", temp14, ignore.case = TRUE)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2015/",
                filename = temp15[grepl("CALISL.TEMP2", temp15, ignore.case = TRUE)]), # T1 missing
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2016/",
  #               filename = temp16[grepl("CALISL.TEMP1", temp16, ignore.case = TRUE)]),
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2017/",
  #               filename = temp17[grepl("CALISL.TEMP3", temp17, ignore.case = TRUE)]), #T1&2 missing
  cal17 |> select(row, timestamp, Degrees_F, timestamp_temp),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2018/",
                filename = temp18[grepl("CALISL.TEMP1", temp18, ignore.case = TRUE)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2019/",
                filename = temp19[grepl("CALISL.TEMP1", temp19, ignore.case = TRUE)]),
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2020/",
  #               filename = temp20[grepl("CALISL.TEMP1", temp20, ignore.case = TRUE)]), # No 2020 data
  prep_tempdata(path = "../data/rocky/temp_data/collected_2021/",
                filename = temp21[grepl("CALISL.TEMP1", temp21, ignore.case = TRUE)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2022/",
                filename = temp22[grepl("CALISL.TEMP1", temp22, ignore.case = TRUE)])
  )

setDT(high_tide)
setDT(CALISL)

# Rolling join will join the nearest temp timestamp with the nearest high tide timestamp.
# Then have to delete records that differ by more than an hour.
ht_temp <- high_tide[CALISL, on = 'timestamp', roll = "nearest"]

ht_temp <- ht_temp |> mutate(time_diff = difftime(timestamp_tide, timestamp_temp, units = 'hours')) |>
  filter(abs(time_diff) <= lubridate::hours(2)) # pull in temps within 2 hours of high tide

head(ht_temp)

ggplot(ht_temp, aes(x = timestamp_temp, y = Degrees_F)) +
  geom_line() + rockyIntertidal::theme_rocky() +
  scale_x_datetime(breaks = scales::breaks_width("3 months"), date_labels = "%m/%y") +
  labs(x = NULL, y = "High Tide Water Temp (F) CALISL T1")+
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = as.POSIXct(as.Date("2011-01-01")), linetype = 2, color = 'red') +
  geom_vline(xintercept = as.POSIXct(as.Date("2012-01-01")), linetype = 2, color = 'red') +
  geom_vline(xintercept = as.POSIXct(as.Date("2013-01-01")), linetype = 2, color = 'red') +
  geom_vline(xintercept = as.POSIXct(as.Date("2014-01-01")), linetype = 2, color = 'red') +
  geom_vline(xintercept = as.POSIXct(as.Date("2015-01-01")), linetype = 2, color = 'red') +
  geom_vline(xintercept = as.POSIXct(as.Date("2016-01-01")), linetype = 2, color = 'red') +
  geom_vline(xintercept = as.POSIXct(as.Date("2017-01-01")), linetype = 2, color = 'red') +
  geom_vline(xintercept = as.POSIXct(as.Date("2018-01-01")), linetype = 2, color = 'red') +
  geom_vline(xintercept = as.POSIXct(as.Date("2019-01-01")), linetype = 2, color = 'red') +
  geom_vline(xintercept = as.POSIXct(as.Date("2020-01-01")), linetype = 2, color = 'red') +
  geom_vline(xintercept = as.POSIXct(as.Date("2021-01-01")), linetype = 2, color = 'red') +
  geom_vline(xintercept = as.POSIXct(as.Date("2022-01-01")), linetype = 2, color = 'red')

ggsave("./testing_scripts/CALISL_T1_high_tide_water_level_F.jpg")

tpath = "../data/rocky/temp_data/Compiled_HT_water_temps/"
write.csv(ht_temp, paste0(tpath, "CALISL_T1-3_2011-2022.csv"), row.names = F)
