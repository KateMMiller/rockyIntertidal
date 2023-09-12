#------------------------------------------------------------------------
# Compiling water temperature data by filtering logger temperature data
# within +/- 2 hours of high tide for LITMOO
#   Code written by Kate Miller 5/19/2023
#------------------------------------------------------------------------


# Logger issues vary from year to year and is hard to come up with a function to generalize
# finding and fixing issues. The solution for now is to run this function every year to compile
# a csv of water temperature data. Summary and plotting functions will then point to these csvs.

library(noaaoceans)
library(lubridate)
library(tidyverse)
library(data.table)

# Get tide data from noaaoceans
tide_dat <- rbind(
  query_coops_data(station_id = "8413320", start_date = "20100101", end_date = "20121231",
                   data_product = 'predictions', interval = 'hilo', datum = 'MLLW',
                   time_zone = 'lst_ldt'), # loGRE daylight savings time
  query_coops_data(station_id = "8413320", start_date = "20130101", end_date = "20191231",
                   data_product = 'predictions', interval = 'hilo', datum = 'MLLW',
                   time_zone = 'lst_ldt'), # loGRE daylight savings time
  query_coops_data(station_id = "8413320", start_date = "20200101", end_date = "20221231",
                   data_product = 'predictions', interval = 'hilo', datum = 'MLLW',
                   time_zone = 'lst_ldt')
)

tide_dat$timestamp <- as.POSIXct(tide_dat$t,
                                 format = "%Y-%m-%d %H:%M", tz = "America/New_York") #Sys.timezone() also works

high_tide <- tide_dat |> filter(type == "H") |> select(timestamp, v) |>
  mutate(timestamp_tide = timestamp) # backup for data.table join

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

# Streamline this
LITMOO <- rbind(
  prep_tempdata(path = "../data/rocky/temp_data/collected_2011/",
                filename = temp11[grepl("LITMOO.TEMP1", temp11, ignore.case = TRUE)]),
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2012/",
  #                filename = temp12[grepl("LITMOO.TEMP1", temp12, ignore.case = TRUE)]), # No 2012 data
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2013/",
  #               filename = temp13[grepl("LITMOO.TEMP1", temp13, ignore.case = TRUE)]), # No 2013 data
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2014/",
  #               filename = temp14[grepl("LITMOO.TEMP1", temp14, ignore.case = TRUE)]), # No 2014 data
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2015/",
  #               filename = temp15[grepl("LITMOO.TEMP1", temp15, ignore.case = TRUE)]), # No 2015 data
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2016/",
  #               filename = temp16[grepl("LITMOO.TEMP1", temp16, ignore.case = TRUE)]), # No 2016 data
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2017/",
  #               filename = temp17[grepl("LITMOO.TEMP1", temp17, ignore.case = TRUE)]), # No 2017 data
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2018/",
  #               filename = temp18[grepl("LITMOO.TEMP1", temp18, ignore.case = TRUE)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2019/",
                filename = temp19[grepl("LITMOO.TEMP1", temp19, ignore.case = TRUE)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2020/",
                filename = temp20[grepl("LITMOO.TEMP1", temp20, ignore.case = TRUE)]),
  prep_tempdata(path = "../data/rocky/temp_data/collected_2021/",
                filename = temp21[grepl("LITMOO.TEMP1", temp21, ignore.case = TRUE)])#,
  # prep_tempdata(path = "../data/rocky/temp_data/collected_2022/",
  #               filename = temp22[grepl("LITMOO.TEMP1", temp22, ignore.case = TRUE)])
  )

setDT(high_tide)
setDT(LITMOO)

# Rolling join will join the nearest temp timestamp with the nearest high tide timestamp.
# Then have to delete records that differ by more than an hour.
ht_temp <- high_tide[LITMOO, on = 'timestamp', roll = "nearest"]

ht_temp <- ht_temp |> mutate(time_diff = difftime(timestamp_tide, timestamp_temp, units = 'hours')) |>
  filter(abs(time_diff) <= lubridate::hours(2)) # pull in temps within 2 hours of high tide

# Add in missing dates so read in as NA instead of connecting lines in plots. BASHAR is the most
# complete timeseries.
tpath = "../data/rocky/temp_data/Compiled_HT_water_temps/"
BASHAR <- read.csv(paste0(tpath, "BASHAR_T1_2011-2022.csv")) |>
  mutate(timestamp = as.POSIXct(timestamp,
                                format = "%Y-%m-%d %H:%M:%S",
                                tz = "America/New_York"),
         timestamp_tide = as.POSIXct(timestamp_tide,
                                     format = "%Y-%m-%d %H:%M:%S",
                                     tz = "America/New_York"),
         timestamp_temp = as.POSIXct(timestamp_temp,
                                     format = "%Y-%m-%d %H:%M:%S",
                                     tz = "America/New_York"))

miss_times <- anti_join(BASHAR, ht_temp, by = c("timestamp")) |>
  filter(timestamp > "2011-11-01") |> filter(timestamp < "2017-05-01")
miss_times$Degrees_F <- NA_real_

ht_temp_final <- rbind(miss_times, ht_temp) |> arrange(timestamp)

ggplot(ht_temp_final, aes(x = timestamp_temp, y = Degrees_F)) +
  geom_line() + rockyIntertidal::theme_rocky() +
  scale_x_datetime(breaks = scales::breaks_width("3 months"), date_labels = "%m/%y") +
  labs(x = NULL, y = "High Tide Water Temp (F) LITHUN T1")+
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

tpath = "../data/rocky/temp_data/Compiled_HT_water_temps/"
write.csv(ht_temp_final, paste0(tpath, "LITMOO_T1_2011-2022.csv"), row.names = F)
