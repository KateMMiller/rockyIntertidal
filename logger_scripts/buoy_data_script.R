#------------------------------------------------------------------------
# Compiling water temperature data from NOAA buoys for ACAD and BOHA
#   Code written by Kate Miller 9/8/2023
#------------------------------------------------------------------------

#devtools::install_github("NOAA-EDAB/buoydata")
library(buoydata)
library(tidyverse)
# ACAD buoys:  Frenchman Bay: ATGM1; South of MDI: 44034; Mt Desert Rock: MDRM1
# Only 44034 has water temp and windspeed.
# BOHA buoys: Boston: BHBM3; East of BOHA: 44013

# Native rnoaa package only allows you to download 1 year at a time and is going to be replaced
# soon by a new package. Using the buoydata package on github until that's ready. The buoydata
# package has you download buoy data to disk, then has functions to import and compile those
# files into multiple years of data.

# Here, we're interested in WTMP (water temp), and WSPD (wind speed). WTMP is helpful to
# compare with NETN loggers. Wind speed may help identify major storms that cause changes in
# water temp.

#----- Get buoy data from NOAA NBDC -----
# Data are logged hourly, but the hour column isn't included in the output. The function below will add that.

add_hour <- function(df){
  df <- df |> group_by(YEAR, MONTH, DAY, DATE) |>
    mutate(hour = sprintf("%02d", row_number()-1)) |> ungroup() |>
    mutate(timestamp = as.POSIXct(paste0(DATE, " ", hour, ":00"),
                                  "%Y-%m-%d %H:%M", tz = "America/New_York"))
  return(df)
}

# Code below is downloading full buoy data by year for each station, then will bind
# the variable of interest together across all years. For now, just looking at
# water temperature (WTMP), wind speed (WSPD), wind direction (WDIR), and
# wave height-m ("WVHT"). Also available are atm pressure (PRES), air temp (ATMP),
# GST, DPD, APD, MWD, PRES, DEWP, VIS, and TIDE.

#---- ACAD ----
get_buoy_data(buoyid = 44034, year = 2013:2022, outDir = "../data/rocky/temp_data/")
wtmp_44034 <- combine_buoy_data(buoyid = "44034", variable = "WTMP", inDir = "../data/rocky/temp_data/") |>
  add_hour() |> mutate(WTMP_F = WTMP*(9/5)+32)
wspd_44034 <- combine_buoy_data(buoyid = "44034", variable = "WSPD", inDir = "../data/rocky/temp_data/") |>
  add_hour()
wdir_44034 <- combine_buoy_data(buoyid = "44034", variable = "WDIR", inDir = "../data/rocky/temp_data/") |>
  add_hour()
wvht_44034 <- combine_buoy_data(buoyid = "44034", variable = "WVHT", inDir = "../data/rocky/temp_data/") |>
  add_hour()

comb_44034 <- purrr::reduce(list(wtmp_44034, wspd_44034, wdir_44034, wvht_44034),
                            full_join, by = c("YEAR", "MONTH", "DAY", "DATE", "hour", "timestamp"))

write.csv(comb_44034, "../data/rocky/temp_data/Buoy_data_2013-2022_ACAD_44034.csv", row.names = F)

ggpubr::ggarrange(
  ggplot(comb_44034, aes(x = DATE, y = WTMP_F)) +
    geom_line() + ylab("Water temp (F)") + rockyIntertidal::theme_rocky() ,
  ggplot(comb_44034, aes(x = DATE, y = WSPD)) +
    geom_line() + ylab("Wind speed (m/s)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb_44034, aes(x = DATE, y = WVHT)) +
    geom_line() + ylab("Wave height (m)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb_44034, aes(x = DATE, y = WDIR)) +
    geom_line() + ylab("Wind direction") +
    rockyIntertidal::theme_rocky(),
  nrow = 4
)

#---- BOHA----
get_buoy_data(buoyid = 44013, year = 2013:2022, outDir = "../data/rocky/temp_data/")
wtmp_44013 <- combine_buoy_data(buoyid = "44013", variable = "WTMP", inDir = "../data/rocky/temp_data/") |>
  add_hour() |> mutate(WTMP_F = WTMP*(9/5)+32)
wspd_44013 <- combine_buoy_data(buoyid = "44013", variable = "WSPD", inDir = "../data/rocky/temp_data/") |>
  add_hour()
wdir_44013 <- combine_buoy_data(buoyid = "44013", variable = "WDIR", inDir = "../data/rocky/temp_data/") |>
  add_hour()
wvht_44013 <- combine_buoy_data(buoyid = "44013", variable = "WVHT", inDir = "../data/rocky/temp_data/") |>
  add_hour()

comb_44013 <- purrr::reduce(list(wtmp_44013, wspd_44013, wdir_44013, wvht_44013),
                            full_join, by = c("YEAR", "MONTH", "DAY", "DATE", "hour", "timestamp"))

write.csv(comb_44013, "../data/rocky/temp_data/Buoy_data_2013-2022_BOHA_44013.csv", row.names = F)

ggpubr::ggarrange(
  ggplot(comb_44013, aes(x = DATE, y = WTMP_F)) +
    geom_line() + ylab("Water temp (F)") + rockyIntertidal::theme_rocky() ,
  ggplot(comb_44013, aes(x = DATE, y = WSPD)) +
    geom_line() + ylab("Wind speed (m/s)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb_44013, aes(x = DATE, y = WVHT)) +
    geom_line() + ylab("Wave height (m)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb_44013, aes(x = DATE, y = WDIR)) +
    geom_line() + ylab("Wind direction") +
    rockyIntertidal::theme_rocky(),
  nrow = 4
)
