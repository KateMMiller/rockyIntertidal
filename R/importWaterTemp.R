#' @title importWaterTemp: Import compiled water temperature and buoy data
#'
#' @importFrom dplyr group_by slice summarize ungroup
#'
#' @description This function imports compiled water temperature data collected within 2 hours of high tide.
#' User must specify the path where the compiled logger data are stored, then imports and defines date fields
#' as POSIXct dates. Buoy data are also imported if buoy = TRUE and the files exist in the specified path.
#'
#' @param path Quoted path of database backend file, including the name of the backend.
#'
#' @param simplify Logical. If TRUE (default), will simplify dataset to only include 1 water
#' temperature measurement per high tide event by selecting the value closest in time to high tide.
#' This will speed up processing and plotting time. If FALSE, will keep all values that are within
#' 2 hours of high tide.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in ROCKY environment. If \code{FALSE}, stores views in global environment
#'
#' @param buoy Logical. If TRUE (default), imports compiled buoy data for ACAD (station 44034)
#' and BOHA (station 44013)
#'
#' @examples
#' \dontrun{
#'
#' # Import water temp data with defaults (new_env = T, simplify = T)
#' path = "Z:/PROJECTS/MONITORING/Rocky_Intertidal/NETN/5_Data/Data_Files/Temperature/Compiled_HT_water_temps_2011-2022/"
#'
#' importWaterTemp(path = path)
#'
#' # Import full water temp dataset into global environment
#' importWaterTemp(path = path, new_env = F, simplify = F)
#'
#' }
#'
#' @return Rocky intertidal compiled water temperature data in specified environment
#'
#' @export


importWaterTemp <- function(path = NA, simplify = TRUE, new_env = TRUE, buoy = TRUE){

  if(is.na(path)){stop("Must specify a path to the compiled water temperature data.")
    } else {
    if(file.exists(path) == FALSE){stop("Specified path does not exist.")}}

  path <- if(substr(path, nchar(path), nchar(path)) != "/"){paste0(path, "/")} else {(paste0(path))}

  stopifnot(is.logical(simplify))
  stopifnot(is.logical(new_env))
  stopifnot(is.logical(buoy))

  if(new_env == TRUE & !exists("ROCKY")){ROCKY <<- new.env()}

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  # Create list of compiled water level files to import and run checks on
  file_list <- list.files(path, pattern = '.csv')
  wt_files <- file_list[!grepl("Buoy", file_list)]
  b_files <- file_list[grepl("Buoy", file_list)]

  if(length(wt_files) < 9){stop("Missing at least one compiled water level file in specified path.")}

  if(!all(grepl(("BASHAR|LITHUN|LITMOO|OTTPOI|SCHPOI|SHIHAR|CALISL|GREISL|OUTBRE"), wt_files))){
    stop("Missing at least one compiled water level file in specified path.")
  }

  if(buoy == TRUE & !all(grepl("ACAD|BOHA", b_files))){
    stop("For buoy = TRUE, must have csvs for ACAD and BOHA
         in specified path with file name starting 'Buoy'.")
  }

  locs <- c("BASHAR", "LITHUN", "LITMOO", "OTTPOI",
            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE")

  # Function to simplify data to 1 water temperature value per high tide event
  # using the logged value closest to high tide
  simp_dat <- function(dat){
    dat1 <- dat |> group_by(timestamp_tide) |>
      slice(which.min(abs(time_diff))) |> ungroup()
    return(dat1)
  }

  prep_tempdata <- function(loc_code){
    dat <- read.csv(paste0(path, wt_files[grep(loc_code, wt_files)]))
    dat$timestamp <- as.POSIXct(dat$timestamp,
                                format = "%Y-%m-%d %H:%M:%S",
                                tz = "America/New_York") #Sys.timezone() also works
    dat$timestamp_tide <- as.POSIXct(dat$timestamp_tide,
                                     format = "%Y-%m-%d %H:%M:%S")
    dat$Date <- as.Date(dat$timestamp)
    dat$vert_tide_height <- dat$v
    dat$Loc_Code <- paste0(loc_code)
    dat$Site_Code <- ifelse(dat$Loc_Code %in% c("CALISL", "GREISL", "OUTBRE"), "BOHA", "ACAD")
    dat$Year <- format(as.Date(dat$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), "%Y")
    dat <- dat[, c("Site_Code", "Loc_Code", "Date", "Year", "timestamp", "Degrees_F",
                   "timestamp_tide", "time_diff", "vert_tide_height")]

    dat1 <-
    if(simplify == TRUE){
      simp_dat(dat)
    } else {dat}

    return(dat1)
  }

  maxpb <- ifelse(buoy == TRUE, 10, 9)

  pb = txtProgressBar(min = 0, max = maxpb, style = 3)

  wt_import <- lapply(seq_along(locs), function(x){
    setTxtProgressBar(pb, x)
    prep_tempdata(locs[x])
    })

  wt_import <- setNames(wt_import, locs)

  list2env(wt_import, envir = env)

  suppressWarnings(

  if(buoy == TRUE & simplify == FALSE){
    ACAD_buoy <- read.csv(paste0(path, b_files[grepl("ACAD", b_files)]))
    BOHA_buoy <- read.csv(paste0(path, b_files[grepl("BOHA", b_files)]))

    # Convert 99 and 999 flags to NA
    ACAD_buoy$WSPD[ACAD_buoy$WSPD == 99] <- NA_real_
    ACAD_buoy$WVHT[ACAD_buoy$WVHT == 99] <- NA_real_
    ACAD_buoy$WTMP[ACAD_buoy$WTMP == 999] <- NA_real_

    BOHA_buoy$WSPD[BOHA_buoy$WSPD == 99] <- NA_real_
    BOHA_buoy$WVHT[BOHA_buoy$WVHT == 99] <- NA_real_
    BOHA_buoy$WTMP[BOHA_buoy$WTMP == 999] <- NA_real_

    assign("ACAD_buoy", ACAD_buoy, envir = env)
    assign("BOHA_buoy", BOHA_buoy, envir = env)
  }
)
  suppressWarnings(
  if(buoy == TRUE & simplify == TRUE){
    ACAD_buoy <- read.csv(paste0(path, b_files[grepl("ACAD", b_files)]))
    BOHA_buoy <- read.csv(paste0(path, b_files[grepl("BOHA", b_files)]))

    # Convert 99 and 999 flags to NA
    ACAD_buoy$WSPD[ACAD_buoy$WSPD == 99] <- NA_real_
    ACAD_buoy$WVHT[ACAD_buoy$WVHT == 99] <- NA_real_
    ACAD_buoy$WTMP[ACAD_buoy$WTMP == 999] <- NA_real_
    ACAD_buoy$DATE <- as.POSIXct(ACAD_buoy$DATE, format = "%Y-%m-%d")

    BOHA_buoy$WSPD[BOHA_buoy$WSPD == 99] <- NA_real_
    BOHA_buoy$WVHT[BOHA_buoy$WVHT == 99] <- NA_real_
    BOHA_buoy$WTMP[BOHA_buoy$WTMP == 999] <- NA_real_
    BOHA_buoy$DATE <- as.POSIXct(BOHA_buoy$DATE, format = "%Y-%m-%d")

    suppressWarnings(
    ACAD_buoy2 <- ACAD_buoy |> group_by(YEAR, MONTH, DAY, DATE) |>
      mutate(max_wspd = ifelse(rank(desc(WSPD)) == 1, TRUE, FALSE)) |>
      summarize(WTMP_F_min = min(WTMP_F, na.rm = T),
                WTMP_F_max = max(WTMP_F, na.rm = T),
                WSPD_max = max(WSPD, na.rm = T),
                WDIR_max = WDIR[which.max(WSPD_max)],
                WVHT_max = max(WVHT, na.rm = T),
                .groups = 'drop'))

    ACAD_buoy2$WTMP_F_min[is.infinite(ACAD_buoy2$WTMP_F_min)] <- NA_real_
    ACAD_buoy2$WTMP_F_max[is.infinite(ACAD_buoy2$WTMP_F_max)] <- NA_real_
    ACAD_buoy2$WSPD_max[is.infinite(ACAD_buoy2$WSPD_max)] <- NA_real_
    ACAD_buoy2$WVHT_max[is.infinite(ACAD_buoy2$WVHT_max)] <- NA_real_

    suppressWarnings(
      BOHA_buoy2 <- BOHA_buoy |> group_by(YEAR, MONTH, DAY, DATE) |>
        summarize(WTMP_F_min = min(WTMP_F, na.rm = T),
                  WTMP_F_max = max(WTMP_F, na.rm = T),
                  WSPD_max = max(WSPD, na.rm = T),
                  WDIR_max = WDIR[which.max(WSPD_max)],
                  WVHT_max = max(WVHT, na.rm = T),
                  .groups = 'drop'))

    BOHA_buoy2$WTMP_F_min[is.infinite(BOHA_buoy2$WTMP_F_min)] <- NA_real_
    BOHA_buoy2$WTMP_F_max[is.infinite(BOHA_buoy2$WTMP_F_max)] <- NA_real_
    BOHA_buoy2$WSPD_max[is.infinite(BOHA_buoy2$WSPD_max)] <- NA_real_
    BOHA_buoy2$WVHT_max[is.infinite(BOHA_buoy2$WVHT_max)] <- NA_real_

    assign("ACAD_buoy", ACAD_buoy2, envir = env)
    assign("BOHA_buoy", BOHA_buoy2, envir = env)
  }
)

  if(buoy == TRUE){setTxtProgressBar(pb, 10)}

  close(pb)

  noquote('Water temperature import complete')

  }
