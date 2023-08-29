#' @title importWaterTemp: Import compiled water temperature data
#'
#' @importFrom dplyr group_by slice ungroup
#'
#' @description This function imports compiled water temperature data collected within 2 hours of high tide.
#' User must specify the path where the compiled logger data are stored, then imports and defines date fields
#' as POSIXct dates.
#'
#' @param path Quoted path of database backend file, including the name of the backend.
#'
#' @param simplify Logical. If TRUE (default), will simplify dataset to only include 1 water temperature measurement per high
#' tide event by selecting the value closest in time to high tide. This will speed up processing and plotting time. If FALSE,
#' will keep all values that are within 2 hours of high tide.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in ROCKY environment. If \code{FALSE}, stores views in global environment
#'
#' @examples
#' \dontrun{
#'
#' # Import views using DSN
#' importData(type='DSN', DSN="rocky_BE") # this is the same as importData()
#'
#' # Import database in specific folder:
#' importData(type='file', path='./Data/NETN_RockyIntertidal_Database_be_20230120.mdb')
#'
#' }
#'
#' @return Rocky intertidal compiled water temperature data in specified environment
#'
#' @export


importWaterTemp <- function(path = NA, simplify = TRUE, new_env = TRUE){

  if(is.na(path)){stop("Must specify a path to the compiled water temperature data.")
    } else {
    if(file.exists(path) == FALSE){stop("Specified path does not exist.")}}

  path <- if(substr(path, nchar(path), nchar(path)) != "/"){paste0(path, "/")} else {(paste0(path))}

  stopifnot(is.logical(simplify))
  stopifnot(is.logical(new_env))

  if(new_env == TRUE & !exists("ROCKY")){ROCKY <<- new.env()}

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  # Create list of compiled water level files to import and run checks on
  wt_files <- list.files(path, pattern = '.csv')

  if(length(wt_files) < 9){stop("Missing at least one compiled water level file in specified path.")}

  if(!all(grepl(("BASHAR|LITHUN|LITMOO|OTTPOI|SCHPOI|SHIHAR|CALISL|GREISL|OUTBRE"), wt_files))){
    stop("Missing at least one compiled water level file in specified path.")
  }

  locs <- c("BASHAR", "LITHUN", "LITMOO", "OTTPOI",
            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE")

  # Function to simplify data to 1 water temperature value per high tide event
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

  pb = txtProgressBar(min = 0, max = 9, style = 3)

  wt_import <- lapply(seq_along(locs), function(x){
    setTxtProgressBar(pb, x)
    prep_tempdata(locs[x])
    })

  wt_import <- setNames(wt_import, locs)

  list2env(wt_import, envir = env)

  close(pb)

  noquote('Water temperature import complete')

}
