#' @title sumPISpecies: summarize point intercept species detection data by species and elevation band
#'
#' @include getPIBoltDistance.R
#' @include getPISpecies.R
#'
#' @importFrom dplyr arrange filter group_by last_dplyr_warnings lead mutate n select ungroup
#' @importFrom data.table setkeyv setDT
#'
#' @description This function relates bolt elevation data with point intercept species detection data by park,
#' location, plot name, and species. Returned values include elevation change between bolts, median elevation
#' between bolts, min/max elevation between bolts, distance between bolts, slope percent and degree between bolts,
#' horizontal distance between bolts, and interpolated elevation for point intercept sampling locations.
#'
#' @param park Include data from all parks, or choose one.
#' \describe{
#' \item{'all'}{Includes all parks monitored in the network}
#' \item{'ACAD'}{Includes only sites in Acadia National Park}
#' \item{'BOHA'}{Includes only sites in Boston Harbor Islands National Recreation Area}
#' }
#'
#' @param location Include data from all locations, or choose specific locations based on location code.
#' \describe{
#' \item{'all'}{Includes all locations returned by other filter arguments in function}
#' \item{"BASHAR"}{Bass Harbor, ACAD}
#' \item{"LITHUN"}{Little Hunter, ACAD}
#' \item{"LITMOO"}{Little Moose, ACAD}
#' \item{"OTTPOI"}{Otter Point, ACAD}
#' \item{"SCHPOI"}{Schoodic Point, ACAD}
#' \item{"SHIHAR"}{Ship Harbor, ACAD}
#' \item{"CALISL"}{Calf Island, BOHA}
#' \item{"GREISL"}{Green Island, BOHA}
#' \item{"OUTBRE"}{Outer Brewster}
#' }
#'
#' @param plotName Filter on plot name. Options include: c("all", "T1", "T2", and "T3")
#'
#' @param species Filter on species code. Options include:
#' c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
#' "BOLT", "CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR",
#' "OTHINV", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "ULVENT", "ULVINT", "ULVLAC",
#' "UNIDEN", "WATER"). If a new species is added, the function will warn the user
#' that an unrecognized species was specified in case it was an error.
#'
#' @param years Filter on year of data collected. Default is 2013 to current year.
#' Can specify a vector of years.
#'
#' @param QAQC Logical. If FALSE (Default), does not return QAQC events. If TRUE,
#' returns all events, including QAQC events.
#'
#' @param drop_missing Logical. If TRUE (Default), drops bolts with missing
#' elevation and distances and drops bolts that are not bolt 1 but have a distance of 0.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns all records
#' spp <- sumPISpecies()
#'
#' # Species detections for ACAD only sites
#' spp <- sumPISpecies(park = "ACAD")
#'
#' # Species detections for specific sites, plots, species, and years
#'
#' spp_t3 <- sumPISpecies(park = "ACAD", plotName = "T3")
#' spp_BOHA2 <- sumPISpecies(location = c("CALISL", "GREISL"))
#' spp_fuc <- sumPISpecies(park = "BOHA", species = c("FUCEPI", "FUCSPP"))
#' spp_5yr <- sumPISpecies(years = 2016:2021)
#' spp_first_last <- sumPISpecies(years = c(2013, 2021))
#' spp21_with_qaqc <- sumPISpecies(years = 2021, QAQC = TRUE)
#' spp_no_drops <- sumPISpecies(drop_missing = FALSE)
#'
#' }
#'
#'
#' @return Returns a data frame of point intercept species detection data filtered by function arguments
#' @export

sumPISpecies <- function(park = "all", location = "all", plotName = "all",
                         species = "all", years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                         QAQC = FALSE, drop_missing = TRUE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))

  unmatch_spp <- setdiff(species, c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
                                    "BOLT", "CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR",
                                    "OTHINV", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "ULVENT", "ULVINT", "ULVLAC",
                                    "UNIDEN", "WATER"))

  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }

  stopifnot(plotName %in% c("all", "T1", "T2", "T3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.


  boltdist1 <- force(getPIBoltDistance(park = park, location = location,
                                       plotName = plotName, species = species,
                                       years = years, QAQC = QAQC))

  boltdist2 <- if(drop_missing == TRUE){
    boltdist1 |> filter(!is.na(Elevation_MLLW_m)) |> # Can't use bolts that don't have elevations
                 filter(!is.na(Distance_m)) |>  # Can't use bolts without elevations (ie BASHAR T1b20 2017)
                 filter(!(!grepl("01", Label) & Distance_m == 0)) # drop bolts with out of sync distances
  } else {
    boltdist1
  }

  suppressWarnings(
  boltdist <- boltdist2 |>
    arrange(Site_Code, Loc_Code, Start_Date, Plot_Name, Label) |>
    mutate(transID = paste0(Loc_Code, "_", Plot_Name, "_", Year,
                            ifelse(QAQC == TRUE, "_Q", "")),
           dist_bolt_first = Distance_m,
           dist_last = dplyr::lead(Distance_m, 1, default = NA),
           dist_bolt_last = dist_last) |> # adding backup b/c dist_last is changed in join
    filter(!dist_bolt_first > dist_bolt_last) |> #drops last bolt that matches to first bolt in next transect
    mutate(elev_first = Elevation_MLLW_m) |>
    group_by(Site_Name, Site_Code, Loc_Name, Loc_Code, Start_Date, Year, QAQC, Plot_Name) |>
    mutate(elev_last = dplyr::lead(Elevation_MLLW_m, 1, default = 0),
           elev_change = abs(elev_first - elev_last),
           dist_slope = dist_bolt_last - dist_bolt_first,
           dist_slope2 = dist_slope^2,
           elev_change2 = elev_change^2,
           dist_hor = sqrt(abs(dist_slope2 - elev_change2)), # dist is sometimes < elev change
           slope_rad = asin(dist_hor/dist_slope)) |> ungroup()
  )

    warns <- last_dplyr_warnings()

    if(length(warns) >= 1){warning(paste0("There were ", length(warns), " bolt distance/elevation combinations are impossible and cannot calculate slope between bolts. Either the tape didn't follow ground contour between this and the following bolt or a recording error occured. Slope was converted to 0 for these values. Bolt errors are provided in bolt_checks data.frame in your global environment. \n"))} else {}

  bolt_checks <- boltdist |> dplyr::filter(is.na(slope_rad)) |>
    select(Loc_Code, Year, Label, QAQC, Elevation_MLLW_m, Distance_m, Notes_Event) |>
    arrange(Loc_Code, Year, Label, QAQC)

  if(nrow(bolt_checks)>0){assign("bolt_checks", bolt_checks, envir = .GlobalEnv)}

  #boltdist$slope_rad[is.nan(boltdist$slope_rad)] <- 0
  boltdist$slope_rad[is.na(boltdist$slope_rad)] <- 0

  sppdist <- force(getPISpecies(park = park, location = location, plotName = plotName,
                                      species = 'all', # join all spp. for geometry to work; filter spp later
                                      years = years, QAQC = QAQC)) |>
              select(-Event_ID, -Plot_ID) |>
              arrange(Site_Code, Loc_Code, Start_Date, Plot_Name, PI_Distance) |>
              mutate(dist_pi = PI_Distance,
                     transID = paste0(Loc_Code, "_", Plot_Name, "_", Year,
                                      ifelse(QAQC == TRUE, "_Q", "")))

  setDT(boltdist)
  setDT(sppdist)

  setkeyv(boltdist, c("transID", "Distance_m"))
  setkeyv(sppdist, c("transID", "PI_Distance"))

  spp_merge1 <- boltdist[sppdist, on = c("Site_Name", "Site_Code", "Loc_Code", "Loc_Name", "Start_Date",
                                        "Year", "QAQC", "Plot_Name", "transID",
                                        "dist_last" = "PI_Distance"), roll = -Inf]

  suppressWarnings( #warnings covered by tryCatch above
    spp_merge <- spp_merge1 |>
    arrange(Site_Code, Loc_Code, Start_Date, Plot_Name, Label, dist_pi) |>
    mutate(dist_pi_last = dplyr::lag(dist_pi, 1, default = NA)) |>
    group_by(Site_Name, Site_Code, Loc_Name, Loc_Code, Start_Date, Year, QAQC, Plot_Name, transID, Label) |>
    mutate(sign = ifelse(elev_first - elev_last > 0, 1, -1),
           num_pis = sum(!is.na(dist_pi)),
           asin_angle = ifelse(is.na(asin(dist_hor/dist_slope)), 0, asin(dist_hor/dist_slope)),
           elev_step_pi = (dist_pi - dist_bolt_first) * cos(asin_angle),
           elev_pi = elev_first - elev_step_pi * sign) |> as.data.frame() |>
    select(Site_Code, Loc_Code, Loc_Name, Start_Date, QAQC, Year, Plot_Name,
           Label, Spp_Name, Spp_Code, Elevation_MLLW_m, elev_first, elev_last, elev_change,
           Distance_m, PI_Distance = dist_pi, PI_Elevation = elev_pi)
  )


  #write.csv(spp_merge, "./testing_scripts/spp_merge.csv", row.names = F)
  spp_final <- if(any(species == "all")){spp_merge
    } else {spp_merge |> filter(Spp_Code %in% species)}

  return(spp_final)
}
