#' @title getEvents: get specified events
#'
#' @importFrom dplyr filter mutate select
#'
#' @description This function filters events and returns notes recorded during sampling events by park,
#' site, plot name, and species.
#'
#' @param park Include data from all parks, or choose one.
#' \describe{
#' \item{'all'}{Includes all parks monitored in the network}
#' \item{'ACAD'}{Includes only sites in Acadia National Park}
#' \item{'BOHA'}{Includes only sites in Boston Harbor Islands National Recreation Area}
#' }
#'
#' @param site Include data from all sites, or choose specific sites based on site code.
#' \describe{
#' \item{'all'}{Includes all sites returned by other filter arguments in function}
#' \item{"BASHAR"}{Bass Harbor, ACAD}
#' \item{"LITHUN"}{Little Hunter, ACAD}
#' \item{"LITMOO"}{Little Moose, ACAD}
#' \item{"OTTPOI"}{Otter Point, ACAD}
#' \item{"SCHPOI"}{Schoodic Point, ACAD}
#' \item{"SHIHAR"}{Ship Harbor, ACAD}
#' \item{"CALISL"}{Calf Island, BOHA}
#' \item{"GREISL"}{Green Island, BOHA}
#' \item{"OUTBRE"}{Outer Brewster, BOHA}
#' }
#'
#' @param plotType Type of plot to filter. Options include:
#' c("all", "Band Transect", "Benchmark", "Photoplot", "Point Intercept Transect",
#' "Recruitment Plot", "Reference", "Temperature Logger")
#'
#' @param plotName Filter on plot name. Options include:
#'   c("A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5", "BM",
#'     "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
#'     "R1", "R2", "R3", "R4", "R5", "REF", "S1", "S2", "S3", "S4", "S5",
#'     "T1", "T2", "T3",
#'     "TEMP1", "TEMP1 ", "TEMP2", "TEMP3",
#'     "U1", "U2", "U3", "U4", "U5", "X1", "X2", "X3")
#'
#'
#' @param years Filter on year of data collected. Default is 2013 to current year.
#' Can specify a vector of years.
#'
#' @param QAQC Logical. If FALSE (Default) does not return QAQC events. If TRUE,
#' returns all events, including QAQC events.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns all records
#' notes <- getEvents()
#'
#' # Event Notes for ACAD only sites
#' notes_ACAD <- getEvents(park = "ACAD")
#'
#' }
#'
#'
#' @return Returns a data frame of point intercept species detection data filtered by function arguments
#' @export

getEvents <- function(park = "all", site = "all", plotName = "all", plotType = 'all',
                      years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                      QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotType %in% c("all", "Band Transect", "Benchmark", "Photoplot", "Point Intercept Transect",
                            "Recruitment Plot", "Reference", "Temperature logger"))
  stopifnot(plotName %in% c("all", "A1", "A2", "A3", "A4", "A5",
                            "B1", "B2", "B3", "B4", "B5", "BM",
                            "F1", "F2", "F3", "F4", "F5",
                            "M1", "M2", "M3", "M4", "M5",
                            "R1", "R2", "R3", "R4", "R5", "REF",
                            "S1", "S2", "S3", "S4", "S5",
                            "T1", "T2", "T3",
                            "TEMP1", #"TEMP1 ", # Delete after cleaned up
                            "TEMP2", "TEMP3",
                            "U1", "U2", "U3", "U4", "U5",
                            "X1", "X2", "X3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(events <- get("Events", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(StartDate, "%Y"))),
           error = function(e){stop("Event_Notes data frame not found. Please import rocky intertidal data.")})

  events_park <- if(any(park %in% 'all')){events
  } else {filter(events, UnitCode %in% park)}

  events_loc <- if(any(site %in% 'all')){ events_park
  } else {filter(events_park, SiteCode %in% site)}

  events_pname <- if(any(plotName %in% 'all')){ events_loc
  } else {filter(events_loc, Plot_Name %in% plotName)}

  events_year <- filter(events_pname, Year %in% years)

  events_qaqc <- if(QAQC == TRUE){events_year
    } else {filter(events_year, QAQC == FALSE)}

  events_final <- events_qaqc |>
    select(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate,
           Year, QAQC, BM_Latitude, BM_Longitude, LatLong_Datum,
           Conditions_Swell, Conditions_Wind, Conditions_Rain, Conditions_RecentRain,
           Substrate_Sediment, Substrate_Scour, Substrate_RockMvmnt,
           Debris_PlantWrack, Debris_Driftwood, Debris_Shells, Debris_DeadAnimals, Debris_Trash, Debris_OilTar,
           Notes_Conditions, Notes_Marker, Notes_Other, Notes_Additional_Spp, IsPointCUI)

  if(nrow(events_final) == 0){warning("Specified arguments returned an empty data frame.")}

  return(events_final)


}
