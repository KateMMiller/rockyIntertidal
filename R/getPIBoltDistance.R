#' @title getPIBoltDistance: get point intercept bolt distance
#'
#' @importFrom dplyr filter mutate select
#'
#' @description This function filters point intercept bolt data data by park,
#' location, plot name, and species.
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
#' @param plotName Filter on plot name. Options include: c("T1", "T2", and "T3")
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
#' spp <- getPIBoltDistance()
#'
#' # Species detections for ACAD only sites
#' spp <- getPIBoltDistance(park = "ACAD")
#'
#' # Species detections for specific sites, plots, species, and years
#'
#' spp_t3 <- getPIBoltDistance(park = "ACAD", plotName = "T3")
#' spp_BOHA2 <- getPIBoltDistance(location = c("CALISL", "GREISL"))
#' spp_fuc <- getPIBoltDistance(park = "BOHA", species = c("FUCEPI", "FUCSPP"))
#' spp_5yr <- getPIBoltDistance(years = 2016:2021)
#' spp_first_last <- getPIBoltDistance(years = c(2013, 2021))
#' spp21_with_qaqc <- getPIBoltDistance(years = 2021, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of point intercept species detection data filtered by function arguments
#' @export

getPIBoltDistance <- function(park = "all", location = "all", plotName = "all",
                               species = "all", years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                               QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "T1", "T2", "T3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(bolt <- get("PointIntercept_BoltDist_C", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(Start_Date, "%Y"))),
           error = function(e){stop("PointIntercept_BoltDist_C data frame not found. Please import rocky intertidal data.")})

  bolt_park <- if(any(park %in% 'all')){ bolt |> filter(!is.na(Site_Code)) #drops non-park sites
  } else {filter(bolt, Site_Code %in% park)}

  bolt_loc <- if(any(location %in% 'all')){ bolt_park
  } else {filter(bolt_park, Loc_Code %in% location)}

  bolt_pname <- if(any(plotName %in% 'all')){ bolt_loc
  } else {filter(bolt_loc, Plot_Name %in% plotName)}

  bolt_year <- filter(bolt_pname, Year %in% years)

  bolt_qaqc <- if(QAQC == TRUE){bolt_year
    } else {filter(bolt_year, QAQC == FALSE)}

  bolt_final <- bolt_qaqc |>
    select(Site_Name, Site_Code, Loc_Name, Loc_Code, Start_Date, Year, QAQC, Plot_Name,
           Label, Elevation_MLLW_m, Distance_m)

  if(nrow(bolt_final) == 0){stop("Specified arguments returned an empty data frame.")}

  return(bolt_final)


}
