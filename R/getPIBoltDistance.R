#' @title getPIBoltDistance: get point intercept bolt distance
#'
#' @importFrom dplyr filter mutate select
#'
#' @description This function filters point intercept bolt data by park, site, plot name, and species.
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
#' @param plotName Filter on plot name. Options include: c("all", "T1", "T2", and "T3")
#'
#' @param years Filter on year of data collected. Default is 2013 to current year.
#' Can specify a vector of years.
#'
#' @param QAQC Logical. If FALSE (Default) does not return QAQC events. If TRUE,
#' returns all events, including QAQC events.
#'
#' @param dropNA Logical. If TRUE (default), blank distances are removed. If FALSE, all records are returned.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns all records excelt QAQC visits and blank distances
#' bolt <- getPIBoltDistance()
#'
#' # PI Bolt distances for ACAD only sites
#' bolt <- getPIBoltDistance(park = "ACAD")
#'
#' # PI Bolt distances for specific parks, sites, plots, and years
#'
#' bolt_t3 <- getPIBoltDistance(park = "ACAD", plotName = "T3")
#' bolt_BOHA2 <- getPIBoltDistance(site = c("CALISL", "GREISL"))
#' bolt_5yr <- getPIBoltDistance(years = 2016:2021)
#' bolt_first_last <- getPIBoltDistance(years = c(2013, 2021))
#' bolt_with_qaqc <- getPIBoltDistance(years = 2021, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of point intercept bolt data filtered by function arguments
#' @export

getPIBoltDistance <- function(park = "all", site = "all", plotName = "all",
                              years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                              QAQC = FALSE, dropNA = TRUE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "T1", "T2", "T3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer",
    years >= 2013)
  stopifnot(class(dropNA) == "logical")

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(bolt <- get("PointIntercept_BoltDist", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(StartDate, "%Y"))),
           error = function(e){
             stop("PointIntercept_BoltDist data frame not found. Please import rocky intertidal data.")})

  bolt_park <- if(any(park %in% 'all')){bolt} else {filter(bolt, UnitCode %in% park)}

  bolt_loc <- if(any(site %in% 'all')){ bolt_park
  } else {filter(bolt_park, SiteCode %in% site)}

  bolt_pname <- if(any(plotName %in% 'all')){ bolt_loc
  } else {filter(bolt_loc, PlotName %in% plotName)}

  bolt_year <- filter(bolt_pname, Year %in% years)

  bolt_qaqc <- if(QAQC == TRUE){bolt_year
    } else {filter(bolt_year, QAQC == FALSE)}

  bolt_na <- if(dropNA == TRUE){bolt_qaqc |> filter(!is.na(Distance_m))} else {bolt_qaqc}

  bolt_final <- bolt_na |>
    select(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate, Year, QAQC,
           PlotName, Label, Elevation_MLLW_m, Distance_m, IsPointCUI)

  if(nrow(bolt_final) == 0){stop("Specified arguments returned an empty data frame.")}

  return(bolt_final)


}
