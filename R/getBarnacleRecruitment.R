#' @title getBarnacleRecruitment: get Barnacle recruitment count data
#'
#' @importFrom dplyr filter left_join mutate select
#'
#' @description This function filters barnacle recruitment count data by park, site, and plot name.
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
#' @param plotName Filter on plot name. Options include:
#' c("all", "summer", "winter", "S1", "S2", "S3", "S4", "S5", "U1", "U2", "U3", "U4", "U5")
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
#' barn <- getBarnacleRecruitment()
#'
#' # Barnacle counts for ACAD only sites
#' barn_acad <- getBarnacleRecruitment(park = "ACAD")
#'
#' # Barnacle counts for specific sites, plots, species, and years
#'
#' barn_summer <- getBarnacleRecruitment(park = "ACAD", plotName = "summer")
#' barn_BOHA <- getBarnacleRecruitment(site = c("CALISL", "GREISL"))
#' barn_5yr <- getBarnacleRecruitment(years = 2016:2021)
#' barn_first_last <- getBarnacleRecruitment(years = c(2013, 2021))
#' barn21_qaqc <- getBarnacleRecruitment(years = 2021, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of barnacle recruitment count data.
#' @export

getBarnacleRecruitment <- function(park = "all", site = "all", plotName = "all",
                                   QAQC = FALSE, years = 2013:as.numeric(format(Sys.Date(), "%Y"))){

  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "summer", "winter",
                            "S1", "S2", "S3", "S4", "S5",
                            "U1", "U2", "U3", "U4", "U5"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  # set up plot name list and catch if summer and winter are both specified when should be 'all' instead
  if(any(plotName %in% "summer" & any(plotName %in% "winter"))){plotName = "all"}
  if(any(plotName %in% "summer")){plotName = c("S1", "S2", "S3", "S4", "S5")}
  if(any(plotName %in% "winter")){plotName = c("U1", "U2", "U3", "U4", "U5")}

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(barn <- get("Barnacle_Recruitment", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(StartDate, "%Y"))),
           error = function(e){
             stop("Barnacle_Recruitment data frame not found. Please import rocky intertidal data.")})

  barn_park <- if(any(park %in% 'all')){ barn
  } else {filter(barn, UnitCode %in% park)}

  barn_loc <- if(any(site %in% 'all')){ barn_park
  } else {filter(barn_park, SiteCode %in% site)}

  barn_pname <- if(any(plotName %in% 'all')){barn_loc
  } else {filter(barn_loc, PlotName %in% plotName)}

  barn_year <- filter(barn_pname, Year %in% years)

  barn_qaqc <- if(QAQC == TRUE){barn_year
  } else {filter(barn_year, QAQC == FALSE)}

  barn2 <- barn_qaqc |>
    select(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate, Year, QAQC, QAQCType,
           PlotName, Count)

  if(nrow(barn2) == 0){stop("Specified arguments returned an empty data frame.")}

  return(barn2)
}
