#' @title: getBirdsMammals
#'
#' @importFrom dplyr filter select
#'
#' @description This function filters bird and mammal detections by park, site, and year.
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
#' #' @param years Filter on year of data collected. Default is 2013 to current year.
#' Can specify a vector of years.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # get mammal/bird observations for BOHA in 2024
#'
#' boha24 <- getBirdsMammals(park = "BOHA", years = 2024)
#'
#' # get observations for Bass Harbor all years
#'
#' bass <- getBirdsMammals(site = "BASHAR")
#'
#' }
#'
#' @return Data frame with bird and mammal observations
#'
#' @export
#'

getBirdsMammals <- function(park = "all", site = "all", years = 2013:as.numeric(format(Sys.Date(), "%Y"))){

  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                        "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(birdmam <- get("BirdsMammals", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(StartDate, "%Y"))),
           error = function(e){stop("BirdsMammals data frame not found. Please import the rocky intertidal data, and check that you're using the latest data package.")})

  bm_park <- if(any(park %in% 'all')){birdmam
  } else {dplyr::filter(birdmam, UnitCode %in% park)}

  bm_loc <- if(any(site %in% 'all')){bm_park
  } else {dplyr::filter(bm_park, SiteCode %in% site)}

  bm_year <- dplyr::filter(bm_loc, Year %in% years)

  if(nrow(bm_year) == 0){warning("Specified arguments returned an empty data frame.")}

  return(bm_year)

}
