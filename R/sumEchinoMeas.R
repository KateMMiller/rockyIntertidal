#' @title sumEchinoMeas: summarize Echinoderm measurement data by species and 5mm size clases
#'
#' @include getEchinoMeas.R
#'
#' @importFrom dplyr filter mutate rename select
#'
#' @description This function summarizes number of individuals in each 5 mm size class by park, site, and species.
#'
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
#'   c("all", "X1", "X2", "X3")
#'
#' @param species Filter on species code. Options include:
#' c("all", "ASTFOR", "ASTRUB", "HENSAN", "STRDRO"). If a new species is added,
#' the function will warn the user that an unrecognized species was specified in case it was an error.
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
#' minv <- sumEchinoMeas()
#'
#' # Motile Invert measurement data for ACAD only sites
#' minv_acad <- sumEchinoMeas(park = "ACAD")
#'
#' # Motile Invert measurement data for specific sites, plots, species, and years
#'
#' minv_r <- sumEchinoMeas(park = "ACAD", plotName = c("X1", "X2"))
#' minv_BOHA2 <- sumEchinoMeas(site = c("CALISL", "GREISL"))
#' minv_lit <- sumEchinoMeas(species = c("ASTFOR", "ASTRUB"))
#' minv_5yr <- sumEchinoMeas(years = 2016:2021)
#' minv_first_last <- sumEchinoMeas(years = c(2013, 2021))
#' minv21_qaqc <- sumEchinoMeas(years = 2021, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of summarized echinoderm measurement data filtered by function arguments.
#' @export

sumEchinoMeas <- function(park = "all", site = "all", plotName = "all",
                                years = 2013:as.numeric(format(Sys.Date(), "%Y")), QAQC = FALSE,
                                species = 'all'){

  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "X1", "X2", "X3"))
  stopifnot(species %in% c("all", "ASTFOR", "ASTRUB", "HENSAN", "STRDRO"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  spp_list <- c("all", "ASTFOR", "ASTRUB", "HENSAN", "STRDRO")

  unmatch_spp <- setdiff(species, c(spp_list, NA))

  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }
  echo <- force(getEchinoMeas(park = park, site = site, plotName = plotName,
                              species = species, years = years, QAQC = QAQC, dropNA = TRUE)) |>
    arrange(Measurement)

  echo$Measurement[echo$Measurement == 0] <- NA_real_

  echo$Meas_5mm <- floor(echo$Measurement/5)*5

  #table(echo$Measurement)

  if(any(!is.na(echo$Measurement) & echo$Measurement > 99.9)){
    warning("Echinoderm length measured over 99.9mm. Function only built to handle lengths up to 99.9mm. If the large measurement is correct, update the measurement factor levels")}

  echo$Meas_5mm_fac <- factor(paste0(echo$Meas_5mm, " to ", echo$Meas_5mm + 4.9),
                                levels = c("0.1 to 4.9", "5 to 9.9",
                                           "10 to 14.9", "15 to 19.9",
                                           "20 to 24.9", "25 to 29.9",
                                           "30 to 34.9", "35 to 39.9",
                                           "40 to 44.9", "45 to 49.9",
                                           "50 to 54.9", "55 to 59.9",
                                           "60 to 64.9", "65 to 69.9",
                                           "70 to 74.9", "75 to 79.9",
                                           "80 to 84.9", "85 to 89.9",
                                           "90 to 94.9", "95 to 99.9"))

  echo_sum <- echo |> group_by(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate,
                               Year, QAQC, ScientificName, CommonName, SpeciesCode, Meas_5mm_fac) |>
    summarize(num_meas = sum(!is.na(Measurement)), .groups = 'drop')

  return(echo_sum)


}
