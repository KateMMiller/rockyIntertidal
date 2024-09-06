#' @title sumEchinoCounts: summarize counts of echinoderms
#'
#' @include getEchinoCounts.R
#'
#' @importFrom dplyr group_by select summarize
#'
#' @description This function summarizes site-level average, median, min and max counts
#' by park, site, plot name, and species.
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
#' @param plotName Filter on plot name. Options include: c("all", "X1", "X2", and "X3")
#'
#' @param species Filter on species code. Options include:
#' c("all", "ASTFOR", "ASTRUB", "HENSAN", "STRDRO"). If a new species is added, the
#' function will warn the user that an unrecognized species was specified in case it was an error.
#'
#' @param years Filter on year of data collected. Default is 2013 to current year.
#' Can specify a vector of years.
#'
#' @param QAQC Logical. If FALSE (Default) does not return QAQC events. If TRUE,
#' returns all events, including QAQC events. If TRUE, also returns QAQC_SameGrid and QAQC_NewGrid fields.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns all records
#' cnts <- sumEchinoCounts ()
#'
#' # Photoplot cover for ACAD only sites
#' cnts <- sumEchinoCounts (park = "ACAD")
#'
#' # Species detections for specific sites, plots, species, and years
#'
#' cnt_a1 <- sumEchinoCounts (park = "ACAD", plotName = "X1")
#' cnt_BOHA2 <- sumEchinoCounts (site = c("CALISL", "GREISL"))
#' cnt_gc <- sumEchinoCounts (park = "BOHA", species = c("CARMAE"))
#' cnt_5yr <- sumEchinoCounts (years = 2016:2021)
#' cnt_first_last <- sumEchinoCounts (years = c(2013, 2021))
#' cnt21_with_qaqc <- sumEchinoCounts (years = 2021, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of site-level echinoderm data filtered by function arguments
#' @export

sumEchinoCounts <- function(park = "all", site = "all", plotName = "all",
                            species = "all",
                            years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                            QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  unmatch_spp <- setdiff(species, c("all", "ASTFOR", "ASTRUB", "HENSAN", "STRDRO"))

  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }

  stopifnot(plotName %in% c("all", "X1", "X2", "X3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.

  cnt <- force(getEchinoCounts(park = park, site = site, plotName = plotName,
                               species = species, years = years, QAQC = QAQC)) |>
    select(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate, Year, QAQC, PlotName,
           SpeciesCode, ScientificName, Count) |>
    filter(!is.na(Count))

  cnt_sum <- cnt |> group_by(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate, Year, QAQC,
                             SpeciesCode, ScientificName) |>
    summarize(count_total = sum(Count, na.rm = T),
              count_med = median(Count, na.rm = T),
              count_avg = mean(Count, na.rm = T),
              count_min = min(Count, na.rm = T),
              count_max = max(Count, na.rm = T),
              count_l25 = quantile(Count, probs = 0.25, na.rm = T),
              count_u75 = quantile(Count, probs = 0.75, na.rm = T),
              .groups = 'drop'
    )

  return(cnt_sum)
}
