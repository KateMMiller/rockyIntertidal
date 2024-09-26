#' @title sumMotileInvertCounts: summarize counts of motile invertebrates
#'
#' @include getMotileInvertCounts.R
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
#' @param plotName Filter on plot name. Options include:
#'   c("all", A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5",
#'     "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
#'     "R1", "R2", "R3", "R4", "R5")
#'
#' @param species Filter on species code. Options include:
#' c("all", "CARMAE", "HEMISAN", "LITLIT",  "LITOBT", "LITSAX", "NUCLAP", "TECTES").
#' If a new species is added, the function will warn the user
#' that an unrecognized species was specified in case it was an error.
#'
#' @param community Filter on target community. Options include:
#' c("Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae")
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
#' cnts <- sumMotileInvertCounts ()
#'
#' # Photoplot cover for ACAD only sites
#' cnts <- sumMotileInvertCounts (park = "ACAD")
#'
#' # Species detections for specific sites, plots, species, and years
#'
#' cnt_a1 <- sumMotileInvertCounts (park = "ACAD", plotName = "A1")
#' cnt_BOHA2 <- sumMotileInvertCounts (site = c("CALISL", "GREISL"))
#' cnt_gc <- sumMotileInvertCounts (park = "BOHA", species = c("CARMAE"))
#' cnt_5yr <- sumMotileInvertCounts (years = 2016:2021)
#' cnt_first_last <- sumMotileInvertCounts (years = c(2013, 2021))
#' cnt21_with_qaqc <- sumMotileInvertCounts (years = 2021, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of site-level photoplot count data filtered by function arguments
#' @export

sumMotileInvertCounts <- function(park = "all", site = "all", plotName = "all",
                          species = "all", community = 'all',
                          years = 2013:as.numeric(format(Sys.Date(), "%Y")), QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(community %in% c("all", "Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae"))
  unmatch_spp <- setdiff(species, c("all", "CARMAE", "HEMISAN", "LITLIT",
                                    "LITOBT", "LITSAX", "NUCLAP", "TECTES"))

  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }

  stopifnot(plotName %in% c("all", "A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5",
                            "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
                            "R1", "R2", "R3", "R4", "R5"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.

  cnt <- force(getMotileInvertCounts(park = park, site = site, plotName = plotName,
                               species = species, community = community,
                               years = years, QAQC = QAQC, dropNA = TRUE)) |>
    mutate(Total_Count = No.Damage + Damage)


  cnt_sum <- cnt |> group_by(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate, Year,
                             QAQC, CommunityType, SpeciesCode, ScientificName, CommonName) |>
    summarize(count_total = sum(Total_Count, na.rm = T),
              count_med = median(Total_Count, na.rm = T),
              count_avg = mean(Total_Count, na.rm = T),
              count_min = min(Total_Count, na.rm = T),
              count_max = max(Total_Count, na.rm = T),
              count_l25 = quantile(Total_Count, probs = 0.25, na.rm = T),
              count_u75 = quantile(Total_Count, probs = 0.75, na.rm = T),

              damaged_total = sum(Damage, na.rm = T),
              damaged_med = median(Damage, na.rm = T),
              damaged_avg = mean(Damage, na.rm = T),
              damaged_min = min(Damage, na.rm = T),
              damaged_max = max(Damage, na.rm = T),
              damaged_l25 = quantile(Damage, probs = 0.25, na.rm = T),
              damaged_u75 = quantile(Damage, probs = 0.75, na.rm = T),

              notdamaged_total = sum(No.Damage, na.rm = T),
              notdamaged_med = median(No.Damage, na.rm = T),
              notdamaged_avg = mean(No.Damage, na.rm = T),
              notdamaged_min = min(No.Damage, na.rm = T),
              notdamaged_max = max(No.Damage, na.rm = T),
              notdamaged_l25 = quantile(No.Damage, probs = 0.25, na.rm = T),
              notdamaged_u75 = quantile(No.Damage, probs = 0.75, na.rm = T),

              .groups = 'drop'
    )

  return(cnt_sum)
}
