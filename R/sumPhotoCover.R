#' @title sumPhotoCover: summarize percent cover data from photo quadrats
#'
#' @include getPhotoCover.R
#'
#' @importFrom dplyr group_by select summarize
#'
#' @description This function summarizes site-level average, min and max percent
#' cover data by park, site, plot name, and species.
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
#' c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
#' "CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR", "NOSAMP",
#' "OTHINV", "OTHPLA", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "SAND", "TAR",
#' "ULVINT", "ULVLAC", "UNIDEN"). If a new species is added, the function will warn the user
#' that an unrecognized species was specified in case it was an error.
#'
#' @param community Filter on target species. Options include:
#' c("Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae")
#'
#' @param category Filter on category. Options include:
#' c("all", "Genus", "Species", "Species Group", and "Substrate")
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
#' cov <- sumPhotoCover()
#'
#' # Photoplot cover for ACAD only sites
#' cov <- sumPhotoCover(park = "ACAD")
#'
#' # Species detections for specific sites, plots, species, and years
#'
#' cov_t3 <- sumPhotoCover(park = "ACAD", plotName = "A3")
#' cov_BOHA2 <- sumPhotoCover(site = c("CALISL", "GREISL"))
#' cov_ab <- sumPhotoCover(park = "BOHA", species = c("ALGBRO"))
#' cov_5yr <- sumPhotoCover(years = 2016:2021)
#' cov23_with_qaqc <- sumPhotoCover(site = "SHIHAR", years = 2023, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of site-level photo plot percent cover data filtered by function arguments
#' @export

sumPhotoCover <- function(park = "all", site = "all", plotName = "all",
                          species = "all", category = "all", community = 'all',
                          years = 2013:as.numeric(format(Sys.Date(), "%Y")), QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(community %in% c("all", "Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae"))
  unmatch_spp <- setdiff(species, c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
                                    "CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR", "NOSAMP",
                                    "OTHINV", "OTHPLA", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "SAND", "TAR",
                                    "ULVINT", "ULVLAC", "UNIDEN"))

  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }

  #stopifnot(category %in% c("all", "Genus", "Species", "Species Group", "Substrate"))
  stopifnot(plotName %in% c("all", "A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5",
                            "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
                            "R1", "R2", "R3", "R4", "R5"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.

  cover <- force(getPhotoCover(park = park, site = site, plotName = plotName,
                               species = species, community = community,
                               category = category,
                               years = years, QAQC = QAQC))

  cover$ScientificName <- ifelse(cover$ScientificName == "NA", cover$CoverType, cover$ScientificName)

  cov_sum <- cover |> group_by(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate,
                               #Bolt_UTM_E, Bolt_UTM_N, Bolt_MLLW_Elev, # indiv. values for each 5 plots
                               Year, QAQC, CommunityType, CoverType, ScientificName, CoverCode) |>
                      summarize(avg_cover = mean(PercentCover, na.rm = T),
                                median_cover = median(PercentCover, na.rm = T),
                                min_cover = min(PercentCover, na.rm = T),
                                max_cover = max(PercentCover, na.rm = T),
                                q25_cover = quantile(PercentCover, probs = 0.25, na.rm = T),
                                q75_cover = quantile(PercentCover, probs = 0.75, na.rm = T),
                                .groups = 'drop'
                                )

  return(cov_sum)
}
