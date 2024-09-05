#' @title getPhotoCover: get percent cover data from photo quadrats
#'
#' @include getBolts.R
#'
#' @importFrom dplyr filter left_join mutate select
#'
#' @description This function filters photo quadrat percent cover data by park,
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
#' @param plotName Filter on plot name. Options include:
#'   c("all", A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5",
#'     "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
#'     "R1", "R2", "R3", "R4", "R5")
#'
#' @param species Filter on species/cover code. Options include:
#' c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
#' "CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR", "NOSAMP",
#' "OTHINV", "OTHPLA", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "SAND", "TAR",
#' "ULVINT", "ULVLAC", "UNIDEN"). If a new species is added, the function will warn the user
#' that an unrecognized species was specified in case it was an error.
#'
#' @param category Filter on category. Options include:
#' c("all", "Genus", "Species", "Species Group", and "Substrate")
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
#' cov <- getPhotoCover()
#'
#' # Photoplot cover for ACAD only sites
#' cov <- getPhotoCover(park = "ACAD")
#'
#' # Species detections for specific sites, plots, species, and years
#'
#' cov_t3 <- getPhotoCover(park = "ACAD", plotName = "T3")
#' cov_BOHA2 <- getPhotoCover(site = c("CALISL", "GREISL"))
#' cov_ab <- getPhotoCover(park = "BOHA", species = c("ALGBRO"))
#' cov_5yr <- getPhotoCover(years = 2016:2021)
#' cov_first_last <- getPhotoCover(years = c(2013, 2021))
#' cov21_with_qaqc <- getPhotoCover(years = 2021, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of photo plot percent cover data filtered by function arguments
#' @export

getPhotoCover <- function(park = "all", site = "all", plotName = "all",
                          species = "all", #category = "all",
                          community = 'all',
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

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(cover <- get("PhotoQuadrats_Cover", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(StartDate, "%Y"))),
           error = function(e){stop("PhotoQuadrats_Cover data frame not found. Please import rocky intertidal data.")})

  bolts1 <- force(getBolts(park = park, site = site, plotName = plotName,
                          community = 'all_records', plotType = 'Photoplot')) |>
    filter(grepl("label", Label))

  bolts <- if(any(community %in% 'all')){bolts1
    } else {filter(bolts1, CommunityType %in% community)}

  cov_park <- if(any(park %in% 'all')){ cover
  } else {filter(cover, UnitCode %in% park)}

  cov_loc <- if(any(site %in% 'all')){ cov_park
  } else {filter(cov_park, SiteCode %in% site)}

  cov_pname <- if(any(plotName %in% 'all')){ cov_loc
  } else {filter(cov_loc, PlotName %in% plotName)}

  cov_species <- if(any(species %in% 'all')){ cov_pname
  } else {filter(cov_pname, CoverCode %in% species)}

  cov_targ <- if(any(community %in% "all")){cov_species
  } else{filter(cov_species, CommunityType %in% community)}

  # cov_cat <- if(any(category %in% 'all')){ cov_targ
  # } else {filter(cov_targ, Category %in% category)}

  cov_year <- filter(cov_targ, Year %in% years)

  cov_qaqc <- if(QAQC == TRUE){cov_year
  } else {cov_year |> filter(QAQC == FALSE) }

  # Join bolt elevation with cover data
  cov_comb <- left_join(bolts |> select(UnitCode, SiteCode, PlotName, CommunityType,
                                        BoltLatitude, BoltLongitude, Bolt_UTM_E, Bolt_UTM_N, Bolt_MLLW_Elev) |> unique(),
                    cov_qaqc,
                    by = c("UnitCode", "SiteCode", "PlotName", "CommunityType")) |>

  select(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate, Year, QAQC,
         PlotName, CommunityType, BoltLatitude, BoltLongitude, Bolt_UTM_E, Bolt_UTM_N, Bolt_MLLW_Elev,
         CoverType, ScientificName, SpeciesCode, PercentCover, IsPointCUI)

  if(nrow(cov_comb) == 0){stop("Specified arguments returned an empty data frame.")}

  return(cov_comb)


}
