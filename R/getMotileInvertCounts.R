#' @title getMotileInvertCounts: get Motile Invertebrate counts
#'
#' @include getBolts.R
#'
#' @importFrom dplyr filter left_join mutate select
#'
#' @description This function filters motile invertebrate count data by park, site, and plot name. The returned data frame lists counts of each species detected for each sampling event and plot. Species codes in the data set are: CARMAE = Carcinus maenas (green crab); HEMISAN = Hemigrapsus sanguineus (Asian shore crab); LITLIT = Littorina littorea (common periwinkle); LITOBT = Littorina obtusata (smooth periwinkle); LITSAX = Littorina saxatilis (rough periwinkle); NUCLAP = Nucella lapillus (dogwhelk), TECTES = Tectura testudinalis (limpet).
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
#' c("all", "CARMAE", "HEMISAN", "LITLIT", "LITOBT", "LITSAX", "NUCLAP", "TECTES"). If a new species is added,
#' the function will warn the user that an unrecognized species was specified in case it was an error.
#'
#' @param community Filter on target community. Options include:
#' c("all", "Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae")
#'
#' @param years Filter on year of data collected. Default is 2013 to current year.
#' Can specify a vector of years.
#'
#' @param QAQC Logical. If FALSE (default) does not return QAQC events. If TRUE,
#' returns all events, including QAQC events.
#'
#' @param dropNA Logical. If TRUE (default), blank counts in the Damage and No.Damage columns are removed.
#' If FALSE, all records are returned.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns all records except QAQC visits and blank counts
#' minv <- getMotileInvertCounts()
#'
#' # Motile Invert counts for ACAD only sites
#' minv_acad <- getMotileInvertCounts(park = "ACAD")
#'
#' # Motile Invert counts for specific sites, plots, species, and years
#'
#' minv_r <- getMotileInvertCounts(park = "ACAD", plotName = c("R1", "R2", "R3", "R4", "R5"))
#' minv_BOHA2 <- getMotileInvertCounts(site = c("CALISL", "GREISL"))
#' minv_lit <- getMotileInvertCounts(species = c("LITLIT", "LITOBT", "LITSAX"))
#' minv_5yr <- getMotileInvertCounts(years = 2016:2021)
#' minv_first_last <- getMotileInvertCounts(years = c(2013, 2021))
#' minv21_qaqc <- getMotileInvertCounts(years = 2021, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of motile invertebrate count data filtered by function arguments.
#' @export

getMotileInvertCounts <- function(park = "all", site = "all", plotName = "all",
                                  species = 'all', years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                                  community = 'all', QAQC = FALSE, dropNA = TRUE){


 # Error handling
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5",
                                   "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
                                   "R1", "R2", "R3", "R4", "R5"))
  stopifnot(community %in% c('all', "Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae"))
  stopifnot(class(dropNA) == "logical")
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  unmatch_spp <- setdiff(species, c("all", "CARMAE", "HEMISAN", "LITLIT", "LITOBT", "LITSAX", "NUCLAP", "TECTES"))
  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(motinv <- get("MotileInvert_Counts", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(StartDate, "%Y"))),
           error = function(e){stop("MotileInvert_Counts data frame not found. Please import rocky intertidal data.")})

  motinv$Damage[motinv$Damage == -999] <- NA_real_
  motinv$No.Damage[motinv$No.Damage == -999] <- NA_real_

  bolts1 <- force(getBolts(park = park, site = site, plotName = plotName,
                          community = 'all_records',
                          plotType = 'Photoplot')) |>
    filter(grepl("label", Label))

  bolts <- if(any(community %in% 'all')){bolts1
  } else {filter(bolts1, CommunityType %in% community)}

  motinv_park <- if(any(park %in% 'all')){ filter(motinv, UnitCode %in% c("ACAD", "BOHA"))
  } else {filter(motinv, UnitCode %in% park)}

  motinv_loc <- if(any(site %in% 'all')){ motinv_park
  } else {filter(motinv_park, SiteCode %in% site)}

  motinv_pname <- if(any(plotName %in% 'all')){ motinv_loc
  } else {filter(motinv_loc, PlotName %in% plotName)}

  motinv_targspp <- if(any(community %in% 'all')){motinv_pname
  } else {filter(motinv_pname, CommunityType %in% community)}

  motinv_spp <- if(any(species %in% 'all')){ motinv_targspp
  } else {filter(motinv_targspp, SpeciesCode %in% species)}

  motinv_year <- filter(motinv_spp, Year %in% years)

  motinv_qaqc <- if(QAQC == TRUE){motinv_year
  } else {filter(motinv_year, QAQC == FALSE)}

  motinv_comb <- left_join(bolts |> select(UnitCode, SiteCode, PlotName, CommunityType,
                                           BoltLatitude, BoltLongitude, Bolt_UTM_E, Bolt_UTM_N, Bolt_MLLW_Elev) |> unique(),
                           motinv_qaqc, by = c("UnitCode", "SiteCode", "PlotName", "CommunityType"))

  motinv_comb2 <- if(dropNA == TRUE){motinv_comb |> filter(!is.na(Damage)) |> filter(!is.na(No.Damage))
    } else {motinv_comb}

  motinv_final <- motinv_comb2 |>
    select(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate, Year, QAQC, PlotName, CommunityType,
           BoltLatitude, BoltLongitude, Bolt_UTM_E, Bolt_UTM_N, Bolt_MLLW_Elev,
           ScientificName, CommonName, SpeciesCode, Damage, No.Damage, Subsampled, IsPointCUI)

  if(nrow(motinv_final) == 0){
    stop("Specified arguments returned an empty data frame. If filtering on species, make sure the code was spelled correctly.")}

  return(motinv_final)


}
