#' @title getMotileInvertCounts: get Motile Invertebrate counts
#'
#' @include getBolts.R
#'
#' @importFrom dplyr filter left_join mutate select
#'
#' @description This function filters motile invertebrate count data by park, location, and plot name. The returned data frame lists counts of each species detected for each sampling event and plot. Species codes in the data set are: CARMAE = Carcinus maenas (green crab); HEMISAN = Hemigrapsus sanguineus (Asian shore crab); LITLIT = Littorina littorea (common periwinkle); LITOBT = Littorina obtusata (smooth periwinkle); LITSAX = Littorina saxatilis (rough periwinkle); NUCLAP = Nucella lapillus (dogwhelk), TECTES = Tectura testudinalis (limpet).
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
#' @param plotName Filter on plot name. Options include:
#'   c("all", A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5",
#'     "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
#'     "R1", "R2", "R3", "R4", "R5")
#'
#' @param species Filter on species code. Options include:
#' c("all", "CARMAE", "HEMISAN", "LITLIT", "LITOBT", "LITSAX", "NUCLAP", "TECTES"). If a new species is added,
#' the function will warn the user that an unrecognized species was specified in case it was an error.
#'
#' @param target_species Filter on target species (ie photoplot). Options include:
#' c("Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae")
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
#' minv <- getMotileInvertCounts()
#'
#' # Motile Invert counts for ACAD only sites
#' minv_acad <- getMotileInvertCounts(park = "ACAD")
#'
#' # Motile Invert counts for specific sites, plots, species, and years
#'
#' minv_r <- getMotileInvertCounts(park = "ACAD", plotName = c("R1", "R2", "R3", "R4", "R5"))
#' minv_BOHA2 <- getMotileInvertCounts(location = c("CALISL", "GREISL"))
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

getMotileInvertCounts <- function(park = "all", location = "all", plotName = "all",
                                  species = 'all', years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                                  target_species = 'all', QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5",
                                   "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
                                   "R1", "R2", "R3", "R4", "R5"))
  stopifnot(target_species %in% c('all', "Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae"))

  unmatch_spp <- setdiff(species, c("all", "CARMAE", "HEMISAN", "LITLIT", "LITOBT", "LITSAX", "NUCLAP", "TECTES"))
  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }


  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(motinv <- get("MotileInvert_Counts", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(Start_Date, "%Y"))),
           error = function(e){stop("MotileInvert_Counts data frame not found. Please import rocky intertidal data.")})

  bolts <- force(getBolts(park = park, location = location, plotName = plotName, species = 'all_records',
                          plotType = 'Photoplot')) |>
    filter(grepl("label", Label))

  motinv_park <- if(any(park %in% 'all')){ filter(motinv, Site_Code %in% c("ACAD", "BOHA"))
  } else {filter(motinv, Site_Code %in% park)}

  motinv_loc <- if(any(location %in% 'all')){ motinv_park
  } else {filter(motinv_park, Loc_Code %in% location)}

  motinv_pname <- if(any(plotName %in% 'all')){ motinv_loc
  } else {filter(motinv_loc, Plot_Name %in% plotName)}

  motinv_targspp <- if(any(target_species %in% 'all')){motinv_pname
  } else {filter(motinv_pname, Target_Species %in% target_species)}

  motinv_spp <- if(any(species %in% 'all')){ motinv_targspp
  } else {filter(motinv_targspp, Spp_Code %in% species)}

  motinv_year <- filter(motinv_spp, Year %in% years)

  motinv_qaqc <- if(QAQC == TRUE){motinv_year
  } else {filter(motinv_year, QAQC == FALSE)}

  motinv_comb <- left_join(bolts |> select(Site_Code, Loc_Code, Plot_Name, Target_Species,
                                           Bolt_UTM_E, Bolt_UTM_N, Bolt_MLLW_Elev) |> unique(),
                           motinv_qaqc, by = c("Site_Code", "Loc_Code", "Plot_Name", "Target_Species"))

  motinv_final <- motinv_comb |>
    select(Site_Name, Site_Code, State_Code, Loc_Name, Loc_Code, Start_Date, Year, QAQC,
           Target_Species, Plot_Name, Spp_Name, Spp_Code, Damage, No.Damage, Subsampled,
           Event_ID, Plot_ID, Bolt_UTM_E, Bolt_UTM_N, Bolt_MLLW_Elev)

  if(nrow(motinv_final) == 0){
    stop("Specified arguments returned an empty data frame. If filtering on species, make sure the code was spelled correctly.")}

  return(motinv_final)


}
