#' @title sumMotileInvertMeas: summarize Motile Invertebrate measurement data by species and 5 mm size classes
#'
#' @include getMotileInvertMeas.R
#'
#' @importFrom dplyr filter mutate rename select
#'
#' @description This function summarizes number of individuals in each 5 mm size class by park, location, and species. Spp_Code codes in the data set are: CARMAE = Carcinus maenas (green crab); HEMISAN = Hemigrapsus sanguineus (Asian shore crab); LITLIT = Littorina littorea (common periwinkle); LITOBT = Littorina obtusata (smooth periwinkle); LITSAX = Littorina saxatilis (rough periwinkle); NUCLAP = Nucella lapillus (dogwhelk), TECTES = Tectura testudinalis (limpet).
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
#' minv <- sumMotileInvertMeas()
#'
#' # Motile Invert measurement data for ACAD only sites
#' minv_acad <- sumMotileInvertMeas(park = "ACAD")
#'
#' # Motile Invert measurement data for specific sites, plots, species, and years
#'
#' minv_r <- sumMotileInvertMeas(park = "ACAD", plotName = c("R1", "R2", "R3", "R4", "R5"))
#' minv_BOHA2 <- sumMotileInvertMeas(location = c("CALISL", "GREISL"))
#' minv_lit <- sumMotileInvertMeas(species = c("LITLIT", "LITOBT", "LITSAX"))
#' minv_5yr <- sumMotileInvertMeas(years = 2016:2021)
#' minv_first_last <- sumMotileInvertMeas(years = c(2013, 2021))
#' minv21_qaqc <- sumMotileInvertMeas(years = 2021, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of summarized motile invertebrate measurement data filtered by function arguments.
#' @export

sumMotileInvertMeas <- function(park = "all", location = "all", plotName = "all",
                                years = 2013:as.numeric(format(Sys.Date(), "%Y")), QAQC = FALSE,
                                species = 'all', target_species = 'all'){

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

  motinv <- force(getMotileInvertMeas(park = park, location = location, plotName = plotName,
                                      species = species, target_species = target_species,
                                      years = years, QAQC = QAQC)) |>
    select(Site_Name, Site_Code, Loc_Name, Loc_Code, Start_Date, Year, QAQC,
           Target_Species, Plot_Name, Spp_Name, Spp_Code, Measurement) |>
    arrange(Measurement)

  motinv$Meas_5mm <- floor(motinv$Measurement/5)*5

  #table(motinv$Measurement)

  if(any(motinv$Measurement) > 99.9){warning("Motile Invertebrate length measured over 99.9mm. Function only built to handle lengths up to 99.9mm. If the large measurement is correct, update the measurement factor levels")}
  motinv$Meas_5mm_fac <- factor(paste0(motinv$Meas_5mm, " to ", motinv$Meas_5mm + 4.9),
                                levels = c("0 to 4.9", "5 to 9.9",
                                           "10 to 14.9", "15 to 19.9",
                                           "20 to 24.9", "25 to 29.9",
                                           "30 to 34.9", "35 to 39.9",
                                           "40 to 44.9", "45 to 49.9",
                                           "50 to 54.9", "55 to 59.9",
                                           "60 to 64.9", "65 to 69.9",
                                           "70 to 74.9", "75 to 79.9",
                                           "80 to 84.9", "85 to 89.9",
                                           "90 to 94.9", "95 to 99.9"))

  motinv_sum <- motinv |> group_by(Site_Name, Site_Code, Loc_Name, Loc_Code, Start_Date, Year,
                                   QAQC, Target_Species, Spp_Code, Spp_Name, Meas_5mm_fac) |>
    summarize(num_meas = sum(!is.na(Measurement)), .groups = 'drop')

  return(motinv_sum)


}
