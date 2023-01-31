#' @title filterBolts: filter bolt data by park, location, plot type, or plot name
#'
#' @importFrom dplyr filter mutate select
#'
#' @description This function filter bolt data by park, location, plot type or plot name.
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
#' \item{"LILTHUN"}{Little Hunter, ACAD}
#' \item{"LITMOO"}{Little Moose, ACAD}
#' \item{"OTTPOI"}{Otter Point, ACAD}
#' \item{"SCHPOI"}{Schoodic Point, ACAD}
#' \item{"SHIHAR"}{Ship Harbor, ACAD}
#' \item{"CALISL"}{Calf Island, BOHA}
#' \item{"GRISL"}{Green Island, BOHA}
#' \item{"OUTBRE"}{Outer Brewster}
#' }
#'
#' @param plotType Type of plot to filter
#' \describe{
#' \item{"all"}{Include bolt locations for all plot types}
#' \item{"band"}{Band transect bolt locations}
#' \item{"benchmark"}{Benchmark bolt locations}
#' \item{"photo"}{Photoplot bolt locations}
#' \item{"pi_transect"}{Bolt locations for point intercept transects}
#' \item{"recruitment"}{Bolt locations for recruitment plots}
#' \item{"reference"}{Reference bolt locations}
#' \item{"temp_logger"}{Bolt locations for temperature loggers}
#'}
#'
#' @param species Filter on target species
#' \describe{
#' \item{"all"}{Include bolt locations for all target species}
#' \item{"ASCO"}{Include only Ascophyllum-related bolt locations}
#' \item{"BARN"}{Include only Barnacle-related bolt locations}
#' \item{"ECHI"}{Include only Echinoderm-related bolt locations}
#' \item{"FUCU"}{Include only Fucus-related bolt locations}
#' \item{"MUSS"}{Include only Mussel-related bolt locations}
#' \item{"REDA"}{Include only Red Algae-related bolt locations}
#' \item{"NOTAPP"}{Include only bolt locations that are not associated with a target species}
#' }
#'
#' @param plotName Filter on plot name. Options include:
#'   c("A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5", "BM",
#'     "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
#'     "R1", "R2", "R3", "R4", "R5", "REF", "S1", "S2", "S3", "S4", "S5",
#'     "T1", "T2", "T3",
#'     "TEMP1", "TEMP1 ", "TEMP2", "TEMP3",
#'     "U1", "U2", "U3", "U4", "U5", "X1", "X2", "X3")
#'
#' @examples
#' \dontrun{
#'
#' #+++++ ADD EXAMPLES +++++
#' # Make sure they include vectors of locs and plots so obvious you can select multiple
#'
#' }
#'
#'
#' @return Returns a data frame of bolt data filtered by function arguments
#' @export

filterBolts <- function(park = c("all", "ACAD", "BOHA"),
                        location = c("all","BASHAR", "LILTHUN", "LITMOO", "OTTPOI",
                                     "SCHPOI", "SHIHAR", "CALISL", "GRISL", "OUTBRE"),
                        plotType = c("all", "band", "benchmark", "photo", "pi_transect",
                                     "recruitment", "reference", "temp_logger"),
                        species = c("all", "ASCO", "BARN", "ECHI", "FUCU", "MUSS", "REDA", "NOTAPP"),
                        plotName = c("all", "A1", "A2", "A3", "A4", "A5",
                                     "B1", "B2", "B3", "B4", "B5", "BM",
                                     "F1", "F2", "F3", "F4", "F5",
                                     "M1", "M2", "M3", "M4", "M5",
                                     "R1", "R2", "R3", "R4", "R5", "REF",
                                     "S1", "S2", "S3", "S4", "S5",
                                     "T1", "T2", "T3",
                                     "TEMP1", "TEMP1 ", "TEMP2", "TEMP3",
                                     "U1", "U2", "U3", "U4", "U5",
                                     "X1", "X2", "X3")
                        ){



  # Match args and class
  park <- match.arg(park, several.ok = TRUE)
  location <- match.arg(location)
  plotType <- match.arg(plotType, several.ok = TRUE)
  species <- match.arg(species, several.ok = TRUE)
  plotName <- match.arg(plotName, several.ok = TRUE)

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(bolts <- get("Bolts", envir = env) |>
             mutate(ParkCode = ifelse(Site_Name == "Acadia NP", "ACAD", "BOHA")),
           error = function(e){stop("Bolts data frame not found. Please import rocky intertidal data.")})


  # Fix warning message about only the first element being used.
  bolts_park <- if(park != 'all'){
    filter(bolts, ParkCode %in% park)
  } else {bolts}

  bolts_loc <- if(location != 'all'){
    filter(bolts_park, Loc_Code %in% location)
  } else {bolts_park}

  bolts_plottype <- if(plotType != 'all'){
    filter(bolts_loc, Plot_Type %in% plotType)
  } else {bolts_loc}

  bolts_species <- if(species != 'all'){
    filter(bolts_plottype, Target_Species %in% species)
  } else {bolts_plottype}

  bolts_pname <- if(plotName != 'all'){
    filter(bolts_species, Plot_Name %in% plotName)
  } else {bolts_species}

  return(bolts_pname)

}
