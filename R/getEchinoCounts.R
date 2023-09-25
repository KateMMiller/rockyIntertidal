#' @title getEchinoCounts: get Echinoderm count data
#'
#' @importFrom dplyr filter left_join mutate select
#' @importFrom tidyr pivot_longer
#'
#' @description This function filters echinoderm count data by park, location, and plot name. The returned
#' data frame is long, rather than wide, which is how it's imported. If a given species was not detected
#' during a visit, its count is 0.
#' Species codes are: ASTFOR = Asterias forbesi (Northern sea star); ASTRUB = Asterias rubens (common sea star);
#' HENSAN = Henricia sanguinolenta (blood sea star); STRDRO = Strongylocentrotus droebachiensis (sea urchin)
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
#' @param plotName Filter on plot name. Options include: c("all", "X1", "X2", and "X3")
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
#' ech <- getEchinoCounts()
#'
#' # Echino counts for ACAD only sites
#' ech_acad <- getEchinoCounts(park = "ACAD")
#'
#' # Echino counts for specific sites, plots, species, and years
#'
#' ech_t3 <- getEchinoCounts(park = "ACAD", plotName = "X3")
#' ech_BOHA2 <- getEchinoCounts(location = c("CALISL", "GREISL"))
#' ech_5yr <- getEchinoCounts(years = 2016:2021)
#' ech_first_last <- getEchinoCounts(years = c(2013, 2021))
#' ech21_qaqc <- getEchinoCounts(years = 2021, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of echinoderm count data in wide form filtered by function arguments.
#' @export

getEchinoCounts <- function(park = "all", location = "all", plotName = "all",
                            species = "all", QAQC = FALSE,
                            years = 2013:as.numeric(format(Sys.Date(), "%Y"))){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "X1", "X2", "X3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  unmatch_spp <- setdiff(species, c("all", "ASTFOR", "ASTRUB", "HENSAN", "STRDRO"))
  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(echino <- get("Echinoderm_Counts", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(Start_Date, "%Y"))),
           error = function(e){
             stop("Echinoderm_Counts data frame not found. Please import rocky intertidal data.")})

  echino_park <- if(any(park %in% 'all')){ filter(echino, Site_Code %in% c("ACAD", "BOHA"))
  } else {filter(echino, Site_Code %in% park)}

  echino_loc <- if(any(location %in% 'all')){ echino_park
  } else {filter(echino_park, Loc_Code %in% location)}

  echino_pname <- if(any(plotName %in% 'all')){ echino_loc
  } else {filter(echino_loc, Plot_Name %in% plotName)}

  echino_year <- filter(echino_pname, Year %in% years)

  echino_qaqc <- if(QAQC == TRUE){echino_year
  } else {filter(echino_year, QAQC == FALSE)}

  echino2 <- echino_qaqc |>
    select(Site_Name, Site_Code, State_Code, Loc_Name, Loc_Code, Start_Date, Year, QAQC,
           Target_Species, Plot_Name, ASTFOR = Count_ASTFOR, ASTRUB = Count_ASTRUB,
           HENSAN = Count_HENSAN, STRDRO = Count_STRDRO, Event_ID, Plot_ID)

  echino_long <- echino2 |> pivot_longer(cols = ASTFOR:STRDRO,
                                         names_to = "Spp_Code", values_to = "Count")

  spp_mat <- data.frame(Spp_Code = c("ASTFOR", "ASTRUB", "HENSAN", "STRDRO"),
                        Spp_Name = c("Asterias forbesi (Northern sea star)",
                                     "Asterias rubens (common sea star)",
                                     "Henricia sanguinolenta (blood sea star)",
                                     "Strongylocentrotus droebachiensis (sea urchin)"))

  echino_long2 <- left_join(echino_long, spp_mat, by = "Spp_Code")

  echino_final <- if(species %in% 'all'){echino_long2
    } else {filter(echino_long2, Spp_Code %in% species)}

  if(nrow(echino_final) == 0){stop("Specified arguments returned an empty data frame.")}

  return(echino_final)
}
