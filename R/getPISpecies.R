#' @title getPISpecies: get point intercept species detection data
#'
#' @importFrom dplyr filter mutate select
#'
#' @description This function filters point intercept species detection data by park,
#' location, plot name, and species.
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
#' @param plotName Filter on plot name. Options include: c("all", "T1", "T2", and "T3")
#'
#' @param species Filter on species code. Options include:
#' c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
#' "BOLT", "CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR",
#' "OTHINV", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "ULVENT", "ULVINT", "ULVLAC",
#' "UNIDEN", "WATER"). If a new species is added, the function will warn the user
#' that an unrecognized species was specified in case it was an error.
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
#' spp <- getPISpecies()
#'
#' # Species detections for ACAD only sites
#' spp <- getPISpecies(park = "ACAD")
#'
#' # Species detections for specific sites, plots, species, and years
#'
#' spp_t3 <- getPISpecies(park = "ACAD", plotName = "T3")
#' spp_BOHA2 <- getPISpecies(location = c("CALISL", "GREISL"))
#' spp_fuc <- getPISpecies(park = "BOHA", species = c("FUCEPI", "FUCSPP"))
#' spp_5yr <- getPISpecies(years = 2016:2021)
#' spp_first_last <- getPISpecies(years = c(2013, 2021))
#' spp21_with_qaqc <- getPISpecies(years = 2021, QAQC = TRUE)
#'
#' }
#'
#'
#' @return Returns a data frame of point intercept species detection data filtered by function arguments
#' @export

getPISpecies <- function(park = "all", location = "all", plotName = "all",
                               species = "all", years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                               QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))

  unmatch_spp <- setdiff(species, c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
                                    "BOLT", "CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR",
                                    "OTHINV", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "ULVENT", "ULVINT", "ULVLAC",
                                    "UNIDEN", "WATER"))

  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }

  stopifnot(plotName %in% c("all", "T1", "T2", "T3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(sppdet <- get("PointIntercept_SppDetections", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(Start_Date, "%Y"))),
           error = function(e){stop("PointIntercept_SppDetections data frame not found. Please import rocky intertidal data.")})

  sppdet_park <- if(any(park %in% 'all')){ filter(sppdet, Site_Code %in% c("ACAD", "BOHA"))
  } else {filter(sppdet, Site_Code %in% park)}

  sppdet_loc <- if(any(location %in% 'all')){ sppdet_park
  } else {filter(sppdet_park, Loc_Code %in% location)}

  sppdet_pname <- if(any(plotName %in% 'all')){ sppdet_loc
  } else {filter(sppdet_loc, Plot_Name %in% plotName)}

  sppdet_species <- if(any(species %in% 'all')){ sppdet_pname
  } else {filter(sppdet_pname, Spp_Code %in% species)}

  sppdet_year <- filter(sppdet_species, Year %in% years)

  sppdet_qaqc <- if(QAQC == TRUE){sppdet_year
    } else {filter(sppdet_year, QAQC == FALSE)}

  sppdet_final <- sppdet_qaqc |>
    select(Site_Name, Site_Code, Loc_Name, Loc_Code, Start_Date, Year, QAQC, Plot_Name,
           PI_Distance, Spp_Code, Spp_Name, Event_ID, Plot_ID)

  if(nrow(sppdet_final) == 0){stop("Specified arguments returned an empty data frame.")}

  return(sppdet_final)


}
