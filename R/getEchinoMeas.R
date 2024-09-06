#' @title getEchinoMeas: get Echinoderm measurement data
#'
#' @importFrom dplyr filter mutate select
#'
#' @description This function filters echinoderm measurement data by park, site, plot name, and species. The returned data frame has a row for each species measured per sampling event and plot.
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
#' @param plotName Filter on plot name. Options include: c("X1", "X2", and "X3")
#'
#' @param species Filter on species code. Options include:
#' c("all", "ASTFOR", "ASTRUB", "HENSAN", "STRDRO")
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
#' ech <- getEchinoMeas()
#'
#' # Echino counts for ACAD only sites
#' ech_acad <- getEchinoMeas(park = "ACAD")
#'
#' # Echino counts for specific sites, plots, species, and years
#'
#' ech_t3 <- getEchinoMeas(park = "ACAD", plotName = "X3")
#' ech_BOHA2 <- getEchinoMeas(site = c("CALISL", "GREISL"))
#' ech_5yr <- getEchinoMeas(years = 2016:2021)
#' ech_first_last <- getEchinoMeas(years = c(2013, 2021))
#' ech21_qaqc <- getEchinoMeas(years = 2021, QAQC = TRUE)
#' ech21_ASTFOR <- getEchinoMeas(species = "ASTFOR")
#'
#' }
#'
#'
#' @return Returns a data frame of echinoderm measurement data filtered by function arguments.
#' @export

getEchinoMeas <- function(park = "all", site = "all", plotName = "all",
                          species = 'all', years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                          QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "X1", "X2", "X3"))

  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

  spp_list <- c("all", "ASTFOR", "ASTRUB", "HENSAN", "STRDRO")

  unmatch_spp <- setdiff(species, c(spp_list, NA))

  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }


  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  tryCatch(echino <- get("Echinoderm_Measurements", envir = env) |>
             dplyr::mutate(Year = as.numeric(format(StartDate, "%Y"))),
           error = function(e){stop("Echinoderm_Measurments data frame not found. Please import rocky intertidal data.")})

  echino_park <- if(any(park %in% 'all')){ filter(echino, UnitCode %in% c("ACAD", "BOHA"))
  } else {filter(echino, UnitCode %in% park)}

  echino_loc <- if(any(site %in% 'all')){ echino_park
  } else {filter(echino_park, SiteCode %in% site)}

  echino_pname <- if(any(plotName %in% 'all')){ echino_loc
  } else {filter(echino_loc, PlotName %in% plotName)}

  echino_species <- if(any(species %in% 'all')){ echino_pname
  } else {filter(echino_pname, SpeciesCode %in% species)}

  echino_year <- filter(echino_species, Year %in% years)

  echino_qaqc <- if(QAQC == TRUE){echino_year
  } else {filter(echino_year, QAQC == FALSE)}

  echino_final <- echino_qaqc |>
    select(GroupCode, GroupName, UnitCode, UnitName, SiteCode, SiteName, StartDate, Year, QAQC,
           PlotName, ScientificName, CommonName, SpeciesCode, Measurement, IsPointCUI)

  if(nrow(echino_final) == 0){stop("Specified arguments returned an empty data frame.")}

  return(echino_final)


}
