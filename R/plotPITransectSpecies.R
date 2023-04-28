#' @title plotPITransectSpecies: plots species detections by transect distance and elevation
#'
#' @include plotPITransects.R
#'
#' @import ggplot2
#'
#' @description This function plots species detections by bolt distance and elevation data by transect for each park,
#' location, and year.
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
#' @param plotName Filter on plot name (transect). Options include: c("all", "T1", "T2", and "T3")
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
#' @param QAQC Logical. If FALSE (Default), does not return QAQC events. If TRUE,
#' returns all events, including QAQC events.
#'
#' @param drop_missing Logical. If TRUE (Default), drops bolts with missing
#' elevation and distances and drops bolts that are not bolt 1 but have a distance of 0.
#'
#' @param xlab Quoted text label for x axis. If not specified, defaults to 'Distance (m)'
#'
#' @param ylab Quoted text label for y axis. If not specified, defaults to 'Elevation MLLW (m)'
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns all records
#' plotPITransects()
#'
#' # Other variations
#' plotPITransects(years = 2018:2021)
#' plotPITransects(park = "ACAD", years = 2019)
#' plotPITransects(location = "GREISL", xlab = 'dist', ylab = 'elev')
#' plotPITransects(location = "BASHAR", plotName = "T1", drop_missing = FALSE)
#'
#' }
#'
#'
#' @return Returns a data frame of point intercept species detection data filtered by function arguments
#' @export

plotPITransectSpecies <- function(park = "all", location = "all", plotName = "all",
                            species = "all", xlab = "Distance (m)", ylab = "Elevation MLLW (m)",
                            years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                            QAQC = FALSE, drop_missing = TRUE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))

  stopifnot(plotName %in% c("all", "T1", "T2", "T3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)

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

  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.

  dat <- force(sumPISppDetections(park = park, location = location, plotName = plotName,
                                  years = years, QAQC = QAQC, drop_missing = drop_missing,
                                  species = species)) |>
    filter(!is.na(elev_pi))

  # Summary stats for species elevations at site level (not transect)
  dat_sum <- dat |> group_by(Site_Code, Loc_Code, Year, QAQC, Spp_Code) |>
    summarize(elev_min = min(elev_pi),
              elev_max = max(elev_pi),
              elev_med = median(elev_pi),
              elev_l95 = quantile(elev_pi, probs = 0.025),
              elev_u95 = quantile(elev_pi, probs = 0.975),
              .groups = 'drop')

  #head(dat_sum)

  #p <-
    ggplot(dat_sum, aes(x = as.factor(Year), y = elev_med,
                        group = as.factor(Spp_Code), color = as.factor(Spp_Code))) +
       geom_point(position = position_dodge(width = 1)) +
       geom_errorbar(aes(ymin = elev_l95, ymax = elev_u95), position = position_dodge(width = 1)) +
       scale_y_reverse() +
       coord_flip() +
       scale_color_brewer("Species", type = 'div', palette = 'Dark2') +
       #facet_wrap(~Loc_Code) +
       theme_rocky() +
       labs(y = "Elevation MLLW (m)", x = "Year")


       #labs(x = xlab, y = ylab) +
       # {if(length(unique(dat$Plot_Name)) > 1 & length(unique(dat$Loc_Code)) > 1)
       #     facet_wrap(~Loc_Code + Plot_Name + Year)} +
       # {if(length(unique(dat$Plot_Name)) == 1 & length(unique(dat$Loc_Code)) > 1)
       #     facet_wrap(~Loc_Code + Year)}+
       # {if(length(unique(dat$Plot_Name)) > 1 & length(unique(dat$Loc_Code)) == 1)
       #     facet_wrap(~Plot_Name + Year)}+
       # scale_color_brewer("Year", type = 'div', palette = 'Dark2') +
       # {if(length(unique(dat$Year)) == 1)
       #     theme(legend.position = 'none')} +

  p
  return(p)

}
