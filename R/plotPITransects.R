#' @title plotPITransects: plots transect distance by elevation by location, year and transect
#'
#' @include sumPISppDetections.R
#'
#' @import ggplot2
#'
#' @description This function relates bolt elevation data with point intercept species detection data by park,
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
#' trans <- sumPItransDetections()
#'
#' plotPITransects(park = "ACAD", location = "BASHAR", years = 2019)
#'
#' }
#'
#'
#' @return Returns a data frame of point intercept species detection data filtered by function arguments
#' @export

plotPITransects <- function(park = "all", location = "all", plotName = "all",
                            xlab = "Distance (m)", ylab = "Elevation MLLW (m)",
                            years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                            QAQC = FALSE, drop_missing = TRUE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))

  stopifnot(plotName %in% c("all", "T1", "T2", "T3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.

  dat <- force(sumPISppDetections(park = park, location = location, plotName = plotName,
                                  years = years, QAQC = QAQC, drop_missing = drop_missing))

  p <- ggplot(dat, aes(x = dist_bolt_first, y = elev_first,
                       group = as.factor(Year), color = as.factor(Year))) +
       geom_line(lwd = 1) +
       labs(x = xlab, y = ylab) +
       {if(length(unique(dat$Plot_Name)) > 1 & length(unique(dat$Loc_Code)) > 1)
           facet_wrap(~Loc_Code + Plot_Name)} +
       {if(length(unique(dat$Plot_Name)) == 1 & length(unique(dat$Loc_Code)) > 1)
           facet_wrap(~Loc_Code)}+
       {if(length(unique(dat$Plot_Name)) > 1 & length(unique(dat$Loc_Code)) == 1)
           facet_wrap(~Plot_Name)}+
       scale_color_brewer("Year", type = 'div', palette = 'Dark2') +
       {if(length(unique(dat$Year)) == 1)
           theme(legend.position = 'none')} +
       theme_rocky()

  return(p)

  }
