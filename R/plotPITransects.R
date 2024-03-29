#' @title plotPITransects: plots transect distance by elevation by location, year and transect
#'
#' @include sumPISpecies.R
#'
#' @import ggplot2
#'
#' @description This function plots bolt distance and elevation data by transect for each park,
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
#' @param facet_scales Quoted options are "fixed", "free", "free_y", or "free_x", and controls whether axes in
#' the facets are all the same, or different (free).
#'
#' @param title If TRUE (Default) prints the full site name on the figure. If FALSE, does not
#' include plot title.
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
#' plotPITransects(park = "ACAD", years = 2019, facet_scales = 'fixed')
#' plotPITransects(location = "GREISL", xlab = 'dist', ylab = 'elev')
#' plotPITransects(location = "BASHAR", plotName = "T1", drop_missing = FALSE)
#'
#' }
#'
#'
#' @return Returns a ggplot object of point intercept species detection data filtered by function arguments
#' @export

plotPITransects <- function(park = "all", location = "all", plotName = "all",
                            xlab = "Distance (m)", ylab = "Elevation MLLW (m)",
                            years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                            title = TRUE,
                            QAQC = FALSE, drop_missing = TRUE, facet_scales = "free"){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))

  stopifnot(plotName %in% c("all", "T1", "T2", "T3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.

  dat <- suppressWarnings(force(sumPISpecies(park = park, location = location, plotName = plotName,
                                  years = years, QAQC = QAQC, drop_missing = drop_missing)) |>
         select(Site_Code:Label, Elevation_MLLW_m, Distance_m) |> unique()) # only concerned with bolts, not spp pi in fxn

  ptitle <- ifelse(all(title == TRUE) & length(location) == 1 & location != "all", unique(dat$Loc_Name), "")

  p <- ggplot(dat, aes(x = Distance_m, y = Elevation_MLLW_m,
                       group = as.factor(Year), color = as.factor(Year))) +
       geom_line(lwd = 1) +
       labs(x = xlab, y = ylab, title = ptitle) +
       {if(length(unique(dat$Plot_Name)) > 1 & length(unique(dat$Loc_Code)) > 1)
           facet_wrap(~Loc_Code + Plot_Name, scales = facet_scales)} +
       {if(length(unique(dat$Plot_Name)) == 1 & length(unique(dat$Loc_Code)) > 1)
           facet_wrap(~Loc_Code, scales = facet_scales)}+
       {if(length(unique(dat$Plot_Name)) > 1 & length(unique(dat$Loc_Code)) == 1)
           facet_wrap(~Plot_Name, scales = facet_scales)}+
       scale_color_brewer("Year", type = 'div', palette = 'Dark2') +
       {if(length(unique(dat$Year)) == 1)
           theme(legend.position = 'none')} +
       theme_rocky()

  return(p)

  }
