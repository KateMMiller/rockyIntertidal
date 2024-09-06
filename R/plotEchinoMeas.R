#' @title plotEchinoMeas: plots a heatmap of echinoderm by species, site and size class
#'
#' @include sumEchinoMeas.R
#'
#' @import ggplot2
#' @importFrom plotly ggplotly
#'
#' @description This function plots a heatmap of the distribution of invertebrate size classes in 1 mm increments
#' by year for each specified site, target species photoplot, and species.
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
#' @param years Filter on year of data collected. Default is 2013 to current year.
#'
#' @param plotName Filter on plot name (transect). Options include:
#'   c("all", "X1", "X2", "X3")
#'
#' @param species Filter on species code. Options include:
#' c("all", "ASTFOR", "ASTRUB", "HENSAN", "STRDRO"). If a new species is added,
#' the function will warn the user that an unrecognized species was specified in case it was an error.
#'
#' @param QAQC Logical. If FALSE (Default), does not return QAQC events. If TRUE,
#' returns all events, including QAQC events.
#'
#' @param xlab Quoted text label for x axis. If not specified, defaults to 'Year'
#'
#' @param ylab Quoted text label for y axis. If not specified, defaults to 'Length (mm)'
#'
#' @param plot_title If specified, plots the title on the figure. If NULL, no plot title included.
#'
#' @param nrow Number of rows in the heatmap facet. Default is 1. Only used when 1 site is selected.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' plotMotileInvertMeas(park = "ACAD", species = c("CARMAE", "HEMISAN"))
#'
#' plotMotileInvertMeas(site = "CALISL", palette = "default", title = FALSE,
#'                       species = "LITLIT")
#'
#'
#' }
#'
#'
#' @return Returns a ggplot object of number of records by measurment by year per species
#' @export

plotEchinoMeas <- function(park = "all", site = "all", plotName = "all",
                           species = 'all',
                           xlab = "Year", ylab = "Length (mm)",
                           years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                           nrow = 1,
                           plot_title = NULL, QAQC = FALSE){


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

  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.


  # create color palette by species code


  cols = c("ASTFOR" = "#F5CF81", "ASTRUB" = "#B16597", "HENSAN" = "#DB4223", "STRDRO" = "#A9C476")

  shps = c("ASTFOR" = 21, "ASTRUB" = 23, "HENSAN" = 24, "STRDRO" = 25)

  sz = c("ASTFOR" = 4, "ASTRUB" = 4, "HENSAN" = 3, "STRDRO" = 3)

  labels <- c("ASTFOR" = "Asterias forbesi (Northern sea star)",
              "ASTRUB" = "Asterias rubens (common sea star)",
              "HENSAN" = "Henricia sanguinolenta (blood sea star)",
              "STRDRO" = "S. droebachiensis (sea urchin)")

  loc_labs <- c("BASHAR" = "Bass Harbor", "LITHUN" = "Little Hunter", "LITMOO" = "Little Moose",
                "OTTPOI" = "Otter Point", "SCHPOI" = "Schoodic Point", "SHIHAR" = "Ship Harbor",
                "CALISL" = "Calf Island", "GREISL" = "Green Island", "OUTBRE" = "Outer Brewster")

  dat <- suppressWarnings(force(sumEchinoMeas(park = park, site = site, plotName = plotName,
                                               years = years, QAQC = QAQC, species = species))) |>
    dplyr::filter(!is.na(num_meas))

  # expand to include all size classes, so see full grid in geom_tile
  grid_full <- expand.grid(UnitCode = unique(dat$UnitCode), SiteCode = unique(dat$SiteCode),
                           Year = unique(dat$Year),
                           SpeciesCode = unique(dat$SpeciesCode), #Spp_Name = unique(dat$Spp_Name),
                           Meas_5mm_fac = unique(dat$Meas_5mm_fac))

  dat_full <- left_join(grid_full, dat, by = c("UnitCode", "SiteCode", "Year",
                                               "SpeciesCode", "Meas_5mm_fac"))

  facet_loc_spp <- if(length(unique(dat$SiteCode)) > 1 &
                      length(unique(dat$SpeciesCode))>1) {TRUE} else {FALSE}

  facet_loc <- if(length(unique(dat$SiteCode)) > 1 &
                      length(unique(dat$SpeciesCode)) == 1) {TRUE} else {FALSE}

  facet_spp <- if(length(unique(dat$SiteCode)) == 1 &
                  length(unique(dat$SpeciesCode)) > 1) {TRUE} else {FALSE}


  dat_full$num_meas[dat_full$num_meas == 0] <- NA_real_ # Change 0 to NA, so plots white
  #table(dat_full$num_meas, useNA = 'always')

  dat_full$text_col <- ifelse(dat_full$num_meas < 20, 'low', 'high')

  text_pal <- c("low" = "black", "high" = "white", `NA_real_` = "white")

  spp_cols <- length(unique(dat_full$SpeciesCode))

  dat_full <- dat_full |> filter(!is.na(Meas_5mm_fac))

  #head(dat_full)
  p <-
      ggplot(dat_full, aes(x = Year, y = Meas_5mm_fac,
                      color = text_col,
                      fill = num_meas, group = Meas_5mm_fac)) +
        geom_tile(color = '#9F9F9F') +
        geom_text(aes(label = ifelse(!is.na(num_meas) & num_meas > 0,
                                     round(num_meas, 0), NA), color = text_col))+
        scale_color_manual(values = text_pal, drop = TRUE) +
        #scale_y_discrete(limits = rev, labels = labels)+
        {if(facet_loc_spp == TRUE) facet_wrap(~SiteCode + SpeciesCode,
                                              labeller = as_labeller(c(loc_labs, labels)),
                                      ncol = spp_cols)} +
        {if(facet_loc == TRUE) facet_wrap(~SiteCode, labeller = as_labeller(c(loc_labs)),
                                          ncol = spp_cols)} +
        {if(facet_spp == TRUE) facet_wrap(~SpeciesCode,
                                          labeller = as_labeller(labels),
                                          ncol = spp_cols)} +
        scale_fill_gradientn(colors = c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4",
                                        "#1D91C0", "#225EA8", "#253494", "#081D58"),
                             #c(rev(viridis::viridis(length(seq(0, 100, 5))))),
                             guide = 'colorbar',
                             name = "Number of Measurments",
                             na.value = 'white') + #,
        scale_x_continuous(breaks = c(unique(dat_full$Year)))+
        labs(y = ylab, x = xlab, title = plot_title) +
        theme_rocky() +
        theme(legend.position = 'bottom',
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        guides(color = "none")

  suppressWarnings(p)

}
