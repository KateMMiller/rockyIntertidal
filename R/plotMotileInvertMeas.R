#' @title plotMotileInvertMeas: plots a heatmap of motile invertebrates by species, location and size class
#'
#' @include sumMotileInvertMeas.R
#'
#' @import ggplot2
#' @importFrom plotly ggplotly
#'
#' @description This function plots a heatmap of the distribution of invertebrate size classes in 1 mm increments
#' by year for each specified location, target species photoplot, and species.
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
#' @param years Filter on year of data collected. Default is 2013 to current year.
#'
#' @param plotName Filter on plot name (transect). Options include:
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
#' plotMotileInvertMeas(location = "CALISL", palette = "default", title = FALSE,
#'                       species = "LITLIT")
#'
#'
#' }
#'
#'
#' @return Returns a ggplot object of number of records by measurment by year per species
#' @export

plotMotileInvertMeas <- function(park = "all", location = "all", plotName = "all",
                           species = 'all', target_species = 'all',
                           xlab = "Year", ylab = "Length (mm)",
                           years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                           nrow = 1,
                           plot_title = NULL, QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5",
                            "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
                            "R1", "R2", "R3", "R4", "R5"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(target_species %in% c('all', "Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae"))

  stopifnot(is.numeric(nrow) | is.integer(nrow))

  spp_list <- c("all", "CARMAE", "HEMISAN", "LITLIT",  "LITOBT", "LITSAX", "NUCLAP", "TECTES")

  unmatch_spp <- setdiff(species, c(spp_list, NA))

  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }

  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.


  # create color palette by species code

  cols = c("CARMAE" = "#875e41", "HEMISAN" = "#7E9144",
           "LITLIT" = "#606b7b",  "LITOBT" = "#d7b74c", "LITSAX" = "#d38d5d",
           "NUCLAP" = "#88a6bf", "TECTES" = "#7F2729")

  cols2 = c("CARMAE" = "#D8C6B8", "HEMISAN" = "#D5E89B",
           "LITLIT" = "#BBD1F1",  "LITOBT" = "#F9E6A8", "LITSAX" = "#FCDCC6",
           "NUCLAP" = "#D9EEFF", "TECTES" = "#FFC6C7")

  shps = c("CARMAE" = 21, "HEMISAN" = 23, "LITLIT" = 21,  "LITOBT" = 25,
           "LITSAX" = 23, "NUCLAP" = 24, "TECTES" = 25)

  sz = c("CARMAE" = 4, "HEMISAN" = 3.5, "LITLIT" = 4,  "LITOBT" = 3,
         "LITSAX" = 3.5, "NUCLAP" = 3, "TECTES" = 3)

  labels = c(c("CARMAE" = "Green crab (Carcinus maenas)",
               "HEMISAN" = "Asian shore crab (H. sanguineus)",
               "LITLIT" = "Common periwinkle (L. littorea)",
               "LITOBT" = "Smooth periwinkle (L. obtusata)",
               "LITSAX" = "Rough periwinkle (L. saxatilis)",
               "NUCLAP" = "Dogwhelk (Nucella lapillus)",
               "TECTES" = "Limpet (Tectura testudinalis)"))

  targ_labs <-  c("Ascophyllum" = "A. nodosum (knotted wrack)",
                  "Barnacle" = "Barnacle",
                  "Fucus" = "Fucus spp. (Rockweed)",
                  "Mussel" = "Mussels",
                  "Red Algae" = "Red algae group")

  loc_labs <- c("BASHAR" = "Bass Harbor", "LITHUN" = "Little Hunter", "LITMOO" = "Little Moose",
                "OTTPOI" = "Otter Point", "SCHPOI" = "Schoodic Point", "SHIHAR" = "Ship Harbor",
                "CALISL" = "Calf Island", "GREISL" = "Green Island", "OUTBRE" = "Outer Brewster")

  dat <- suppressWarnings(force(sumMotileInvertMeas(park = park, location = location, plotName = plotName,
                                                     years = years, QAQC = QAQC,
                                               species = species, target_species = target_species))) |>
    dplyr::filter(!is.na(num_meas))


  # expand to include all size classes, so see full grid in geom_tile
  grid_full <- expand.grid(Site_Code = unique(dat$Site_Code), Loc_Code = unique(dat$Loc_Code),
                           Year = unique(dat$Year), Target_Species = unique(dat$Target_Species),
                           Spp_Code = unique(dat$Spp_Code), #Spp_Name = unique(dat$Spp_Name),
                           Meas_5mm_fac = unique(dat$Meas_5mm_fac))

  dat_full <- left_join(grid_full, dat, by = c("Site_Code", "Loc_Code", "Year",
                                               "Target_Species", "Spp_Code", "Meas_5mm_fac"))
  dat_full$Target_Species <- factor(dat_full$Target_Species, levels = c("Barnacle", "Mussel", "Fucus", "Ascophyllum", "Red Algae"))

  facet_loc_cat <- if(length(unique(dat_full$Loc_Code)) > 1 & length(unique(dat_full$Target_Species)) > 1) {TRUE} else {FALSE}
  facet_loc <- if(length(unique(dat_full$Loc_Code)) > 1 & length(unique(dat_full$Target_Species)) == 1) {TRUE} else {FALSE}
  facet_targ <- if(length(unique(dat_full$Loc_Code)) == 1 & length(unique(dat_full$Target_Species)) > 1) {TRUE} else {FALSE}

  dat_full$num_meas[dat_full$num_meas == 0] <- NA_real_ # Change 0 to NA, so plots white
  table(dat_full$num_meas, useNA = 'always')

  dat_full$text_col <- ifelse(dat_full$num_meas < 20, 'low', 'high')

  text_pal <- c("low" = "black", "high" = "white", `NA_real_` = "white")
#  tile_maxlim = max(ceiling(dat$num_meas/5)*5, na.rm = T)
#  max(dat$num_meas)
  # table(dat_full$num_meas, useNA = 'always')
  # head(dat_full)

  spp_cols <- length(unique(dat_full$Spp_Code))

  p <-
      ggplot(dat_full, aes(x = Year, y = Meas_5mm_fac,
                      color = text_col,
                      fill = num_meas, group = Meas_5mm_fac)) +
        geom_tile(color = '#9F9F9F') +
        geom_text(aes(label = ifelse(!is.na(num_meas) & num_meas > 0,
                                     round(num_meas, 0), NA), color = text_col))+
        scale_color_manual(values = text_pal, drop = FALSE) +
        #scale_y_discrete(limits = rev, labels = labels)+
        {if(facet_loc_cat == TRUE) facet_wrap(~Target_Species + Loc_Code,
                                              labeller = as_labeller(c(targ_labs, loc_labs)), ncol = spp_cols)} +
        {if(facet_targ == TRUE) facet_wrap(~Target_Species + Spp_Code, drop = T, ncol = spp_cols,
                                           labeller = as_labeller(c(targ_labs, labels)))} +
        scale_fill_gradientn(colors = c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4",
                                        "#1D91C0", "#225EA8", "#253494", "#081D58"),
                             #c(rev(viridis::viridis(length(seq(0, 100, 5))))),
                             guide = 'colorbar',
                             name = "Number of Measurments",
                             na.value = 'white') + #,
        scale_x_continuous(breaks = c(unique(dat$Year)))+
        labs(y = ylab, x = xlab, title = plot_title) +
        theme_rocky() +
        theme(legend.position = 'bottom',
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        guides(color = "none")

  suppressWarnings(p)

}
