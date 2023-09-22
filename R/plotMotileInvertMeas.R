#' @title plotMotileInvertMeas: plots a heatmap of motile invertebrates by species, location and size class
#'
#' @include sumPhotoCover.R
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
#' @param plotName Filter on plot name (transect). Options include: c("all", "T1", "T2", and "T3")
#'
#' @param species Filter on species code. Options include:
#' c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
#' "CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR", "NOSAMP",
#' "OTHINV", "OTHPLA", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "SAND", "TAR",
#' "ULVINT", "ULVLAC", "UNIDEN"). If a new species is added, the function will warn the user
#' that an unrecognized species was specified in case it was an error.
#' If a new species is added, the function will warn the user that an unrecognized species was specified
#' in case it was an error. The default color palette will also need to be updated, if a new species is added.
#' The viridis palette will work with new species without adaptation, but labels will only be species codes.
#'
#' @param target_species Filter on target species (ie photoplot). Options include:
#' c("Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae")
#'
#' @param QAQC Logical. If FALSE (Default), does not return QAQC events. If TRUE,
#' returns all events, including QAQC events.
#'
#' @param palette Choices are "default" or "viridis". Default assigns logical colors to common species.
#' Viridis uses a color-blind friendly palette of blues, purples and yellows.
#'
#' @param xlab Quoted text label for x axis. If not specified, defaults to 'Year'
#'
#' @param ylab Quoted text label for y axis. If not specified, defaults to '% Cover'
#'
#' @param plot_title If specified, plots the title on the figure. If NULL, no plot title included.
#'
#' @param nrow Number of rows in the heatmap facet. Default is 1. Only used when 1 site is selected and heatmap = T.
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
                           species = 'all', category = "all", target_species = 'all',
                           palette = c('default'),
                           xlab = "Year", ylab = "Measurement (mm)",
                           years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                           plotly = F, nrow = 1,
                           plot_title = NULL, QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "T1", "T2", "T3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(palette %in% c("default", "viridis"))
  stopifnot(category %in% c("all", "Genus", "Species", "Species Group", "Substrate"))
  stopifnot(target_species %in% c('all', "Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae"))
  stopifnot(is.numeric(top_spp) | is.null(top_spp))
  stopifnot(is.logical(heatmap))
  stopifnot(is.logical(plotly))
  stopifnot(is.logical(xaxis))
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
               "LITLIT" = "Common periwinkle (Littorina littorea)",
               "LITOBT" = "Smooth periwinkle (Littorina obtusata)",
               "LITSAX" = "Rough periwinkle (Littorina saxatilis)",
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


  dat$Target_Species <- factor(dat$Target_Species, levels = c("Barnacle", "Mussel", "Fucus", "Ascophyllum", "Red Algae"))

  facet_loc_cat <- if(length(unique(dat$Loc_Code)) > 1 & length(unique(dat$Target_Species)) > 1) {TRUE} else {FALSE}
  facet_loc <- if(length(unique(dat$Loc_Code)) > 1 & length(unique(dat$Target_Species)) == 1) {TRUE} else {FALSE}
  facet_targ <- if(length(unique(dat$Loc_Code)) == 1 & length(unique(dat$Target_Species)) > 1) {TRUE} else {FALSE}


  p <-
      ggplot(dat, aes(x = Year, y = as.factor(Measurement),
                      color = num_meas, fill = num_meas, group = Spp_Code)) +
        geom_tile(color = '#9F9F9F') +
#        geom_text(aes(label = ifelse(num_meas > 0, round(num_meas, 0), NA)), color = 'black')+
        #scale_y_discrete(limits = rev, labels = labels)+
        {if(facet_loc_cat == TRUE) facet_wrap(~Target_Species + Loc_Code,
                                              labeller = as_labeller(c(targ_labs, loc_labs)))} +
        {if(facet_targ == TRUE) facet_wrap(~Target_Species + Spp_Code, drop = T,
                                           labeller = as_labeller(c(targ_labs, labels)))} +
        scale_fill_gradient(low = "white", high = "#5969B0", guide = 'legend',
                            name = "Number of Measurments") +
        scale_x_continuous(breaks = c(unique(dat$Year)))+
        ylab(NULL) +
        theme_rocky() +
        theme(legend.position = 'bottom')


  if(plotly == TRUE){
    pp <-
      plotly::ggplotly(p, tooltip = 'text', layerData = 1, originalData = F)
    #++++ ENDED HERE ++++
    # spp_mat <- unique(dat_nz[, c("Spp_Code", "Spp_Name")])
    # #--- Simplify plotly traces in legend ---
    # # Get the names of the legend entries
    # pdf <- data.frame(id = seq_along(pp$x$data),
    #                   legend_entries = unlist(lapply(pp$x$data, `[[`, "name")))
    # # Extract the group identifier
    # pdf$legend_group <- substr(gsub("[^A-Za-z///]", "", pdf$legend_entries), 1, 6)
    # # Determine the points based on max number of chars
    # max_char <- max(nchar(pdf$legend_entries))
    # pdf$points <- ifelse(nchar(pdf$legend_entries) == max_char, TRUE, FALSE)
    # # Add an indicator for the first entry per group
    # pdf$is_first1 <- !duplicated(pdf$legend_group[pdf$points == TRUE])
    # pdf$is_first <- ifelse(pdf$is_first1 == TRUE & pdf$points == TRUE, TRUE, FALSE)
    # pdf <- dplyr::left_join(pdf, spp_mat, by = c("legend_entries" = "Spp_Code"))
    #
    # for (i in seq_along(pdf$id)) {
    #   # Is the layer the first entry of the group?
    #   is_first <- pdf$is_first[[i]]
    #   # Assign the group identifier to the name and legendgroup arguments
    #   pp$x$data[[i]]$name <- pdf$Spp_Name[[i]]
    #   pp$x$data[[i]]$legendgroup <- pp$x$data[[i]]$name
    #   # Show the legend only for the first layer of the group
    #   if (!is_first) pp$x$data[[i]]$showlegend <- FALSE
    # }
    #
  } else {pp <- p}


  suppressWarnings(pp)

}
