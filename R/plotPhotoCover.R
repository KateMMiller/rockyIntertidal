#' @title plotPhotoCover: plots average cover by species and year
#'
#' @include sumPhotoCover.R
#'
#' @import ggplot2
#' @importFrom dplyr arrange group_by slice_max summarize
#'
#' @description This function plots median percent cover by species for a given park, location, years and
#' target species. The point for each species is the median cover across the photoplots that site, year
#' and target species. The error bars are the minimum and maximum cover recorded in photoplots for a target
#' species. Note that if more than 1 location is specified, more than one target species is specified, or both,
#' the resulting figure will facet on those variables.
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
#' @param category Filter on category. Options include:
#' c("all", "Genus", "Species", "Species Group", and "Substrate")
#'
#' @param target_species Filter on target species (ie photoplot). Options include:
#' c("Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae")
#'
#' @param top_spp Integer. If specified, only plots the top n species by cover for each target species. If not
#' specified, plots all species, or only the species specified in the species argument. Note that species ties are
#' included, so could include more than the number specified. Species with less than 0.1% total cover are also
#' dropped when a top_spp is specififed.
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
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns a plot faceted on location and target species group
#' plotPhotoCover()
#'
#' # Other variations
#' spp = c("ALGRED", "ASCNOD", "BARSPP", "NONCOR", "FUCSPP", "ULVLAC")
#' plotPhotoCover(location = "CALISL", palette = "default", title = FALSE,
#'                species = spp)
#'
#' # Return only the top 3 species for barnacle photo plots
#' plotPhotoCover(location = "SHIHAR", palette = "default",
#'                target_species = "Barnacle", top_spp = 3)
#'
#' }
#'
#'
#' @return Returns a ggplot object of percent cover from photos filtered by function arguments
#' @export

plotPhotoCover <- function(park = "all", location = "all", plotName = "all",
                           species = "all", category = "all", target_species = 'all',
                           top_spp = NULL, palette = c('default'),
                           xlab = "Year", ylab = "% Cover",
                           years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                           plot_title = NULL, QAQC = FALSE, drop_missing = TRUE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "T1", "T2", "T3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(palette %in% c("default", "viridis"))
  stopifnot(category %in% c("all", "Genus", "Species", "Species Group", "Substrate"))
  stopifnot(target_species %in% c('all', "Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae"))
  stopifnot(is.numeric(top_spp))

  unmatch_spp <- setdiff(species, c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
                                    "CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR", "NOSAMP",
                                    "OTHINV", "OTHPLA", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "SAND", "TAR",
                                    "ULVINT", "ULVLAC", "UNIDEN"))

  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }

  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.

  # create color palette by species code

  cols = c("ALGBRO" = "#A4755B", "ALGGRE" = "#C4E133", "ALGRED" = "#FF4C53", "ARTCOR" = "#D78AAE",
           "ASCEPI" = "#85733B", "ASCNOD" = "#C5B47B", "BARSPP" = "#A9A9A9", "CHOMAS" = "#772C27",
           "CRUCOR" = "#F9F5A1", "NONCOR" = "#9565C9","FUCEPI" = "#D5A82A", "FUCSPP" = "#FFD560",
           "KELP"   = "#4DA551", "MUSSPP" = "#6F88BF", "OTHINV" = "#F59617", "OTHSUB" = "#8A838A",
           "PALPAL" = "#5E5571", "PORSPP" = "#8E3B4A", "ULVENT" = "#699052", "ULVINT" = "#9FCF87",
           "ULVLAC" = "#73EB31", "UNIDEN" = "#696969", "BOLT"   = "#EAEAEA", "ROCK"   = "#FED5FF",
           "WATER"  = "#7FC7E1", "SAND" = "#CACACA", "TAR" = "#111111")

  shps = c("ALGBRO" = 21, "ALGGRE" = 23, "ALGRED" = 24, "ARTCOR" = 25,
           "ASCEPI" = 21, "ASCNOD" = 23, "BARSPP" = 24, "CHOMAS" = 25,
           "CRUCOR" = 21, "NONCOR" = 23,"FUCEPI" = 24, "FUCSPP" = 25,
           "KELP"   = 21, "MUSSPP" = 23, "OTHINV" = 24, "OTHSUB" = 25,
           "PALPAL" = 21, "PORSPP" = 23, "ULVENT" = 24, "ULVINT" = 25,
           "ULVLAC" = 21, "UNIDEN" = 23, "BOLT"   = 24, "ROCK"   = 25,
           "WATER"  = 21, "SAND" = 23, "TAR" = 24)

  sz = c("ALGBRO" = 4, "ALGGRE" = 3.5, "ALGRED" = 3, "ARTCOR" = 3,
         "ASCEPI" = 4, "ASCNOD" = 3.5, "BARSPP" = 3, "CHOMAS" = 3,
         "CRUCOR" = 4, "NONCOR" = 3.5,"FUCEPI" = 3, "FUCSPP" = 3,
         "KELP"   = 4, "MUSSPP" = 3.5, "OTHINV" = 3, "OTHSUB" = 3,
         "PALPAL" = 4, "PORSPP" = 3.5, "ULVENT" = 3, "ULVINT" = 3,
         "ULVLAC" = 4, "UNIDEN" = 3.5, "BOLT"   = 3, "ROCK"   = 3,
         "WATER"  = 4, "SAND" = 3.5, "TAR" = 3)

  labels = c("ALGBRO" = "Algae - Brown", "ALGGRE" = "Algae - Green", "ALGRED" = "Algae - Red",
             "ARTCOR" = "Articulated Corallines", "ASCEPI" = "Ascophyllum epibiont",
             "ASCNOD" = "Ascophyllum nodosum (Knotted wrack)", "CHOMAS" = "Irish moss",
             "CRUCOR" = "Crustose coraline", "NONCOR" = "Crustose non-coraline",
             "BARSPP" = "Barnacles",
             "FUCEPI" = "Fucus epibiont", "FUCSPP" = "Fucus spp. (Rockweed)",
             "KELP"   = "Kelp", "MUSSPP" = "Mussels",
             "OTHINV" = "Other invertebrates", "OTHSUB" = "Other substrate",
             "PALPAL" = "Dulse", "PORSPP" = "Laver", "ULVENT" = "Ulva/Enteromorpha",
             "ULVINT" = "Ulva intestinalis (Grass kelp)", "ULVLAC" = "Ulva lactuca (Sea lettuce)",
             "UNIDEN" = "Unidentified", "BOLT" = "Bolt", "ROCK" = "Rock", "WATER"  = "Water",
             "SAND" = "Sand", "TAR" = "Tar")

  loc_labs <- c("BASHAR" = "Bass Harbor", "LITHUN" = "Little Hunter", "LITMOO" = "Little Moose",
                "OTTPOI" = "Otter Point", "SCHPOI" = "Schoodic Point", "SHIHAR" = "Ship Harbor",
                "CALISL" = "Calf Island", "GREISL" = "Green Island", "OUTBRE" = "Outer Brewster")

  dat1 <- suppressWarnings(force(sumPhotoCover(park = park, location = location, plotName = plotName,
                                              category = category, years = years, QAQC = QAQC,
                                              species = species, target_species = target_species))) |>
    filter(!is.na(median_cover))

  if(!is.null(top_spp)){
    # Create df of top 4 species per target_species by cover
    top4_df <- dat1 |>
      group_by(Site_Code, Loc_Code, Target_Species, Spp_Code) |>
      summarize(tot_cov = sum(median_cover, na.rm = TRUE), .groups = 'drop') |>
      ungroup() |>
      group_by(Site_Code, Loc_Code, Target_Species) |>
      slice_max(order_by = tot_cov, n = top_spp) |> ungroup() |>
      filter(tot_cov > 0.1) |>
      unique()

    # left join dat 1 top 4 to drop less abundant species
    dat <- left_join(top4_df, dat1, by = c("Site_Code", "Loc_Code", "Target_Species", "Spp_Code"))
    } else {dat <- dat1}

  # This is all to make NONCOR species name sort alphabetically with Cs
  sppcode <- c("ALGBRO", "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD",
               "BARSPP", "CHOMAS", "CRUCOR", "NONCOR", "FUCEPI", "FUCSPP", "KELP",
               "MUSSPP", "OTHINV", "OTHSUB", "PALPAL", "PORSPP", "ULVENT", "ULVINT",
               "ULVLAC", "UNIDEN", "BOLT", "ROCK", "WATER")


  dat$Spp_Code <- factor(dat$Spp_Code, levels = sppcode) |> droplevels()
  spp <- levels(dat$Spp_Code)
  dat$Target_Species <- factor(dat$Target_Species, levels = c("Barnacle", "Mussel", "Fucus", "Ascophyllum", "Red Algae"))

  facet_loc_cat <- if(length(unique(dat$Loc_Code)) > 1 & length(unique(dat$Target_Species)) > 1) {TRUE} else {FALSE}
  facet_loc <- if(length(unique(dat$Loc_Code)) > 1 & length(unique(dat$Target_Species)) == 1) {TRUE} else {FALSE}
  facet_targ <- if(length(unique(dat$Loc_Code)) == 1 & length(unique(dat$Target_Species)) > 1) {TRUE} else {FALSE}

  p <-
  ggplot(dat, aes(x = Year, y = median_cover,  color = Spp_Code, fill = Spp_Code,
                  shape = Spp_Code, size = Spp_Code)) +
         geom_errorbar(aes(ymin = min_cover, ymax = max_cover), linewidth = 1) +
         geom_line(aes(x = Year, y = avg_cover), linewidth = 1) +
         geom_point(aes(x = Year, y = avg_cover, fill = Spp_Code, size = Spp_Code), color = 'black') +
         scale_shape_manual(values = shps, name = "Species", breaks = names(shps),
                            labels = labels) +
         scale_size_manual(values = sz, name = "Species", breaks = names(sz),
                           labels = labels) +
         {if(all(palette == 'default'))
           scale_color_manual(values = cols, name = "Species",
                              breaks = names(cols), labels = labels)} +
         {if(all(palette == 'default'))
           scale_fill_manual(values = cols, name = "Species",
                              breaks = names(cols), labels = labels)} +
         {if(all(palette == 'viridis')) scale_color_viridis_d("Species")}+
         {if(facet_loc_cat == TRUE) facet_wrap(~Target_Species + Loc_Code)} +
         {if(facet_loc == TRUE) facet_wrap(~Loc_Code, labeller = as_labeller(loc_labs))} +
         {if(facet_targ == TRUE) facet_wrap(~Target_Species, labeller = as_labeller(labels))} +
         scale_x_continuous(breaks = c(unique(dat$Year)))+
         #coord_flip() +
         theme_rocky() +
         labs(y = ylab, x = xlab, title = plot_title)+
         theme(legend.position = 'bottom',
               axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

  return(p)

}
