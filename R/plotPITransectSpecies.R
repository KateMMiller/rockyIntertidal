#' @title plotPITransectSpecies: plots species detections by transect distance and elevation
#'
#' @include plotPITransects.R
#'
#' @import ggplot2
#'
#' @description This function plots species by bolt elevation a given park, location, and years.
#' The point for each species is the median elevation across the three transects for that year.
#' The thicker lines on the error bars are the 25% and 75% quartiles of elevation across the transects.
#' The thinner error bars that end with vertical lines are the minimum and maximum elevation detected
#' along the three transects.
#'
#' @param park Include data from all parks, or choose one.
#' \describe{
#' \item{'all'}{Includes all parks monitored in the network}
#' \item{'ACAD'}{Includes only sites in Acadia National Park}
#' \item{'BOHA'}{Includes only sites in Boston Harbor Islands National Recreation Area}
#' }
#'
#' @param location Include data from all locations, or choose specific locations based on location code.
#' Can only specify one location for each function call.
#'
#' @param years Filter on year of data collected. Default is 2013 to current year.
#'
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
#' that an unrecognized species was specified in case it was an error. The default
#' color palette will also need to be updated, if a new species is added. The viridis palette
#' will work with new species without adaptation, but labels will only be species codes.
#'
#' @param palette Choices are "default" or "viridis". Default assigns logical colors to common species.
#' Viridis uses a color-blind friendly palette of blues, purples and yellows.
#'
#' @param facet Logical. If TRUE, will plot species in separate facets. FALSE (default) plots all species
#' on one figure.
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
#' @param title If TRUE (Default) prints the full site name on the figure. If FALSE, does not
#' include plot title.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns a plot for BASHAR (have to pick a site)
#' plotPITransectSpecies()
#'
#' # Other variations
#' spp = c("ALGRED", "ASCNOD", "BARSPP", "NONCOR", "FUCSPP", "ULVLAC")
#' plotPITransectSpecies(location = "CALISL", palette = "default", title = FALSE, facet = TRUE,
#'                       species = spp)
#'
#' plotPITransectSpecies(location = "SHIHAR", palette = "default",
#'                       facet = FALSE, species = c("BARSPP"))
#'
#' }
#'
#'
#' @return Returns a ggplot object of point intercept species detection data filtered by function arguments
#' @export

plotPITransectSpecies <- function(park = "all", location = "BASHAR", plotName = "all",
                            species = "all", palette = c('default'),
                            xlab = "Distance (m)", ylab = "Elevation MLLW (m)",
                            years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                            facet = FALSE, title = TRUE,
                            QAQC = FALSE, drop_missing = TRUE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "T1", "T2", "T3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(palette %in% c("default", "viridis"))

  unmatch_spp <- setdiff(species, c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
                                    "BOLT", "CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR",
                                    "OTHINV", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "ULVENT", "ULVINT", "ULVLAC",
                                    "UNIDEN", "WATER"))

  if(length(location) > 1){
    stop('Must only specify one location or one year, cannot plot multiple locations and years.')}

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
           "KELP"   = "#4DA551", "MUSSPP" = "#6F88BF", "OTHINV" = "#F59617", "OTHSUB" = "#000000",
           "PALPAL" = "#5E5571", "PORSPP" = "#8E3B4A", "ULVENT" = "#699052", "ULVINT" = "#9FCF87",
           "ULVLAC" = "#73EB31", "UNIDEN" = "#696969", "BOLT"   = "#EAEAEA", "ROCK"   = "#FED5FF",
           "WATER"  = "#7FC7E1")

  labels = c("ALGBRO" = "Algae - Brown", "ALGGRE" = "Algae - Green", "ALGRED" = "Algae - Red",
             "ARTCOR" = "Articulated Corallines", "ASCEPI" = "Ascophyllum epibiont",
             "ASCNOD" = "Ascophyllum nodosum (Knotted wrack)", "CHOMAS" = "Irish moss",
             "CRUCOR" = "Crustose coraline", "NONCOR" = "Crustose non-coraline",
             "BARSPP" = "Barnacles",
             "FUCEPI" = "Fucus epibiont", "FUCSPP" = "Fucus spp. (Rockweed)",
             "KELP"   = "Kelp", "MUSSPP" = "Muscles",
             "OTHINV" = "Other invertebrates", "OTHSUB" = "Other substrate",
             "PALPAL" = "Dulse", "PORSPP" = "Laver", "ULVENT" = "Ulva/Enteromorpha",
             "ULVINT" = "Ulva intestinalis (Grass kelp)", "ULVLAC" = "Ulva lactuca (Sea lettuce)",
             "UNIDEN" = "Unidentified", "BOLT"   = "Bolt", "ROCK"   = "Rock", "WATER"  = "Water")

  dat <- suppressWarnings(force(sumPISppDetections(park = park, location = location, plotName = plotName,
                                  years = years, QAQC = QAQC, drop_missing = drop_missing,
                                  species = species))) |>
    filter(!is.na(PI_Elevation))

  ptitle <- ifelse(title == TRUE, unique(dat$Loc_Name), "")

  # Summary stats for species elevations at site level (not transect)
  dat_sum <- dat |> group_by(Site_Code, Loc_Code, Year, QAQC, Spp_Code, Spp_Name) |>
    summarize(elev_min = min(PI_Elevation, na.rm = T),
              elev_max = max(PI_Elevation, na.rm = T),
              elev_med = median(PI_Elevation, na.rm = T),
              elev_l95 = quantile(PI_Elevation, probs = 0.025, na.rm = T),
              elev_u95 = quantile(PI_Elevation, probs = 0.975, na.rm = T),
              elev_l25 = quantile(PI_Elevation, probs = 0.25, na.rm = T),
              elev_u75 = quantile(PI_Elevation, probs = 0.75, na.rm = T),
              .groups = 'drop')

  # This is all to make NONCOR species name sort alphabetically with Cs
  sppcode <- c("ALGBRO", "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD",
               "BARSPP", "CHOMAS", "CRUCOR", "NONCOR", "FUCEPI", "FUCSPP", "KELP",
               "MUSSPP", "OTHINV", "OTHSUB", "PALPAL", "PORSPP", "ULVENT", "ULVINT",
               "ULVLAC", "UNIDEN", "BOLT", "ROCK", "WATER")

  dat_sum$Spp_Code <- factor(dat_sum$Spp_Code, levels = sppcode) |> droplevels()
  spp <- levels(dat_sum$Spp_Code)

  # cols <- cols[spp]
  # labels <- labels[spp]

  p <-
  ggplot(dat_sum, aes(x = Year, y = desc(elev_max),
                        group = Spp_Code, color = Spp_Code, fill = Spp_Code)) +
         geom_errorbar(aes(ymin = elev_l25, ymax = elev_u75),
                       position = position_dodge(width = 1), width = 0, linewidth = 1.5) +
         geom_errorbar(aes(ymin = elev_min, ymax = elev_max),
                      position = position_dodge(width = 1), linewidth = 0.5) +
         geom_point(aes(x = Year, y = elev_med),
                    position = position_dodge(width = 1), size = 3, shape = 21, color = 'black') +
         {if(all(palette == 'default'))
           scale_color_manual(values = cols, name = "Species",
                              breaks = names(cols), labels = labels)} +
         {if(all(palette == 'default'))
           scale_fill_manual(values = cols, name = "Species",
                              breaks = names(cols), labels = labels)} +
         {if(all(palette == 'viridis')) scale_color_viridis_d("Species")}+
         {if(facet == TRUE) facet_wrap(~Spp_Code)} +
         scale_y_reverse(limits = c(max(dat_sum$elev_max), min(dat_sum$elev_min))) +
         scale_x_reverse(breaks = c(unique(dat_sum$Year)))+
         coord_flip() +
         theme_rocky() +
         labs(y = ylab, x = xlab, title = ptitle)

  return(p)

}
