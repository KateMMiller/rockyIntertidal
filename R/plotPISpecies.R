#' @title plotPISpecies: plots species detections by elevation and year for individual sites.
#'
#' @include sumPISpecies.R
#' @include theme_rocky.R
#'
#' @import ggplot2
#' @importFrom dplyr desc filter group_by left_join mutate summarize ungroup
#' @importFrom plotly ggplotly
#'
#' @description This function plots species by bolt elevation a given park, site, and years.
#' The point for each species is the median elevation across the three transects for that year. The
#' ribbon represents upper 75% and lower 25% elevation recorded for a species within a given year.
#' The thicker lines on the error bars are the 25% and 75% quantiles of elevation across the transects.
#' The thinner error bars that end with vertical lines are the minimum and maximum elevation detected along
#' the three transects.
#'
#' @param park Include data from all parks, or choose one.
#' \describe{
#' \item{'all'}{Includes all parks monitored in the network}
#' \item{'ACAD'}{Includes only sites in Acadia National Park}
#' \item{'BOHA'}{Includes only sites in Boston Harbor Islands National Recreation Area}
#' }
#'
#' @param site Choose specific site based on site code.
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
#'
#' @param years Filter on year of data collected. Default is 2013 to current year.
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
#' @param main_groups Logical. If TRUE, only plots red algae (combined Irish moss and red algae group), Fucus spp.,
#' Ascophyllum nodosum (epibionts included), mussels, barnacles, and Crustose non-coraline. If FALSE (Default),
#' plots all species or only species specified. If species are specified, this argument will be ignored.
#'
#' @param palette Choices are "default" or "viridis". Default assigns logical colors to common species.
#' Viridis uses a color-blind friendly palette of blues, purples and yellows.
#'
#' @param ribbon Logical. If TRUE plots a min/max ribbon around the median elevation for an individual species.
#' If FALSE (default), plots points and error bars for each species. Ribbon only works if multiple years are plotted.
#'
#' @param plotly Logical. If TRUE, converts ggplot object to plotly object and includes tooltips. If FALSE (default),
#' plots a ggplot object.
#'
#' @param facet Logical. If TRUE, will plot species in separate facets. FALSE (default) plots all species
#' on one figure.
#'
#' @param rev_axis Logical. If TRUE (default), the Y axis will be year and the x axis will be elevation. If
#' FALSE, the Y axis will be elevation and x will be year.
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
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns a plot for BASHAR (have to pick a site)
#' plotPISpecies()
#'
#' # Other variations
#' spp = c("ALGRED", "ASCNOD", "BARSPP", "NONCOR", "FUCSPP", "ULVLAC")
#' plotPISpecies(site = "CALISL", palette = "default", title = FALSE, facet = TRUE,
#'                       species = spp)
#'
#' plotPISpecies(site = "SHIHAR", palette = "default",
#'                       facet = FALSE, species = c("BARSPP"))
#'
#' }
#'
#'
#' @return Returns a ggplot object of point intercept species detection data filtered by function arguments
#' @export

plotPISpecies <- function(park = "all", site = "all", plotName = "all",
                                  species = NA, palette = c('default'), ribbon = FALSE,
                                  main_groups = FALSE, plotly = FALSE,
                                  xlab = "Year", ylab = "Elevation MLLW (m)",
                                  years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                                  facet = FALSE, title = TRUE, rev_axis = TRUE,
                                  QAQC = FALSE, drop_missing = TRUE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "T1", "T2", "T3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(palette %in% c("default", "viridis"))
  stopifnot(is.logical(facet))
  stopifnot(is.logical(ribbon))
  stopifnot(is.logical(plotly))

  if(!requireNamespace("plotly", quietly = TRUE) & plotly == TRUE){
    stop("Package 'plotly' needed for this function for plotly = TRUE. Please install it or set plotly = FALSE.", call. = FALSE)
  }

  if(length(years) == 1 & ribbon == TRUE){
    warning("Must specify at least 2 years to plot a ribbon. Setting ribbon = FALSE and plotting error bars.")
    ribbon = FALSE}

  spp_list <- c("all", "ALGBRO",  "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD", "BARSPP",
                "BOLT", "CHOMAS", "CRUCOR", "FUCEPI", "FUCSPP", "KELP", "MUSSPP", "NONCOR",
                "OTHINV", "OTHSUB", "PALPAL", "PORSPP", "ROCK", "ULVENT", "ULVINT", "ULVLAC",
                "UNIDEN", "WATER")

  if(all(species %in% spp_list) & main_groups == TRUE){
    warning("Can't specify species and set main_groups to TRUE. Only plotting the 4 main species groups.")
  }

  unmatch_spp <- setdiff(species, c(spp_list, NA))

  # if(length(site) > 1){
  #   stop('Must only specify one site or one year, cannot plot multiple sites and years.')}

  if(length(unmatch_spp) > 0){
    warning(paste0("Unrecognized species were specified in the species argument: ",
                   paste0(unmatch_spp, collapse = ", "),
                   "\n",
                   "Check that this wasn't a typo."))
  }

  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.

  if(all(is.na(species)) & main_groups == TRUE){
    species = c("ASCNOD",  "ASCEPI", "BARSPP",
                "FUCSPP", "FUCEPI", "NONCOR",
                "MUSSPP", "ALGRED", "CHOMAS")
  }

  if(all(is.na(species))){species = 'all'}

  # if(all(is.na(species)) & main_groups == FALSE){
  #   warning("Species not specified. Plotting all species.")
  #   species <- 'all'
  # }

  if(all(is.na(species)) & main_groups == TRUE){
    species = c("ASCNOD",  "ASCEPI", "BARSPP",
                "FUCSPP", "FUCEPI", "NONCOR",
                "MUSSPP", "ALGRED", "CHOMAS")
  }


  # create color palette by species code

  cols = c("ALGBRO" = "#A4755B", "ALGGRE" = "#C4E133", "ALGRED" = "#FF4C53", "ARTCOR" = "#D78AAE",
           "ASCEPI" = "#85733B", "ASCNOD" = "#C5B47B", "BARSPP" = "#A9A9A9", "CHOMAS" = "#772C27",
           "CRUCOR" = "#F9F5A1", "NONCOR" = "#574F91","FUCEPI" = "#D5A82A", "FUCSPP" = "#FFD560",
           "KELP"   = "#4DA551", "MUSSPP" = "#6F88BF", "OTHINV" = "#F59617", "OTHSUB" = "#8A838A",
           "PALPAL" = "#5E5571", "PORSPP" = "#8E3B4A", "ULVENT" = "#699052", "ULVINT" = "#9FCF87",
           "ULVLAC" = "#73EB31", "UNIDEN" = "#696969", "BOLT"   = "#EAEAEA", "ROCK"   = "#FED5FF",
           "WATER"  = "#7FC7E1", "REDGRP" = "#FF4C53")

  shps = c("ALGBRO" = 21, "ALGGRE" = 23, "ALGRED" = 24, "ARTCOR" = 25,
           "ASCEPI" = 21, "ASCNOD" = 23, "BARSPP" = 24, "CHOMAS" = 25,
           "CRUCOR" = 21, "NONCOR" = 23,"FUCEPI" = 24, "FUCSPP" = 25,
           "KELP"   = 21, "MUSSPP" = 23, "OTHINV" = 24, "OTHSUB" = 25,
           "PALPAL" = 21, "PORSPP" = 23, "ULVENT" = 24, "ULVINT" = 25,
           "ULVLAC" = 21, "UNIDEN" = 23, "BOLT"   = 24, "ROCK"   = 25,
           "WATER"  = 21, "REDGRP" = 25)

  sz = c("ALGBRO" = 3, "ALGGRE" = 2.5, "ALGRED" = 2, "ARTCOR" = 2,
         "ASCEPI" = 3, "ASCNOD" = 2.5, "BARSPP" = 2, "CHOMAS" = 2,
         "CRUCOR" = 3, "NONCOR" = 2.5,"FUCEPI" = 2, "FUCSPP" = 2,
         "KELP"   = 3, "MUSSPP" = 2.5, "OTHINV" = 2, "OTHSUB" = 2,
         "PALPAL" = 3, "PORSPP" = 2.5, "ULVENT" = 2, "ULVINT" = 2,
         "ULVLAC" = 3, "UNIDEN" = 2.5, "BOLT"   = 2, "ROCK"   = 2,
         "WATER"  = 3, "REDGRP" = 2)


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
             "UNIDEN" = "Unidentified", "BOLT"   = "Bolt", "ROCK"   = "Rock", "WATER"  = "Water",
             "REDGRP" = "Red algae group")

  loc_labs <- c("BASHAR" = "Bass Harbor", "LITHUN" = "Little Hunter", "LITMOO" = "Little Moose",
                "OTTPOI" = "Otter Point", "SCHPOI" = "Schoodic Point", "SHIHAR" = "Ship Harbor",
                "CALISL" = "Calf Island", "GREISL" = "Green Island", "OUTBRE" = "Outer Brewster")

  dat1 <- suppressWarnings(
    force(sumPISpecies(park = park, site = site, plotName = plotName,
                       years = years, QAQC = QAQC, drop_missing = drop_missing,
                       species = species)) |>
          dplyr::filter(!is.na(PI_Elevation)))

  dat <-
  if(main_groups == TRUE){
    dat1 |> dplyr::mutate(CoverCode = case_when(CoverCode %in% c("ALGRED", "CHOMAS") ~ "REDGRP",
                                                CoverCode %in% c("FUCSPP", "FUCEPI") ~ "FUCSPP",
                                                CoverCode %in% c("ASCNOD", "ASCEPI") ~ "ASCNOD",
                                                TRUE ~ CoverCode),
                          CoverType = case_when(CoverCode %in% "REDGRP" ~ "Red algae group",
                                               CoverCode %in% "FUCSPP" ~ "Fucus spp. (Rockweed)",
                                               CoverCode %in% "ASCNOD" ~ "A. nodosum (knotted wrack)",
                                               TRUE ~ CoverType)) |>
            dplyr::filter(CoverCode %in% c("REDGRP", "ASCNOD", "FUCSPP", "BARSPP", "MUSSPP", "NONCOR"))
  } else {dat1}

  ptitle <- ifelse(title == TRUE, unique(dat$SiteName), "")

  # Summary stats for species elevations at site level (not transect)
  dat_sum <- dat |> group_by(UnitCode, SiteCode, Year, QAQC, CoverCode, CoverType) |>
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
               "ULVLAC", "UNIDEN", "BOLT", "ROCK", "WATER", "REDGRP")

  dat_sum$CoverCode <- factor(dat_sum$CoverCode, levels = sppcode) |> droplevels()
  spp <- levels(dat_sum$CoverCode)

  # Ordering sites from west to east
  dat_sum$SiteCode <- factor(dat_sum$SiteCode,
                             levels = c("CALISL", "GREISL", "OUTBRE",
                                        "BASHAR", "SHIHAR", "LITHUN",
                                        "OTTPOI", "SCHPOI", "LITMOO"))
  # cols <- cols[spp]
  # labels <- labels[spp]

  locs <-
    if(all(site == 'all')){c("BASHAR", "LITHUN", "LITMOO", "OTTPOI", "SCHPOI",
                                 "SHIHAR", "CALISL", "GREISL", "OUTBRE")
    } else {site}

  facet_loc_spp <- if(length(locs) > 1 & length(unique(dat$CoverCode)) > 1 & ribbon == FALSE) {TRUE} else {FALSE}
  facet_loc_ribbon <- if(length(locs) > 1 & ribbon == TRUE) {TRUE} else {FALSE}
  facet_loc <- if(length(locs) > 1 & length(unique(dat$CoverCode)) == 1) {TRUE} else {FALSE}
  facet_spp <- if(facet_loc == FALSE & facet_loc_spp == FALSE & facet == TRUE) {TRUE} else {FALSE}

  leg_position <- ifelse(any(facet_loc_spp, facet_loc, facet_spp) == TRUE, 'none', 'right')

  p <- suppressWarnings( #suppress tooltip warning
  ggplot(dat_sum, aes(x = Year, y = elev_med, #desc(elev_max),
                      group = CoverCode, color = CoverCode, fill = CoverCode,
                      shape = CoverCode)) +
         {if(ribbon == TRUE) geom_ribbon(aes(ymax = elev_u75, ymin = elev_l25, #group = CoverCode,
                                             #color = CoverCode, fill = CoverCode,
                                             text = paste0("Upper 75% and lower 25% elev.", "<br>",
                                                           "Species: ", CoverType, "<br>")),
                                         alpha = 0.2, linewidth = 0.5)}+
         {if(ribbon == TRUE) geom_line(aes(x = Year, y = elev_med, #group = CoverCode,
                                           #linewidth = 0.5,
                                           #color = CoverCode,
                                           #fill = CoverCode,
                                           text = paste0("Median elev.", "<br>",
                                                         "Species: ", CoverType, "<br>")),
                                       linewidth = 0.5)} +
         {if(ribbon == TRUE) geom_point(aes(x = Year, y = elev_med, #fill = CoverCode, color = CoverCode,
                                            size = CoverCode, #shape = CoverCode, group = CoverCode,
                                            text = paste0("Median elevation: ", round(elev_med, 2), "<br>",
                                                          "Species: ", CoverType, "<br>",
                                                          "Year: ", Year)))} +#,
         {if(ribbon == FALSE)
         geom_errorbar(aes(ymin = elev_l25, ymax = elev_u75),
                       #position = position_dodge(width = 1),
                       width = 0, linewidth = 1.5)} +
         {if(ribbon == FALSE)
         geom_errorbar(aes(ymin = elev_min, ymax = elev_max),
                      #position = position_dodge(width = 1),
                      linewidth = 0.5)} +
         {if(ribbon == FALSE)
         geom_point(aes(x = Year, y = elev_med, fill = CoverCode,
                     size = CoverCode, shape = CoverCode), color = 'black')} +
         scale_shape_manual(values = shps, name = "Species", breaks = names(shps),
                            labels = labels) +
         scale_size_manual(values = sz, name = "Species", breaks = names(sz),
                            labels = labels) +
         theme(legend.position = leg_position) +
         {if(rev_axis == FALSE) theme(axis.text.x = element_text(angle = 45))} +
         {if(all(palette == 'default'))
           scale_color_manual(values = cols, name = "Species",
                              breaks = names(cols), labels = labels)} +
         {if(all(palette == 'default'))
           scale_fill_manual(values = cols, name = "Species",
                              breaks = names(cols), labels = labels)} +
         {if(all(palette == 'viridis')) scale_color_viridis_d("Species")} +
         {if(facet_loc_ribbon == TRUE) facet_wrap(~SiteCode, labeller = as_labeller(loc_labs))} +
         {if(facet_spp == TRUE) facet_wrap(~CoverCode, labeller = as_labeller(labels))} +
         {if(facet_loc_spp == TRUE) facet_wrap(~CoverCode + SiteCode)} +
         {if(facet_loc == TRUE) facet_wrap(~SiteCode, labeller = as_labeller(loc_labs))} +
         {if(rev_axis == TRUE) scale_y_continuous(limits = c(min(dat_sum$elev_min), max(dat_sum$elev_max)))} +
         {if(rev_axis == TRUE) scale_x_reverse(breaks = c(unique(dat_sum$Year)))} +
         {if(rev_axis == TRUE) coord_flip()} +
         {if(rev_axis == FALSE) scale_y_continuous(limits = c(min(dat_sum$elev_min), max(dat_sum$elev_max)))} +
         {if(rev_axis == FALSE) scale_x_continuous(breaks = c(unique(dat_sum$Year)))} +
         theme_rocky() +
         labs(y = ylab, x = xlab, title = ptitle)
  )

  if(plotly == TRUE){
    pp <- plotly::ggplotly(p, tooltip = 'text')#, layerData = 2, originalData = F)

    spp_mat <- unique(dat_sum[, c("CoverCode", "CoverType")])

    #--- Simplify plotly traces in legend ---
    # Get the names of the legend entries
    pdf <- data.frame(id = seq_along(pp$x$data),
                      legend_entries = unlist(lapply(pp$x$data, `[[`, "name")),
                      mode = unlist(lapply(pp$x$data, `[[`, 'mode')))
    # Extract the group identifier
    pdf$legend_group <- substr(gsub("[^A-Za-z///]", "", pdf$legend_entries), 1, 6)
    pdf <- pdf |> group_by(legend_group, mode) |>
      mutate(rank = row_number(),
             keep = ifelse(mode == 'markers' & rank == 1, TRUE, FALSE)) |>
      data.frame()
    #pdf$keep <- pdf$mode == 'markers'

    pdf <- dplyr::left_join(pdf, spp_mat, by = c("legend_group" = "CoverCode"))

    for (i in seq_along(pdf$id)) {
      # Is the layer the first entry of the group?
      keep <- pdf$keep[[i]]
      # Assign the group identifier to the name and legendgroup arguments
      pp$x$data[[i]]$name <- pdf$CoverType[[i]]
      pp$x$data[[i]]$legendgroup <- pp$x$data[[i]]$name
      # Show the legend only for the first layer of the group
      if(!keep) pp$x$data[[i]]$showlegend <- FALSE
      if(keep) pp$x$data[[i]]$showlegend <- TRUE
    }

    } else {pp <- p}

  suppressWarnings(pp)

  }
