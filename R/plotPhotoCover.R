#' @title plotPhotoCover: plots average cover by species and year
#'
#' @include sumPhotoCover.R
#'
#' @import ggplot2
#' @importFrom dplyr arrange filter group_by mutate row_number slice_max summarize
#' @importFrom plotly ggplotly
#'
#' @description This function plots median percent cover by species for a given park, location, years and
#' target species. The point for each species is the median cover across the photoplots that site, year
#' and target species. The error bars are the minimum and maximum cover recorded in photoplots for a target
#' species. Note that if more than 1 location is specified, more than one target species is specified, or both,
#' the resulting figure will facet on those variables. If elev = TRUE, data will be plotted by elevation
#' instead of plot name.
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
#' @param main_groups Logical. If TRUE, only plots red algae (combined Irish moss and red algae group), Fucus spp.,
#' Ascophyllum nodosum, mussels, and barnacles. If FALSE (Default), plots all species or only species specified. If species
#' are specified, this argument will override it and only plot main species groups.
#'
#' @param heatmap Logical. If FALSE (Default), will plot species cover (y) by year (x) as points with error bars,
#' and faceted on target species. If TRUE, will plot results as heat map with species as y, year by x, and color
#' values representing percent cover.
#'
#' @param top_spp Integer. If specified, only plots the top n species by cover for each target species. If not
#' specified, plots all species, or only the species specified in the species argument. Note that species ties are
#' included, so could include more than the number specified. Species with less than 0.1% total cover are
#' dropped when a top_spp is specified. This argument overrides the species argument.
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
#' # Only plot main 5 species groups for BOHA sites
#' plotPhotoCover(park = "BOHA", main_groups = TRUE)
#'
#' # Plot heatmap for main species groups for Green Island
#' plotPhotoCover(location = "GREISL", main_groups = TRUE, heatmap = TRUE)
#'
#' # Plot top 4 species in barnacle photoplots only
#' plotPhotoCover(park = "ACAD", top_spp = 4, target_species = "Barnacle")
#'
#' }
#'
#'
#' @return Returns a ggplot object of percent cover from photos filtered by function arguments
#' @export

plotPhotoCover <- function(park = "all", location = "all", plotName = "all",
                           species = 'all', category = "all", target_species = 'all',
                           heatmap = FALSE, top_spp = NULL, palette = c('default'),
                           xlab = "Year", ylab = "% Cover", main_groups = FALSE,
                           years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                           nrow = 1, plotly = FALSE,
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
  stopifnot(is.numeric(top_spp) | is.null(top_spp))
  stopifnot(is.logical(heatmap))
  stopifnot(is.logical(plotly))

  if(plotly == TRUE & heatmap == TRUE){stop("Plotly not enabled for heatmap.")}

  spp_list <- c("all", "ALGBRO", "ALGGRE", "ALGRED", "ARTCOR",
                "ASCEPI", "ASCNOD", "BARSPP", "CHOMAS",
                "CRUCOR", "NONCOR","FUCEPI", "FUCSPP",
                "KELP", "MUSSPP", "OTHINV", "OTHSUB",
                "PALPAL", "PORSPP", "ULVENT", "ULVINT",
                "ULVLAC", "UNIDEN", "BOLT", "ROCK",
                "WATER", "SAND", "TAR")

  unmatch_spp <- setdiff(species, c(spp_list, NA))

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
           "CRUCOR" = "#F9F5A1", "NONCOR" = "#574F91","FUCEPI" = "#D5A82A", "FUCSPP" = "#FFD560",
           "KELP"   = "#4DA551", "MUSSPP" = "#6F88BF", "OTHINV" = "#F59617", "OTHSUB" = "#8A838A",
           "PALPAL" = "#5E5571", "PORSPP" = "#8E3B4A", "ULVENT" = "#699052", "ULVINT" = "#9FCF87",
           "ULVLAC" = "#73EB31", "UNIDEN" = "#696969", "BOLT"   = "#EAEAEA", "ROCK"   = "#FED5FF",
           "WATER"  = "#7FC7E1", "SAND" = "#CACACA", "TAR" = "#111111", "REDGRP" = "#FF4C53")

  shps = c("ALGBRO" = 21, "ALGGRE" = 23, "ALGRED" = 24, "ARTCOR" = 25,
           "ASCEPI" = 21, "ASCNOD" = 23, "BARSPP" = 24, "CHOMAS" = 25,
           "CRUCOR" = 21, "NONCOR" = 23,"FUCEPI" = 24, "FUCSPP" = 25,
           "KELP"   = 21, "MUSSPP" = 23, "OTHINV" = 24, "OTHSUB" = 25,
           "PALPAL" = 21, "PORSPP" = 23, "ULVENT" = 24, "ULVINT" = 25,
           "ULVLAC" = 21, "UNIDEN" = 23, "BOLT"   = 24, "ROCK"   = 25,
           "WATER"  = 21, "SAND" = 23, "TAR" = 24, "REDGRP" = 25)

  sz = c("ALGBRO" = 4, "ALGGRE" = 3.5, "ALGRED" = 3, "ARTCOR" = 3,
         "ASCEPI" = 4, "ASCNOD" = 3.5, "BARSPP" = 3, "CHOMAS" = 3,
         "CRUCOR" = 4, "NONCOR" = 3.5,"FUCEPI" = 3, "FUCSPP" = 3,
         "KELP"   = 4, "MUSSPP" = 3.5, "OTHINV" = 3, "OTHSUB" = 3,
         "PALPAL" = 4, "PORSPP" = 3.5, "ULVENT" = 3, "ULVINT" = 3,
         "ULVLAC" = 4, "UNIDEN" = 3.5, "BOLT"   = 3, "ROCK"   = 3,
         "WATER"  = 4, "SAND" = 3.5, "TAR" = 3, "REDGRP" = 2)

  labels = c("ALGBRO" = "Algae - Brown", "ALGGRE" = "Algae - Green", "ALGRED" = "Algae - Red",
             "ARTCOR" = "Articulated Corallines", "ASCEPI" = "Ascophyllum epibiont",
             "ASCNOD" = "A. nodosum (Knotted wrack)", "CHOMAS" = "Irish moss",
             "CRUCOR" = "Crustose coraline", "NONCOR" = "Crustose non-coraline",
             "BARSPP" = "Barnacles",
             "FUCEPI" = "Fucus epibiont", "FUCSPP" = "Fucus spp. (Rockweed)",
             "KELP"   = "Kelp", "MUSSPP" = "Mussels",
             "OTHINV" = "Other invertebrates", "OTHSUB" = "Other substrate",
             "PALPAL" = "Dulse", "PORSPP" = "Laver", "ULVENT" = "Ulva/Enteromorpha",
             "ULVINT" = "Ulva intestinalis (Grass kelp)", "ULVLAC" = "Ulva lactuca (Sea lettuce)",
             "UNIDEN" = "Unidentified", "BOLT" = "Bolt", "ROCK" = "Rock", "WATER"  = "Water",
             "SAND" = "Sand", "TAR" = "Tar",
             "REDGRP" = "Red algae group")

  targ_labs <-  c("Ascophyllum" = "A. nodosum (knotted wrack)",
                  "Barnacle" = "Barnacle",
                  "Fucus" = "Fucus spp. (Rockweed)",
                  "Mussel" = "Mussels",
                  "Red Algae" = "Red algae group")

  loc_labs <- c("BASHAR" = "Bass Harbor", "LITHUN" = "Little Hunter", "LITMOO" = "Little Moose",
                "OTTPOI" = "Otter Point", "SCHPOI" = "Schoodic Point", "SHIHAR" = "Ship Harbor",
                "CALISL" = "Calf Island", "GREISL" = "Green Island", "OUTBRE" = "Outer Brewster")

  dat1 <- suppressWarnings(force(sumPhotoCover(park = park, location = location, plotName = plotName,
                                              category = category, years = years, QAQC = QAQC,
                                              species = species, target_species = target_species))) |>
                                 dplyr::filter(!is.na(median_cover))

  if(!is.null(top_spp) & main_groups == TRUE){
    warning("Can't select top species and plot main groups. Will only plot main species groups.
            Set main_groups = FALSE if want top species.")
    top_spp <- NULL
    }

  if(!is.null(top_spp)){
    # Create df of top 4 species per target_species by cover
    top_df <- dat1 |> group_by(Site_Code, Loc_Code, Target_Species, Spp_Code) |>
                      summarize(tot_cov = sum(median_cover, na.rm = TRUE), .groups = 'drop') |>
                      ungroup() |>
                      group_by(Site_Code, Loc_Code, Target_Species) |>
                      slice_max(order_by = tot_cov, n = top_spp) |> ungroup() |>
                      filter(tot_cov > 0.1) |>
                      unique()

    # left join dat 1 top 4 to drop less abundant species
    dat <- left_join(top_df, dat1,
                     by = c("Site_Code", "Loc_Code", "Target_Species", "Spp_Code"))

    } else if (main_groups == TRUE){
    dat <- dat1 |> dplyr::mutate(Spp_Code = ifelse(Spp_Code %in% c("ALGRED", "CHOMAS"), "REDGRP", Spp_Code),
                                 Spp_Name = ifelse(Spp_Code %in% "REDGRP", "Red algae group", Spp_Name)) |>
                   dplyr::filter(Spp_Code %in% c("REDGRP", "ASCNOD", "FUCSPP", "BARSPP", "MUSSPP", "NONCOR")) |>
                   group_by(Site_Code, Loc_Code, Year, Target_Species, Spp_Code, Spp_Name) |>
                   summarize(avg_cover = sum(avg_cover),
                             median_cover = sum(median_cover),
                             min_cover = sum(min_cover),
                             max_cover = sum(max_cover),
                             q25_cover = sum(q25_cover),
                             q75_cover = sum(q75_cover),
                             .groups = 'drop')
  } else {dat <- dat1}

  # This is all to make NONCOR species name sort alphabetically with Cs
  sppcode <- c("ALGBRO", "ALGGRE", "ALGRED", "ARTCOR", "ASCEPI", "ASCNOD",
               "BARSPP", "CHOMAS", "CRUCOR", "NONCOR", "FUCEPI", "FUCSPP", "KELP",
               "MUSSPP", "OTHINV", "OTHSUB", "PALPAL", "PORSPP", "ULVENT", "ULVINT",
               "ULVLAC", "UNIDEN", "BOLT", "ROCK", "WATER", "REDGRP")

  dat$Spp_Code <- factor(dat$Spp_Code, levels = sppcode) |> droplevels()
  spp <- levels(dat$Spp_Code)
  dat$Target_Species <- factor(dat$Target_Species, levels = c("Barnacle", "Mussel", "Fucus", "Ascophyllum", "Red Algae"))

  if(main_groups == TRUE){
    dat$Spp_Code <- factor(dat$Spp_Code, levels = c("REDGRP", "ASCNOD", "FUCSPP", "MUSSPP", "BARSPP", "NONCOR"))
    dat$Target_Species <- factor(dat$Target_Species,
                                 levels = c("Barnacle", "Mussel", "Fucus", "Ascophyllum", "Red Algae"))
    dat <- droplevels(dat)
  }

  facet_loc_cat <- if(length(unique(dat$Loc_Code)) > 1 & length(unique(dat$Target_Species)) > 1) {TRUE} else {FALSE}
  facet_loc <- if(length(unique(dat$Loc_Code)) > 1 & length(unique(dat$Target_Species)) == 1) {TRUE} else {FALSE}
  facet_targ <- if(length(unique(dat$Loc_Code)) == 1 & length(unique(dat$Target_Species)) > 1) {TRUE} else {FALSE}

  dat <- dat |> group_by(Site_Code, Loc_Code, Target_Species, Spp_Code, Spp_Name) |>
    mutate(nz = ifelse(sum(median_cover) == 0, 0, 1))

  dat_nz <- dat |> filter(nz > 0) |> droplevels()

  p <-
  if(heatmap == FALSE){
    if(plotly == FALSE){
    ggplot(dat_nz, aes(x = Year, y = median_cover,  color = Spp_Code, fill = Spp_Code,
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
         {if(facet_loc_cat == TRUE) facet_wrap(~Target_Species + Loc_Code,
                                               labeller = as_labeller(c(targ_labs, loc_labs)))} +
         {if(facet_loc == TRUE) facet_wrap(~Loc_Code, labeller = as_labeller(loc_labs))} +
         {if(facet_targ == TRUE) facet_wrap(~Target_Species)} +
         scale_x_continuous(breaks = c(unique(dat$Year)))+
         theme_rocky() +
         labs(y = ylab, x = xlab, title = plot_title)+
         theme(legend.position = 'bottom',
               axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
    } else if(plotly == TRUE){
      suppressWarnings(
        ggplot(dat_nz, aes(x = Year, y = median_cover, fill = Spp_Code, color = Spp_Code,
                         shape = Spp_Code, group = Spp_Code)) +
        geom_ribbon(aes(ymin = q25_cover, ymax = q75_cover, #fill = Spp_Code, color = Spp_Code,
                        text = paste0("Upper 75% and lower 25% cover", "<br>",
                                      "Species: ", Spp_Name, "<br>")),
                    alpha = 0.3, linewidth = 0.5) +
        geom_line(aes(x = Year, y = median_cover, #linewidth = 0.1,
                      text = paste0("Median cover", "<br>",
                                    "Species: ", Spp_Name, "<br>")),
                  linewidth = 0.5) +
        geom_point(aes(x = Year, y = median_cover, #size = Spp_Code,
                       text = paste0("Median cover: ", median_cover, "<br>",
                                     "Species: ", Spp_Name, "<br>",
                                     "Year: ", Year, "<br>")),
                   color = 'black', size = 4) +
        scale_shape_manual(values = shps, name = "Species", breaks = names(shps),
                           labels = labels) +
        # scale_size_manual(values = sz, name = "Species", breaks = names(sz),
        #                   labels = labels) +
        facet_wrap(~Target_Species, labeller = as_labeller(targ_labs)) +
        {if(all(palette == 'default'))
          scale_color_manual(values = cols, name = "Species",
                             breaks = names(cols), labels = labels)} +
        {if(all(palette == 'default'))
          scale_fill_manual(values = cols, name = "Species",
                            breaks = names(cols), labels = labels)} +
        {if(all(palette == 'viridis')) scale_color_viridis_d("Species")}+
        {if(facet_loc_cat == TRUE) facet_wrap(~Target_Species + Loc_Code,
                                              labeller = as_labeller(c(targ_labs, loc_labs)))} +
        {if(facet_loc == TRUE) facet_wrap(~Loc_Code, labeller = as_labeller(loc_labs))} +
        {if(facet_targ == TRUE) facet_wrap(~Target_Species, nrow = 1)} +
        scale_x_continuous(breaks = c(unique(dat$Year)))+
        #coord_flip() +
        theme_rocky() +
        labs(y = ylab, x = xlab, title = plot_title)+
        theme(legend.position = 'bottom',
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
      )} # end of plotly == T
    } else if(heatmap == TRUE){
      ggplot(dat, aes(x = Year, y = Spp_Code,  color = median_cover, fill = median_cover)) +
        geom_tile(color = '#9F9F9F') +
        geom_text(aes(label = ifelse(median_cover > 0, round(median_cover, 1), NA)),
                      color = 'black')+
        scale_y_discrete(limits = rev, labels = labels)+
        {if(facet_loc_cat == TRUE) facet_wrap(~Target_Species + Loc_Code,
                                              labeller = as_labeller(c(targ_labs, loc_labs)))} +
        {if(facet_loc_cat == FALSE) facet_wrap(~Target_Species, drop = T, nrow = nrow)} +
        scale_fill_gradient(low = "white", high = "#5969B0", guide = 'legend',
                            name = "Median % Cover") +
        scale_x_continuous(breaks = c(unique(dat$Year)))+
        ylab(NULL) +
        theme_rocky() +
        theme(legend.position = 'bottom')
      }

  if(plotly == TRUE){
    pp <-
      plotly::ggplotly(p, tooltip = 'text', layerData = 1, originalData = F)

    spp_mat <- data.frame(unique(dat_nz[, c("Spp_Code", "Spp_Name")]))
    #--- Simplify plotly traces in legend ---
    # Get the names of the legend entries
    pdf <- data.frame(id = seq_along(pp$x$data),
                      legend_entries = unlist(lapply(pp$x$data, `[[`, "name")))
    # Extract the group identifier
    pdf$legend_group <- substr(gsub("[^A-Za-z///]", "", pdf$legend_entries), 1, 6)
    # Determine the points based on max number of chars
    max_char <- max(nchar(pdf$legend_entries))
    pdf$points <- ifelse(nchar(pdf$legend_entries) == max_char, TRUE, FALSE)

    pdf <- pdf |> group_by(legend_group, points) |>
      mutate(rank = row_number(),
             is_first = ifelse(points == TRUE & rank == 1, TRUE, FALSE)) |>
      data.frame()

    # Add an indicator for the first entry per group
    pdf <- dplyr::left_join(pdf, spp_mat, by = c("legend_group" = "Spp_Code"))

    for (i in seq_along(pdf$id)) {
      # Is the layer the first entry of the group?
      is_first <- pdf$is_first[[i]]
      # Assign the group identifier to the name and legendgroup arguments
      pp$x$data[[i]]$name <- pdf$Spp_Name[[i]]
      pp$x$data[[i]]$legendgroup <- pp$x$data[[i]]$name
      # Show the legend only for the first layer of the group
      if (!is_first) pp$x$data[[i]]$showlegend <- FALSE
    }

    } else {pp <- p}


  suppressWarnings(pp)

  }
