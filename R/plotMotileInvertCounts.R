#' @title plotMotileInvertCounts: summary plot of counts by species and year
#'
#' @include sumMotileInvertCounts.R
#'
#' @import ggplot2
#' @importFrom dplyr arrange filter group_by slice_max summarize
#' @importFrom plotly ggplotly
#'
#' @description This function plots median count by species for a given park, site, years and
#' target species. The point for each species is the median count across the photoplots that site, year
#' and target species. The error bars are the middle 50% (lower 25% and upper 75% quantiles)
#' recorded in photoplots for a target species. Note that if more than 1 site is specified,
#' more than one target species is specified, or both, the resulting figure will facet on those
#' variables.
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
#' @param plotName Filter on plot name. Options include:
#'   c("all", A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5",
#'     "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
#'     "R1", "R2", "R3", "R4", "R5")
#'
#' @param species Filter on species code. Options include:
#' c("all", "CARMAE", "HEMISAN", "LITLIT",  "LITOBT", "LITSAX", "NUCLAP", "TECTES").
#' If a new species is added, the function will warn the user
#' that an unrecognized species was specified in case it was an error.
#'
#' @param community Filter on target community. Options include:
#' c("Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae")
#'
#' @param QAQC Logical. If FALSE (Default), does not return QAQC events. If TRUE,
#' returns all events, including QAQC events.
#'
#' @param plotly Logical. If TRUE, converts ggplot object to plotly object and includes tooltips. If FALSE (default),
#' plots a ggplot object.
#'
#' @param palette Choices are "default" or "viridis". Default assigns logical colors to common species.
#' Viridis uses a color-blind friendly palette of blues, purples and yellows.
#'
#' @param xlab Quoted text label for x axis. If not specified, defaults to 'Year'
#'
#' @param ylab Quoted text label for y axis. If not specified, defaults to '% Cover'
#'
#' @param xaxis Logical. If TRUE (Default), plots years on x axis. If FALSE, drops x axis.
#' FALSE is useful for combining plotly photocover plots in a subplot.
#'
#' @param plot_title If specified, plots the title on the figure. If blank, no plot title included.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns a plot faceted on site and target species group
#' plotMotileInvertCounts()
#'
#' # Other variations
#'
#' plotMotileInvertCounts(park = "ACAD", community = "Barnacle")
#'
#' spp = c("CARMAE", "HEMISAN")
#' plotMotileInvertCounts(site = "CALISL", palette = "default", title = FALSE,
#'                species = spp)
#'
#' }
#'
#'
#' @return Returns a ggplot object of counts from photoplots filtered by function arguments
#' @export

plotMotileInvertCounts <- function(park = "all", site = "all", plotName = "all",
                           species = 'all', category = "all", community = 'all',
                           top_spp = NULL, palette = c('default'),
                           xlab = "Year", ylab = "Median Count", main_groups = FALSE,
                           years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                           nrow = 1, plotly = F, xaxis = TRUE,
                           plot_title = NULL, QAQC = FALSE, drop_missing = TRUE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "A1", "A2", "A3", "A4", "A5", "B1", "B2", "B3", "B4", "B5",
                            "F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5",
                            "R1", "R2", "R3", "R4", "R5"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(palette %in% c("default", "viridis"))
  stopifnot(category %in% c("all", "Genus", "Species", "Species Group", "Substrate"))
  stopifnot(community %in% c('all', "Ascophyllum", "Barnacle", "Fucus", "Mussel", "Red Algae"))
  stopifnot(is.numeric(top_spp) | is.null(top_spp))
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

  dat <- suppressWarnings(force(
    sumMotileInvertCounts(park = park, site = site, plotName = plotName, years = years, QAQC = QAQC,
                                              species = species, community = community))) |>
                                 dplyr::filter(!is.na(count_med))


  dat$CommunityType <- factor(dat$CommunityType, levels = c("Barnacle", "Mussel", "Fucus", "Ascophyllum", "Red Algae"))

  facet_loc_cat <- if(length(unique(dat$SiteCode)) > 1 & length(unique(dat$CommunityType)) > 1) {TRUE} else {FALSE}
  facet_loc <- if(length(unique(dat$SiteCode)) > 1 & length(unique(dat$CommunityType)) == 1) {TRUE} else {FALSE}
  facet_targ <- if(length(unique(dat$SiteCode)) == 1 & length(unique(dat$CommunityType)) > 1) {TRUE} else {FALSE}
#head(as.data.frame(dat))
  dat <- dat |> group_by(UnitCode, SiteCode, CommunityType, SpeciesCode, ScientificName) |>
    mutate(nz = ifelse(sum(count_med) == 0, 0, 1))

  # Drop species that are all 0
  dat_nz <- dat |> filter(nz > 0)

  p <- suppressWarnings(
   ggplot(dat_nz, aes(x = Year, y = count_med, fill = SpeciesCode, color = SpeciesCode,
                  shape = SpeciesCode, group = SpeciesCode)) +
         geom_ribbon(aes(ymin = count_l25, ymax = count_u75, #fill = SpeciesCode, color = SpeciesCode,
                         text = paste0("Upper 75% and lower 25% counts", "<br>",
                                       "Species: ", ScientificName, "<br>")),
                     alpha = 0.3, linewidth = 0.5) +
         geom_line(aes(x = Year, y = count_med, #linewidth = 0.1,
                       text = paste0("Median count", "<br>",
                                     "Species: ", ScientificName, "<br>")),
                   linewidth = 0.5) +
         geom_point(aes(x = Year, y = count_med, #size = SpeciesCode,
                        text = paste0("Median count: ", count_med, "<br>",
                                      "Species: ", ScientificName, "<br>",
                                      "Year: ", Year, "<br>")),
                    color = 'black', size = 4) +
         scale_shape_manual(values = shps, name = "Species", breaks = names(shps),
                            labels = labels) +
         scale_size_manual(values = sz, name = "Species", breaks = names(sz),
                           labels = labels) +
         facet_wrap(~CommunityType, labeller = as_labeller(targ_labs)) +
         {if(all(palette == 'default'))
           scale_color_manual(values = cols, name = "Species",
                              breaks = names(cols), labels = labels)} +
         {if(all(palette == 'default'))
           scale_fill_manual(values = cols, name = "Species",
                              breaks = names(cols), labels = labels)} +
         {if(all(palette == 'viridis')) scale_color_viridis_d("Species")}+
         {if(facet_loc_cat == TRUE) facet_wrap(~CommunityType + SiteCode,
                                             labeller = as_labeller(c(targ_labs, loc_labs)))} +
         {if(facet_loc == TRUE) facet_wrap(~SiteCode, labeller = as_labeller(loc_labs))} +
         {if(facet_targ == TRUE) facet_wrap(~CommunityType, nrow = 1)} +
         scale_x_continuous(breaks = c(unique(dat$Year)))+
         #coord_flip() +
         theme_rocky() +
         labs(y = ylab, x = xlab, title = plot_title) +
         {if(xaxis == FALSE) theme(
           legend.position = 'bottom',
           axis.text.x = element_blank(),
           axis.title.x = element_blank())} +
         {if(xaxis == TRUE) theme(
           legend.position = 'bottom',
           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))} #+
         # guides(color = guide_legend(nrow = 3), shape = guide_legend(nrow = 3),
         #        fill = guide_legend(nrow = 3))
        )
  if(plotly == TRUE){
  pp <-
    plotly::ggplotly(p, tooltip = 'text', layerData = 1, originalData = F)

  spp_mat <- unique(dat_nz[, c("SpeciesCode", "ScientificName")])

  #--- Simplify plotly traces in legend ---
  # Get the names of the legend entries
  pdf <- data.frame(id = seq_along(pp$x$data),
                    legend_entries = unlist(lapply(pp$x$data, `[[`, "name")))
  # Extract the group identifier
  pdf$legend_group <- substr(gsub("[^A-Za-z///]", "", pdf$legend_entries), 1, 6)
  # Determine the points based on max number of chars
  max_char <- max(nchar(pdf$legend_entries))
  pdf$points <- ifelse(nchar(pdf$legend_entries) == max_char, TRUE, FALSE)
# Add an indicator for the first entry per group
  pdf$is_first1 <- !duplicated(pdf$legend_group[pdf$points == TRUE])
  pdf$is_first <- ifelse(pdf$is_first1 == TRUE & pdf$points == TRUE, TRUE, FALSE)
  pdf <- dplyr::left_join(pdf, spp_mat, by = c("legend_group" = "SpeciesCode"))

  for (i in seq_along(pdf$id)) {
    # Is the layer the first entry of the group?
    is_first <- pdf$is_first[[i]]
    # Assign the group identifier to the name and legendgroup arguments
    pp$x$data[[i]]$name <- pdf$ScientificName[[i]]
    pp$x$data[[i]]$legendgroup <- pp$x$data[[i]]$name
    # Show the legend only for the first layer of the group
    if (!is_first) pp$x$data[[i]]$showlegend <- FALSE
  }
  } else {pp <- p}

  suppressWarnings(pp)

  }
