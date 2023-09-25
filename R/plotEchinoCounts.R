#' @title plotEchinoCounts: summary plot of counts by species by year
#'
#' @include sumEchinoCounts.R
#'
#' @import ggplot2
#' @importFrom dplyr arrange filter group_by mutate summarize
#' @importFrom plotly ggplotly
#'
#' @description This function plots average counts by species for a given park, location, years and
#' species. The point for each species is the median count across the tidepools for that site, year
#' and pecies. The error bars are the middle 50% (lower 25% and upper 75% quantiles)
#' recorded in tidepools for a species. Note that if more than 1 location is specified,
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
#' @param plotName Filter on plot name (transect). Options include: c("all", "X1", "X2", and "X3")
#'
#' @param species Filter on species code. Options include:
#' c("all", "ASTFOR", "ASTRUB", "HENSAN", "STRDRO"). If a new species is added, the
#' function will warn the user that an unrecognized species was specified in case it was an error.
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
#' @param plot_title If specified, plots the title on the figure. If blank, no plot title included.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns a plot faceted on location and species
#' plotEchinoCounts()
#'
#' # Other variations
#'
#' plotEchinoCounts(park = "ACAD", location = "SHIHAR")
#'
#' stars = c("ASTFOR", "ASTRUB", "HENSAN")
#' plotEchinoCounts(location = "CALISL", palette = "default", species = stars)
#'
#' }
#'
#'
#' @return Returns a ggplot object of percent cover from photos filtered by function arguments
#' @export

plotEchinoCounts <- function(park = "all", location = "all", plotName = "all",
                                   species = 'all',
                                   palette = c('default'),
                                   xlab = "Year", ylab = "Average Count", main_groups = FALSE,
                                   years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                                   nrow = 1, plotly = F, xaxis = TRUE,
                                   plot_title = NULL, QAQC = FALSE, drop_missing = TRUE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "X1", "X2", "X3"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(palette %in% c("default", "viridis"))
  stopifnot(is.logical(plotly))

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

  dat <- suppressWarnings(force(
    sumEchinoCounts(park = park, location = location, plotName = plotName, years = years, QAQC = QAQC,
                    species = species))) |>
    dplyr::filter(!is.na(count_total))

  facet_loc <- if(length(unique(dat$Loc_Code)) > 1 ) {TRUE} else {FALSE}

  dat <- dat |> group_by(Site_Code, Loc_Code, Spp_Code, Spp_Name) |>
    mutate(nz = ifelse(sum(count_total) == 0, 0, 1))

  # # Drop species that are all 0
  dat_nz <- dat |> filter(nz > 0)

  p <- suppressWarnings(
  ggplot(dat_nz, aes(x = Year, y = count_avg, fill = Spp_Code, color = Spp_Code,
                       shape = Spp_Code, group = Spp_Code)) +
      geom_ribbon(aes(ymin = count_l25, ymax = count_u75, #fill = Spp_Code, color = Spp_Code,
                      text = paste0("Upper 75% and lower 25% counts", "<br>",
                                    "Species: ", Spp_Name, "<br>")),
                  alpha = 0.3, linewidth = 0.5) +
      geom_line(aes(x = Year, y = count_med, #linewidth = 0.1,
                    text = paste0("Mean count", "<br>",
                                  "Species: ", Spp_Name, "<br>")),
                linewidth = 0.5) +
      geom_point(aes(x = Year, y = count_med, #size = Spp_Code,
                     text = paste0("Mean count: ", count_avg, "<br>",
                                   "Species: ", Spp_Name, "<br>",
                                   "Year: ", Year, "<br>")),
                 color = 'black', size = 4) +
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
      {if(facet_loc == TRUE) facet_wrap(~Loc_Code, labeller = as_labeller(loc_labs))} +
      scale_x_continuous(breaks = c(unique(dat$Year)))+
      #coord_flip() +
      theme_rocky() +
      labs(y = ylab, x = xlab, title = plot_title) +
      theme(axis.text.x = element_text(angle = 45))
    # guides(color = guide_legend(nrow = 3), shape = guide_legend(nrow = 3),
    #        fill = guide_legend(nrow = 3))
  )

  if(plotly == TRUE){
    pp <-
      plotly::ggplotly(p, tooltip = 'text', layerData = 1, originalData = F)

    spp_mat <- data.frame(Spp_Code = c("ASTFOR", "ASTRUB", "HENSAN", "STRDRO"),
                          Spp_Name = c("A. forbesi (Northern sea star)",
                                       "A. rubens (common sea star)",
                                       "H. sanguinolenta (blood sea star)",
                                       "S. droebachiensis (sea urchin)"))

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
