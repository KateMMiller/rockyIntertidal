#' @title plotBarnacleRecruitment
#'
#' @include sumBarnacleRecruitment.R
#' @include theme_rocky.R
#'
#'
#' @import ggplot2
#' @importFrom dplyr arrange filter select
#'
#' @description This function plots median barnacle recruitment counts across the five summer plots per site and year,
#' with error bars representing min and max counts among the five plots. If only 1 plot is specified, then the raw
#' counts will be plotted. If both summer and winter plots are specified, resulting plot will be color-coded by season.
#' If more than one site and year is specified, results will be faceted by site.
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
#' c("all", "summer", "winter", "S1", "S2", "S3", "S4", "S5", "U1", "U2", "U3", "U4", "U5").
#' By default, only "summer" plots are included in the plots.
#'
#' @param years Filter on year of data collected. Default is 2013 to current year.
#' Can specify a vector of years.
#'
#' @param QAQC Logical. If FALSE (default) does not return QAQC records. If TRUE,
#' returns all records, including QAQC scoring records. This differs from other functions in that
#' QAQC is determined at the record level, not the visit level.
#'
#' @param plot_title Logical. If TRUE (default), will add site name as a title to the plot. If FALSE,
#' not plot title will be added. Only enabled when one site is specified, as facets will include the
#' site name in the facet strip.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # plot barnacle counts for ACAD summer counts for all sites and all years.
#' plotBarnacleRecruitment(park = "ACAD")
#'
#' # plot summer and winter counts for Bass Harbor
#' plotBarnacleRecruitment(site = "BASHAR", plotName = "all")
#'
#' # same as above, but drop plot title
#' plotBarnacleRecruitment(site = "BASHAR", plotName = "all", plot_title = F)
#'
#' # plot summer counts for all sites in ACAD in 2023
#' plotBarnacleRecruitment(park = "ACAD", years = 2023)
#'
#' # plot summer and winter counts for all BOHA sites
#' plotBarnacleRecruitment(park = "BOHA", plotName = "all")
#' }
#'
#'
#' @return Returns a ggplot object of percent cover from photos filtered by function arguments
#' @export

plotBarnacleRecruitment <- function(park = "all", site = "all", plotName = "summer",
                           QAQC = FALSE, years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                           plot_title = TRUE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(site %in% c("all","BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                        "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(plotName %in% c("all", "summer", "winter",
                            "S1", "S2", "S3", "S4", "S5",
                            "U1", "U2", "U3", "U4", "U5"))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2013)
  stopifnot(class(plot_title) == "logical")

  barn <- sumBarnacleRecruitment(park = park, site = site, plotName = plotName,
                                 QAQC = QAQC, years = years)

  steps <- ifelse(length(years) > 6, 2, 1)
  x_breaks <- seq(min(years), max(years), steps)

  num_sites = length(unique(barn$SiteCode))

  facet_site <- if(num_sites > 1){TRUE} else {FALSE}

  sumwin <- if(length(unique(barn$plot_type)) > 1){TRUE} else {FALSE}

  ptitle <- if(facet_site == FALSE & plot_title == TRUE){unique(barn$SiteName)} else {NULL}

    bp <-
    ggplot(barn, aes(x = Year,
                     y = median_count,
                     group = if(length(unique(plot_type)) > 1){plot_type} else {NULL})) +
      {if(facet_site == TRUE) facet_wrap(~SiteName)} +
      theme_rocky() +

      {if(sumwin == FALSE) geom_errorbar(aes(ymin = min_count, ymax = max_count), width = 0.2)} +
      {if(sumwin == FALSE) geom_point(size = 2, shape = 21, fill = "#696969", color = "black")} +

      {if(sumwin == TRUE) geom_errorbar(aes(ymin = min_count, ymax = max_count, color = plot_type))}+
      {if(sumwin == TRUE) geom_point(aes(color = plot_type, fill = plot_type), size = 2, shape = 21, alpha = 0.8)}+
      {if(sumwin == TRUE) scale_color_manual(values = c("#cb674e", "#4e9bcb"), aesthetics = c("color", "fill"))} +

      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(n.breaks = 10) + #scales::pretty_breaks(n = 10)) +
      labs(x = NULL, y = "Median Barnacle Count", color = "Plot Type", fill = "Plot Type", title = ptitle)

    return(bp)

  }
