#' @title plotBuoyData: plots daily buoy statistics
#'
#' @import ggplot2
#' @importFrom dplyr between case_when filter mutate
#' @importFrom gridExtra grid.arrange
#'
#' @description This function plots water temperature at high tide by location. Note that function works on
#' compiled logger data, rather than working off the raw logger data. Compiled datasets should be imported
#' using importWaterTemp(). To speed up plotting of multiple locations or long time-series, plot the simplified
#' water temperature data derived from importWaterTemp(simplify = TRUE).
#'
#' @param park Choose the park to plot. Can only plot one park at a time.
#' \describe{
#' \item{'ACAD'}{Includes only sites in Acadia National Park}
#' \item{'BOHA'}{Includes only sites in Boston Harbor Islands National Recreation Area}
#' }
#'
#' @param years Filter on year of data collected. Default is 2011 to current year.
#'
#' @param metric Select the metric to plot. Options are below. Can either choose 1 plot or all plots.
#' \describe{
#' \item{'all'}{Plot individual facet for each metric.}
#' \item{'temp'}{Minimum and Maximum daily water temperature in F}
#' \item{'wspd'}{Maximum daily windspeed in miles/hour and wind direction recorded during
#' max. windspeed for winds > 35 miles/hour.}
#' }
#'
#' @param plot_title If specified, plots the title on the figure. If NULL, no plot title included.
#'
#' @param palette Choices are "default", "viridis", or "black". Default assigns specific colors to each metric.
#' Viridis uses a color-blind friendly palette of blues, purples and yellows.
#'
#' @param facet_col Numeric. Number of columns for the facet to plot. Defaults to 1.
#'
#' @param plotly Logical. If TRUE, converts ggplot object to plotly object and includes tooltips. If FALSE (default),
#' plots a ggplot object. Currently not functional.
#'
#'
#' @examples
#' \dontrun{
#'
#' path <-
#'   "Z:/PROJECTS/MONITORING/Rocky_Intertidal/NETN/5_Data/Data_Files/Temperature/Compiled_HT_water_temps_2011-2022/"
#' importWaterTemp(path, simplify = TRUE, buoy = TRUE) # import water temp and buoy data and simplify to daily stats.
#'
#' # Default filter returns a plot of the closest buoys to ACAD and BOHA
#' plotBuoyData()
#'
#' # Other variations
#' # Windspeed and direction only
#' plotBuoyData(metric = 'wspd')
#'
#' plotBuoyData(park = "ACAD", years = 2013:2022, palette = 'black')
#'
#' }
#'
#' @return Returns a ggplot object of water temperature data
#' @export

plotBuoyData <- function(park = "ACAD", palette = c('default'),
                          metric = "all",
                          years = 2011:as.numeric(format(Sys.Date(), "%Y")),
                          plot_title = NULL, facet_col = 1, plotly = FALSE){

  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("ACAD", "BOHA"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2011)
  stopifnot(palette %in% c("default", "viridis", "black"))
  stopifnot(metric %in% c("all", "temp", "wspd"))

  # if(!requireNamespace("mgcv", quietly = TRUE) & gam == TRUE){
  #   stop("Package 'mgcv' needed for this function to work. Please install it.", call. = FALSE)
  # }

  if(!requireNamespace("plotly", quietly = TRUE) & plotly == TRUE){
    stop("Package 'plotly' needed for this function for plotly = TRUE. Please install it or set plotly = FALSE.", call. = FALSE)
  }

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  # Check for loaded buoy data.
  if(!"ACAD_buoy" %in% ls(envir = env)){
    stop("Must import ACAD and BOHA buoy data.")
  }

  if(!("WTMP_F_min" %in% names(get("ACAD_buoy", envir = env)))){
    stop("Must import ACAD and BOHA buoy data using simplify = TRUE in importWaterData().")
  }

  cols <- switch(palette, #WTMP_min, WTMP_max, WSPD_max, WVHT_max, WDIR_max
                 "default" = c("WTMP_F_min" = "#7CC4D8",
                               "WTMP_F_max" = "#277489",
                               "WSPD_max" = "#737373",
                               "WVHT_max" = "#003A7F"),
                 "viridis" = c("WTMP_F_min" = "#440154",
                               "WTMP_F_max" = "#414487",
                               "WSPD_max" = "#2A788E",
                               "WVHT_max" = "#22A884"),
                 "black" = c("WTMP_F_min" = "black",
                             "WTMP_F_max" = "black",
                             "WSPD_max" = "black",
                             "WVHT_max" = "black"))

  labels <- c("WTMP_F_min" = "Min.",
              "WTMP_F_max" = "Max.",
              "WSPD_max" = "Max. Daily Windspeed (m/s)",
              "WVHT_max" = "Max. Daily Wave height (m)",
              "WDIR_max" = "Avg. Daily Wind direction (degrees)")

  dat <- switch(park,
                "ACAD" = get("ACAD_buoy", envir = env),
                "BOHA" = get("BOHA_buoy", envir = env)
                )

  dat$WSPD_max_mph <- dat$WSPD_max * 2.23694

  dat2 <- dat |> filter(YEAR %in% years)

  p_temp <-
    ggplot(dat2, aes(x = DATE)) +
    geom_line(aes(y = WTMP_F_min, color = "WTMP_F_min")) +
    geom_line(aes(y = WTMP_F_max, color = "WTMP_F_max")) +
    scale_color_manual(name = "Daily Water Temp.",
                       values = cols,
                       labels = labels) +
    theme_rocky() +
    theme(legend.position = 'bottom',
          legend.margin=margin(t=-5),
          legend.text = element_text(size = 9),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
          legend.title = element_text(size = 9)) +
    ylab( "Daily Water Temp. (F)") + xlab(NULL) +
    {if(length(years) > 3)
      scale_x_datetime(breaks = scales::breaks_width("6 months"), date_labels = "%m/%y")} +
    {if(length(years) <= 3 & length(years) > 1)
      scale_x_datetime(breaks = scales::breaks_width("2 months"), date_labels = "%m/%y")} +
    {if(length(years) == 1) scale_x_datetime(breaks = scales::breaks_width("1 month"), date_labels = "%m/%y")}

  dat2 <- dat2 |> mutate(WDIR_txt = case_when(between(WDIR_max, 0, 45) ~ "Northerly",
                                            between(WDIR_max, 45, 135) ~ "Easterly",
                                            between(WDIR_max, 135, 225) ~ "Southerly",
                                            between(WDIR_max, 225, 315) ~ "Westerly",
                                            between(WDIR_max, 315, 360) ~ "Northerly"),
                         WDIR_txt2 = case_when(between(WDIR_max, 348.75, 360) ~ "N",
                                               between(WDIR_max, 0, 11.25) ~ "N",
                                               between(WDIR_max, 11.25, 33.75) ~'NNE',
                                               between(WDIR_max, 33.75, 56.25) ~'NE',
                                               between(WDIR_max, 56.25, 78.75) ~'ENE',
                                               between(WDIR_max, 78.75, 101.25) ~'E',
                                               between(WDIR_max, 101.25, 123.75) ~'ESE',
                                               between(WDIR_max, 123.75, 146.25) ~'SE',
                                               between(WDIR_max, 146.25, 168.75) ~'SSE',
                                               between(WDIR_max, 168.75, 191.25) ~'S',
                                               between(WDIR_max, 191.25, 213.75) ~'SSW',
                                               between(WDIR_max, 213.75, 236.25) ~'SW',
                                               between(WDIR_max, 236.25, 258.75) ~'WSW',
                                               between(WDIR_max, 258.75, 281.25) ~'W',
                                               between(WDIR_max, 281.25, 303.75) ~'WNW',
                                               between(WDIR_max, 303.75, 326.25) ~'NW',
                                               between(WDIR_max, 326.25, 348.75) ~'NNW'))
  wdir <- dat2 |> filter(WSPD_max_mph > 35)

  wind_cols = c('Northerly' = '#2b83ba',
                'Easterly' = '#abdda4',
                'Southerly' = '#fdae61',
                'Westerly' = '#d7191c')

#  if(plotly == FALSE){
  p_wspd <- suppressWarnings(
      ggplot(dat2, aes(x = DATE, y = WSPD_max_mph, color = WDIR_txt)) +
        geom_line(color = cols[3]) +
        #geom_text(data = wdir, aes(angle = -WDIR_max + 90), label = expression(symbol('\256')), size = 10) +
        geom_text(data = wdir, aes(angle = -WDIR_max + 90), label = sprintf('\u2794'), size = 10) +
        ylim(0, max(dat2$WSPD_max_mph) + 5) +
        theme_rocky() +
        theme(legend.position = 'bottom',
              legend.margin = margin(t=-5),
              legend.text = element_text(size = 9),
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
              legend.title = element_text(size = 9)) +
        ylab( "Max. Wind Speed (mph)") + xlab(NULL) +
        scale_color_manual(values = wind_cols, breaks = names(wind_cols), name = "Wind direction") +
        {if(length(years) > 3)
          scale_x_datetime(breaks = scales::breaks_width("6 months"), date_labels = "%m/%y")} +
        {if(length(years) <= 3 & length(years) > 1)
          scale_x_datetime(breaks = scales::breaks_width("2 months"), date_labels = "%m/%y")} +
        {if(length(years) == 1) scale_x_datetime(breaks = scales::breaks_width("1 month"), date_labels = "%m/%y")}
  )
#  } else {
#   p_wspd <- suppressWarnings(
#     ggplot(dat2, aes(x = DATE, y = WSPD_max_mph, color = WDIR_txt, group = WDIR_txt)) +
#       geom_line(aes(text = paste0("Date: ", DATE, "<br>",
#                                   "Max. Wind (mph): ", WSPD_max_mph, "<br>",
#                                   "Avg. Wind Dir.: ", WDIR_max, "<br>",
#                                   "Wind Dir. Text: ", WDIR_txt2, "<br>")),
#                 color = cols[3]) +
#       ylim(0, max(dat2$WSPD_max_mph) + 5) +
#       theme_rocky() +
#       theme(legend.position = 'bottom',
#             legend.text = element_text(size = 9),
#             legend.title = element_text(size = 9)) +
#       ylab( "Max. Wind Speed (mph)") +
#       scale_color_manual(values = wind_cols, breaks = names(wind_cols), name = "Wind direction") +
#       {if(length(years) > 3)
#         scale_x_datetime(breaks = scales::breaks_width("6 months"), date_labels = "%m/%y")} +
#       {if(length(years) <= 3 & length(years) > 1)
#         scale_x_datetime(breaks = scales::breaks_width("2 months"), date_labels = "%m/%y")} +
#       {if(length(years) == 1) scale_x_datetime(breaks = scales::breaks_width("1 month"), date_labels = "%m/%y")}
#   )
#
#
#   wind_pal <- c('#2b83ba', '#abdda4', '#fdae61','#d7191c')
#
#   wdir$color <- case_when(wdir$WDIR_txt == "Northerly" ~ '#2b83ba',
#                           wdir$WDIR_txt == "Easterly" ~ '#abdda4',
#                           wdir$WDIR_txt == "Southerly" ~ '#fdae61',
#                           wdir$WDIR_txt == "Westerly" ~ '#d7191c')
#
#   p_wspd_p <-
#     plotly::plot_ly(dat2, x = ~DATE, y = ~WSPD_max_mph) |>
#     plotly::add_trace(x = ~DATE, y = ~WSPD_max_mph, type = 'scatter', mode = 'lines',
#                       line = list(shape = 'linear', color = '#737373', width = 1),
#                       text = ~paste0("Date: ", DATE, "<br>",
#                                      "Max. Wind (mph): ", round(WSPD_max_mph,1), "<br>",
#                                      "Avg. Wind Dir.: ", round(WDIR_max, 1), "<br>",
#                                      "Wind Dir. Text: ", WDIR_txt2, "<br>")) |>
#     plotly::add_annotations(x = wdir$DATE, y = wdir$WSPD_max_mph,
#                             color = ~as.factor(wdir$WDIR_txt),
#                             text = "→",
#                             showarrow = FALSE,
#                             font = list(size = 25, colors = wdir$color),
#                             textangle = wdir$WDIR_max-90)
# }


plots <- gridExtra::grid.arrange(p_wspd, p_temp, ncol = 1, heights = c(4.55, 4.0))

p <- switch(metric,
            'all' = plots,
            'temp' = p_temp,
            'wspd' = p_wspd,
            'wvht' = p_wvht)

  p


  }
