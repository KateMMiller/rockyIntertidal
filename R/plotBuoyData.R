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
#' \item{'wspd'}{Maximum daily windspeed in miles/hour and wind direction for winds > 35 miles/hour.}
#' }
#'
#' @param plot_title If specified, plots the title on the figure. If NULL, no plot title included.
#'
#' @param palette Choices are "default", "viridis", or "black". Default assigns specific colors to each metric.
#' Viridis uses a color-blind friendly palette of blues, purples and yellows.
#'
#' @param facet_col Numeric. Number of columns for the facet to plot. Defaults to 1.
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
#' plotBuoyData(metric = 'wspd'))
#'
#' plotWaterTemp(location = "SCHPOI", years = 2013:2022, palette = 'black')
#'
#' }
#'
#' @return Returns a ggplot object of water temperature data
#' @export

plotBuoyData <- function(park = "ACAD", palette = c('default'),
                          metric = "all",
                          years = 2011:as.numeric(format(Sys.Date(), "%Y")),
                          plot_title = NULL, facet_col = 1){

  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("ACAD", "BOHA"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2011)
  stopifnot(palette %in% c("default", "viridis", "black"))
  stopifnot(metric %in% c("all", "temp", "wspd"))

  # if(!requireNamespace("mgcv", quietly = TRUE) & gam == TRUE){
  #   stop("Package 'mgcv' needed for this function to work. Please install it.", call. = FALSE)
  # }

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  # Check for loaded buoy data.
  if(!"ACAD_buoy" %in% ls(envir = env)){
    stop("Must import ACAD and BOHA buoy data.")
  }

  if(!("WTMP_F_min" %in% names(get("ACAD_buoy", envir = env)))){
    stop("Must import ACAD and BOHA buoy data using simplify = TRUE in importWaterData().")
  }

  cols <- switch(palette, #WTMP_min, WTMP_max, WSPD_max, WVHT_max, WDIR_mean
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
              "WDIR_mean" = "Avg. Daily Wind direction (degrees)")

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
              legend.text = element_text(size = 9),
              legend.title = element_text(size = 9)) +
        ylab( "Daily Water Temp. (F)") +
        {if(length(years) > 3)
          scale_x_datetime(breaks = scales::breaks_width("6 months"), date_labels = "%m/%y")} +
        {if(length(years) <= 3 & length(years) > 1)
          scale_x_datetime(breaks = scales::breaks_width("2 months"), date_labels = "%m/%y")} +
        {if(length(years) == 1) scale_x_datetime(breaks = scales::breaks_width("1 month"), date_labels = "%m/%y")}
    # } else if(any(metric %in% "WTMP_F_min")){
    #     ggplot(dat, aes(x = DATE, y = WTMP_F_min)) +
    #       geom_line(color = cols[1]) +
    #       theme_rocky() +
    #       ylab( "Min. Water Temp. (F)") +
    #       {if(length(years) > 3)
    #         scale_x_datetime(breaks = scales::breaks_width("6 months"), date_labels = "%m/%y")} +
    #       {if(length(years) <= 3 & length(years) > 1)
    #         scale_x_datetime(breaks = scales::breaks_width("2 months"), date_labels = "%m/%y")} +
    #       {if(length(years) == 1) scale_x_datetime(breaks = scales::breaks_width("1 month"), date_labels = "%m/%y")}

  p_wvht <-
      ggplot(dat2, aes(x = DATE, y = WVHT_max)) +
        geom_line(color = cols[4]) +
        theme_rocky() +
        ylab( "Max. Wave Height (m)") +
        {if(length(years) > 3)
          scale_x_datetime(breaks = scales::breaks_width("6 months"), date_labels = "%m/%y")} +
        {if(length(years) <= 3 & length(years) > 1)
          scale_x_datetime(breaks = scales::breaks_width("2 months"), date_labels = "%m/%y")} +
        {if(length(years) == 1) scale_x_datetime(breaks = scales::breaks_width("1 month"), date_labels = "%m/%y")}

  dat2 <- dat2 |> mutate(WDIR_txt = case_when(between(WDIR_mean, 0, 45) ~ "Northerly",
                                            between(WDIR_mean, 45, 135) ~ "Easterly",
                                            between(WDIR_mean, 135, 225) ~ "Southerly",
                                            between(WDIR_mean, 225, 315) ~ "Westerly",
                                            between(WDIR_mean, 315, 360) ~ "Northerly"))
  wdir <- dat2 |> filter(WSPD_max_mph > 35)

  wind_cols = c('Northerly' = '#2b83ba',
                'Easterly' = '#abdda4',
                'Southerly' = '#fdae61',
                'Westerly' = '#d7191c')

    p_wspd <-
      ggplot(dat2, aes(x = DATE, y = WSPD_max_mph, color = WDIR_txt)) +
        geom_line(color = cols[3]) +
        geom_text(data = wdir, aes(angle = -WDIR_mean + 90), label="→", size = 10) +
        ylim(0, max(dat2$WSPD_max_mph) + 5) +
        theme_rocky() +
        theme(legend.position = 'bottom',
              legend.text = element_text(size = 9),
              legend.title = element_text(size = 9)) +
        ylab( "Max. Wind Speed (mph)") +
        scale_color_manual(values = wind_cols, breaks = names(wind_cols), name = "Wind direction") +
        {if(length(years) > 3)
          scale_x_datetime(breaks = scales::breaks_width("6 months"), date_labels = "%m/%y")} +
        {if(length(years) <= 3 & length(years) > 1)
          scale_x_datetime(breaks = scales::breaks_width("2 months"), date_labels = "%m/%y")} +
        {if(length(years) == 1) scale_x_datetime(breaks = scales::breaks_width("1 month"), date_labels = "%m/%y")}

  #p_list <- c(p_temp, p_wvht, p_wspd)

  p <- switch(metric,
              'all' = gridExtra::grid.arrange(p_temp, p_wspd, ncol = 1),
              'temp' = p_temp,
              'wspd' = p_wspd,
              'wvht' = p_wvht)

suppressWarnings(print(p))


}
