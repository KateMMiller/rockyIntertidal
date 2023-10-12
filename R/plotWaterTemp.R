#' @title plotWaterTemp: plots water temperature at high tide time series
#'
#' @import ggplot2
#' @importFrom dplyr group_by filter left_join mutate right_join select summarize
#' @importFrom purrr map_dfr
#' @importFrom scales breaks_width
#'
#' @description This function plots water temperature at high tide by location. Note that function works on
#' compiled logger data, rather than working off the raw logger data. Compiled datasets should be imported
#' using importWaterTemp(). To speed up plotting of multiple locations or long time-series, plot the simplified
#' water temperature data derived from importWaterTemp(simplify = TRUE).
#'
#' @param park Include data from all parks, or choose one.
#' \describe{
#' \item{'all'}{Includes all parks monitored in the network}
#' \item{'ACAD'}{Includes only sites in Acadia National Park}
#' \item{'BOHA'}{Includes only sites in Boston Harbor Islands National Recreation Area}
#' }
#'
#' @param location Specify location to generate water temperature plot. If more than one location chosen, will
#' plot lines on the same plot unless facet = TRUE. Note that plotting data from all locations will be slow.
#'  \describe{
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
#' @param years Filter on year of data collected. Default is 2011 to current year.
#'
#' @param xlab Quoted text label for x axis. If not specified, defaults to 'Year'
#'
#' @param ylab Quoted text label for y axis. If not specified, defaults to 'High Tide Temp (F)'
#'
#' @param plot_title If specified, plots the title on the figure. If NULL, no plot title included.
#'
#' @param palette Choices are "default", "viridis", "greyscale", or "black". Default assigns logical colors to common species.
#' Viridis uses a color-blind friendly palette of blues, purples and yellows.
#'
#' @param facet Logical. If TRUE, will plot locations in separate facets. FALSE (default) plots all locations
#' on one figure.
#'
#' @param plot_tmin Logical. If TRUE, will plot a line connecting minimum recorded temperatures across years.
#'
#' @param plot_tmax Logical. If TRUE, will plot a line connecting maximum recorded temperatures across years.
#'
#' @param facet_col Numeric. Number of columns for the facet to plot. Defaults to 1.
#'
#' @param legend_position Position of legend. Options are legend.position options in ggplot2. Default is 'bottom'.
#' For no legend, specify 'none'.
#'
#' @param plotly Logical. If TRUE, converts ggplot object to plotly object and includes tooltips. If FALSE (default),
#' plots a ggplot object.
#'
#' @param gam Logical. If FALSE (default), only plots temperature values. If TRUE, plots a trend line
#' derived from generalize additive modelling. NOT CURRENTLY FUNCTIONAL
#'
#' @examples
#' \dontrun{
#'
#' path <-
#'   "Z:/PROJECTS/MONITORING/Rocky_Intertidal/NETN/5_Data/Data_Files/Temperature/Compiled_HT_water_temps_2011-2022/"
#' importWaterTemp(path, simplify = TRUE, buoy = TRUE) # import water temp and buoy data and simplify to daily stats.
#'
#' # Default filter returns a plot for BASHAR
#' plotWaterTemp()
#'
#' # Other variations
#' plotWaterTemp(location = "CALISL", years = 2016:2022, plot_title = "Calf Island",
#' plot_tmax = T, plot_tmin = T)
#'
#' plotWaterTemp(location = "SCHPOI", years = 2013:2022, palette = 'black')
#'
#' }
#'
#' @return Returns a ggplot object of water temperature data
#' @export

plotWaterTemp <- function(park = "all", location = "all", palette = c('default'),
                          xlab = "Year", ylab = "High Tide Water Temp (F)", gam = FALSE,
                          facet = TRUE, plot_tmin = FALSE, plot_tmax = FALSE,
                          years = 2011:as.numeric(format(Sys.Date(), "%Y")),
                          plot_title = NULL, facet_col = 1, plotly = FALSE, legend_position = 'bottom'){


  if(!requireNamespace("plotly", quietly = TRUE) & plotly == TRUE){
    stop("Package 'plotly' needed for this function for plotly = TRUE. Please install it or set plotly = FALSE.", call. = FALSE)
  }

  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c("all", "ACAD", "BOHA"))
  stopifnot(location %in% c("all", "BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2011)
  stopifnot(palette %in% c("default", "viridis", "black", "greyscale"))
  stopifnot(is.logical(plot_tmin))
  stopifnot(is.logical(plot_tmax))


  # if(!requireNamespace("mgcv", quietly = TRUE) & gam == TRUE){
  #   stop("Package 'mgcv' needed for this function to work. Please install it.", call. = FALSE)
  # }

  env <- if(exists("ROCKY")){ROCKY} else {.GlobalEnv}

  # Check for loaded logger data. It won't catch everything, but will catch when no logger files have been loaded.
   if(!any(c("BASHAR", "LITHUN", "LITMOO", "OTTPOI",
             "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE") %in% ls(envir = env))){
     stop("Must have at least one compiled logger file loaded in global environment and named its location code in all uppercase.")
   }

  locs <-
    if(all(location == 'all')){c("BASHAR", "LITHUN", "LITMOO", "OTTPOI", "SCHPOI",
                            "SHIHAR", "CALISL", "GREISL", "OUTBRE")
      } else {location}

  cols <- c("BASHAR" = "#440154", "LITHUN" = "#472D7B", "LITMOO" = "#3B528B",
            "OTTPOI" = "#2C728E", "SCHPOI" = "#21908c", "SHIHAR" = "#27ad81",
            "CALISL" = "#5dc863", "GREISL" = "#aadc32", "OUTBRE" = "#fde725")
  #scales::show_col(show_col::viridis_pal()(9))

  labels <- c("BASHAR" = "Bass Harbor", "LITHUN" = "Little Hunter", "LITMOO" = "Little Moose",
              "OTTPOI" = "Otter Point", "SCHPOI" = "Schoodic Point", "SHIHAR" = "Ship Harbor",
              "CALISL" = "Calf Island", "GREISL" = "Green Island", "OUTBRE" = "Outer Brewster")


  # row bind all the locations specified together into 1 data frame
  ht_temp <- purrr::map_dfr(locs, function(x){get(x, envir = env)})

  # Set Loc_Code as factor, so facet is ordered by park
  ht_temp$Loc_Code <- factor(ht_temp$Loc_Code, levels = c("BASHAR", "LITHUN", "LITMOO",
                                                          "OTTPOI", "SCHPOI", "SHIHAR",
                                                          "CALISL", "GREISL", "OUTBRE"))

  # Filter data based on fxn arguments
  ht_temp_park <- if(any(park == 'all')){ht_temp
  } else {filter(ht_temp, Site_Code %in% park)}

  ht_temp_loc <- if(any(location == 'all')){ht_temp_park
  } else {filter(ht_temp_park, Loc_Code %in% location)}

  ht_temp_years <- filter(ht_temp_loc, Year %in% years)

  temp_stats <- ht_temp_years |> group_by(Site_Code, Loc_Code, Year) |>
    summarize(tmax = max(Degrees_F, na.rm = T),
              tmin = min(Degrees_F, na.rm = T),
              tmed = median(Degrees_F, na.rm = T),
              .groups = 'drop')

  # Left join to find date of tmax and tmin
  ht_tmax <- left_join(temp_stats |> select(-tmin), ht_temp_years,
                        by = c("Site_Code", "Loc_Code", "Year", "tmax" = "Degrees_F"))
  ht_tmin <- left_join(temp_stats |> select(-tmax), ht_temp_years,
                        by = c("Site_Code", "Loc_Code", "Year", "tmin" = "Degrees_F"))

  leg_position <- ifelse(facet == TRUE, 'none', legend_position)

  p <- suppressWarnings(
    ggplot(ht_temp_years, aes(x = timestamp, y = Degrees_F, color = Loc_Code, group = Loc_Code)) +
    geom_line(aes(color = Loc_Code, text = paste0("Site: ", Loc_Code, "<br>",
                                                  "Time: ", timestamp, "<br>",
                                                  "Degrees F: ", Degrees_F))) +
    theme_rocky() +
    {if(length(years) > 3) scale_x_datetime(breaks = scales::breaks_width("6 months"), date_labels = "%m/%y")}+
    {if(length(years) <= 3 & length(years) > 1)
      scale_x_datetime(breaks = scales::breaks_width("2 months"), date_labels = "%m/%y")} +
    {if(length(years) == 1) scale_x_datetime(breaks = scales::breaks_width("1 month"), date_labels = "%m/%y")}+
    {if(all(palette == 'default'))
      scale_color_manual(values = cols, name = "Location", breaks = names(cols), labels = labels)} +
    {if(all(palette == 'viridis')) scale_color_viridis_d("Loc_Name")} +
    {if(all(palette == "black")) scale_color_manual(values = rep("black", 9))} +
    {if(all(palette == "greyscale")) scale_color_manual(values = rep("#676767", 9))} +
    {if(facet == TRUE) facet_wrap(~Loc_Code, labeller = as_labeller(labels), ncol = facet_col)} +
    {if(plot_tmax == TRUE) geom_line(data = ht_tmax, aes(x = timestamp, y = tmax), linetype = 'dashed')} +
    {if(plot_tmin == TRUE) geom_line(data = ht_tmin, aes(x = timestamp, y = tmin), linetype = 'dashed')} +
    labs(y = ylab, x = xlab, title = plot_title) +
    theme(legend.position = leg_position,
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
  )

  pp <-
  if(plotly == TRUE){plotly::ggplotly(p, tooltip = 'text')} else {p}

  return(pp)
}
