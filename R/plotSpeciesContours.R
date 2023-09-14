#' @title plotSpeciesContours: plots point intercept species ranges and photocover as pie charts
#'
#' @include getPhotoCover.R sumPISpecies.R
#'
#' @import ggplot2
#' @importFrom dplyr filter group_by mutate select summarize
#' @importFrom tidyr pivot_wider
#' @importFrom scatterpie geom_scatterpie
#' @importFrom ggpubr as_ggplot get_legend ggarrange
#'
#' @description This function plots a loess smoothed contour averaging the transects across all years specified.
#' Point intercept ranges are plotted along the contours by year for each of the five main species groups
#' (REDGRP, ASCNOD, FUCSPP, MUSSPP, BARSPP). Photoplot cover is plotted as median cover and median elevation
#' for each target species plot.
#'
#' @param park Choose a park to plot
#' \describe{
#' \item{'ACAD'}{Includes only sites in Acadia National Park (default)}
#' \item{'BOHA'}{Includes only sites in Boston Harbor Islands National Recreation Area}
#' }
#'
#' @param location Must choose a location to plot
#' \describe{
#' \item{"BASHAR"}{Bass Harbor, ACAD (default)}
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
#' Can specify a vector of years.
#'
#' @param palette Choices are "default" or "viridis". Default assigns logical colors to common species.
#' Viridis uses a color-blind friendly palette of blues, purples and yellows.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns a plot faceted on location and target species group
#' plotSpeciesContours()
#'
#' # Other variations
#' spp = c("ALGRED", "ASCNOD", "BARSPP", "NONCOR", "FUCSPP", "ULVLAC")
#' plotSpeciesContours(location = "CALISL", palette = "default", title = FALSE,
#'                    species = spp)
#'
#' }
#'
#'
#' @return Returns a ggplot object of percent cover from photos filtered by function arguments
#' @export

plotSpeciesContours <- function(park = "ACAD", location = "BASHAR",
                           palette = c('default'),
                           xlab = "Distance (m)", ylab = "Elevation MLLW (m)",
                           years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                           plot_title = NULL, QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(park %in% c( "ACAD", "BOHA"))
  stopifnot(location %in% c("BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(palette %in% c("default", "viridis"))


  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.

  # create color palette by species code

  cols = c("ASCNOD" = "#C5B47B", "BARSPP" = "#A9A9A9", "FUCSPP" = "#FFD560",
           "MUSSPP" = "#6F88BF", "REDGRP" = "#FF4C53")

  shps = c("ASCNOD" = 23, "BARSPP" = 24, "FUCSPP" = 25,
           "MUSSPP" = 23, "REDGRP" = 25)

  sz = c("ASCNOD" = 3.5, "BARSPP" = 3, "FUCSPP" = 3,
         "MUSSPP" = 3.5, "REDGRP" = 2)

  labels = c("ASCNOD" = "A. nodosum (Knotted wrack)",
             "BARSPP" = "Barnacles",
             "FUCSPP" = "Fucus spp. (Rockweed)",
             "MUSSPP" = "Mussels",
             "REDGRP" = "Red algae group")

  targ_labs <-  c("Ascophyllum" = "A. nodosum (knotted wrack)",
                  "Barnacle" = "Barnacle",
                  "Fucus" = "Fucus spp. (Rockweed)",
                  "Mussel" = "Mussels",
                  "Red Algae" = "Red algae group")

  loc_labs <- c("BASHAR" = "Bass Harbor", "LITHUN" = "Little Hunter", "LITMOO" = "Little Moose",
                "OTTPOI" = "Otter Point", "SCHPOI" = "Schoodic Point", "SHIHAR" = "Ship Harbor",
                "CALISL" = "Calf Island", "GREISL" = "Green Island", "OUTBRE" = "Outer Brewster")

  # Compile photo data
  photo1 <- suppressWarnings(force(getPhotoCover(park = park, location = location, plotName = 'all',
                                               category = 'all', years = years, QAQC = FALSE,
                                               species = c("ASCNOD", "BARSPP", "FUCSPP",
                                                           "MUSSPP", "ALGRED", "CHOMAS"),
                                               target_species = 'all'))) |>
                           dplyr::filter(!is.na(Perc_Cover))

  # Combine ALGRED and CHOMAS
  photo <- photo1 |> mutate(Spp_Code = ifelse(Spp_Code %in% c("ALGRED", "CHOMAS"), "REDGRP", Spp_Code),
                          Spp_Name = ifelse(Spp_Code %in% "REDGRP", "Red algae group", Spp_Name))

  # combine cover for red group
  photo_sum1 <- photo |> group_by(Site_Code, Loc_Code, Year, Spp_Code, Plot_Name,
                                Target_Species, Bolt_MLLW_Elev) |>
                       summarize(tot_cov = sum(Perc_Cover, na.rm = T), .groups = 'drop') |> ungroup()

  # summarize median cover

  photo_sum <- photo_sum1 |> group_by(Site_Code, Loc_Code, Year, Target_Species, Spp_Code) |>
    summarize(avg_cover = mean(tot_cov, na.rm = T),
              med_cover = median(tot_cov, na.rm = T),
              elev = median(Bolt_MLLW_Elev, na.rm = T),
              .groups = 'drop')

  # Compile species PI data
  spdat1 <- suppressWarnings(force(sumPISpecies(park = park, location = location, plotName = 'all',
                                               years = years,
                                               QAQC = FALSE,
                                               species = c("ASCNOD", "BARSPP", "FUCSPP",
                                                           "MUSSPP", "ALGRED", "CHOMAS"))))

  # Combine ALGRED and CHOMAS
  spdat <- spdat1 |> mutate(Spp_Code = ifelse(Spp_Code %in% c("ALGRED", "CHOMAS"), "REDGRP", Spp_Code),
                            Spp_Name = ifelse(Spp_Code %in% "REDGRP", "Red algae group", Spp_Name))

  trdat <- spdat |> select(Site_Code, Loc_Code, #Year, Plot_Name,
                           elev = Elevation_MLLW_m, dist = Distance_m) |>
                    unique() |> na.omit()

  # Smooth contours across all transects and years
  trsm <- loess(dist ~ elev, data = trdat, span = 0.3)
  trsm_dat <- cbind(trdat, dist_pred = predict(trsm))

  # Predict distance for photo plots
  photo_dist <- cbind(photo_sum, dist = predict(trsm, newdata = photo_sum$elev)) |> filter(!is.na(dist))
  photo_dist_wide <- photo_dist |> select(Site_Code, Loc_Code, Year, Target_Species,
                                          Spp_Code, avg_cover, elev, dist) |>
                                   pivot_wider(names_from = Spp_Code,
                                               values_from = avg_cover, values_fill = 0)

  # Summarize species PIs, then predict distance from elevations
  sp_sum <- spdat |> group_by(Site_Code, Loc_Code, Year, Spp_Code, Spp_Name) |>
    summarize(elev_min = min(PI_Elevation, na.rm = T),
              elev_max = max(PI_Elevation, na.rm = T),
              elev_med = median(PI_Elevation, na.rm = T),
              elev_l95 = quantile(PI_Elevation, probs = 0.025, na.rm = T),
              elev_u95 = quantile(PI_Elevation, probs = 0.975, na.rm = T),
              elev_l25 = quantile(PI_Elevation, probs = 0.25, na.rm = T),
              elev_u75 = quantile(PI_Elevation, probs = 0.75, na.rm = T),
              .groups = 'drop')

  # Create new datasets for prediction for each stat
  sp_min <- sp_sum |> select(Site_Code:Spp_Name, elev = elev_min)
  sp_med <- sp_sum |> select(Site_Code:Spp_Name, elev = elev_med)
  sp_max <- sp_sum |> select(Site_Code:Spp_Name, elev = elev_max)
  sp_l25 <- sp_sum |> select(Site_Code:Spp_Name, elev = elev_l25)
  sp_u75 <- sp_sum |> select(Site_Code:Spp_Name, elev = elev_u75)

  # Predict distances for each stat based on its elevation
  sp_dist <- cbind(sp_sum,
                   dist_med = predict(trsm, newdata = sp_med),
                   dist_min = predict(trsm, newdata = sp_min),
                   dist_max = predict(trsm, newdata = sp_max),
                   dist_l25 = predict(trsm, newdata = sp_l25),
                   dist_u75 = predict(trsm, newdata = sp_u75))

  # Plotting will be elevation (y) ~ distance (x), but needed the reverse to predict dist
  # in the loess model.
  p1 <-
  ggplot(trsm_dat, aes(y = elev, x = dist_pred)) + theme_rocky() +
   geom_smooth(data = trsm_dat, method = 'loess', formula = 'y~x',
                  color = '#676767', span = 0.2, se = F) +
   geom_segment(data = sp_dist,
                aes(y = elev_min, yend = elev_max,
                    x = dist_min, xend = dist_max,
                    color = Spp_Code, group = Spp_Code),
                linewidth = 2.5, alpha = 0.7) +
   geom_point(data = sp_dist, aes(x = dist_med, y = elev_med,
                                  fill = Spp_Code, group = Spp_Code,
                                  shape = Spp_Code),
              position = position_dodge2(width = 5), size = 2, color = 'black') +
   scale_shape_manual(values = shps, name = "Species", breaks = names(shps),
                      labels = labels) +
   scale_color_manual(values = cols, name = "Species",
                      breaks = names(cols), labels = labels) +
   scale_fill_manual(values = cols, name = "Species",
                     breaks = names(cols), labels = labels) +
   facet_wrap(~Year, ncol = 1) + theme(legend.position = 'right') +
   ylim(range(trsm_dat$elev)[1]-1, range(trsm_dat$elev)[2] + 1)

  p_leg <- as_ggplot(get_legend(p1))

  p2 <- p1 +
   geom_scatterpie(data = photo_dist_wide, aes(x = dist, y = elev + 1.2), pie_scale = 3,
                   cols = c("ASCNOD", "BARSPP", "FUCSPP", "MUSSPP", "REDGRP")) +
   coord_equal() + labs(x = "Distance (m)", y = "Elevation MLLW (m)") +
    theme(legend.position = 'none')

  p <- ggpubr::ggarrange(p2, p_leg, nrow = 1, ncol = 2, widths = c(4, 1.5))

  suppressWarnings(print(p))

}
