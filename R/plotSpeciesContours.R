#' @title plotSpeciesContours: plots point intercept species ranges and photocover as pie charts
#'
#' @include getPhotoCover.R sumPISpecies.R
#'
#' @import ggplot2
#' @importFrom dplyr case_when filter group_by mutate select summarize
#' @importFrom tidyr pivot_wider
#' @importFrom scatterpie geom_scatterpie
#' @importFrom ggpubr as_ggplot get_legend ggarrange
#' @importFrom gridExtra grid.arrange
#'
#' @description This function plots a loess smoothed contour averaging the transects across all years specified.
#' Point intercept ranges are plotted along the contours by year for each of the five main species groups
#' (REDGRP, ASCNOD, FUCSPP, MUSSPP, BARSPP). Photoplot cover is plotted as median cover and median elevation
#' for each target species plot.
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
#' @param plot_title If TRUE (default), plots location code as plot title. If FALSE, doesn't include a title.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns a plot faceted on location and target species group
#' plotSpeciesContours(location = "OTTPOI")
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

plotSpeciesContours <- function(location = "BASHAR",
                           palette = c('default'),
                           xlab = "Distance (m)", ylab = "Elevation MLLW (m)",
                           years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                           plot_title = TRUE, QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(location %in% c("BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(palette %in% c("default", "viridis"))

  if(length(location) > 1){stop("Multiple locations specified. Function can only plot one location at a time.")}

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
  photo1 <- suppressWarnings(force(getPhotoCover(location = location, plotName = 'all',
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
  spdat1 <- suppressWarnings(force(sumPISpecies(location = location, plotName = 'all',
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
  trsm <- loess(dist ~ elev, data = trdat, span = 0.6)
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

#  pie_size <- diff(range(trsm_dat$dist_pred))/diff(range(trsm_dat$elev)) * 0.2

  pie_size <- case_when(location %in% c("BASHAR", "CALISL", "OTTPOI", "SCHPOI") ~ 2.5,
                        location %in% c("LITHUN") ~ 3,
                        location %in% c("GREISL") ~ 6,
                        location %in% c("LITMOO") ~ 1.5,
                        location %in% c("CALISL") ~ 2,
                        location %in% c("OUTBRE") ~ 15,
                        location %in% c("SHIHAR") ~ 13,
                        )

  pie_ynudge <- case_when(location %in% c("BASHAR", "CALISL") ~ pie_size * 0.5,
                          location %in% c("LITHUN") ~ pie_size * 0.3,
                          location %in% c("LITMOO") ~ pie_size * 0.2,
                          location %in% c("OTTPOI", "SCHPOI") ~ pie_size * 0.5,
                          location %in% c("OUTBRE") ~ pie_size * 0.4,
                          location %in% c("GREISL") ~ pie_size * 0.2,
                          location %in% c("SHIHAR") ~ pie_size * 0.2
  )

  pie_ylim <- ifelse(location %in% "SHIHAR", 0.4, 0.3)

  # Nudge elevation of vertical transect species bands, for when they overlap
  sp_dist <- sp_dist |> mutate(elev_min_nudge =
                                 case_when(Spp_Code %in% "BARSPP" ~ elev_min - 0.2,
                                           Spp_Code %in% "MUSSPP" ~ elev_min,
                                           Spp_Code %in% "ASCNOD" ~ elev_min + 0.2,
                                           Spp_Code %in% "FUCSPP" ~ elev_min + 0.4,
                                           Spp_Code %in% "REDGRP" ~ elev_min + 0.6,
                                           TRUE ~ elev_min),
                               elev_max_nudge =
                                 case_when(Spp_Code %in% "BARSPP" ~ elev_max - 0.2,
                                           Spp_Code %in% "MUSSPP" ~ elev_max,
                                           Spp_Code %in% "ASCNOD" ~ elev_max + 0.2,
                                           Spp_Code %in% "FUCSPP" ~ elev_max + 0.4,
                                           Spp_Code %in% "REDGRP" ~ elev_max + 0.6,
                                           TRUE ~ elev_max),
                               elev_l25_nudge =
                                 case_when(Spp_Code %in% "BARSPP" ~ elev_l25 - 0.2,
                                           Spp_Code %in% "MUSSPP" ~ elev_l25,
                                           Spp_Code %in% "ASCNOD" ~ elev_l25 + 0.2,
                                           Spp_Code %in% "FUCSPP" ~ elev_l25 + 0.4,
                                           Spp_Code %in% "REDGRP" ~ elev_l25 + 0.6,
                                           TRUE ~ elev_l25),
                               elev_u75_nudge =
                                 case_when(Spp_Code %in% "BARSPP" ~ elev_u75 - 0.2,
                                           Spp_Code %in% "MUSSPP" ~ elev_u75,
                                           Spp_Code %in% "ASCNOD" ~ elev_u75 + 0.2,
                                           Spp_Code %in% "FUCSPP" ~ elev_u75 + 0.4,
                                           Spp_Code %in% "REDGRP" ~ elev_u75 + 0.6,
                                           TRUE ~ elev_u75))


  p1 <-
  ggplot(trsm_dat, aes(y = elev, x = dist_pred)) + theme_rocky() +
   # geom_smooth(data = trsm_dat, method = 'loess', formula = 'y~x',
   #                color = '#676767', span = 0.2, se = F) +
   geom_line(color = '#676767')+
   geom_segment(data = sp_dist,
                aes(y = elev_min_nudge, yend = elev_max_nudge,
                    x = dist_min, xend = dist_max,
                    color = Spp_Code, group = Spp_Code),
                linewidth = 2.5, alpha = 0.7) +
   # geom_segment(data = sp_dist,
   #              aes(y = elev_l25_nudge, yend = elev_u75_nudge,
   #                  x = dist_l25, xend = dist_u75,
   #                  color = Spp_Code, group = Spp_Code),
   #                  linewidth = 2.5, alpha = 0.7) +
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
   {if(length(years) > 1)facet_wrap(~Year, ncol = 1)} +
   {if(plot_title == TRUE)labs(title = location)} +
   theme(legend.position = 'right',
         plot.margin = unit(c(0.1, 0.01, 0.1, 0.01), 'cm'),
         legend.margin = margin(r = 0, l = 0, unit = 'cm'),
         legend.box.margin = margin(r = 0, l = 0, unit = 'cm'))#+
   #ylim(-2, 7) #+
   #xlim(range(trsm_dat$dist)[1] * 0.95, range(trsm_dat$dist)[2] * 1.05)

  p_leg <- as_ggplot(get_legend(p1))

  # Manually nudge ASCNOD and FUSSPP photoplot pies where they're both present at the location
  photo_dist_wide <- photo_dist_wide |>
    mutate(dist_nudge = case_when(Loc_Code == "LITMOO" & Target_Species == "Ascophyllum" ~ dist - pie_size/2,
                                  Loc_Code == "LITMOO" & Target_Species == "Fucus" ~ dist + pie_size/2,

                                  Loc_Code == "OTTPOI" & Target_Species == "Ascophyllum" ~ dist + pie_size/2,
                                  Loc_Code == "OTTPOI" & Target_Species == "Fucus" ~ dist - pie_size/2,

                                  Loc_Code == "SCHPOI" & Target_Species == "Ascophyllum" ~ dist - pie_size/2,
                                  Loc_Code == "SCHPOI" & Target_Species == "Fucus" ~ dist + pie_size/2,

                                  Loc_Code == "CALISL" & Target_Species == "Ascophyllum" ~ dist - pie_size/2,
                                  Loc_Code == "CALISL" & Target_Species == "Fucus" ~ dist + pie_size/2,

                                  Loc_Code == "GREISL" & Target_Species == "Ascophyllum" ~ dist + 0.6,
                                  Loc_Code == "GREISL" & Target_Species == "Fucus" ~ dist - 0.6,

                                  TRUE ~ dist
                                  ))

  p2 <- p1 +
   geom_scatterpie(data = photo_dist_wide, aes(x = dist_nudge, y = elev + pie_ynudge), pie_scale = pie_size,
                   cols = c("ASCNOD", "BARSPP", "FUCSPP", "MUSSPP", "REDGRP")) +
   coord_equal(expand = TRUE) + labs(x = "Distance (m)", y = "Elevation MLLW (m)") +
    theme(legend.position = 'none')

  if(length(years == 1)){
    p <- gridExtra::grid.arrange(p2, p_leg, nrow = 2, ncol = 1, heights = c(7, 1.5),
                                 layout_matrix = (cbind(c(1), c(2))))
  } else{
  #p <- ggpubr::ggarrange(p2, p_leg, nrow = 1, ncol = 2, widths = c(4, 1.5))
   p <- gridExtra::grid.arrange(p2, p_leg, nrow = 2, ncol = 2, widths = c(5, 1.5),
                            layout_matrix = (cbind(c(1, 1), c(2, NA))))
  }
  suppressWarnings(print(p))

}
