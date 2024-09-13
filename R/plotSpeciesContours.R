#' @title plotSpeciesContours: plots point intercept species ranges and photocover as pie charts
#'
#' @include getPhotoCover.R sumPISpecies.R
#'
#' @import ggplot2
#' @importFrom dplyr case_when filter group_by mutate select summarize
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom scatterpie geom_scatterpie
#' @importFrom ggpubr as_ggplot get_legend
#' @importFrom gridExtra grid.arrange
#' @importFrom cowplot draw_grob
#' @importFrom purrr pmap_dfr
#'
#' @description This function plots a loess smoothed contour averaging the transects
#' across all years specified. Point intercept minimum and maximum elevation ranges
#' are plotted along the contours by year for each of the main species groups:
#' (REDGRP, ASCNOD, FUCSPP, MUSSPP, BARSPP, NONCOR). Photoplot cover is plotted as
#' median cover and median elevation for each target species plot.
#'
#' @param site Must choose a site to plot
#' \describe{
#' \item{"BASHAR"}{Bass Harbor, ACAD (default)}
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
#' Can specify a vector of years.
#'
#' @param palette Choices are "default" or "viridis". Default assigns logical colors to common species.
#' Viridis uses a color-blind friendly palette of blues, purples and yellows.
#'
#' @param plot_title Logical. If TRUE (default), plots site code as plot title. If FALSE, doesn't include a title.
#'
#' @examples
#' \dontrun{
#'
#' importData()
#'
#' # Default filter returns a plot faceted on site and target species group
#' plotSpeciesContours(site = "OTTPOI")
#'
#' # Other variations
#' spp = c("ALGRED", "ASCNOD", "BARSPP", "NONCOR", "FUCSPP", "ULVLAC")
#' plotSpeciesContours(site = "CALISL", palette = "default", title = FALSE,
#'                    species = spp)
#'
#' }
#'
#'
#' @return Returns a ggplot object of point intercept and percent cover data filtered by function arguments
#' @export

plotSpeciesContours <- function(site = "BASHAR",
                           palette = c('default'),
                           xlab = "Distance (m)", ylab = "Elevation MLLW (m)",
                           years = 2013:as.numeric(format(Sys.Date(), "%Y")),
                           plot_title = TRUE, QAQC = FALSE){


  # Match args and class; match.args only checks first match in vector, so have to do it more manually.
  stopifnot(site %in% c("BASHAR", "LITHUN", "LITMOO", "OTTPOI",
                            "SCHPOI", "SHIHAR", "CALISL", "GREISL", "OUTBRE"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2013)
  stopifnot(palette %in% c("default", "viridis"))
  stopifnot(is.logical(plot_title))

  if(length(site) > 1){stop("Multiple sites specified. Function can only plot one site at a time.")}

  stopifnot(exists("ROCKY") | exists("Bolts")) # Checks that ROCKY env exists, or Bolts view is in global env.

  # create color palette by species code

  cols = c("NONCOR" = "#574F91", "BARSPP" = "#A9A9A9", #"ALGGRE" = "#C4E133",
           "MUSSPP" = "#6F88BF",
           "ASCNOD" = "#C5B47B", "FUCSPP" = "#FFD560", "REDGRP" = "#FF4C53")

  shps = c("NONCOR" = 23, "BARSPP" = 24, #"ALGGRE" = 23,
           "MUSSPP" = 23,
           "ASCNOD" = 23, "FUCSPP" = 25, "REDGRP" = 25)

  sz = c("NONCOR" = 5.5, "BARSPP" = 5, #"ALGGRE" = 4.5,
         "MUSSPP" = 5.5,
         "ASCNOD" = 5.5, "FUCSPP" = 5, "REDGRP" = 4)

  labels = c("NONCOR" = "Crustose non-coraline",
             "BARSPP" = "Barnacles",
             #"ALGGRE" = "Algae - Green",
             "MUSSPP" = "Mussels",
             "ASCNOD" = "A. nodosum (Knotted wrack)",
             "FUCSPP" = "Fucus spp. (Rockweed)",
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
  photo1 <- suppressWarnings(force(getPhotoCover(site = site, plotName = 'all',
                                               category = 'all', years = years, QAQC = FALSE,
                                               species = c("ASCNOD",  "ASCEPI", "BARSPP",
                                                           "FUCSPP", "FUCEPI", "NONCOR",
                                                           "MUSSPP", "ALGRED", "CHOMAS"), #"ALGGRE"),
                                               community = 'all'))) |>
                           dplyr::filter(!is.na(PercentCover))

  # Combine ALGRED and CHOMAS
  photo <- photo1 |> mutate(CoverCode = case_when(CoverCode %in% c("ALGRED", "CHOMAS") ~ "REDGRP",
                                                 CoverCode %in% c("FUCSPP", "FUCEPI") ~ "FUCSPP",
                                                 CoverCode %in% c("ASCNOD", "ASCEPI") ~ "ASCNOD",
                                                 TRUE ~ CoverCode),
                            CoverType = case_when(CoverCode %in% "REDGRP" ~ "Red algae group",
                                                 CoverCode %in% "FUCSPP" ~ "Fucus spp. (Rockweed)",
                                                 CoverCode %in% "ASCNOD" ~ "A. nodosum (knotted wrack)",
                                                 TRUE ~ CoverType))

  # combine cover for red group
  photo_sum1 <- photo |> group_by(UnitCode, SiteCode, Year, CoverCode, CoverType, PlotName,
                                 CommunityType, Bolt_MLLW_Elev) |>
                         summarize(tot_cov = sum(PercentCover, na.rm = T),
                                   .groups = 'drop') |> ungroup()

  # summarize median cover
  photo_sum <- photo_sum1 |> group_by(UnitCode, SiteCode, Year, CommunityType, CoverCode) |>
    summarize(avg_cover = mean(tot_cov, na.rm = T),
              med_cover = median(tot_cov, na.rm = T),
              elev = median(Bolt_MLLW_Elev, na.rm = T),
              .groups = 'drop')

  # Compile species PI data
  spdat1 <- suppressWarnings(force(sumPISpecies(site = site, plotName = 'all',
                                               years = years,
                                               QAQC = FALSE,
                                               species = c("ASCNOD", "BARSPP", "FUCSPP",#"ALGGRE",
                                                           "MUSSPP", "ALGRED", "CHOMAS", "NONCOR"))))

  # Combine ALGRED and CHOMAS
  spdat <- spdat1 |> mutate(CoverCode = ifelse(CoverCode %in% c("ALGRED", "CHOMAS"), "REDGRP", CoverCode),
                            CoverType = ifelse(CoverCode %in% "REDGRP", "Red algae group", CoverType))

  #OUTBRE smooth is really funky, so only smoothing one transect
  trdat <- if(site == "OUTBRE"){
    spdat |> filter(Plot_Name == "T1") |>
    select(UnitCode, SiteCode, Year, PlotName,
           elev = PI_Elevation, dist = PI_Distance) |>
    unique() |> na.omit() |> arrange(elev)
  } else{
    spdat |> arrange(PI_Elevation) |>
      select(UnitCode, SiteCode,
             elev = PI_Elevation, dist = PI_Distance) |>
      unique() |> na.omit() |> arrange(elev)
  }


  # Smooth contours across all transects and years
  span_loc <- ifelse(site == "OUTBRE", 0.5, 0.6)
  trsm <- loess(dist ~ elev, data = trdat, span = span_loc, degree = 1)

  trsm_dat <- cbind(trdat, dist_pred = predict(trsm, trdat))

  # ggplot(trsm_dat, aes(y = elev, x = dist_pred)) + theme_rocky() +
  #   geom_line(color = '#676767') +
  #   geom_line(data = spdat, aes(y = PI_Elevation, x = PI_Distance), color = 'lightblue')+
  #   geom_line(data = spdat1, aes(y = PI_Elevation, x = PI_Distance), color = 'lightgreen')

  # Predict distance for photo plots
  photo_dist <- cbind(photo_sum, dist = predict(trsm, newdata = photo_sum$elev)) #|> filter(!is.na(dist))

  # Red algae photo plots are sometimes lower elevation than the transect, and loess smoother
  # won't predict a distance value for those elevations. Replacing NA with farthest distance in trsm_dat
  # SHIHAR Ascophyllum photoplots are higher than the transect elevations. Adjusting by changing the max
  # elevation for Ascophyllum to be close to median of Asco point intercepts from 2013
  max_dist <- max(trsm_dat$dist_pred, na.rm = T)
  shidist <- 0.553

  photo_dist$dist[is.na(photo_dist$dist) & photo_dist$CommunityType == "Red Algae"] <- max_dist
  photo_dist$dist[is.na(photo_dist$dist) & photo_dist$CommunityType == "Ascophyllum" &
    photo_dist$SiteCode == "SHIHAR"] <- shidist

  photo_dist_wide <- photo_dist |> select(UnitCode, SiteCode, Year, CommunityType,
                                          CoverCode, avg_cover, elev, dist) |>
                                   pivot_wider(names_from = CoverCode,
                                               values_from = avg_cover, values_fill = 0)

  # Summarize species PIs, then predict distance from elevations
  sp_sum <- spdat |> group_by(UnitCode, SiteCode, Year, CoverCode, CoverType) |>
    summarize(elev_min = min(PI_Elevation, na.rm = T),
              elev_max = max(PI_Elevation, na.rm = T),
              elev_med = median(PI_Elevation, na.rm = T),
              elev_l95 = quantile(PI_Elevation, probs = 0.025, na.rm = T),
              elev_u95 = quantile(PI_Elevation, probs = 0.975, na.rm = T),
              elev_l25 = quantile(PI_Elevation, probs = 0.25, na.rm = T),
              elev_u75 = quantile(PI_Elevation, probs = 0.75, na.rm = T),
              .groups = 'drop')

  # Create new datasets for prediction for each stat
  sp_min <- sp_sum |> select(UnitCode:CoverType, elev = elev_min)
  sp_med <- sp_sum |> select(UnitCode:CoverType, elev = elev_med)
  sp_max <- sp_sum |> select(UnitCode:CoverType, elev = elev_max)
  sp_l25 <- sp_sum |> select(UnitCode:CoverType, elev = elev_l25)
  sp_u75 <- sp_sum |> select(UnitCode:CoverType, elev = elev_u75)

  # Predict distances for each stat based on its elevation
  sp_dist <- cbind(sp_sum,
                   dist_med = predict(trsm, newdata = sp_med),
                   dist_min = predict(trsm, newdata = sp_min),
                   dist_max = predict(trsm, newdata = sp_max),
                   dist_l25 = predict(trsm, newdata = sp_l25),
                   dist_u75 = predict(trsm, newdata = sp_u75))

  pie_size <- case_when(site %in% c("BASHAR") ~ 3,
                        site %in% c("LITHUN") ~ 6,
                        site %in% c("LITMOO") ~ 3,
                        site %in% c("OTTPOI") ~ 3.5,
                        site %in% c("SCHPOI") ~ 3.5,
                        site %in% c("SHIHAR") ~ 1.75,
                        site %in% c("CALISL") ~ 3,
                        site %in% c("GREISL") ~ 4,
                        site %in% c("OUTBRE") ~ 5
                        )

  pie_ynudge <- case_when(site %in% c("LITHUN") ~ 2,
                          site %in% c("SHIHAR") ~ 4,
                          site %in% c("CALISL", "GREISL") ~ 3,
                          TRUE ~ pie_size
  )

  spdat_smooth <- cbind(spdat, dist_pred = predict(trsm, spdat |> select(elev = PI_Elevation)))

 p1 <-
  ggplot(trsm_dat, aes(y = elev, x = dist_pred)) + theme_rocky() +
   geom_line(color = '#676767')+
   geom_jitter(data = spdat_smooth, aes(x = dist_pred, y = PI_Elevation, color = CoverCode,
                                       fill = CoverCode, group = CoverCode),
              position = position_jitter(height = 1), alpha = 0.7) + #,
              #shape = 21, color = '#797979') +
   geom_point(data = sp_dist, aes(x = dist_med, y = elev_med,
                                  fill = CoverCode, group = CoverCode,
                                  shape = CoverCode),
              position = position_dodge2(width = 1), size = 2, color = 'black',
              stroke = 1.3) +
   scale_shape_manual(values = shps, name = "Species", breaks = names(shps),
                      labels = labels) +
   scale_color_manual(values = cols, name = "Species",
                      breaks = names(cols), labels = labels) +
   scale_fill_manual(values = cols, name = "Species",
                     breaks = names(cols), labels = labels) +
   {if(length(years) > 1)facet_wrap(~Year, ncol = 1)} +
   {if(plot_title == TRUE)labs(title = site)} +
   theme(legend.position = 'right', #+
         plot.margin = unit(c(0, 1.5, 0, 1), 'cm') )
         #legend.margin = margin(r = 1, l = 1, unit = 'cm')) #+
         #legend.box.margin = margin(r = 0.2, l = 0.2, unit = 'cm'))#+
   #ylim(-2, 7) #+
   #xlim(range(trsm_dat$dist)[1] * 0.95, range(trsm_dat$dist)[2] * 1.05)

  p_leg <- ggpubr::as_ggplot(ggpubr::get_legend(p1))

  # Manually nudge ASCNOD and FUSSPP photoplot pies where they're both present at the site
  photo_dist_wide <- photo_dist_wide |>
    mutate(dist_nudge = case_when(SiteCode == "LITMOO" & CommunityType == "Ascophyllum" ~ dist - pie_size/2,
                                  SiteCode == "LITMOO" & CommunityType == "Fucus" ~ dist + pie_size/2,

                                  SiteCode == "LITHUN" & CommunityType == "Mussel" ~ dist + 2,#pie_size/6,
                                  SiteCode == "LITHUN" & CommunityType == "Ascophyllum" ~ dist + 0.5, #pie_size/4,

                                  SiteCode == "OTTPOI" & CommunityType == "Ascophyllum" ~ dist + pie_size/2,
                                  SiteCode == "OTTPOI" & CommunityType == "Fucus" ~ dist - pie_size/2,

                                  SiteCode == "SCHPOI" & CommunityType == "Ascophyllum" ~ dist - pie_size/2,
                                  SiteCode == "SCHPOI" & CommunityType == "Fucus" ~ dist + pie_size/2,

                                  SiteCode == "CALISL" & CommunityType == "Ascophyllum" ~ dist - pie_size/2,
                                  SiteCode == "CALISL" & CommunityType == "Fucus" ~ dist + pie_size/2,

                                  SiteCode == "GREISL" & CommunityType == "Ascophyllum" ~ dist + 1.5,
                                  SiteCode == "GREISL" & CommunityType == "Fucus" ~ dist - 1.5,

                                  TRUE ~ dist
                                  ))

  #photo_dist_wide$elev_hor <- max(trsm_dat$elev + pie_size * 1.5)
  photo_dist_wide$pie_ynudge = photo_dist_wide$elev + photo_dist_wide$dist_nudge

  p2 <- p1 +
   geom_scatterpie(data = photo_dist_wide, aes(x = dist_nudge, y = pie_ynudge),
                   pie_scale = pie_size,
                   cols = c("ASCNOD", "BARSPP", "FUCSPP", "MUSSPP", "REDGRP")) +
   coord_equal(expand = TRUE) + labs(x = "Distance (m)", y = "Elevation MLLW (m)") +
    theme(legend.position = 'none')

  # if(length(years == 1)){
  #   # p <- gridExtra::grid.arrange(p2, p_leg, nrow = 2, ncol = 1, heights = c(7, 1.5),
  #   #                              layout_matrix = (cbind(c(1), c(2))))
  #
  #   p <- cowplot::plot_grid(p2, p_leg, nrow = 1, ncol = 2, rel_heights = c(0.9, 0.1),
  #                           align = 'hv')
  # } else{
  #p <- ggpubr::ggarrange(p2, p_leg, nrow = 1, ncol = 2, widths = c(4, 1.5))
   # p <- gridExtra::grid.arrange(p2, p_leg, nrow = 2, ncol = 2, widths = c(8, 1.5), heights = c(7, 1.5),
   #                          layout_matrix = (cbind(c(1, 1), c(2, NA))))

    # p <-
    #      cowplot::plot_grid(p2, p_leg, nrow = 1, ncol = 2)
    #                         #rel_widths = c(0.5, 1))#,
    #                         #rel_heights = c(10, 1))
    #                         #align = 'hv')

  p <- gridExtra::grid.arrange(p2, p_leg, nrow = 1, ncol = 2, widths = c(4, 1))
  # }
  return(p)
  #print(p)

}
