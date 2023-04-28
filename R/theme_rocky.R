#' @title theme_rocky: custom ggplot2 theme for rockyIntertidal
#'
#' @import ggplot2
#'
#' @description This function customizes a theme for plotting data in the package, including removing the
#' default panel grids from ggplot2 figures.
#'
#' @return This function must be used in conjunction with a ggplot object
#'
#' @examples
#' \dontrun{
#' importData()
#' spp_pi <- sumPISppDetections(drop_missing = T)
#' ggplot(spp_pi |> filter(Plot_Name == "T3"),
#'          aes(x = dist_bolt_first, y = elev_first,
#'              group = as.factor(Year), color = as.factor(Year))) +
#'        geom_line(lwd = 1) +
#'        facet_wrap(~Loc_Code, scales = 'free') +
#'        scale_color_brewer("Year", type = 'div', palette = 'Dark2') +
#'        theme_rocky()
#'}
#' @export


theme_rocky <- function(){theme(panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.background = element_rect(color = '#696969', fill = 'white', size = 0.4),
                                plot.background = element_blank(),
                                strip.background = element_rect(color = '#696969', fill = 'grey90', size = 0.4),
                                legend.key = element_blank(),
                                axis.line.x = element_line(color = "#696969", size = 0.4),
                                axis.line.y = element_line(color = "#696969", size = 0.4),
                                axis.ticks = element_line(color = "#696969", size = 0.4)
)}
