#' Subset to the specific hurricane that you will be mapping (Hurricane Ike) and to a single observation
#' time for that hurricane
#'
#' Subset hurricane data to single storm and observation
#'
#' This function takes a storm name and year and returns a single observation for the
#' storm at a specific moment in time.
#'
#' @param storm_name a string of the storm name (e.g. "Ike")
#' @param date_time the time of the observation
#' @return storms data frame with a single observation
#' @importFrom lubridate year
#' @importFrom stringr str_to_title
#' @importFrom dplyr filter
#'
#' @export
stormObservation <- function(storm_name, date_time){
  yr <- lubridate::year(date_time)
  search_id <- paste0(stringr::str_to_title(storm_name),"-",yr)
  storms %>%
    dplyr::filter(storm_id == search_id & date == date_time)
}

#' Function to construct a new class for the geom_hurricane, this function will create a wind radii for a
#' given storm obseravtion. It is to be used with maps construct with get_map
#'
#' @importFrom ggplot2 ggproto
#' @importFrom grid polygonGrob gpar
#'
#' @param required_aes required aesthetic arguments for the geom_hurricane supplied in character vector
#' @param default_aes default values for aesthetic arguments
#' @param draw_key the function to draw the legend with the associated geom
#' @param draw_group where the bulk of this geom is constructed
#'
#' @examples
#' \dontrun{
#'   geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude, r_ne = ne, r_se = se, r_nw =
#'   nw, r_sw = sw, fill = wind_speed, color = wind_speed)
#' }
#' @export
GeomHurricane <- ggplot2::ggproto("GeomHurricane", Geom,
                                  required_aes = c("x","y","r_ne","r_se","r_nw","r_sw"),
                                  default_aes = aes(fill = 1, colour = 1, alpha = 0.7, line_width = 1, scale_radii = 1),
                                  draw_key = draw_key_polygon,
                                  draw_group = function(data, panel_params, coord) {
                                    coords <- coord$transform(data, panel_params)

                                    first_row <- coords[1, , drop = FALSE]

                                    # Create polygon grob for panel
                                    grid::polygonGrob(
                                      x= coords$x,
                                      y = coords$y,
                                      default.units = "native",
                                      id = coords$group,
                                      gp = grid::gpar(fill = first_row$fill, alpha = first_row$alpha, lwd = first_row$line_width, col = first_row$fill)
                                    )
                                  }
)

#' Function to build layer for the geom_hurricane proto function
#'
#' @importFrom ggplot2 layer
#'
#' @param mapping Set of aesthetic mappings created by aes or aes_. If specified and inherit.aes = TRUE (the
#' default), it is combined with the default mapping at the top level of the plot. You must supply mapping if
#' there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options: If NULL, the default, the
#' data is inherited from the plot data as specified in the call to ggplot. A data.frame, or other object,
#' will override the plot data. All objects will be fortified to produce a data frame. See fortify for which
#' variables will be created. A function will be called with a single argument, the plot data. The return
#' value must be a data.frame., and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment
#' function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are
#' silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any
#' aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is
#' most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from
#' the default plot specification, e.g. borders.
#'
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "hurricane",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE,  ...) {
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Constructs a hurricane wind radii stat
#'
#' Performs distance calculations, transforming data into appropriate format for plotting
#' as wind radii polygons
#' @param required_aes aesthetics required for computing polygon coordinates
#' @param compute_group function to calculate polygon for each observation group
#'
#' @importFrom tidyr gather
#' @importFrom dplyr mutate right_join
#' @importFrom geosphere destPoint
#' @return data frame with x,y coordinates for plotting wind radii
#'
#' @export
StatHurricane <- ggproto("StatHurricane", Stat,
                         required_aes = c("x","y","r_ne","r_se","r_nw","r_sw"),
                         compute_group = function(data,scales){
                           data <- data %>% dplyr::mutate(r_ne=r_ne*1852.0,
                                                          r_nw=r_nw*1852.0,
                                                          r_se=r_se*1852.0,
                                                          r_sw=r_sw*1852.0)

                           # Create data frame to hold longitude / latitude for each angle position
                           angle <- seq(1,360,length.out = 360)
                           angle_assignment <- c(rep("r_ne",90), rep("r_se",90), rep("r_sw",90), rep("r_nw",90))
                           angle_df <- data.frame(angle = angle, quadrant = angle_assignment)
                           angle_df <- data %>%
                             tidyr::gather(quadrant, distance, r_ne:r_sw) %>%
                             dplyr::mutate(quadrant = as.factor(quadrant)) %>%
                             dplyr::right_join(angle_df, by="quadrant")

                           # Calculate longitude / latitude for each angle position
                           p <- angle_df %>% select(x,y) %>% as.vector()
                           b <- angle_df %>% select(angle) %>% as.vector()
                           d <- angle_df %>% select(distance) %>% as.vector()
                           dp <- geosphere::destPoint(p,b,d) %>% tbl_df()

                           # Assign logitude / latitude to x, y coordinates
                           angle_df$x <- dp$lon
                           angle_df$y <- dp$lat

                           angle_df
                         }
)

#' Function to build layer for the StatHurricane proto function
#'
#' @importFrom ggplot2 layer
#'
#' @param mapping Set of aesthetic mappings created by aes or aes_. If specified and inherits
#' .aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options: If NULL, the default, the
#' data is inherited from the plot data as specified in the call to ggplot. A data.frame, or other object,
#' will override the plot data. All objects will be fortified to produce a data frame. See fortify for which
#' variables will be created. A function will be called with a single argument, the plot data. The return
#' value must be a data.frame., and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment
#' function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are
#' silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any
#' aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is
#' most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from
#' the default plot specification, e.g. borders.
#'
#' @export
stat_hurricane <- function(mapping = NULL, data = NULL, geom = "hurricane",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatHurricane,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' Map an observation of a storm
#'
#' @param obs an observation retrieved from \code{stormObservation}
#'
#' @export
mapObservation <- function(obs){
  map_data <- ggmap::get_map(location = c(lon=first_row$longitude,lat=first_row$latitude), zoom = 6, maptype = "toner-background")
  base_map <- ggmap(map_data, extent = "device")
  g <- base_map + geom_hurricane(data = obs, aes(x = longitude, y = latitude,
                                                 r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                                                 fill = wind_speed, color = wind_speed)) +
    scale_color_manual(name = "Wind speed (kts)",
                       values = c("red", "orange", "yellow")) +
    scale_fill_manual(name = "Wind speed (kts)",
                      values = c("red", "orange", "yellow"))
  g
}
