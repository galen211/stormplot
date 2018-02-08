#' Clean raw storm data
#'
#' Imports \code{txt} file and formats
#' @importFrom readr read_fwf
#' @return data frame with tidy data
parseStormData <- function(){
  ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                         4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
  ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                           "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "pressure_2",
                           paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                           "storm_type", "distance_to_land", "final")

  ext_tracks <- readr::read_fwf("inst/extdata/ebtrk_atlc_1988_2015.txt",
                                fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                                na = "-99")
  cleanStormData(ext_tracks)
}


#' Reformat storm data
#'
#' Changes format for `storm_id`, transforms longitude for values in Western Hemisphere,
#' combines columns describing data and time to create a single variable for each
#' observation, and converts data into "long" format with separate rows for each of the
#' three wind speeds for wind radii (34 kts, 50 kts, and 64 kts).
#'
#' @param data input data from fwf imported using \code{parseStormData}
#' @return data frame with tidy data
#' @importFrom dplyr mutate select
#' @importFrom tidyr gather separate spread
#' @importFrom stringr str_to_title
#'
#' @export
cleanStormData <- function(data){
  data %>%
    dplyr::mutate(storm_id = paste0(stringr::str_to_title(storm_name),"-",year),
                  date = ISOdatetime(year,month,day,hour,"00","00", tz = "GMT"),
                  longitude = longitude*-1) %>%
    tidyr::gather(txt_radius, distance, radius_34_ne:radius_64_nw, na.rm = TRUE) %>%
    tidyr::separate(txt_radius, c("txt","wind_speed","orientation"), sep = "_") %>%
    tidyr::spread(orientation, distance) %>%
    dplyr::mutate(wind_speed = as.factor(wind_speed)) %>%
    dplyr::select(storm_id, date, latitude, longitude, wind_speed, ne, nw, se, sw)
}
