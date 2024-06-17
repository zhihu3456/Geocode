#' GCJ02_WGS84
#'
#' @param lon original longitude based on the GCJ02 coordinate system.
#' @param lat original latitude based on the GCJ02 coordinate system.
#'
#' @return longtitude and latitude based on the WGS84 coordinate system.
#'
#' @export
#'
#' @examples GCJ02_WGS84(100.00,40.00)

GCJ02_WGS84 <- function(lon, lat) {
  pi <- 3.1415926535897932384626
  a <- 6378245
  ee <- 0.00669342162296594323

  transform_lon <- function(lon, lat) {
    dlon = 300.0 + lon + 2.0 * lat + 0.1 * lon * lon +
      0.1 * lon * lat + 0.1 * sqrt(abs(lon)) +
      (20.0 * sin(6.0 * lon * pi) + 20.0 * sin(2.0 * lon * pi)) *
      2.0 / 3.0 + (20.0 * sin(lon * pi) + 40.0 * sin(lon / 3.0 * pi)) *
      2.0 / 3.0 + (150.0 * sin(lon / 12.0 * pi) + 300.0 * sin(lon / 30.0 * pi)) *
      2.0 / 3.0
    return(dlon)
  }
  transform_lat <- function(lon, lat) {
    dlat = -100.0 + 2.0 * lon + 3.0 * lat + 0.2 * lat * lat +
      0.1 * lon * lat  + 0.2 * sqrt(abs(lon)) +
      (20.0 * sin(6.0 * lon * pi) + 20.0 * sin(2.0 * lon * pi)) *
      2.0 / 3.0 + (20.0 * sin(lat * pi) + 40.0 * sin(lat / 3.0 * pi)) *
      2.0 / 3.0 + (160.0 * sin(lat / 12.0 * pi) + 320 * sin(lat * pi / 30.0)) *
      2.0 / 3.0
    return(dlat)
  }

  dlon = transform_lon(lon - 105, lat - 35)
  dlat = transform_lat(lon - 105, lat - 35)
  radlat = lat / 180.0 * pi
  magic = 1 - ee * (sin(radlat))^2
  sqrtMagic = sqrt(magic)
  dlon = (dlon * 180.0) / (a / sqrtMagic * cos(radlat) * pi)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtMagic) * pi)
  mglon = lon + dlon
  mglat = lat + dlat
  lon <- (lon * 2 - mglon)
  lat <- (lat * 2 - mglat)
  return(data.frame(lon = lon, lat = lat))
}
