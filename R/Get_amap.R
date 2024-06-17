#' Get_amap
#'
#' @param address detailed address in Chinese
#' @param city which city does it belong to
#' @param key amap API key: '156a263e434219631f61d4e2e402f0cb' or 'e885b568bdb9500b97cf1f46d572ff1e'
#'
#' @return formatted_address, country, province, city, district, longitude, latitude based on GCJ02/WGS84 coordinate system.
#' @export
#'
#' @examples Get_amap('北京大学')

Get_amap <- function(address,city=NULL,key='156a263e434219631f61d4e2e402f0cb'){

  library(jsonlite)
  library(tidyverse)

  # address='北京大学'
  # city='北京市'
  # key = '156a263e434219631f61d4e2e402f0cb'
  # key = 'e885b568bdb9500b97cf1f46d572ff1e'

  if(!is.null(city)){
    fromJSON(URLdecode(paste0("https://restapi.amap.com/v3/geocode/geo?address=",address,
                              "&output=JSON&key=",key,"&city=",city))) -> tmp
  }else{
    fromJSON(URLdecode(paste0("https://restapi.amap.com/v3/geocode/geo?address=",address,
                              "&output=JSON&key=",key))) -> tmp
  }

  tmp$geocodes %>%
    dplyr::as_tibble() %>%
    dplyr::select(formatted_address, country, province, city, district, citycode, level, location) %>%
    bind_cols(address=address,.) %>%
    separate(location,into=c('lon_GCJ02','lat_GCJ02'),sep=',') %>%
    mutate(lon_GCJ02=as.numeric(lon_GCJ02),
           lat_GCJ02=as.numeric(lat_GCJ02)) -> result

  # transform to wgs84
  GCJ02_WGS84(result$lon_GCJ02,result$lat_GCJ02) -> wgs84
  names(wgs84) <- c('lon_WGS84','lat_WGS84')

  result <- bind_cols(result,wgs84)
  return(result)
}
