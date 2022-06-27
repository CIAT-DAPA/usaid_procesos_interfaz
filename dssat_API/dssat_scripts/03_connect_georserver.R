### Function to doawnload data from aclimate geoserver "https://geo.aclimate.org/geoserver/" 
# Author: Rodriguez-Espinoza J., Mesa J., 
# https://github.com/jrodriguez88/
# 2022




#library(tidyverse)
#library(raster)
 

# Function to doawnload data from aclimate geoserver "https://geo.aclimate.org/geoserver/"

## Arguments
#crop <- "wheat"
#data_type <- "fertilizer"
##data_type <- "climate"
#format <- "geotiff"
##format <- "shp" # just works for some areas
#country <- "et" # et =  ethiopia, co = colombia 
#outpath <- "inputs/"

get_geoserver_data <- function(crop, data_type, country, format, outpath){
  
  url_base <- "https://geo.aclimate.org/geoserver/"
  
  if(data_type == "climate"){
    
    data_type_string <- paste0("a", data_type, "_", country)
    
    layers <- c("seasonal_country_et_probabilistic_above", 
                "seasonal_country_et_probabilistic_normal", 
                "seasonal_country_et_probabilistic_below")
    
    names_tif <- paste0(outpath, "/", layers, ".tif")
    
    bbox_raster <- "bbox=33.0%2C3.0%2C48.0%2C17.0&width=768&height=716"
    
  } else if (data_type == "fertilizer"){
    
    data_type_string <- paste0(data_type, "_", country)
    
    layers <- c(paste0("et_", crop, "_nps_probabilistic_normal"),
                paste0("et_", crop, "_urea_probabilistic_normal"))
    
    bbox_raster <- "bbox=33.051816455%2C5.352336938999999%2C43.448502149%2C14.749081456&width=768&height=694"
    
    names_tif <- paste0(outpath, "/", layers, ".tif")
    
  } else {message("No recognizing data type")}
  
  format_string <- if(format ==  "geotiff"){format
  } else if (format == "shp"){
      paste0("SHAPE-ZIP")
  }
  
  
  # bbox <- change by country
  
  
  url <- paste0(url_base, data_type_string,
                "/wms?service=WMS&version=1.1.0&request=GetMap&layers=",
                data_type_string, "%3A", layers, 
                "&", bbox_raster, "&srs=EPSG%3A4326&styles=&format=image%2F",
                format_string)
  
  
  walk2(url, names_tif, ~download.file(url = .x, destfile = .y, method = "curl"))
  
  return(names_tif)


}

#Usage
#downloaded_tif <- get_geoserver_data(crop, "fertilizer", country, format, outpath)


#lat <- 9.37
#lon <- 42

extract_raster_geos <- function(raster_geos, lat, lon) {
  
  data_raster  <- raster_geos %>% map(raster) %>%
    raster::stack() %>% 
    raster::extract(., SpatialPoints(cbind(lon, lat),  proj4string = CRS("+init=epsg:4326"))) %>% 
    t() %>% #as.tibble()
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "file") %>%
    tibble()
  
  return(data_raster)
}


#https://geo.aclimate.org/geoserver/fertilizer_et/wms?service=WMS&version=1.1.0&request=GetMap&layers=fertilizer_et%3Aet_wheat_urea_probabilistic_normal&bbox=33.051816455%2C5.352336938999999%2C43.448502149%2C14.749081456&width=768&height=694&srs=EPSG%3A4326&styles=&format=image%2Fgeotiff
#https://geo.aclimate.org/geoserver/aclimate_et/wms?service=WMS&version=1.1.0&request=GetMap&layers=aclimate_et%3Aseasonal_country_et_probabilistic_above&bbox=33.0%2C3.0%2C48.0%2C17.0&width=768&height=716&srs=EPSG%3A4326&styles=&format=image%2Fgeotiff

#test <- extract_raster_geos(downloaded_tif, 35, 8.38)

#"inputs/seasonal_country_et_probabilistic_above.tif" %>% raster() %>%  raster::extract(., cbind(41.88538, 8.6352))


#convert_FertApp_dssat <- function(N, P)

  
#list.files("inputs/", full.names = T, pattern = "tif") %>% map(raster)


#
#nps <-raster("inputs/fertilizer_et-et_wheat_optimal_nutrients_p_normal.tif")
#aclimate <-raster("inputs/climate_geo/aclimate_et-seasonal_country_et_probabilistic_above.tif")
#
#
#
#range(nps[], na.rm = T)
#range(aclimate[]*100/255, na.rm = T)
#
#
#shp <-   st_read("inputs/climate_geo/et_wheat_fertilizer_recommendation_normal.shp")



