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
  
 if(all(map_lgl(names_tif, file.exists))){message("Geoserver tif files already downloaded!")
 } else {
   
      walk2(url, names_tif, ~download.file(url = .x, destfile = .y, method = "curl"))
   
   }
  
 
  
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

# urea : Urea amount (kg/ha
# nps : NPS amount (kg/ha)
# apps_dap : Number application - Days after planting
# urea_split = Rate application
# nps_split : Rate application


convert_FertApp_dssat <- function(urea, nps, apps_dap = c(1, 40), urea_split = c(1/3, 2/3), nps_split = c(1, 0)){
  
  
  base_tb <- bind_rows(tibble(dap = apps_dap, fert = "nps", value = nps * nps_split) %>% 
                         dplyr::filter(value > 0),
                       tibble(dap = apps_dap, fert= "urea", value = urea * urea_split) %>% 
                         dplyr::filter(value > 0)) 
  
  
  
  fert_to_N <-function(fert, amount){
    
    if(fert == "nps"){
      N = amount*0.19   # https://www.tandfonline.com/doi/full/10.1080/23311932.2018.1439663
    } else if(fert == "urea"){
      N = amount*0.46
    } else {
      message("No detected Fertilizer")
      N = -99
    }
    
    return(N)
  }
  
  fert_to_P <-function(fert, amount){
    
    if(fert == "nps"){
      P = amount*0.38
    } else if(fert == "urea"){
      P = -99
    } else {
      message("No detected Fertilizer")
      P = -99
    }
    
    return(P)
  }
  
  # AP001    Broadcast, not incorporated            
  # AP002    Broadcast, incorporated
  
  # FE005    Urea
  # FE006    Diammonium phosphate (DAP)     
  # FE028    NPK - urea  
  
  base_tb <- base_tb %>% 
    mutate(N = map2_dbl(fert, value, fert_to_N),
           P = map2_dbl(fert, value, fert_to_P),
           FMCD = case_when(fert == "nps" ~ "FE006",
                            fert == "urea" ~"FE005",
                            TRUE  ~ NA_character_),
           FACD = case_when(dap < 5 ~ "AP002",
                            dap > 15 ~ "AP001",
                            TRUE ~ NA_character_),
           FDEP = case_when(dap < 5 ~ 5,
                            dap > 15 ~ 1,
                            TRUE ~ NA_real_))
  
  
  # De acuerdo a las recomendaciones: 2 aplicaciones,
  # 1 app: (nps) + 1(urea)/3  -- Incorporated
  # 2 app: 2(urea)/3          --  No incorporated
  
  #*FERTILIZERS (INORGANIC)
  #@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME
  # 1     1 FE006 AP002     5    10    20   -99   -99   -99   -99 fertApp
  # 1     1 FE005 AP002     5    30   -99   -99   -99   -99   -99 fertApp
  # 1    40 FE005 AP001     1    10    30    10   -99   -99   -99 fertApp
  
  FDATE <- base_tb$dap
  FMCD <-  base_tb$FMCD
  FACD <-  base_tb$FACD
  FDEP <-  base_tb$FDEP 
  FAMN <-  round(base_tb$N)
  FAMP <-  round(base_tb$P)
  FAMK <-  -99
  FAMC <-  -99 
  FAMO <-  -99
  FOCD <-  -99
  FERNAME <- "AgroClimR"
  #  
  #  fertilizer <- data.frame(F = 1, FDATE, FMCD, FACD, FDEP, FAMN, FAMP, FAMK,
  #                           FAMC, FAMO, FOCD, FERNAME)
  
  
  
  
  fertilizer <- tibble(F = 1, FDATE, FMCD, FACD, FDEP, FAMN, FAMP, FAMK,
                       FAMC, FAMO, FOCD, FERNAME)
  
  
  return(fertilizer)
  
}

#c(FALSE , "auto", "fertapp")


  
  
  



