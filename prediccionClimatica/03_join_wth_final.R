# Download and join data from NASA and CHIRP. 
# Authors: Rodriguez-Espinoza J. Esquivel Arias.
# 2021


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 0. Function to download Chirp data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This function 
# INPUT
# ini.date start date to download.
# end.date: end date to download.
# year_to: resampling year. 
# path_Chirp: path to save raster files. 
# no_cores = # cores to use in parallel. 

# OUTPUT: save chirp raster layers.


# OUTPUT: save chirp raster layers. 
download_data_chirp <- function(ini.date, end.date, year_to, path_Chirp, no_cores){
  
  fechas <- seq(as.Date(ini.date), as.Date(end.date), "days") %>% str_replace_all("-", ".")
  #urls <- paste("https://data.chc.ucsb.edu/products/CHIRP/daily/",year_to,"/chirp.",fechas,".tif",sep="") Esta ruta con https pone problemas en el servidor de linux
  urls <- paste("http://data.chc.ucsb.edu/products/CHIRP/daily/",year_to,"/chirp.",fechas,".tif",sep="")
  file <- basename(urls)
  path_Chirp_all <- paste0(path_Chirp,"/",file)

  mclapply(1:length(urls), function(i) {
     download.file(urls[i], path_Chirp_all[i], mode = "w")

   }, mc.cores = no_cores, mc.preschedule = F)
  #download.file(url=urls[1], path_Chirp_all[1])
  # if(.Platform$OS.type == "unix") {cl <- makeCluster(no_cores, type = "FORK")}
  # cl <- makeCluster(no_cores)
  # clusterMap(cl, download.file, url = urls, destfile = path_Chirp_all, mode = "wb", 
  #            .scheduling = 'dynamic')
  
  # stopCluster(cl)
  return("CHIRPS data downloaded!") }


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 1. Function to extract NASA POWER daily data 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This function 
# INPUT
# * data : station data. 
# * special_data: this contains (lat: latitude of the station / site of interest, 
# lon: longitud of the station / site of interest, year_to: actual year, month_to: actual month).

# OUTPUT: NASA series. 
# It could be possible NASA API in some case sometimes don't work. 


## up 2021 jre: Se modifica esta funcion para la version 2.0 de la API 
## 
download_data_nasa <- function(ini.date, end.date, station_data, special_data){
  # data <- Cerete
  # special_data <- tibble(lat, lon, year_to, month_to)
  options(timeout = 120)
  lat <- special_data$lat
  lon <- special_data$lon
  year_to <- year(ini.date)
  month_to <- month(ini.date)
  
  
  #json_file <- paste0("https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?&request=execute&identifier=SinglePoint&parameters=ALLSKY_SFC_SW_DWN,T2M_MAX,T2M_MIN&startDate=19830101&endDate=",format(Sys.Date(),"%Y%m%d"),"&userCommunity=AG&tempAverage=DAILY&outputList=ASCII&lat=",lat,"&lon=",lon)
  json_file <- paste0("https://power.larc.nasa.gov/api/temporal/daily/point?start=20150101&end=", format(end.date,"%Y%m%d"),"&latitude=",lat,"&longitude=",lon, "&community=ag&parameters=ALLSKY_SFC_SW_DWN,T2M_MAX,T2M_MIN&header=true&time-standard=lst")
  print(json_file)
  
  json_data <- jsonlite::fromJSON(json_file)
  
  
  data_nasa <- json_data$properties$parameter %>% 
    map(bind_cols) %>%
    map2(.y = names(.),
         ~pivot_longer(.x, cols = everything(), values_to = .y, names_to = "date") %>%
           mutate(date = lubridate::ymd(date))) %>%
    reduce(left_join, by = "date") %>% 
    setNames(c("date", "sol_rad", "t_max", "t_min")) %>%
    mutate(year_n = year(date), month = month(date), day = day(date)) %>%
    dplyr::select(date, year_n, month, day, t_min, t_max, sol_rad) %>% 
    na_if(-999)
  
  

  # Join observed and NASA data. 
  all_data <- right_join( station_data %>% 
                            filter(year %in% unique(data_nasa$year_n) ) %>% dplyr::select(-prec),
                          data_nasa %>% 
                            filter(year_n %in% unique(data$year)) %>% 
                            purrr::set_names(c('dates', 'year', 'month', 'day', 't_min_N', 't_max_N', 'sol_rad_N')))
  
  
  # Bias between observed data and NASA data. 
  mean_less <- all_data %>% 
    summarise(mean_max = mean(t_max-t_max_N, na.rm = TRUE), 
              mean_min = mean(t_min - t_min_N, na.rm = TRUE),
              mean_sol_rad = mean(sol_rad - sol_rad_N, na.rm = TRUE))
  
  # data full with mean NASA. 
  nasa_data_dw <- data_nasa %>% 
    filter(year_n == year_to, month == month_to) %>% 
    mutate(t_max = t_max + mean_less$mean_max, 
           t_min = t_min + mean_less$mean_min,
           sol_rad = sol_rad + mean_less$mean_sol_rad) %>% 
    mutate(t_max = ifelse(is.na(t_max), mean(t_max, na.rm = TRUE), t_max),
           t_min = ifelse(is.na(t_min), mean(t_min, na.rm = TRUE), t_min),
           sol_rad = ifelse(is.na(sol_rad), mean(sol_rad, na.rm = TRUE), sol_rad))
  
  return(nasa_data_dw)}

## Convert to safety function
download_nasa_safety <- possibly(download_data_nasa, NULL) 

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 2. Function to extract Chirp data and Join with NASA POWER DATA. 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# ***** INPUT 
# * data:  path for save files.
# * special_data: data from Chirp and nasa for each station.
# *****  OUTPUT
#  This return resampling scenaries join with satellite data. 
#
# ***** Note: This function save files.



### up 2021, jre: se dividio esta funcion en 2, una primera para obtener los datos de chirp y otro para realizar el join de datos finales
extract_chirp_data <- function(path_Chirp, special_data){
  
  list.files(path_Chirp, full.names = TRUE, pattern = '.tif') %>% 
    stack() %>%
    raster::extract(., data.frame(x= special_data$lon,y= special_data$lat)) %>% # parte sensible a modificacion
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "file") %>%
    tibble() %>% rename(prec = V1) %>%
    mutate(date = lubridate::ymd(str_sub(file, -10,-1)),
           day  = lubridate::day(date),
           month = month(date),
           year = year(date)) %>%
    dplyr::select(day, month, year, prec)
  
}

## Se convierte function en modo seguro
extract_chirp_safety <- possibly(extract_chirp_data, NULL)



## Funcion para crear mes previo promedio a partir de los datos historicos
## Mes previo  = month(Sys.month) - 1

###day month  year t_max t_min  prec sol_rad
clim_extract <- function(wth_data, ini.date){
  
  wth_data %>% 
  group_by(day, month) %>%
  summarise(t_max  = mean(t_max),
            t_min = mean(t_min),
            prec = median(prec),
            sol_rad = mean(sol_rad)) %>% 
  ungroup() %>% mutate(year = year(ini.date)) %>%
  filter(month == month(ini.date)) %>%
  dplyr::select(day, month,  year, t_max, t_min,  prec, sol_rad)
  
  
}


# Funcion para unir bases de datos y extraer escenarios finales
#Escenaries <- all_wth_data$Escenaries[[1]]#$data[[1]]$data[[1]] 
#chirp_data <- all_wth_data$chirp_data[[1]]
#nasa_data <- all_wth_data$nasa_data[[1]]
#climatology_mean <- all_wth_data$climatology_mean[[1]]

join_wth_escenarios <- function(Escenaries, chirp_data, nasa_data, climatology_mean){
  
  if(is.null(chirp_data)){
    
    prev_month <- climatology_mean
    
  } else if(is.null(nasa_data)){
    
    prev_month <- climatology_mean %>%# dplyr::select(climatology_mean, -prec) %>% 
      left_join(chirp_data, by = c("day", "month", "year")) %>%
      mutate(prec = ifelse(is.na(prec.y), prec.x, prec.y)) %>%
      dplyr::select(day, month,  year, t_max, t_min,  prec, sol_rad)
    
  } else {
    prev_month <- nasa_data %>%# dplyr::select(climatology_mean, -prec) %>% 
      left_join(chirp_data, by = c("day", "month", "year")) %>%
      #     mutate(prec = if_else(is.na(prec.y), prec.x, prec.y)) %>%
      dplyr::select(day, month,  year, t_max, t_min,  prec, sol_rad)
    
  }
  
  
  Escenaries %>% 
    mutate(data = map(data, 
                      ~.x %>% 
                        mutate(data = map(data, 
                                          ~bind_rows(prev_month, .x)))))
  
}


####################################################################

## up 2021, jre: Se estandariza el uso de formato fecha, se requiere paquete lubridate

# -----------
ini.date <- (Sys.Date() - months(1)) %>% lubridate::floor_date(unit = "month")
# -----------
end.date <- (Sys.Date() - months(1)) %>% lubridate::ceiling_date(unit = "month") - days(1)
# -----------
year_to <- year(ini.date)
# -----------
month_to <- month(ini.date)
# -----------
path_Chirp <- path_output
# -----------



# =-=-=-= --------------------------------------------
# =-=-=-=-= Change this parameter for run in parallel. 
#no_cores <- as.numeric(Sys.getenv("N_CORES"))

# =-=-= Here we download Chirps data (This download is only done once). 
download_data_chirp(ini.date, end.date, year_to, path_Chirp, no_cores)



wth_escenaries <-  Resam %>% 
  mutate(chirp_data = map(coord, ~extract_chirp_safety(path_Chirp, .x)),
         nasa_data  = map2(stations, coord, ~download_nasa_safety(ini.date, end.date, .x, .y)),
         climatology_mean = map(stations, ~clim_extract(.x, ini.date)),
         wth_final = pmap(.l = list(Escenaries, chirp_data, nasa_data, climatology_mean), 
                          .f = join_wth_escenarios))




#purrr::map2(.x = wth_escenaries$id, .y = wth_escenaries$wth_final, .f = function_to_save, path_out = path_output)

##Saving wth_escenaries in case the process get interrupted 
saveRDS(wth_escenaries, file = paste0("/forecast/workdir/", currentCountry, "_wth_scenaries", ".Rds"))

# Recorrer la lista de 100 en 100
write_scenaries <- function(current_index=NULL){
  ##In case i needs to be diferent than one
  current_index <- if(is.null(current_index)) 1 else current_index
  ##In case process run out of memory and wth_escenaries needs to be loaded
  wth_escenaries <- if(exists("wth_escenaries")) wth_escenaries else readRDS(file =  paste0("/forecast/workdir/", currentCountry, "_wth_scenaries", ".Rds"))

  length_wth <- nrow(wth_escenaries)
  for (current_index in seq(current_index, length_wth, 100)) {
    
    Map(function_to_save_safe, wth_escenaries$id[current_index:min(current_index+99, length_wth)],  wth_escenaries$wth_final[current_index:min(current_index+99, length_wth)], path_output)
    print(paste0(current_index, "-", min(current_index+99, length_wth), " scenaries written"))

  }

}

write_scenaries()

# This remove chirp files.
file.remove(list.files(path_output,pattern = ".tif",full.names=T))