
replaceName <- function(prefix) {
  
  folder_name = switch(  
    prefix,  
    "Prec"="precipitation_flux",  
    "S.Rad"="solar_radiation_flux",  
    "T.Max"="2m_temperature",  
    "T.Min"="2m_temperature")  
}
replaceVar <- function(prefix) {
  
  folder_name = switch(  
    prefix,  
    "Prec"="Precipitation-Flux",  
    "S.Rad"="Solar-Radiation-Flux",  
    "T.Max"="Temperature-Air-2m",  
    "T.Min"="Temperature-Air-2m")  
}

extractDataAgera5 <- function(variable, dateStart, dateEnd,output_path,data_stations,database_path){
  S.Rad.data.coordinates = list()
    
  coordinates = data_stations #read.table(paste(output_path,csv_names,sep=""), head=T, sep=",") #Read coordinates
  points = coordinates[,-3]
  names(points)[1] = c('Lon')
  points$start_date = as.character(dateStart)
  points$end_date = as.character(dateEnd)
  if(variable == "T.Max" || variable == "T.Min"){
    statistic = "Min-24h"
    if(variable == "T.Max"){
      statistic = "Max-24h"
    }
    test = ag5Tools::ag5_extract(coords = points, 
                                 lon = "Lon",
                                 lat = "Lat",
                                 start_date= "start_date",
                                 end_date = "end_date",
                                 variable = replaceVar(variable),
                                 statistic = statistic,
                                 path = database_path)
    
    test_final = t(do.call("rbind",test)) # Transpose data from rows to columns
    
    test_final = test_final - 273.15
  }else{
    test = ag5Tools::ag5_extract(coords = points, 
                                 lon = "Lon",
                                 lat = "Lat",
                                 start_date= "start_date",
                                 end_date = "end_date",
                                 variable = replaceVar(variable),
                                 path = database_path)
    
    test_final = t(do.call("rbind",test)) # Transpose data from rows to columns
  }
  S.Rad.data.coordinates[[1]] = test_final
    
    
  
  
  S.Rad.data.coordinates_final = do.call("rbind",S.Rad.data.coordinates)
  
  ## convert S. Rad data from J m-2 day-1 to  MJ m-2 day-1 ##
  if(variable == "S.Rad"){
    S.Rad.data.coordinates_final = (S.Rad.data.coordinates_final / 1000000)
  }
  
  
  ## Convert data as "data frame" format ##
  S.Rad.data.coordinates_final = as.data.frame(S.Rad.data.coordinates_final)
  
  ## Read coordinates with station names ##
  #coordinates = read.table(paste(output_path,csv_names,sep=""), head=T, sep=",")
  
  array = c()
  
  for (coor in 1:nrow(coordinates)) {
    array = c(array,gsub(" ", "", coordinates [coor,3]))
  }
  
  ## Give column names (i.e. station names) ##
  names(S.Rad.data.coordinates_final)[1:nrow(coordinates)] = array
  
  ## Export Solar Radiation data as ".csv" ##
  write.table(S.Rad.data.coordinates_final,paste(output_path,paste(variable,".data.observed.csv",sep=""),sep=""), row.names = TRUE, sep=",", col.names=NA)
}

