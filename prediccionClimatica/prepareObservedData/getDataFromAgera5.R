
# START Functions to replace depends of the var

replaceName <- function(prefix) {
  
  folder_name = switch(  
    prefix,  
    "Prec"="precipitation_flux",  
    "S.Rad"="solar_radiation_flux",  
    "T.Max"="2m_temperature",  
    "T.Min"="2m_temperature")  
  return(folder_name)
}
replaceVar <- function(prefix) {
  
  replace_var = switch(  
    prefix,  
    "Prec"="Precipitation-Flux",  
    "S.Rad"="Solar-Radiation-Flux",  
    "T.Max"="Temperature-Air-2m",  
    "T.Min"="Temperature-Air-2m")  
  return(replace_var)
}

replaceRegex <- function(prefix) {
  
  regex = switch(  
    prefix,  
    "Prec"="Precipitation-Flux",  
    "S.Rad"="Solar-Radiation-Flux",  
    "T.Max"="Temperature-Air-2m-Max-24h",  
    "T.Min"="Temperature-Air-2m-Min-24h")  
  return(regex)
}

shapeFilePath <- function(prefix, output_path) {
  shapefiles_path = paste0(dir_prepare_observed_data, "observedDataShapefiles/")
  country_name = switch(  
    prefix,  
    "COLOMBIA"=paste0(shapefiles_path,"COL/", "COL_adm0.shp"),  
    "ANGOLA"=paste0(shapefiles_path,"AGO/", "AGO_adm0.shp"),  
    "ETHIOPIA"=paste0(shapefiles_path,"ETH/", "ETH_adm0.shp"),  
    "GUATEMALA"=paste0(shapefiles_path,"GTM/", "GTM_adm0.shp"))  
  return(country_name)
}

# END Functions to replace depends of the var

# START SET DINAMYCALLY VARIABLES DEPENDS OF THE VAR

#variable = "S.Rad"
#database_path = paste(output_path,replaceName(variable),sep="")
#


extractDataFromAgera5 <- function(variable, dateStart, dateEnd, output_path, data_stations, database_path, country_name){
  var_folder = paste0(database_path,replaceName(variable),"/")
  
  
  #setwd(var_folder)
  
  country = readOGR(shapeFilePath(country_name))
  
  # END SET DINAMYCALLY VARIABLES DEPENDS OF THE VAR
  
  # START SET DATA
  
  data.coordinates = list()
  
  coordinates = data_stations #Read coordinates
  points = coordinates[,-3]
  names(points)[1] = c('Lon')
  
  # END SET DATA
  
  # START Initializes the progress bar
  
  # END Initializes the progress bar
  
  
  
  ##--- Read S.Radiation file names for each year ---##
  rastlist = list.files(paste0(database_path,"/"), pattern=replaceRegex(variable), full.names = TRUE, recursive = FALSE)
  
  allrasters = lapply(rastlist, raster)
  
  ##-- Change raster names for each day --##
  
  for(j in 1:length(allrasters)){
    
    names(allrasters[[j]]) = paste0(variable,"_",getZ(allrasters[[j]]))
    
  }
  
  allrasters = stack(allrasters)
  
  ##-- Crop and mask solar radiation maps to Angola shapefile --##
  var_to_extract = crop(allrasters, country)
  var_to_extract = mask(var_to_extract, country)
  
  ##-- Extract the Tmax data for points --##
  var_to_extract.Points = raster::extract(var_to_extract, points)
  
  ##-- Transpose data from rows to columns --##
  data_final = t(var_to_extract.Points)
  
  data_final = as.data.frame(data_final)
  
  array = c()
  
  for (coor in 1:nrow(coordinates)) {
    array = c(array,gsub(" ", "", coordinates [coor,3]))
  }
  
  ## Give column names (i.e. station names) ##
  names(data_final)[1:nrow(coordinates)] = array
  
  
  
  data.coordinates = data_final   
  
  # Sets the progress bar to the current state
  
  
  
  
  
  if(variable == "S.Rad"){
    data.coordinates = (data.coordinates / 1000000)
  }else if(variable == "T.Max" || variable == "T.Min"){
    data.coordinates = data.coordinates - 273.15
  }
  
  
  data.coordinates = as.data.frame (data.coordinates)
  
  Dates = data.frame (seq(as.Date(dateStart), as.Date(dateEnd), by="days"))
  names(Dates)[1] = c("Date")
  Dates$Date = format(strptime(as.character(Dates$Date), "%Y-%m-%d"),"%m/%d/%Y" )
  
  
  
  Rwanda.Final = cbind(Dates[1:nrow(data.coordinates),], data.coordinates)
  
  
  rownames(Rwanda.Final) <- Rwanda.Final[,1]
  Rwanda.Final[,1] <- NULL
  
  write.table(Rwanda.Final,paste0(output_path,variable,".data.observed.csv"), row.names = TRUE, sep=",", col.names=NA)
  
  print(paste0(variable," was completed"))
}
