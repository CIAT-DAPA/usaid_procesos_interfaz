library(rgdal)
library(sp)
library(raster)
library(ncdf4)
library(lubridate)
library("remotes")

install_github("agrdatasci/ag5Tools", build_vignettes = TRUE, build = FALSE)
library(ag5Tools)

setwd(dir_prepare_observed_data)
source("getDataFromAgera5.R")
source("createScenarios.R")

#daily_path = "D:/CIAT/plantingWindow/dailyData" 
#output_path = "D:/CIAT/plantingWindow/resampling/" 
#country_name = "COLOMBIA"
#date_now = "12/01/2022"

moveFiles <- function(path,year){
  list_of_files = list.files(path = paste(path,year,sep=""), pattern="^.*nc$", all.files=TRUE, full.names=TRUE)
  for (file in list_of_files) {
    file.copy(from = file,
              to   = path)
  }
  unlink(paste(path,year,sep=""),recursive=TRUE)
}

extract_data  <- function(variable, month, year, output_path) {
  path_to_export = paste(paste(output_path,replaceName(variable),sep=""),"/",sep="")
  dir.create(paste(output_path,replaceName(variable),sep=""), showWarnings = FALSE)
  if(variable == "T.Max" || variable == "T.Min"){
    statistic = "24_hour_minimum"
    if(variable == "T.Max"){
      statistic = "24_hour_maximum"
    }
    test = ag5Tools::ag5_download(variable = replaceName(variable),
                                  statistic = statistic,
                                  day = "all",
                                  month = month,
                                  year = year,
                                  path = path_to_export
    )
    
  }else{
    test = ag5Tools::ag5_download(variable = replaceName(variable),
                                  day = "all",
                                  month = month,
                                  year = year,
                                  path = path_to_export
    )
    
  }
  moveFiles(path_to_export,year)
  
}



downloadObservedData <- function(daily_path, date_now, output_path, country_name) {
  
  # START Extract Coordinates and station Ids
  
  variables_to_extract = c("S.Rad","T.Max","T.Min","Prec")
  regex = "^.*_coords.csv$" 
  files = list.files(path = daily_path, pattern=regex, all.files=TRUE, full.names=TRUE)
  data_stations = data.frame(Lon=character(),Lat=character(),station=character())
  for (station in files) {
    split_station = strsplit(station, "/")[[1]]
    file_name = split_station[length(split_station)]
    file_name = strsplit(file_name, "_")[[1]]
    station_id = file_name[1]
    coordinates = read.table(station, head=T, sep=",")
    
    coordinates = coordinates[c(2,1)]
    coordinates$station = station_id
    data_stations = rbind(data_stations,coordinates)
  }
  names(data_stations)[1:3] = c("Lon","Lat","Station")
  
  # END Extract Coordinates and station Ids data_stations
  
  # START get months to extract data
  
  split_date_now = strsplit(date_now, "/")[[1]]
  year_to_download = c(strtoi(split_date_now[3]))
  month_to_download = c()
  month_to_download_temp = c()

  if(strtoi(month(date_now)) <= 6){
    year_to_download = c(year_to_download,year_to_download[1]-1)
    for(index in 6:1){
      if((strtoi(month(date_now)) - index) <= 0 ){
        month_to_download_temp = c(month_to_download_temp,(strtoi(month(date_now)) - index)+12)
      }else{
        month_to_download = c(month_to_download,(strtoi(month(date_now)) - index))
      }
    }
  }else{
    actu_month = strtoi(month(date_now))
    month_to_download = c(actu_month-6,actu_month-5,actu_month-4,actu_month-3,actu_month-2,actu_month-1)
  }
  
  # END get months to download data
  
  # START Download data

  count = 1
  for (year in year_to_download) {
    for (variable in variables_to_extract) {
      if(count != 1){
        tryCatch(
          {
            extract_data(variable,month_to_download_temp,year,output_path)
          },
          error=function(cond) {
            extract_data(variable,month_to_download_temp,year,output_path)
          })
        
      }else{
        if(!is.null(month_to_download)){
          tryCatch(
          {
            extract_data(variable,month_to_download,year,output_path)
          },
          error=function(cond) {
            extract_data(variable,month_to_download,year,output_path)
          })
          
        }
        
      }
      
    }
    count = count + 1
  }

  # END Download data
  
  # START Format date to get dateStart and dateEnd
  
  dateStart = ''
  dateEnd = ''
  if(!is.null(month_to_download_temp)){

    dateStart = paste0(year_to_download[2],"-",month_to_download_temp[1],"-","01")
    if(!is.null(month_to_download)){
      if(length(year_to_download) == 2 && month_to_download[length(month_to_download)] >= 7){
        dateEnd = paste0(year_to_download[2],"-",month_to_download[length(month_to_download)],"-","01") 
      }else{
        dateEnd = paste0(year_to_download[1],"-",month_to_download[length(month_to_download)],"-","01") 
      }
      
    }else{
      if(length(year_to_download) == 2 && month_to_download_temp[length(month_to_download_temp)] >= 7){
      dateEnd = paste0(year_to_download[2],"-",month_to_download_temp[length(month_to_download_temp)],"-","01") 
      }else{
        dateEnd = paste0(year_to_download[1],"-",month_to_download_temp[length(month_to_download_temp)],"-","01") 
      }
    }
    
    
    dateStart = format(strptime(as.character(dateStart), "%Y-%m-%d"),"%Y-%m-%d" )
    dateEnd = format(strptime(as.character(dateEnd), "%Y-%m-%d"),"%Y-%m-%d" )
    dateEnd = as.Date(dateEnd) + months(1) - days(1)
    
  }else{
    dateStart = paste(paste(paste(year_to_download[1],"-",sep=""),paste(month_to_download[1],"-",sep=""),sep=""),"01",sep="")
    dateEnd = paste(paste(paste(year_to_download[1],"-",sep=""),paste(month_to_download[length(month_to_download)],"-",sep=""),sep=""),"01",sep="")
    dateStart = format(strptime(as.character(dateStart), "%Y-%m-%d"),"%Y-%m-%d" )
    dateEnd = format(strptime(as.character(dateEnd), "%Y-%m-%d"),"%Y-%m-%d" )
    dateEnd = as.Date(dateEnd) + months(1) - days(1)
  }
  
  # END Format date to get dateStart and dateEnd
  
  # START Extract data from nc files
  
  for (variable in variables_to_extract) {
    database_path = paste0(output_path,replaceName(variable))
    tryCatch(
      {
        extractDataFromAgera5(variable, dateStart, dateEnd, output_path, data_stations, database_path, country_name)
      },
      error=function(cond) {
        extractDataFromAgera5(variable, dateStart, dateEnd, output_path, data_stations, database_path, country_name)
      }
    )
    
    
  }
  
  # END Extract data from nc files
  
  
  # START Create files with format to create scenarios
  
  createScenarios(data_stations, output_path)
  
  # END Create files with format to create scenarios
  
  for (var_folder in variables_to_extract) {
    if(var_folder != "T.Min"){
      unlink(paste(output_path,replaceName(var_folder),sep=""),recursive=TRUE)
    }
    if (file.exists(paste(output_path,paste(var_folder,".data.observed.csv",sep=""),sep=""))) {
      file.remove(paste(output_path,paste(var_folder,".data.observed.csv",sep=""),sep=""))
    }
  }
  
}


#downloadObservedData(daily_path,date_now,output_path)