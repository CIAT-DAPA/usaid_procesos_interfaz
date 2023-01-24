replaceNameCsv <- function(prefix) {
  
  folder_name = switch(  
    prefix,  
    "Prec"="prec",  
    "S.Rad"="sol_rad",  
    "T.Max"="t_max",  
    "T.Min"="t_min")  
}

createScenarios <- function(data_stations, output_path) {
  coordinates = data_stations
  range <- 1:nrow(coordinates)
  filenames = c("Prec","T.Min","T.Max","S.Rad")
  ranges = read.csv(file = paste(output_path,paste(filenames[[2]],".data.observed.csv",sep="" ),sep=""),header=T, row.names=1)
  list = list()
  date = list()
  rows = nrow(ranges)
  cols = ncol(ranges)
  
  
  for (station in range) {
    new_csv = data.frame()[1:rows, (length(filenames)+3)]
    for (filename in filenames) {
      
      dataset = NULL
  
      output_path_get = paste(output_path,paste(filename,".data.observed.csv",sep="" ),sep="")
      dataset = read.csv(file =output_path_get,header=T, row.names=1)
      
      split_dataset = dataset[1:rows,]
      if(filename == "Prec"){
        if(!is.na(format(strptime(as.character(row.names(split_dataset[1,][1])), "%m/%d/%Y"),"%d" ))){
          new_csv$day = as.numeric(format(strptime(as.character(row.names(split_dataset)), "%m/%d/%Y"),"%d" ))
          new_csv$month = as.numeric(format(strptime(as.character(row.names(split_dataset)), "%m/%d/%Y"),"%m" ))
          new_csv$year = as.numeric(format(strptime(as.character(row.names(split_dataset)), "%m/%d/%Y"),"%Y" ))
        }else{
          new_csv$day = as.numeric(format(strptime(as.character(row.names(split_dataset)), "%Y-%m-%d"),"%d" ))
          new_csv$month = as.numeric(format(strptime(as.character(row.names(split_dataset)), "%Y-%m-%d"),"%m" ))
          new_csv$year = as.numeric(format(strptime(as.character(row.names(split_dataset)), "%Y-%m-%d"),"%Y" ))
        }
      }
      new_csv[replaceNameCsv(filename)] = cbind(split_dataset[station])
      
    }
    new_csv = new_csv[c(1,2,3,6,5,4,7)]
    output_file_name = paste("/",coordinates[station,3],sep="")
    dir.create(paste(output_path,"resampling_observed",sep=""), showWarnings = FALSE)
    write.table(new_csv,paste(paste(output_path,"resampling_observed",sep=""),paste(output_file_name,"_escenario.csv",sep="" ),sep = ""), row.names = FALSE, sep=",")
    cat("Done: ")
    cat(coordinates[station,3])
    cat("\n")
    cat("\n")
  }
}




