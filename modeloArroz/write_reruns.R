## Proof
# region = 'Saldaña'
# CNTR <- 'USAID'
# IYEAR <- 2017
# STTIME <- 32
# EMD <- 32
# dir_run <- 'D:/CIAT/USAID/Oryza/usaid_forecast_rice/Prueba/'
# ISTN <- 1
# data <- settins_reruns(region, CNTR, ISTN, IYEAR, STTIME, EMD, dir_run)

# write_reruns(data, dir_run)

write_reruns <- function(data, dir_run){
  
  
  file_name <- paste0(dir_run, data$CNTR[1], '.rer')
  
  # print(file_name)
  if(file.exists(file_name)){
    
    file.remove(file_name)
    
  } 
  
  sink(file = file_name, append = T)
  for(i in 1:nrow(data)){
    
    
    cat('********************', sep = '\n')
    cat(paste("FILEIT = ", "'",  data[i, 'FILEIT'], "'", sep = ""), sep = '\n')
    cat(paste("FILEI2 = ", "'", data[i, 'FILEI2'], "'", sep = ""), sep = '\n')
    cat(paste("FILEI1 = ", "'", data[i, 'FILEI1'], "'", sep = ""), sep = '\n')
    cat(paste("CNTR = ",   "'", data[i, 'CNTR'], "'", sep = ""), sep = '\n')
    cat(paste("ISTN = ", data[i, 'ISTN'], sep = ""), sep = '\n')
    cat(paste("IYEAR =  ",  data[i, 'IYEAR'], sep = ""), sep = '\n')
    cat(paste("STTIME =  ",  data[i, 'STTIME'], ".", sep = ""), sep = '\n')
    cat(paste("EMD =  ", data[i, 'EMD'], sep = ""), sep = '\n')
    cat(paste("EMYR = ", data[i, 'EMYR'], sep = ""), sep = '\n')
    cat(paste("WTRDIR = ", as.character(data[i, 'WTRDIR']), sep = ""), sep = '\n')
    
    
  }
  
  sink()
  
}
