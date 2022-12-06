

read_PlantGro = function(filep){ 
  if(!file.exists(filep)) {
    print("Forcing return from read_file...") 
    return(NULL)
  }
  print(paste0("Loading file: ",filep))
  suppressMessages({fOUT = readLines(filep)})
  nottrashLines = grep(pattern = '[^ ]', fOUT)[!(grep(pattern = '[^ ]', fOUT) %in% 
                                                    c(grep(pattern = '^\\$', fOUT), 
                                                    grep(pattern = '^\\*', fOUT),
                                                    grep(pattern = '.+:', fOUT),
                                                    grep(pattern = '^\\!', fOUT)))]
  treatmentsLines<-grep(pattern = '.+RUN', fOUT)
  treatments<-sapply(treatmentsLines,function(v){scan(text = fOUT[v], what = "")[2]})
  fOUT_clean = fOUT[nottrashLines]
  if(length(grep(pattern="HWAD",fOUT_clean)) > 0 && length(grep(pattern="GWAD",fOUT_clean)) > 0){
    fOUT_clean <- gsub(pattern = "HWAD", replace = "GWAD", fOUT_clean)
    fOUT_clean <- gsub(pattern = "WFGD", replace = "WSGD", fOUT_clean)
    fOUT_clean <- gsub(pattern = "NFGD", replace = "NSTD", fOUT_clean)
  }
  #print(head(fOUT_clean, n=7))
  trtHeaders=which(grepl(pattern = '^@', fOUT_clean))
  if(length(trtHeaders)<=0) {
    print("Forcing return from read_file... No Headers") 
    return(NULL)
  }
  varN = lapply(trtHeaders, function(i) {
    make.names(scan(text = gsub(pattern = '^@', 
                                replacement = '',fOUT_clean[i]),
                    what = character()))
  })
  pos <- c(trtHeaders,length(fOUT_clean)+1)
  tmpA = lapply(seq(1,length(pos)-1), function(w) {
    res<-read.table(text = fOUT_clean[seq(from=pos[w], length.out = pos[w+1]-pos[w])],
                    skip = 1, 
                    col.names = varN[[w]],
                    na.strings = c('-99', '-99.0', '-99.'))
    if(length(unique(lengths(varN)))>1)
      res <- res %>% dplyr::select(YEAR, DOY, DAS, DAP, GSTD, LAID, LWAD, SWAD, GWAD, RWAD, CWAD, HIAD, SLAD, RDPD, WSGD, NSTD)
    res$TRT <- treatments[w]
    res
  })
  if(Reduce(identical,varN)==FALSE){
    commonCol <- Reduce(intersect, varN)
    varN <- rep(list(commonCol), length(varN))
  }
  data <- do.call("rbind",tmpA)
  data <- data[c('TRT',varN[[1]])]
  return(data)
}



stress_risk = function(folder,type,limits){
  # PlantGro_file <- "C:/DSSAT48/Wheat/PlantGro.OUT"
  #PlantGro_file <- paste0("D:/workdir/dssat_API700/outputs/1/PlantGro.OUT"
  PlantGro_file <- paste0(folder,"/PlantGro.OUT")
  df_plantgro <- read_PlantGro(PlantGro_file)

  nstress<-matrix(NA, nrow = 1, ncol = 99)
  # Loop for limits
  # The limits are the crop stage. It can change for each crop
  
  answer = lapply(1:nrow(limits), function(j) {
    
    measure = paste0(limits[j,c("name")],"_",type)
    
    for(i in 1:99){
      a<-subset(df_plantgro,df_plantgro$GSTD > limits[j,c("min")] & df_plantgro$GSTD < limits[j,c("max")])
      df_sub<- a[a$TRT==i,]
      if (type == "n"){
        df_sub$NFGD[df_sub$NFGD == 0] <- NA
        b<-mean(df_sub$NFGD, na.rm=T)
      }
      else if (type == "w"){
        df_sub$WFGD[df_sub$WFGD == 0] <- NA
        b<-mean(df_sub$WFGD, na.rm=T)
      }
      nstress[,i]<-b
    }
    output = c(measure,
                mean(nstress),
                median(nstress),
                min(nstress),
                max(nstress),
                quantile(nstress, 0.25,na.rm=T),
                quantile(nstress, 0.50,na.rm=T),
                quantile(nstress, 0.75,na.rm=T),
                0,
                0,
                sd(nstress),
                quantile(nstress, 0.05,na.rm=T),
                quantile(nstress, 0.95,na.rm=T),
                sd(nstress) / mean(nstress) * 100) 
    return(output)
  })
  return(answer)
}
#Gets stress risk for all planting dates
stress_risk_all <- function(data_files_all, dir_inputs_setup){
  
  crop_conf = read.csv(paste0(dir_inputs_setup,"crop_conf.csv"), header = T)
  stresses_list <- list()

  data <- lapply(1:length(data_files_all), function(i) {
    data_files <- paste0(data_files_all[i])

    values_w <- stress_risk(data_files,"w",crop_conf)
    values_n <- stress_risk(data_files,"n",crop_conf)

    stresses_list <- append(stresses_list, c(values_w, values_n))
  
  })
  return(data)

}


