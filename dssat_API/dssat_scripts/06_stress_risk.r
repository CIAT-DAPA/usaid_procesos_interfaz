

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
                    na.strings = c('-99', '-99.0', '-99.'),
                    fill=TRUE)
    if(length(unique(lengths(varN)))>1)
      res <- res %>% dplyr::select(YEAR, DOY, DAS, DAP, GSTD, LAID, LWAD, SWAD, GWAD, RWAD, CWAD, HIAD, SLAD, RDPD, WSGD, WFGD, NSTD)
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
  PlantGro_file <- paste0(folder,"PlantGro.OUT")
  df_plantgro <- read_PlantGro(PlantGro_file)
  #df_plantgro$DATE<-as.Date(df_plantgro$DAP, format, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),origin=as.Date(paste0(year(initial_date),"-01-01")))
  df_plantgro$DATE <- initial_date + days(df_plantgro$DAP)

  nstress<-matrix(NA, nrow = 1, ncol = 99)
  # Loop for limits
  # The limits are the crop stage. It can change for each crop

  conf_lower <- function(var){
    
  
    #t.test(var)$conf.int[1]

    tryCatch(
        {t.test(var)$conf.int[1]},
        error=function(w){
          #print(df_sub)
          return(NA)
          # print(df_sub)
        }
      )
    
    #t.test(var, y = NULL, mu = 0, paired = FALSE, var.equal = FALSE)$conf.int[1]
  }
  
  conf_upper <- function(var){

    tryCatch(
        {t.test(var)$conf.int[2]},
        error=function(w){
          #print(df_sub)
          return(NA)
          # print(df_sub)
        }
      )
    
    #t.test(var, y = NULL, mu = 0, paired = FALSE, var.equal = FALSE)$conf.int[2]
  }
  
  
  answer = lapply(1:nrow(limits), function(j) {
    
    measure = paste0("st_",limits[j,c("name")],"_",type)

    for(i in 1:99){
      a<-subset(df_plantgro,df_plantgro$GSTD > as.numeric(limits[j,c("min")]) & df_plantgro$GSTD < as.numeric(limits[j,c("max")]))
      df_sub<- a[a$TRT==i,]
      
      #max(df_plantgro$DATE)
      #min(df_plantgro$DATE)
      if (type == "n"){
        ##df_sub$NFGD[df_sub$NFGD == 0] <- NA
        # wheat and maize
        b<-mean(if (crop == "wheat") df_sub$NFGD else df_sub$NSTD, na.rm=T)
        
        # tryCatch(
        # {},
        # warning=function(w){
        #   #print(df_sub)
        #   print(df_sub$NFGD)
        #   # print(df_sub)
        # }
      #)
       
      }
      else if (type == "w"){
        ##if(df_sub$WFGD[df_sub$WFGD < 0.1]){
          ##df_sub$WFGD[df_sub$WFGD < 0.1] <- NA
        ##}
        # wheat and maize
        b<-mean(if (crop == "wheat") df_sub$WFGD else df_sub$WSGD, na.rm=T)
      
      }

      nstress[,i]<-b
    }
    output = c(measure,
                mean(nstress,na.rm=T),
                median(nstress,na.rm=T),
                min(nstress,na.rm=T),
                max(nstress,na.rm=T),
                quantile(nstress, 0.25,na.rm=T),
                quantile(nstress, 0.50,na.rm=T),
                quantile(nstress, 0.75,na.rm=T),
                conf_lower = conf_lower(nstress),
                conf_upper = conf_upper(nstress),
                sd(nstress,na.rm=T),
                quantile(nstress, 0.05,na.rm=T),
                quantile(nstress, 0.95,na.rm=T),
                sd(nstress,na.rm=T) / mean(nstress,na.rm=T) * 100) 
    return(output)
  })
  return(answer)
}

#Gets stress risk for all planting dates
stress_risk_all <- function(data_files_all, dir_inputs_setup){
  
  crop_conf = read_csv(paste0(dir_inputs_setup,"crop_conf.csv"))
  stresses_list <- list()

  data <- mclapply(1:length(data_files_all), function(i) {
    data_files <- paste0(data_files_all[i])

    values_w <- stress_risk(data_files,"w",crop_conf)
    values_n <- stress_risk(data_files,"n",crop_conf)

    stresses_list <- append(stresses_list, c(values_w, values_n))
  
  }, mc.cores = no_cores, mc.preschedule = F)
  return(data)

}

stress_risk_all_safe <- purrr::possibly(stress_risk_all, NULL)


