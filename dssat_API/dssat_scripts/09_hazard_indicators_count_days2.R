#g=gc();rm(list=ls())

#Sp_load(lubridate, tidyverse,readr,stringr,tidyr,data.table)

##################
##### ACRONYMS  ##
##################
# ndr10_t
# ndr40_t
# ndr5_h_m
# ndr40_bh_m
# cdr5_h_f
# cdr5_f_m
# ndt2_b_f
# ndt28_b_f
# acronym <- "ndr40_t"

hazards_count_days2 <- function(root, PlantGro_r , Weather_r, crop_conf_r, ind ){
 #####################
  # Procesar PlantGro #
  #####################
  
  scenarios_plantgro = list()
  PlantGro_1 = read_lines(PlantGro_r)
  pos_head <- grep("@YEAR", PlantGro_1)
  
  for (i in 1:length(pos_head)){
    
    if(i < length(pos_head)){
      
      Ini = pos_head[i]      # The first row of the plant growth data
      End = pos_head[i+1]-10 # The last row of the plant growth data
      
      PlantGro = fread(PlantGro_r, header=T,check.names = T, skip=(pos_head[i]-1),fill=TRUE,sep = " ",dec=".", nrows = End - Ini)
      PlantGro = PlantGro[,c("X.YEAR","DOY","DAS","GSTD")]
      
      colnames(PlantGro) <- c("@YEAR","DOY","DAS","GSTD")
      scenarios_plantgro[[i]] = PlantGro
      
    } else{ ## the case to read the weather for the last simulation, i.e. simulation number 99 ##
      
      Ini = pos_head[i]      # First row of the climate data
      End = length(PlantGro_1) # Last row of the climate data
      
      PlantGro = fread (PlantGro_r, header=T,check.names = T, skip=(pos_head[i]-1),fill=TRUE,sep = " ",dec=".", nrows = End - Ini)
      PlantGro = PlantGro[,c("X.YEAR","DOY","DAS","GSTD")]
      colnames(PlantGro) <- c("@YEAR","DOY","DAS","GSTD")
      scenarios_plantgro[[i]] = PlantGro
    }
    
  }
  
  # Procesar Clima
  
  scenarios_weather = list()
  
  Weather_1 = read_lines(Weather_r)
  pos_head <- grep("@YEAR", Weather_1)
  
  for (i in 1:length(pos_head)){
    
    if(i < length(pos_head)){
      
      Ini = pos_head[i]      # The first row of the climate data
      End = pos_head[i+1]-10 # The last row of the climate data
      
      Weather = fread (Weather_r, header=T,check.names = T, skip=(pos_head[i]-1),fill=TRUE,sep = " ",dec=".", nrows = End - Ini)
      Weather = Weather[,c("X.YEAR","DOY","DAS","PRED")]
      colnames(Weather) = c("@YEAR", "DOY",	"DAS", "PRED")
      scenarios_weather[[i]] = Weather
      
    } else { ## the case to read the weather for the last simulation, i.e. simulation number 99 ##
      
      Ini = pos_head[i]      # First row of the climate data
      End = length(Weather_1) # Last row of the climate data
      
      Weather = fread (Weather_r, header=T,check.names = T, skip=(pos_head[i]-1),fill=TRUE,sep = " ",dec=".", nrows = End - Ini)
      Weather = Weather[,c("X.YEAR","DOY","DAS","PRED")]
      colnames(Weather) = c("@YEAR", "DOY",	"DAS", "PRED")
      scenarios_weather[[i]] = Weather
    }
  }
  
  # Calcular indicadores 
  
  crop_conf_inf <- read.csv(crop_conf_r,row.names = NULL)

  #### Nueva condicion 
  # Disponibilidad de datos 
  
  #Numero dias plantgro - weather 
  
  num_plan <- length(scenarios_plantgro)
  num_clim <- length(scenarios_weather)
  
  av <- num_plan - num_clim
  
  if(av < 0){
    scenarios <- length(scenarios_plantgro)
  }else{
    scenarios <- length(scenarios_weather) 
  }
  
  
  
  ###################  ndr40 0
  if("ndr40_0" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="ndr40_0")   
    start <- con$min
   
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      cat(paste("procesando : ",i, "\n"))
      df <- scenarios_plantgro[[i]] %>% as.data.frame()
      df_condition  = subset(df, GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    
    
    NDR40_0  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 10)
      ind  <- data.frame(Acronym = "ndr40_0" , value = inx)
      return(ind)
    })
    
    NDR40_0 <- do.call(rbind,NDR40_0)
    
  }else{NDR40_0 <- data.frame(Acronym= NA, value= NA)}
  
  ################### ndr40  1
  if("ndr40_1" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="ndr40_1")   
    start <- con$min
    
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      cat(paste("procesando : ",i, "\n"))
      df <- scenarios_plantgro[[i]] %>% as.data.frame()
      df_condition  = subset(df, GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    
    
    NDR40_1  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 40)
      ind  <- data.frame(Acronym = "ndr40_1" , value = inx)
      return(ind)
    })
    
    NDR40_1 <- do.call(rbind,NDR40_1)
    
  }else{NDR40_1 <- data.frame(Acronym= NA, value= NA)}
  
  
  ################### ndr40 2
  if("ndr40_2" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="ndr40_2")   
    start <- con$min
    
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      cat(paste("procesando : ",i, "\n"))
      df <- scenarios_plantgro[[i]] %>% as.data.frame()
      df_condition  = subset(df, GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    
    
    NDR40_2  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 40)
      ind  <- data.frame(Acronym = "ndr40_2" , value = inx)
      return(ind)
    })
    
    NDR40_2 <- do.call(rbind,NDR40_2)
    
  }else{NDR40_2 <- data.frame(Acronym= NA, value= NA)}
  
  
  
  ###################  ndr40 3
  if("ndr40_3" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="ndr40_3")   
    start <- con$min
    
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      cat(paste("procesando : ",i, "\n"))
      df <- scenarios_plantgro[[i]] %>% as.data.frame()
      df_condition  = subset(df, GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    
    
    NDR40_3  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 40)
      ind  <- data.frame(Acronym = "ndr40_3" , value = inx)
      return(ind)
    })
    
    NDR40_3 <- do.call(rbind,NDR40_3)
    
  }else{NDR40_3 <- data.frame(Acronym= NA, value= NA)}
  
  
  
  ###################  ndr40 4
  if("ndr40_4" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="ndr40_4")   
    start <- con$min
    
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      cat(paste("procesando : ",i, "\n"))
      df <- scenarios_plantgro[[i]] %>% as.data.frame()
      df_condition  = subset(df, GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    
    
    NDR40_4  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 40)
      ind  <- data.frame(Acronym = "ndr40_4" , value = inx)
      return(ind)
    })
    
    NDR40_4 <- do.call(rbind,NDR40_4)
    
  }else{NDR40_4 <- data.frame(Acronym= NA, value= NA)}
  
  
  ###################  ndr40 5
  if("ndr40_5" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="ndr40_5")   
    start <- con$min
    
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      cat(paste("procesando : ",i, "\n"))
      df <- scenarios_plantgro[[i]] %>% as.data.frame()
      df_condition  = subset(df, GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    
    
    NDR40_5  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 40)
      ind  <- data.frame(Acronym = "ndr40_5" , value = inx)
      return(ind)
    })
    
    NDR40_5 <- do.call(rbind,NDR40_5)
    
  }else{NDR40_5 <- data.frame(Acronym= NA, value= NA)}
  
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  
  ##############################  ndr5_0 
  
  if("ndr5_0" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "ndr5_0")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start)
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDR5_0  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 5)
      ind  <- data.frame(Acronym = "ndr5_0" , value = inx)
      return(ind)
    })
    
    NDR5_0 <- do.call(rbind,NDR5_0)
    
  }else{NDR5_0 <- data.frame(Acronym= NA, value= NA)}
  
  ##############################  ndr5_1 
  
  if("ndr5_1" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "ndr5_1")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start)
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDR5_1  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 5)
      ind  <- data.frame(Acronym = "ndr5_1" , value = inx)
      return(ind)
    })
    
    NDR5_1 <- do.call(rbind,NDR5_1)
    
  }else{NDR5_1 <- data.frame(Acronym= NA, value= NA)} 
  
  
  ##############################  ndr5_2 
  
  if("ndr5_2" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "ndr5_2")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start)
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDR5_2  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 5)
      ind  <- data.frame(Acronym = "ndr5_2" , value = inx)
      return(ind)
    })
    
    NDR5_2 <- do.call(rbind,NDR5_2)
    
  }else{NDR5_2 <- data.frame(Acronym= NA, value= NA)} 
  
  
  ##############################  ndr5_3 
  
  if("ndr5_3" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "ndr5_3")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start)
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDR5_3  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 5)
      ind  <- data.frame(Acronym = "ndr5_3" , value = inx)
      return(ind)
    })
    
    NDR5_3 <- do.call(rbind,NDR5_3)
    
  }else{NDR5_3 <- data.frame(Acronym= NA, value= NA)}  
  
  
  ##############################  ndr5_4 
  
  if("ndr5_4" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "ndr5_4")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start)
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDR5_4  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 5)
      ind  <- data.frame(Acronym = "ndr5_4" , value = inx)
      return(ind)
    })
    
    NDR5_4 <- do.call(rbind,NDR5_4)
    
  }else{NDR5_4 <- data.frame(Acronym= NA, value= NA)}  
  
  
  ##############################  ndr5_5 
  
  if("ndr5_5" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "ndr5_5")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start)
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDR5_5  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 5)
      ind  <- data.frame(Acronym = "ndr5_5" , value = inx)
      return(ind)
    })
    
    NDR5_5 <- do.call(rbind,NDR5_5)
    
  }else{NDR5_5 <- data.frame(Acronym= NA, value= NA)}  
  
  ################################################################################
  ################################################################################
  ################################################################################
  ################################################################################
  
  ############################  cdr5 0
  
  if("cdr5_0" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "cdr5_0")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    CDR5_0  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      dr_stress <- function(data, p_thresh=5){
        runs <- rle(data > p_thresh)
        cons_days <- max(runs$lengths[runs$values==1], na.rm=TRUE)
        return(cons_days)
      }
      library(compiler)
      dr_stressCMP <- cmpfun(dr_stress)
      
      inx  <- dr_stressCMP(data)
      ind  <- data.frame(Acronym = "cdr5_0" , value = inx)
      return(ind)
    })
    
    CDR5_0 <- do.call(rbind,CDR5_0)
    CDR5_0$value[which(CDR5_0$value == "-Inf")] <- 0
    
  }else{CDR5_0 <- data.frame(Acronym= NA, value= NA)}
  
  
  ############################  cdr5 1
  
  if("cdr5_1" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "cdr5_1")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    CDR5_1  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      dr_stress <- function(data, p_thresh=5){
        runs <- rle(data > p_thresh)
        cons_days <- max(runs$lengths[runs$values==1], na.rm=TRUE)
        return(cons_days)
      }
      library(compiler)
      dr_stressCMP <- cmpfun(dr_stress)
      
      inx  <- dr_stressCMP(data)
      ind  <- data.frame(Acronym = "cdr5_1" , value = inx)
      return(ind)
    })
    
    CDR5_1 <- do.call(rbind,CDR5_1)
    CDR5_1$value[which(CDR5_1$value == "-Inf")] <- 0
    
  }else{CDR5_1 <- data.frame(Acronym= NA, value= NA)}
  
  
  ############################  cdr5 2
  
  if("cdr5_2" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "cdr5_2")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    CDR5_2  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      dr_stress <- function(data, p_thresh=5){
        runs <- rle(data > p_thresh)
        cons_days <- max(runs$lengths[runs$values==1], na.rm=TRUE)
        return(cons_days)
      }
      library(compiler)
      dr_stressCMP <- cmpfun(dr_stress)
      
      inx  <- dr_stressCMP(data)
      ind  <- data.frame(Acronym = "cdr5_2" , value = inx)
      return(ind)
    })
    
    CDR5_2 <- do.call(rbind,CDR5_2)
    CDR5_2$value[which(CDR5_2$value == "-Inf")] <- 0
    
  }else{CDR5_2 <- data.frame(Acronym= NA, value= NA)} 
  
  
  ############################  cdr5 3
  
  if("cdr5_3" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "cdr5_3")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    CDR5_3  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      dr_stress <- function(data, p_thresh=5){
        runs <- rle(data > p_thresh)
        cons_days <- max(runs$lengths[runs$values==1], na.rm=TRUE)
        return(cons_days)
      }
      library(compiler)
      dr_stressCMP <- cmpfun(dr_stress)
      
      inx  <- dr_stressCMP(data)
      ind  <- data.frame(Acronym = "cdr5_3" , value = inx)
      return(ind)
    })
    
    CDR5_3 <- do.call(rbind,CDR5_3)
    CDR5_3$value[which(CDR5_3$value == "-Inf")] <- 0
    
  }else{CDR5_3 <- data.frame(Acronym= NA, value= NA)} 
  
  
  ############################  cdr5 4
  
  if("cdr5_4" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "cdr5_4")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    CDR5_4  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      dr_stress <- function(data, p_thresh=5){
        runs <- rle(data > p_thresh)
        cons_days <- max(runs$lengths[runs$values==1], na.rm=TRUE)
        return(cons_days)
      }
      library(compiler)
      dr_stressCMP <- cmpfun(dr_stress)
      
      inx  <- dr_stressCMP(data)
      ind  <- data.frame(Acronym = "cdr5_4" , value = inx)
      return(ind)
    })
    
    CDR5_4 <- do.call(rbind,CDR5_4)
    CDR5_4$value[which(CDR5_4$value == "-Inf")] <- 0
    
  }else{CDR5_4 <- data.frame(Acronym= NA, value= NA)}  
  
  
  ############################  cdr5 5
  
  if("cdr5_5" %in%  ind){
    
    con <- dplyr::filter(crop_conf_inf, name ==  "cdr5_5")   
    start <- con$min
    
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    CDR5_5  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      dr_stress <- function(data, p_thresh=5){
        runs <- rle(data > p_thresh)
        cons_days <- max(runs$lengths[runs$values==1], na.rm=TRUE)
        return(cons_days)
      }
      library(compiler)
      dr_stressCMP <- cmpfun(dr_stress)
      
      inx  <- dr_stressCMP(data)
      ind  <- data.frame(Acronym = "cdr5_5" , value = inx)
      return(ind)
    })
    
    CDR5_5 <- do.call(rbind,CDR5_5)
    CDR5_5$value[which(CDR5_5$value == "-Inf")] <- 0
    
  }else{CDR5_5 <- data.frame(Acronym= NA, value= NA)}
  
  
  
  ##############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  
  
  ################################# ndt33 0 
  
  if("ndt33_0" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name ==  "ndt33_0")   
    start <- con$min
    
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDT33_0  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$TMXD  
      inx  <- sum(data > 33)
      ind  <- data.frame(Acronym = "ndt33_0" , value = inx)
      return(ind)
    })
    
    NDT33_0 <- do.call(rbind,NDT33_0)
  }else{NDT33_0 <- data.frame(Acronym= NA, value= NA)}
  
 
  ################################# ndt33 1 
  
  if("ndt33_1" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name ==  "ndt33_1")   
    start <- con$min
    
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDT33_1  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$TMXD  
      inx  <- sum(data > 33)
      ind  <- data.frame(Acronym = "ndt33_1" , value = inx)
      return(ind)
    })
    
    NDT33_1 <- do.call(rbind,NDT33_1)
  }else{NDT33_1 <- data.frame(Acronym= NA, value= NA)}
  
  
  ################################# ndt33 2 
  
  if("ndt33_2" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name ==  "ndt33_2")   
    start <- con$min
    
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDT33_2  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$TMXD  
      inx  <- sum(data > 33)
      ind  <- data.frame(Acronym = "ndt33_2" , value = inx)
      return(ind)
    })
    
    NDT33_2 <- do.call(rbind,NDT33_2)
  }else{NDT33_2 <- data.frame(Acronym= NA, value= NA)}
  
  
  ################################# ndt33 3 
  
  if("ndt33_3" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name ==  "ndt33_3")   
    start <- con$min
    
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDT33_3  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$TMXD  
      inx  <- sum(data > 33)
      ind  <- data.frame(Acronym = "ndt33_3" , value = inx)
      return(ind)
    })
    
    NDT33_3 <- do.call(rbind,NDT33_3)
  }else{NDT33_3 <- data.frame(Acronym= NA, value= NA)}
  
  ################################# ndt33 4 
  
  if("ndt33_4" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name ==  "ndt33_4")   
    start <- con$min
    
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDT33_4  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$TMXD  
      inx  <- sum(data > 33)
      ind  <- data.frame(Acronym = "ndt33_4" , value = inx)
      return(ind)
    })
    
    NDT33_4 <- do.call(rbind,NDT33_4)
  }else{NDT33_4 <- data.frame(Acronym= NA, value= NA)}  
  
  ################################# ndt33 5 
  
  if("ndt33_5" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name ==  "ndt33_5")   
    start <- con$min
    
    # list_weather_condition = list()
    
    clima_con <- lapply(1:scenarios, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    NDT33_5  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$TMXD  
      inx  <- sum(data > 33)
      ind  <- data.frame(Acronym = "ndt33_5" , value = inx)
      return(ind)
    })
    
    NDT33_5 <- do.call(rbind,NDT33_5)
  }else{NDT33_5 <- data.frame(Acronym= NA, value= NA)}  
  
  
  resultado <- list(NDR40_0,NDR40_1,NDR40_2,NDR40_3,NDR40_4,NDR40_5,
                    NDR5_0 ,NDR5_1 ,NDR5_2 ,NDR5_3 ,NDR5_4 ,NDR5_5 ,
                    CDR5_0 ,CDR5_1 ,CDR5_2 ,CDR5_3 ,CDR5_4 ,CDR5_5 ,
                    NDT33_0,NDT33_1,NDT33_2,NDT33_3,NDT33_4,NDT33_5)

}

hazards_count_days_safe <- purrr::possibly(hazards_count_days, NA)

#Gets indicators for all planting dates
hazard_count_days_all2 <- function(data_files_all){

  ind = c("ndr40_0","ndr40_1","ndr40_2","ndr40_3","ndr40_4","ndr40_5",
                        "ndr5_0","ndr5_1","ndr5_2","ndr5_3","ndr5_4","ndr5_5",
                        "cdr5_0","cdr5_1","cdr5_2","cdr5_3","cdr5_4","cdr5_5",
                        "ndt33_0","ndt33_1","ndt33_2","ndt33_3","ndt33_4","ndt33_5")

  indicadores <- mclapply(1:length(data_files_all), function(i) {
    data_files <- paste0(data_files_all[i])
    PlantGro_r <-  paste0(data_files, "PlantGro.OUT")
    Weather_r  <-  paste0(data_files, "Weather.OUT")
    crop_conf_r  <-  paste0(data_files, "crop_conf.csv")
    
    df <-  hazards_count_days2(root = data_files,
          PlantGro= PlantGro_r,
          Weather= Weather_r,
          crop_conf_r=crop_conf_r,
          ind=ind)

    df[df == "-Inf" | is.na(df)] <- NA
    df[df == "Inf" | is.na(df)] <- NA
    return(df) 
  
    }, mc.cores = no_cores, mc.preschedule = F)
    
    resumen <- lapply(1:length(indicadores), function(i){
      
      lista <- indicadores[[i]]
      
      df <- lapply(1:length(lista), function(j){
        
        df <- lista[[j]]
        data <- df
        
        conf_upper <- function(var){
          t.test(var)$conf.int[2]
        }
        conf_lower <- function(var){
          
          t.test(var)$conf.int[1]
        } 
        CV <- function(var){
          
          (sd(var)/mean(var))*100
          
        }
        if(!is.na(df)){
          result <- data.frame(measure = unique(paste0("hs_",df$Acronym)),
                              avg = mean(df$value, na.rm = T),
                              median = median(df$value, na.rm = T),
                              min = min(df$value, na.rm = T),
                              max = max(df$value, na.rm = T),
                              quar_1 = quantile(df$value, 0.25, na.rm = T),
                              quar_2 = quantile(df$value, 0.50, na.rm = T),
                              quar_3 = quantile(df$value, 0.75, na.rm = T),
                              conf_lower <- tryCatch({conf_lower(df$value)}, error = function(e) {0}),
                              conf_upper <- tryCatch({conf_upper(df$value)}, error = function(e) {0}),
                              sd = sd(df$value, na.rm = T), 
                              perc_5 = quantile(df$value, 0.05, na.rm = T),
                              perc_95 = quantile(df$value, 0.95, na.rm = T), 
                              coef_var = CV(df$value))
                              
            colnames(result) <- c("measure","avg",
                                  "median","min","max","quar_1","quar_2","quar_3","conf_lower","conf_upper",
                                  "sd","perc_5","perc_95","coef_var") 
                                  
            result[result == "-Inf" | is.na(result)] <- 0
            result[result == "Inf" | is.na(result)] <- 0                  

        } else {
          result = NA
        }
        return(result)                  
      })
    final <- do.call(rbind,df)
    row.names(final) <- 1:nrow(final)
    final <- na.omit(final)
    return(final)
    })
  return(resumen)
}






