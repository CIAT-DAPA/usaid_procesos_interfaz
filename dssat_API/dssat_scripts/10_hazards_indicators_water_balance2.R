#g=gc();rm(list=ls())

#Sp_load(lubridate, tidyverse,readr,stringr,tidyr,data.table)

##################
##### ACRONYMS  ##
##################
# hb_s_e
# hb_t
# hb_ei_b
# hb_bh_m
# hb_s_m
# acronym <- "hb_t"

hazards_water2 <- function(root, PlantGro_r , Weather_r, crop_conf_r,ET_r,ind ){
  
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
  
  
  #####################
  #  Procesar Clima   #
  #####################
  
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
  
  
  #####################
  #  Procesar ET      #
  #####################
  
  scenarios_ET = list()
  ET_1 = read_lines(ET_r)
  pos_head <- grep("@YEAR", ET_1)
  for (i in 1:length(pos_head)){
    
    if(i < length(pos_head)){
      
      Ini = pos_head[i]      # The first row of the climate data
      End = pos_head[i+1]-12 # The last row of the climate data
      
      ET = fread (ET_r, header=T,check.names = T, skip=(pos_head[i]-1),fill=TRUE,sep = " ",dec=".", nrows = End - Ini)
      ET <- ET[,c("X.YEAR","DOY","DAS","ETAA")]
      colnames(ET) <-  c("@YEAR","DOY","DAS","ETAA")
      scenarios_ET[[i]] = ET
      
    } else { ## the case to read the weather for the last simulation, i.e. simulation number 99 ##
      
      Ini = pos_head[i]      # First row of the climate data
      End = length(Weather_1) # Last row of the climate data
      
      ET = fread (ET_r, header=T,check.names = T, skip=(pos_head[i]-1),fill=TRUE,sep = " ",dec=".", nrows = End - Ini)
      ET <- ET[,c("X.YEAR","DOY","DAS","ETAA")]
      colnames(ET) <-  c("@YEAR","DOY","DAS","ETAA")
      scenarios_ET[[i]] = ET
      
    }
    
  }
  
  #########################
  # Calcular indicadores  #
  ######################### 
  
  crop_conf_inf <- read_csv(crop_conf_r)
  
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
  
  
  av_n <- scenarios - length(scenarios_ET)
  
  if(av_n < 0){
    scenarios_n <- scenarios
  }else{
    scenarios_n <- length(scenarios_ET) 
  }
  
  
  ###################   hb_0
  if("hb_0" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="hb_0")   
    start <- con$min
    
    clima_con <- lapply(1:scenarios_n, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    clima_new  <- lapply(1:length(clima_con) , function(i){
      
      df_clima <- clima_con[[i]]
      df_et    <- scenarios_ET[[i]]
      df_clima_start <- min(df_clima$DAS )
      df_clima_end   <- max(df_clima$DAS )
      df_et_con <-  subset(df_et, DAS >= df_clima_start & DAS <= df_clima_end,)
      clima_new <- cbind(df_clima,df_et_con$ETAA )
      colnames(clima_new) <- c(colnames(df_clima), "ETAA")
      return(clima_new)
    })
    
    hb_0  <- lapply(1: length(clima_new), function(i){
      data <- clima_new[[i]]
      prec  <- data$PRED
      evap  <- data$ETAA
      inx  <- 1 - (sum(evap) / sum(prec) )
      ind  <- data.frame(Acronym = "hb_0" , value = inx)
      return(ind)
    })
    
    hb_0 <- do.call(rbind,hb_0)
    hb_0[is.na(hb_0)] <- 0
    
  }else{hb_0 <- data.frame(Acronym= NA, value= NA)}
  
  
  ##################   hb_1
  if("hb_1" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="hb_1")   
    start <- con$min
    
    clima_con <- lapply(1:scenarios_n, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    clima_new  <- lapply(1:length(clima_con) , function(i){
      
      df_clima <- clima_con[[i]]
      df_et    <- scenarios_ET[[i]]
      df_clima_start <- min(df_clima$DAS )
      df_clima_end   <- max(df_clima$DAS )
      df_et_con <-  subset(df_et, DAS >= df_clima_start & DAS <= df_clima_end,)
      clima_new <- cbind(df_clima,df_et_con$ETAA )
      colnames(clima_new) <- c(colnames(df_clima), "ETAA")
      return(clima_new)
    })
    
    hb_1  <- lapply(1: length(clima_new), function(i){
      data <- clima_new[[i]]
      prec  <- data$PRED
      evap  <- data$ETAA
      inx  <- 1 - (sum(evap) / sum(prec) )
      ind  <- data.frame(Acronym = "hb_1" , value = inx)
      return(ind)
    })
    
    hb_1 <- do.call(rbind,hb_1)
    hb_1[is.na(hb_1)] <- 0
    
  }else{hb_1 <- data.frame(Acronym= NA, value= NA)}
  
  
  ##################   hb_2
  if("hb_2" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="hb_2")   
    start <- con$min
    
    clima_con <- lapply(1:scenarios_n, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    clima_new  <- lapply(1:length(clima_con) , function(i){
      
      df_clima <- clima_con[[i]]
      df_et    <- scenarios_ET[[i]]
      df_clima_start <- min(df_clima$DAS )
      df_clima_end   <- max(df_clima$DAS )
      df_et_con <-  subset(df_et, DAS >= df_clima_start & DAS <= df_clima_end,)
      clima_new <- cbind(df_clima,df_et_con$ETAA )
      colnames(clima_new) <- c(colnames(df_clima), "ETAA")
      return(clima_new)
    })
    
    hb_2  <- lapply(1: length(clima_new), function(i){
      data <- clima_new[[i]]
      prec  <- data$PRED
      evap  <- data$ETAA
      inx  <- 1 - (sum(evap) / sum(prec) )
      ind  <- data.frame(Acronym = "hb_2" , value = inx)
      return(ind)
    })
    
    hb_2 <- do.call(rbind,hb_2)
    hb_2[is.na(hb_2)] <- 0
    
  }else{hb_2 <- data.frame(Acronym= NA, value= NA)}
  
  
  
  ##################   hb_3
  if("hb_3" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="hb_3")   
    start <- con$min
    
    clima_con <- lapply(1:scenarios_n, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    clima_new  <- lapply(1:length(clima_con) , function(i){
      
      df_clima <- clima_con[[i]]
      df_et    <- scenarios_ET[[i]]
      df_clima_start <- min(df_clima$DAS )
      df_clima_end   <- max(df_clima$DAS )
      df_et_con <-  subset(df_et, DAS >= df_clima_start & DAS <= df_clima_end,)
      clima_new <- cbind(df_clima,df_et_con$ETAA )
      colnames(clima_new) <- c(colnames(df_clima), "ETAA")
      return(clima_new)
    })
    
    hb_3  <- lapply(1: length(clima_new), function(i){
      data <- clima_new[[i]]
      prec  <- data$PRED
      evap  <- data$ETAA
      inx  <- 1 - (sum(evap) / sum(prec) )
      ind  <- data.frame(Acronym = "hb_3" , value = inx)
      return(ind)
    })
    
    hb_3 <- do.call(rbind,hb_3)
    hb_3[is.na(hb_3)] <- 0
    
  }else{hb_3 <- data.frame(Acronym= NA, value= NA)}
  
  
  ##################   hb_4
  if("hb_4" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="hb_4")   
    start <- con$min
    
    clima_con <- lapply(1:scenarios_n, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    clima_new  <- lapply(1:length(clima_con) , function(i){
      
      df_clima <- clima_con[[i]]
      df_et    <- scenarios_ET[[i]]
      df_clima_start <- min(df_clima$DAS )
      df_clima_end   <- max(df_clima$DAS )
      df_et_con <-  subset(df_et, DAS >= df_clima_start & DAS <= df_clima_end,)
      clima_new <- cbind(df_clima,df_et_con$ETAA )
      colnames(clima_new) <- c(colnames(df_clima), "ETAA")
      return(clima_new)
    })
    
    hb_4  <- lapply(1: length(clima_new), function(i){
      data <- clima_new[[i]]
      prec  <- data$PRED
      evap  <- data$ETAA
      inx  <- 1 - (sum(evap) / sum(prec) )
      ind  <- data.frame(Acronym = "hb_4" , value = inx)
      return(ind)
    })
    
    hb_4 <- do.call(rbind,hb_4)
    hb_4[is.na(hb_4)] <- 0
    
  }else{hb_4 <- data.frame(Acronym= NA, value= NA)}
  
  
  ##################   hb_5
  if("hb_5" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="hb_5")   
    start <- con$min
    
    clima_con <- lapply(1:scenarios_n, function(i){
      df_condition  = subset(scenarios_plantgro[[i]], GSTD == start )
      DAS <- sort(df_condition$DAS,decreasing = F)
      First_DAS_condition = df_condition$DAS[1]
      Last_DAS_condition = df_condition$DAS[length(DAS)]
      weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
      return(weather_data_condition)
    })
    
    clima_new  <- lapply(1:length(clima_con) , function(i){
      
      df_clima <- clima_con[[i]]
      df_et    <- scenarios_ET[[i]]
      df_clima_start <- min(df_clima$DAS )
      df_clima_end   <- max(df_clima$DAS )
      df_et_con <-  subset(df_et, DAS >= df_clima_start & DAS <= df_clima_end,)
      clima_new <- cbind(df_clima,df_et_con$ETAA )
      colnames(clima_new) <- c(colnames(df_clima), "ETAA")
      return(clima_new)
    })
    
    hb_5  <- lapply(1: length(clima_new), function(i){
      data <- clima_new[[i]]
      prec  <- data$PRED
      evap  <- data$ETAA
      inx  <- 1 - (sum(evap) / sum(prec) )
      ind  <- data.frame(Acronym = "hb_5" , value = inx)
      return(ind)
    })
    
    hb_5 <- do.call(rbind,hb_5)
    hb_5[is.na(hb_5)] <- 0
    
  }else{hb_5 <- data.frame(Acronym= NA, value= NA)} 
  
  
  resultado <- list(hb_0,hb_1,hb_2,hb_3,hb_4,hb_5)
  return(resultado)
  
}

#hazards_water_safe <- purrr::possibly(hazards_water, NA)

#Gets indicators for all planting dates
hazard_water_all2 <- function(data_files_all){

  ind = c("hb_0","hb_1","hb_2","hb_3","hb_4","hb_5")

  indicadores <- mclapply(1:length(data_files_all), function(i) {
    data_files <- paste0(data_files_all[i])
    PlantGro_r <-  paste0(data_files, "PlantGro.OUT")
    Weather_r  <-  paste0(data_files, "Weather.OUT")
    ET_r         <-  paste0(data_files, "ET.OUT")
    crop_conf_r  <-  paste0(data_files, "crop_conf.csv")
    
      df <-  hazards_water2(root = data_files,
                    PlantGro= PlantGro_r ,
                    Weather= Weather_r,
                    crop_conf_r=crop_conf_r,
                    ET_r=ET_r,
                    ind= ind)

      df <- lapply(1:length(ind), function(k){
        tbl <- df[[k]]
        tbl[tbl == "-Inf"] <- NA
        tbl[tbl == "Inf"] <- NA
        return(tbl)
      })
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
                        conf_lower <- tryCatch({conf_lower(df$value)}, error = function(e) {NA}),
                        conf_upper <- tryCatch({conf_upper(df$value)}, error = function(e) {NA}),
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













