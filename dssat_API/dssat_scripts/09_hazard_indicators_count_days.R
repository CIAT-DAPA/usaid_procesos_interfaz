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

hazards_count_days <- function(root, PlantGro_r , Weather_r, crop_conf_r, ind ){
  
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
      
      PlantGro = fread(PlantGro_r, header=T,check.names = T, skip=(pos_head[i]-1),sep = " ",dec=".", nrows = End - Ini)
      
      var_names_PlantGro = c("@YEAR", "DOY",	"DAS",	"DAP",	"TMEAN",	"TKILL",	"GSTD",	"L#SD",	"PARID",
                             "PARUD",	"AWAD",	"LAID",	"SAID", "CAID",	"TWAD",	"SDWAD",	"RWAD",	"CWAD",	"LWAD",	"SWAD",	"HWAD",
                             "HIAD",	"CHWAD",	"EWAD",	"RSWAD",	"SNWPD",	"SNWLD",	"SNWSD",	"RS%D",	"H#AD",	"HWUD",	"T#AD",
                             "SLAD",	"RDPD",	"PTFD",	"SWXD",	"WAVRD",	"WUPRD",	"WFTD",	"WFPD",	"WFGD",	"NFTD",	"NFPD",	"NFGD",
                             "NUPRD",	"TFPD",	"TFGD",	"VRNFD",	"DYLFD")
      
      names(PlantGro)[1:49] = var_names_PlantGro
      
      scenarios_plantgro[[i]] = PlantGro
      
    } else{ ## the case to read the weather for the last simulation, i.e. simulation number 99 ##
      
      Ini = pos_head[i]      # First row of the climate data
      End = length(PlantGro_1) # Last row of the climate data
      
      PlantGro = fread (PlantGro_r, header=T,check.names = T, skip=(pos_head[i]-1),sep = " ",dec=".", nrows = End - Ini)
      
      var_names_PlantGro = c("@YEAR", "DOY",	"DAS",	"DAP",	"TMEAN",	"TKILL",	"GSTD",	"L#SD",	"PARID",
                             "PARUD",	"AWAD",	"LAID",	"SAID", "CAID",	"TWAD",	"SDWAD",	"RWAD",	"CWAD",	"LWAD",	"SWAD",	"HWAD",
                             "HIAD",	"CHWAD",	"EWAD",	"RSWAD",	"SNWPD",	"SNWLD",	"SNWSD",	"RS%D",	"H#AD",	"HWUD",	"T#AD",
                             "SLAD",	"RDPD",	"PTFD",	"SWXD",	"WAVRD",	"WUPRD",	"WFTD",	"WFPD",	"WFGD",	"NFTD",	"NFPD",	"NFGD",
                             "NUPRD",	"TFPD",	"TFGD",	"VRNFD",	"DYLFD")
      
      names(PlantGro)[1:49] = var_names_PlantGro
      
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
      
      Weather = fread (Weather_r, header=T,check.names = T, skip=(pos_head[i]-1),sep = " ",dec=".", nrows = End - Ini)
      
      var_names_weather = c("@YEAR", "DOY",	"DAS", "PRED", "DAYLD", "TWLD",
                            "SRAD", "PARD", "CLDD", "TMXD", "TMND", "TAVD",
                            "TDYD", "TDWD", "TGAD", "TGRD", "WDSD", "CO2D",
                            "VPDF", "VPD", "OZON7", "WDATE")
      
      names(Weather)[1:22] = var_names_weather
      
      scenarios_weather[[i]] = Weather
      
    } else { ## the case to read the weather for the last simulation, i.e. simulation number 99 ##
      
      Ini = pos_head[i]      # First row of the climate data
      End = length(Weather_1) # Last row of the climate data
      
      Weather = fread (Weather_r, header=T,check.names = T, skip=(pos_head[i]-1),sep = " ",dec=".", nrows = End - Ini)
      
      var_names_weather = c("@YEAR", "DOY",	"DAS", "PRED", "DAYLD", "TWLD",
                            "SRAD", "PARD", "CLDD", "TMXD", "TMND", "TAVD",
                            "TDYD", "TDWD", "TGAD", "TGRD", "WDSD", "CO2D",
                            "VPDF", "VPD", "OZON7", "WDATE")
      
      names(Weather)[1:22] = var_names_weather
      
      scenarios_weather[[i]] = Weather
      
    }
    
  }
  
  # Calcular indicadores 
  
  crop_conf_inf <- read_csv(crop_conf_r)
  #colnames(crop_conf_inf) <- colnames(crop_conf_inf[,2:5]) 
  #crop_conf_inf <- crop_conf_inf[,1:4]
  
  
  ###################   1
  if("ndr10_t" %in%  ind){
  con <- dplyr::filter(crop_conf_inf, name =="ndr10_t")   
  start <- con$min
  end   <- con$max
  # list_weather_condition = list()
  
  clima_con <- lapply(1:length(scenarios_weather), function(i){
    df_condition  = subset(scenarios_plantgro[[i]], GSTD >= start & GSTD <= end,)
    DAS <- sort(df_condition$DAS,decreasing = F)
    First_DAS_condition = df_condition$DAS[1]
    Last_DAS_condition = df_condition$DAS[length(DAS)]
    weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
    return(weather_data_condition)
  })
  
  
  
    NDR10_T  <- lapply(1: length(clima_con), function(i){
      data <- clima_con[[i]]
      data <- data$PRED
      inx  <- sum(data > 10)
      ind  <- data.frame(Acronym = "ndr10_t" , value = inx)
      return(ind)
    })
    
    NDR10_T <- do.call(rbind,NDR10_T)
    
  }else{NDR10_T <- data.frame(Acronym= NA, value= NA)}
  
  
  
  ############      2
  
  if("ndr40_t" %in%  ind){
  con <- dplyr::filter(crop_conf_inf, name ==  "ndr40_t")  
  start <- con$min
  end   <- con$max
  # list_weather_condition = list()
  
  clima_con <- lapply(1:length(scenarios_weather), function(i){
    df_condition  = subset(scenarios_plantgro[[i]], GSTD >= start & GSTD <= end,)
    DAS <- sort(df_condition$DAS,decreasing = F)
    First_DAS_condition = df_condition$DAS[1]
    Last_DAS_condition = df_condition$DAS[length(DAS)]
    weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
    return(weather_data_condition)
  })
  
  NDR40_T  <- lapply(1: length(clima_con), function(i){
    data <- clima_con[[i]]
    data <- data$PRED
    inx  <- sum(data > 40)
    ind  <- data.frame(Acronym = "ndr40_t" , value = inx)
    return(ind)
  })
  
  NDR40_T <- do.call(rbind,NDR40_T)
  }else{NDR40_T <- data.frame(Acronym= NA, value= NA)}
  
  ###################      3
  
  if("ndr5_h_m" %in%  ind){
  
  con <- dplyr::filter(crop_conf_inf, name ==  "ndr5_h_m")   
  start <- con$min
  end   <- con$max
  # list_weather_condition = list()
  
  clima_con <- lapply(1:length(scenarios_weather), function(i){
    df_condition  = subset(scenarios_plantgro[[i]], GSTD >= start & GSTD <= end,)
    DAS <- sort(df_condition$DAS,decreasing = F)
    First_DAS_condition = df_condition$DAS[1]
    Last_DAS_condition = df_condition$DAS[length(DAS)]
    weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
    return(weather_data_condition)
  })
  
  NDR5_H_M  <- lapply(1: length(clima_con), function(i){
    data <- clima_con[[i]]
    data <- data$PRED
    inx  <- sum(data > 5)
    ind  <- data.frame(Acronym = "ndr5_h_m" , value = inx)
    return(ind)
  })
  
  NDR5_H_M <- do.call(rbind,NDR5_H_M)
  
  }else{NDR5_H_M <- data.frame(Acronym= NA, value= NA)}
  
  ######################   4
  
  if("ndr5_h_m" %in%  ind){
    
  con <- dplyr::filter(crop_conf_inf, name ==  "ndr40_bh_m")   
  start <- con$min
  end   <- con$max
  # list_weather_condition = list()
  
  clima_con <- lapply(1:length(scenarios_weather), function(i){
    df_condition  = subset(scenarios_plantgro[[i]], GSTD >= start & GSTD <= end,)
    DAS <- sort(df_condition$DAS,decreasing = F)
    First_DAS_condition = df_condition$DAS[1]
    Last_DAS_condition = df_condition$DAS[length(DAS)]
    weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
    return(weather_data_condition)
  })
  
  NDR40_BH_M  <- lapply(1: length(clima_con), function(i){
    data <- clima_con[[i]]
    data <- data$PRED
    inx  <- sum(data > 40)
    ind  <- data.frame(Acronym = "ndr40_bh_m" , value = inx)
    return(ind)
  })
  
  NDR40_BH_M <- do.call(rbind,NDR40_BH_M)
  }else{NDR40_BH_M <- data.frame(Acronym= NA, value= NA)}
  
  ############################  5
  
  if("ndr5_h_m" %in%  ind){
  
  con <- dplyr::filter(crop_conf_inf, name ==  "cdr5_h_f")   
  start <- con$min
  end   <- con$max
  # list_weather_condition = list()
  
  clima_con <- lapply(1:length(scenarios_weather), function(i){
    df_condition  = subset(scenarios_plantgro[[i]], GSTD >= start & GSTD <= end,)
    DAS <- sort(df_condition$DAS,decreasing = F)
    First_DAS_condition = df_condition$DAS[1]
    Last_DAS_condition = df_condition$DAS[length(DAS)]
    weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
    return(weather_data_condition)
  })
  
  CDR5_H_F  <- lapply(1: length(clima_con), function(i){
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
    ind  <- data.frame(Acronym = "cdr5_h_f" , value = inx)
    return(ind)
  })
  
  CDR5_H_F <- do.call(rbind,CDR5_H_F)
  CDR5_H_F$value[which(CDR5_H_F$value == "-Inf")] <- 0
  
  }else{CDR5_H_F <- data.frame(Acronym= NA, value= NA)}
  
  ##############################  6
  
  if("ndr5_h_m" %in%  ind){
  con <- dplyr::filter(crop_conf_inf, name ==  "cdr5_f_m")   
  start <- con$min
  end   <- con$max
  # list_weather_condition = list()
  
  clima_con <- lapply(1:length(scenarios_weather), function(i){
    df_condition  = subset(scenarios_plantgro[[i]], GSTD >= start & GSTD <= end,)
    DAS <- sort(df_condition$DAS,decreasing = F)
    First_DAS_condition = df_condition$DAS[1]
    Last_DAS_condition = df_condition$DAS[length(DAS)]
    weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
    return(weather_data_condition)
  })
  
  CDR5_F_M  <- lapply(1: length(clima_con), function(i){
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
    ind  <- data.frame(Acronym = "cdr5_f_m" , value = inx)
    return(ind)
  })
  
  CDR5_F_M <- do.call(rbind,CDR5_F_M)
  CDR5_F_M$value[which(CDR5_F_M$value == "-Inf")] <- 0
  
  }else{CDR5_F_M <- data.frame(Acronym= NA, value= NA)}
  
  ################################  7 
  
  
  if("ndr5_h_m" %in%  ind){
  con <- dplyr::filter(crop_conf_inf, name ==  "ndt2_b_f")   
  start <- con$min
  end   <- con$max
  # list_weather_condition = list()
  
  clima_con <- lapply(1:length(scenarios_weather), function(i){
    df_condition  = subset(scenarios_plantgro[[i]], GSTD >= start & GSTD <= end,)
    DAS <- sort(df_condition$DAS,decreasing = F)
    First_DAS_condition = df_condition$DAS[1]
    Last_DAS_condition = df_condition$DAS[length(DAS)]
    weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
    return(weather_data_condition)
  })
  
  NDT2_B_F  <- lapply(1: length(clima_con), function(i){
    data <- clima_con[[i]]
    data <- data$TMND  
    inx  <- sum(data < 2)
    ind  <- data.frame(Acronym = "ndt2_b_f" , value = inx)
    return(ind)
  })
  
  NDT2_B_F <- do.call(rbind,NDT2_B_F)
  
  }else{NDT2_B_F <- data.frame(Acronym= NA, value= NA)}
  
  ################################# 8 
  
  if("ndr5_h_m" %in%  ind){
  con <- dplyr::filter(crop_conf_inf, name ==  "ndt28_b_f")   
  start <- con$min
  end   <- con$max
  # list_weather_condition = list()
  
  clima_con <- lapply(1:length(scenarios_weather), function(i){
    df_condition  = subset(scenarios_plantgro[[i]], GSTD >= start & GSTD <= end,)
    DAS <- sort(df_condition$DAS,decreasing = F)
    First_DAS_condition = df_condition$DAS[1]
    Last_DAS_condition = df_condition$DAS[length(DAS)]
    weather_data_condition= subset(scenarios_weather[[i]], DAS >= First_DAS_condition & DAS <= Last_DAS_condition,)
    return(weather_data_condition)
  })
  
  NDT28_B_F  <- lapply(1: length(clima_con), function(i){
    data <- clima_con[[i]]
    data <- data$TMXD  
    inx  <- sum(data >28)
    ind  <- data.frame(Acronym = "ndt28_b_f" , value = inx)
    return(ind)
  })
  
  NDT28_B_F <- do.call(rbind,NDT28_B_F)
  }else{NDT28_B_F <- data.frame(Acronym= NA, value= NA)}
  
  resultado <- list(NDR10_T,NDR40_T,NDR5_H_M,NDR40_BH_M,CDR5_H_F,CDR5_F_M,NDT2_B_F,NDT28_B_F  )
  
  
}

hazards_count_days_safe <- purrr::possibly(hazards_count_days, NA)

#Gets indicators for all planting dates
hazard_count_days_all <- function(data_files_all){

  ind <- c("ndr10_t","ndr40_t","ndr5_h_m","ndr40_bh_m","cdr5_h_f","cdr5_f_m","ndt2_b_f","ndt28_b_f")

  indicadores <- mclapply(1:length(data_files_all), function(i) {
    data_files <- paste0(data_files_all[i])
    PlantGro_r <-  paste0(data_files, "PlantGro.OUT")
    Weather_r  <-  paste0(data_files, "Weather.OUT")
    crop_conf_r  <-  paste0(data_files, "crop_conf.csv")
    
    df <-  hazards_count_days_safe(root = data_files,
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
                                  
            result[result == "-Inf" | is.na(result)] <- NA
            result[result == "Inf" | is.na(result)] <- NA                  

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






