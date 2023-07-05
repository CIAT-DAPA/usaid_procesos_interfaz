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

hazards_water <- function(root, PlantGro_r , Weather_r, crop_conf_r,ET_r,ind ){
  
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
  
  ####################################################################################################
  
  scenarios_ET = list()
  
  ET_1 = read_lines(ET_r)
  pos_head <- grep("@YEAR", ET_1)
  
  for (i in 1:length(pos_head)){
    
    if(i < length(pos_head)){
      
      Ini = pos_head[i]      # The first row of the climate data
      End = pos_head[i+1]-10 # The last row of the climate data
      
      ET = fread (ET_r, header=T,check.names = T, skip=(pos_head[i]-1),sep = " ",dec=".", nrows = End - Ini)
      
      var_names_ET = c("@YEAR","DOY","DAS","SRAA","TMAXA","TMINA","REFA","EOAA","EOPA","EOSA",
                       "ETAA","EPAA","ESAA","EFAA","EMAA","EOAC","ETAC","EPAC","ESAC","EFAC",
                       "EMAC","KCAA","KBSA","KEAA","ES1D","ES2D","ES3D","ES4D","ES5D","ES6D",
                       "ES7D","TRWUD","TWUPD")
      
      
      
      
      names(ET)[1:33] = var_names_ET
      
      scenarios_ET[[i]] = ET
      
    } else { ## the case to read the weather for the last simulation, i.e. simulation number 99 ##
      
      Ini = pos_head[i]      # First row of the climate data
      End = length(Weather_1) # Last row of the climate data
      
      ET = fread (ET_r, header=T,check.names = T, skip=(pos_head[i]-1),sep = " ",dec=".", nrows = End - Ini)
      
      var_names_ET = c("@YEAR","DOY","DAS","SRAA","TMAXA","TMINA","REFA","EOAA","EOPA","EOSA",
                       "ETAA","EPAA","ESAA","EFAA","EMAA","EOAC","ETAC","EPAC","ESAC","EFAC",
                       "EMAC","KCAA","KBSA","KEAA","ES1D","ES2D","ES3D","ES4D","ES5D","ES6D",
                       "ES7D","TRWUD","TWUPD")
      
      names(ET)[1:22] = var_names_ET
      
      scenarios_ET[[i]] = ET
      
    }
    
  }
  ####################################################################################################
  
  # Calcular indicadores 
  crop_conf_inf <- read_csv(crop_conf_r)
  colnames(crop_conf_inf) <- colnames(crop_conf_inf[,2:5]) 
  crop_conf_inf <- crop_conf_inf[,1:4]
  
  ###################   1
  if("hb_s_e" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="hb_s_e")   
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
    
    hb_s_e  <- lapply(1: length(clima_new), function(i){
      data <- clima_new[[i]]
      prec  <- data$PRED
      evap  <- data$ETAA
      inx  <- 1 - (sum(evap) / sum(prec) )
      ind  <- data.frame(Acronym = "hb_s_e" , value = inx)
      return(ind)
    })
    
    hb_s_e <- do.call(rbind,hb_s_e)
    
  }else{hb_s_e <- data.frame(Acronym= NA, value= NA)}
  
  
  ###################   2
  if("hb_t" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="hb_t")   
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
    
    hb_t  <- lapply(1: length(clima_new), function(i){
      data <- clima_new[[i]]
      prec  <- data$PRED
      evap  <- data$ETAA
      inx  <- 1 - (sum(evap) / sum(prec) )
      ind  <- data.frame(Acronym = "hb_t" , value = inx)
      return(ind)
    })
    
    hb_t <- do.call(rbind,hb_t)
    
  }else{hb_t <- data.frame(Acronym= NA, value= NA)}
  
  
  ###################   3
  if("hb_ei_b" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="hb_ei_b")   
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
    
    hb_ei_b  <- lapply(1: length(clima_new), function(i){
      data <- clima_new[[i]]
      prec  <- data$PRED
      evap  <- data$ETAA
      inx  <- 1 - (sum(evap) / sum(prec) )
      ind  <- data.frame(Acronym = "hb_ei_b" , value = inx)
      return(ind)
    })
    
    hb_ei_b <- do.call(rbind,hb_ei_b)
    
  }else{hb_ei_b <- data.frame(Acronym= NA, value= NA)}
  
  ###################   4
  if("hb_bh_m" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="hb_bh_m")   
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
    
    hb_bh_m  <- lapply(1: length(clima_new), function(i){
      data <- clima_new[[i]]
      prec  <- data$PRED
      evap  <- data$ETAA
      inx  <- 1 - (sum(evap) / sum(prec) )
      ind  <- data.frame(Acronym = "hb_bh_m" , value = inx)
      return(ind)
    })
    
    hb_bh_m <- do.call(rbind,hb_bh_m)
    
  }else{hb_bh_m <- data.frame(Acronym= NA, value= NA)}
  
  ###################  5
  if("hb_s_m" %in%  ind){
    con <- dplyr::filter(crop_conf_inf, name =="hb_s_m")   
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
    
    hb_s_m  <- lapply(1: length(clima_new), function(i){
      data <- clima_new[[i]]
      prec  <- data$PRED
      evap  <- data$ETAA
      inx  <- 1 - (sum(evap) / sum(prec) )
      ind  <- data.frame(Acronym = "hb_s_m" , value = inx)
      return(ind)
    })
    
    hb_s_m <- do.call(rbind,hb_s_m)
    
  }else{hb_s_m <- data.frame(Acronym= NA, value= NA)}
  
  
  resultado <- list(hb_s_e,hb_t,hb_ei_b,hb_bh_m,hb_s_m)
  return(resultado)
  
  
}

hazards_water_safe <- purrr::possibly(hazards_water, NA)

#Gets indicators for all planting dates
hazard_water_all <- function(data_files_all){

  ind <- c("hb_s_e","hb_t","hb_ei_b","hb_bh_m","hb_s_m")

  indicadores <- mclapply(1:length(data_files_all), function(i) {
    data_files <- paste0(data_files_all[i])
    PlantGro_r <-  paste0(data_files, "PlantGro.OUT")
    Weather_r  <-  paste0(data_files, "Weather.OUT")
    ET_r         <-  paste0(data_files, "ET.OUT")
    crop_conf_r  <-  paste0(data_files, "crop_conf.csv")
    
      df <-  hazards_water(root = data_files,
                    PlantGro= PlantGro_r ,
                    Weather= Weather_r,
                    crop_conf_r=crop_conf_r,
                    ET_r=ET_r,
                    ind= ind)

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













