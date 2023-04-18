g=gc();rm(list=ls())
require(pacman)
p_load(lubridate, tidyverse,readr,stringr,tidyr,data.table)

root <- "D:/OneDrive - CGIAR/Desktop/aclimate_ETH/DSSAT_outputs/"
PlantGro_r <-  paste0(root, "test/PlantGro.OUT")
Weather_r  <-  paste0(root, "test/Weather.OUT")
crop_conf_r  <-  paste0(root, "test/crop_conf.csv")
##################
##### ACRONYMS  ##
##################
# hs_ndr10_t
# hs_ndr40_t
# hs_ndr5_h_m
# hs_ndr40_bh_m
# hs_cdr5_h_f
# hs_cdr5_f_m
# hs_ndt2_b_f
# hs_ndt28_b_f

hazards <- function(root, PlantGro_r , Weather_r, crop_conf_r,acronym ){
 
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
  
  crop_conf_inf <- read.csv(crop_conf_r)
  
  if(acronym == "hs_ndr10_t"){
  
    con <- dplyr::filter(crop_conf_inf, name ==  acronym)   
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
      ind  <- data.frame(Acronym = acronym , value = inx)
      return(ind)
    })
    
    NDR10_T <- do.call(rbind,NDR10_T)
    return(NDR10_T)
  } else {}
  if(acronym == "hs_ndr40_t"){
    
    con <- dplyr::filter(crop_conf_inf, name ==  acronym)   
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
      ind  <- data.frame(Acronym = acronym , value = inx)
      return(ind)
    })
    
    NDR40_T <- do.call(rbind,NDR40_T)
    return(NDR40_T)
  } else {}
  if(acronym == "hs_ndr5_h_m"){
    
    con <- dplyr::filter(crop_conf_inf, name ==  acronym)   
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
      ind  <- data.frame(Acronym = acronym , value = inx)
      return(ind)
    })
    
    NDR5_H_M <- do.call(rbind,NDR5_H_M)
    return(NDR5_H_M)
  } else {}
  if(acronym == "hs_ndr40_bh_m"){
    
    con <- dplyr::filter(crop_conf_inf, name ==  acronym)   
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
      ind  <- data.frame(Acronym = acronym , value = inx)
      return(ind)
    })
    
    NDR40_BH_M <- do.call(rbind,NDR40_BH_M)
    return(NDR40_BH_M)
  } else {}
  if(acronym == "hs_cdr5_h_f"){
    
    con <- dplyr::filter(crop_conf_inf, name ==  acronym)   
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
      ind  <- data.frame(Acronym = acronym , value = inx)
      return(ind)
    })
    
    CDR5_H_F <- do.call(rbind,CDR5_H_F)
    CDR5_H_F$value[which(CDR5_H_F$value == "-Inf")] <- 0
    return(CDR5_H_F)
    } else {}
  if(acronym == "hs_cdr5_f_m"){
    
    con <- dplyr::filter(crop_conf_inf, name ==  acronym)   
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
      ind  <- data.frame(Acronym = acronym , value = inx)
      return(ind)
    })
    
    CDR5_F_M <- do.call(rbind,CDR5_F_M)
    CDR5_F_M$value[which(CDR5_F_M$value == "-Inf")] <- 0
    return(CDR5_F_M)
  } else {}
  if(acronym == "hs_ndt2_b_f"){
    
    con <- dplyr::filter(crop_conf_inf, name ==  acronym)   
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
      ind  <- data.frame(Acronym = acronym , value = inx)
      return(ind)
    })
    
    NDT2_B_F <- do.call(rbind,NDT2_B_F)
    return(NDT2_B_F)
  } else {}
  if(acronym == "hs_ndt28_b_f"){
    
    con <- dplyr::filter(crop_conf_inf, name ==  acronym)   
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
      ind  <- data.frame(Acronym = acronym , value = inx)
      return(ind)
    })
    
    NDT28_B_F <- do.call(rbind,NDT28_B_F)
    return(NDT28_B_F)
  } else {}
  
  
}
ind <- c("hs_ndr10_t","hs_ndr40_t","hs_ndr5_h_m","hs_ndr40_bh_m","hs_cdr5_h_f","hs_cdr5_f_m","hs_ndt2_b_f","hs_ndt28_b_f" )
indicadores <- lapply(1:length(ind) , function(i){
  
 df <-  hazards(root = root,
          PlantGro= PlantGro_r ,
          Weather= Weather_r,
          crop_conf_r=crop_conf_r,
          acronym= ind[i])
 return(df) 
  
})
resumen <- lapply(1:length(indicadores), function(i){
  
  df <- indicadores[[i]]
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
  result <- data.frame(measure = unique(df$Acronym),
                       avg = mean(df$value),
                       median = median(df$value),
                       min = min(df$value),
                       max = max(df$value),
                       quar_1 = quantile(df$value, 0.25),
                       quar_2 = quantile(df$value, 0.50),
                       quar_3 = quantile(df$value, 0.75),
                       conf_lower <- conf_lower(df$value),
                       conf_upper <- conf_upper(df$value),
                       sd = sd(df$value), 
                       perc_5 = quantile(df$value, 0.05),
                       perc_95 = quantile(df$value, 0.95), 
                       coef_var = CV(df$value))
                       
    colnames(result) <- c("measure","avg",
                          "median","min","max","quar_1","quar_2","quar_3","conf_lower","conf_upper",
                          "sd","perc_5","perc_95","coef_var")                   
    return(result)                  
  
  
})
final <- do.call(rbind,resumen)
row.names(final) <- 1:nrow(final)

final <- na.omit(final)

 






