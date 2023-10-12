getPhenologicalPhaseDates = function(folder,limits,initial_date,station,cultivar,soil){
  
  PlantGro_file <- paste0(folder,"PlantGro.OUT")
  
  scenarios_plantgro = list()
  PlantGro_1 = read_lines(PlantGro_file)
  pos_head <- grep("@YEAR", PlantGro_1)
  for (i in 1:length(pos_head)){
    
    if(i < length(pos_head)){
      
      Ini = pos_head[i]      # The first row of the plant growth data
      End = pos_head[i+1]-10 # The last row of the plant growth data
      
      PlantGro = fread(PlantGro_file, header=T,check.names = T, skip=(pos_head[i]-1),fill=TRUE,sep = " ",dec=".", nrows = End - Ini)
      PlantGro = PlantGro[,c("X.YEAR","DOY","DAS","GSTD","DAP")]
      
      colnames(PlantGro) <- c("@YEAR","DOY","DAS","GSTD","DAP")
      scenarios_plantgro[[i]] = PlantGro
      
    } else{ ## the case to read the weather for the last simulation, i.e. simulation number 99 ##
      
      Ini = pos_head[i]      # First row of the climate data
      End = length(PlantGro_1) # Last row of the climate data
      
      PlantGro = fread (PlantGro_file, header=T,check.names = T, skip=(pos_head[i]-1),fill=TRUE,sep = " ",dec=".", nrows = End - Ini)
      PlantGro = PlantGro[,c("X.YEAR","DOY","DAS","GSTD","DAP")]
      colnames(PlantGro) <- c("@YEAR","DOY","DAS","GSTD","DAP")
      scenarios_plantgro[[i]] = PlantGro
    }
    scenarios_plantgro[[i]]$DATE <- initial_date + days(scenarios_plantgro[[i]]$DAP)
  }
  
  trat <- lapply(1: length(scenarios_plantgro),function(i){
    df <- scenarios_plantgro[[i]]
    df$TRT <- i
    return(df)
  })

  df_plantgro <- do.call(rbind, trat)

  ciclo <- df_plantgro %>% nest(data  = -TRT) %>% pull(data) %>%
    map_dbl(nrow)
  columns_names = c("phase", "start_phase_date", "end_phase_date", "initial_date", "end_date","w_station","cultivar","soil")
  phenologic_phases = data.frame()
  
  for(j in 1:nrow(limits)){
    phase = paste0(limits[j,c("name")])
    phase_treatments = data.frame(matrix(ncol = length(columns_names), nrow = 99))
    colnames(phase_treatments) <- columns_names
    for(i in 1:99){
      a<-subset(df_plantgro,df_plantgro$GSTD >= as.numeric(limits[j,c("min")]) & df_plantgro$GSTD < as.numeric(limits[j,c("max")]))
      df_sub<- a[a$TRT==i,]
      
      min_date = format(strptime(as.character(min(df_sub$DATE)), "%Y-%m-%d"),"%Y-%m-%d" )
      max_date = format(strptime(as.character(max(df_sub$DATE)), "%Y-%m-%d"),"%Y-%m-%d" )

      phase_treatments[i,1] = phase
      phase_treatments[i,2] = min_date
      phase_treatments[i,3] = max_date
      phase_treatments[i,4] = initial_date
      phase_treatments[i,5] = initial_date
      phase_treatments[i,6] = station
      phase_treatments[i,7] = cultivar
      phase_treatments[i,8] = soil
      

    }
    phase_treatments = phase_treatments %>% 
      dplyr::mutate(end_date =  initial_date + ciclo)
    phase_treatments$initial_date = format(as.Date(phase_treatments$initial_date,origin = lubridate::origin), "%Y-%m-%d")
    phase_treatments$end_date = format(as.Date(phase_treatments$end_date,origin = lubridate::origin), "%Y-%m-%d")
    phenologic_phases_tmp = data.frame(matrix(ncol = length(columns_names), nrow = 1))

    phenologic_phases_tmp[1,1] = phase_treatments$phase[[1]]
    phenologic_phases_tmp[1,2] = min(phase_treatments$start_phase_date)
    phenologic_phases_tmp[1,3] = max(phase_treatments$end_phase_date)
    phenologic_phases_tmp[1,4] = min(phase_treatments$initial_date)
    phenologic_phases_tmp[1,5] = max(phase_treatments$end_date)
    phenologic_phases_tmp[1,6] = station
    phenologic_phases_tmp[1,7] = cultivar
    phenologic_phases_tmp[1,8] = soil

    phenologic_phases = rbind(phenologic_phases, phenologic_phases_tmp)
    
  }
  colnames(phenologic_phases) <- columns_names
  return(phenologic_phases)
}

#Gets stress risk for all planting dates
getPhenologicalPhaseDatesAll <- function(dir_run,crop_conf,initial_date,id_station,id_cultivar,id_soil){
  phenological_list <- list()

  data = mclapply(1:length(dir_run), function(i) {
    data_files <- paste0(dir_run[i])
   
    phenological_list <- c(phenological_list, getPhenologicalPhaseDates(data_files,crop_conf,initial_date,id_station,id_cultivar,id_soil))

  }, mc.cores = no_cores, mc.preschedule = F)
  
  pheno_data_frame = NULL
  pheno_data_frame <- bind_rows(data)
  final_pheno <- pheno_data_frame %>% group_by(phase, w_station, cultivar, soil) %>% summarise(start_phase_date = min(start_phase_date),
  end_phase_date = max(end_phase_date),
  initial_date = min(initial_date),
  end_date = max(end_date))
  columns_names = c("phase", "start_phase_date", "end_phase_date", "initial_date", "end_date","w_station","cultivar","soil")
  final_pheno <- final_pheno[, columns_names]
  return(final_pheno)
}
