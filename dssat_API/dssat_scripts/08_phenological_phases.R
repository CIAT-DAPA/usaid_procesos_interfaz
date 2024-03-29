getPhenologicalPhaseDates = function(folder,limits,initial_date,station,cultivar,soil){
  
  PlantGro_file <- paste0(folder,"PlantGro.OUT")
  df_plantgro <- read_PlantGro(PlantGro_file)
  df_plantgro$DATE <- initial_date + days(df_plantgro$DAP)

  ciclo <- df_plantgro %>% nest(data  = -TRT) %>% pull(data) %>%
    map_dbl(nrow)
  columns_names = c("phase", "start_phase_date", "end_phase_date", "initial_date", "end_date","w_station","cultivar","soil")
  phenologic_phases = data.frame()
  
  for(j in 1:nrow(limits)){
    phase = paste0(limits[j,c("name")])
    phase_treatments = data.frame(matrix(ncol = length(columns_names), nrow = 99))
    colnames(phase_treatments) <- columns_names
    for(i in 1:99){
      a<-subset(df_plantgro,df_plantgro$GSTD > as.numeric(limits[j,c("min")]) & df_plantgro$GSTD < as.numeric(limits[j,c("max")]))
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