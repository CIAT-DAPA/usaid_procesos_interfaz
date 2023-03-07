getPhenologicalPhaseDates = function(folder,limits,initial_date,final_date,station,cultivar,soil){

  PlantGro_file <- paste0(folder,"PlantGro.OUT")
  df_plantgro <- read_PlantGro(PlantGro_file)
  df_plantgro$DATE <- initial_date + days(df_plantgro$DAP)

  
  phenologic_phases = data.frame()
  
  for(j in 1:nrow(limits)){
    measure = paste0(limits[j,c("name")])
    phase_treatments = data.frame(matrix(ncol = 6, nrow = 99))
    colnames(phase_treatments) <- c("measure", "start_phase_date", "end_phase_date", "diference", "initial_date", "end_date","w_station","cultivar","soil")
    for(i in 1:99){
      a<-subset(df_plantgro,df_plantgro$GSTD > as.numeric(limits[j,c("min")]) & df_plantgro$GSTD < as.numeric(limits[j,c("max")]))
      df_sub<- a[a$TRT==i,]
      
      min_date = format(strptime(as.character(min(df_sub$DATE)), "%Y-%m-%d"),"%Y-%m-%d" )
      max_date = format(strptime(as.character(max(df_sub$DATE)), "%Y-%m-%d"),"%Y-%m-%d" )

      phase_treatments[i,1] = measure
      phase_treatments[i,2] = min_date
      phase_treatments[i,3] = max_date
      phase_treatments[i,4] = difftime(as.Date(max_date),as.Date(min_date), units="days")
      phase_treatments[i,5] = initial_date
      phase_treatments[i,6] = final_date
      phase_treatments[i,7] = station
      phase_treatments[i,8] = cultivar
      phase_treatments[i,9] = soil
      

    }
    phenologic_phases = rbind(phenologic_phases, phase_treatments)
    
  }
  return(phenologic_phases)
}