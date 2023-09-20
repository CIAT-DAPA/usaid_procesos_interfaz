addObsDataToPlanting <- function(path_resampling_observed,weather_station_id,climate_scenaries,path_planting_window, daily_data_path){
  
  path_resampling_observed = paste0(path_resampling_observed,"resampling_observed/")
  regex_scenario = paste(paste("^",weather_station_id,sep=""),"_escenario\\.csv$",sep="")
  
  ## Read Files
  
  planting_window = read.table(path_planting_window, head=T, sep=",", stringsAsFactors = FALSE)
  observed_scenarios_file = list.files(path = path_resampling_observed, pattern=regex_scenario, all.files=TRUE, full.names=TRUE)
  observed_scenarios = read.table(observed_scenarios_file, head=T, sep=",")
  
  daily_data = paste0(daily_data_path,"/",weather_station_id,".csv")
  daily_data = read.table(daily_data, head=T, sep=",")


  observed_scenarios = verify_months(observed_scenarios, daily_data)

  ##Prepare date formats
  
  start_date_obs = paste(paste(paste(observed_scenarios[1,]$year,"-",sep=""),paste(observed_scenarios[1,]$month,"-",sep=""),sep=""),observed_scenarios[1,]$day,sep="")
  start_date_obs = format(strptime(as.character(start_date_obs), "%Y-%m-%d"),"%Y-%m-%d" )
  end_date_obs = paste(paste(paste(observed_scenarios[nrow(observed_scenarios),]$year,"-",sep=""),paste(observed_scenarios[nrow(observed_scenarios),]$month,"-",sep=""),sep=""),observed_scenarios[nrow(observed_scenarios),]$day,sep="")
  end_date_obs = format(strptime(as.character(end_date_obs), "%Y-%m-%d"),"%Y-%m-%d" )
  
  ## Get planting window years
  start_month_planting_window = as.numeric(planting_window[2,2])
  start_year_planting_window = year(Sys.time())
  end_month_planting_window  = as.numeric(planting_window[3,2])
  end_year_planting_window = year(Sys.time())
  if(end_month_planting_window < start_month_planting_window) {
    end_year_planting_window = end_year_planting_window + 1
  }
  
  date_format_start_planting = paste0(start_month_planting_window,"/1/",start_year_planting_window)
  date_format_end_planting = paste0(end_month_planting_window,"/1/",end_year_planting_window)
  
  start_date_plant = format(strptime(as.character(date_format_start_planting), "%m/%d/%Y"),"%Y-%m-%d" )
  end_date_plant = format(strptime(as.character(date_format_end_planting), "%m/%d/%Y"),"%Y-%m-%d" )
  
  ##Difference of months between the end of observed and beginning of planting window
  months_to_get = 0
  if(as.Date(end_date_plant) - as.Date(end_date_obs ) > 0 && as.Date(end_date_obs) - as.Date(start_date_plant ) > 0){
    
    if(observed_scenarios[nrow(observed_scenarios),]$year - as.numeric(start_year_planting_window) > 0){
      months_to_get = (observed_scenarios[nrow(observed_scenarios),]$month + 12) - as.numeric(start_month_planting_window)
    }else{
      months_to_get = observed_scenarios[nrow(observed_scenarios),]$month - as.numeric(start_month_planting_window)
    }
    
  }
  
  
  
  
  ##Create data frame with the data of the months observed within the planting window
  
  new_scenario = data.frame()
  for (index in 0:months_to_get) {
    year = as.numeric(start_year_planting_window)
    month = as.numeric(start_month_planting_window)
    if(month + index > 12){
      year = year + 1
      month = month + index - 12
    }else{
      month = month + index
    }
    new_scenario = rbind(new_scenario, observed_scenarios[observed_scenarios$month == month & observed_scenarios$year == year,])
  }
  
  
  ##Join observed data with predicted data
  diff_planting = 0
  if(as.numeric(end_year_planting_window) - as.numeric(start_year_planting_window) > 0){
    diff_planting = (as.numeric(end_month_planting_window) + 12) - as.numeric(start_month_planting_window)
  }else{
    diff_planting = as.numeric(end_month_planting_window) - as.numeric(start_month_planting_window)
  }
  

  all_new_scenarios = vector(mode = "list", length = length(climate_scenaries))
  
  for (line in 1:length(climate_scenaries)) {
    
    paste_new_scenarios = data.frame()
    
    new_data = climate_scenaries[line]
    
    new_data = new_data[[1]]
    paste_new_scenarios = rbind(new_scenario)
    
    for (count in 1:(diff_planting - months_to_get)) {
      year = observed_scenarios[nrow(observed_scenarios),]$year
      month = observed_scenarios[nrow(observed_scenarios),]$month
      if(month + count > 12){
        year = year + 1
        month = month + count - 12
      }else{
        month = month + count
      }
      paste_new_scenarios = rbind(paste_new_scenarios, new_data[new_data$month == month & new_data$year == year,][,1:7])
      names = 1:nrow(paste_new_scenarios)
      row.names(paste_new_scenarios) = names
    }
    all_new_scenarios[[line]] = paste_new_scenarios
  }
  
  climate_scenaries = all_new_scenarios
  
}



verify_months <- function(dataframe, daily_data) {

  result = dataframe
  days_per_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  year_bi = dataframe[dataframe$month == 2,][1,]$year
  if (!is.na(year_bi) && ((year_bi %% 4 == 0 && year_bi %% 100 != 0) || year_bi %% 400 == 0)) {
    # Si es bisiesto, febrero tiene 29 días
      days_per_month[2] <- 29
  }
  months = unique(dataframe$month)

  if (12 %in% months) {
    # Encontrar la posición de 12 en el arreglo
    pos_12 <- which(months == 12)
    
    # Sumar 12 a los meses después de 12
    months[(pos_12 + 1):length(months)] <- months[(pos_12 + 1):length(months)] + 12
  }

  for(month in months[1]:months[length(months)]){
    if(month > 12){
      month = month - 12
    }
    data_frame_filtered = dataframe[dataframe$month == month,]
    year = data_frame_filtered[1,]$year
    

    if(nrow(data_frame_filtered) != days_per_month[month] || any(is.na(data_frame_filtered))){
      daily_data_filtered = daily_data[daily_data$month == month,]

      years = unique(daily_data_filtered$year)

      new_data_frame = data.frame(matrix(nrow = days_per_month[month], ncol = ncol(data_frame_filtered)))
      names(new_data_frame) = c("day", "month",  "year", "t_max", "t_min",  "prec", "sol_rad")

      for(day in 1:days_per_month[month]){
        day_row = daily_data_filtered[daily_data_filtered$day == day,]

        mean = day_row %>% 
        group_by(day, month) %>% 
        summarise(t_max  = mean(t_max),
            t_min = mean(t_min),
            prec = median(prec),
            sol_rad = mean(sol_rad)) %>% 
        ungroup() %>% mutate(year = year) %>%
        dplyr::select(day, month,  year, t_max, t_min,  prec, sol_rad)

        new_data_frame[day,] = mean
      }
      na_rows <- apply(is.na(data_frame_filtered), 1, any)
      data_frame_filtered[na_rows, ] <- new_data_frame[na_rows, ]

      df_final <- bind_rows(data_frame_filtered, new_data_frame) %>%
        distinct(day, month, year, .keep_all = TRUE)

      new_rows <- df_final[!(paste(df_final$year, df_final$month, df_final$day) %in% paste(result$year, result$month, result$day)), ]
      result <- rbind(result, new_rows)
      
      result[result$month == month, ] <- df_final
      result <- result %>%
        arrange(year, month, day)
      
    }
    

    
  }
  return(result)
}