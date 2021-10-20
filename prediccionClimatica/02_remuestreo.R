# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Made by:     Alejandra Esquivel Arias. 
# Created in:  Date: 8-2019.
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Resampling Methodology 

# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=- First part 1 (without download satellite data).

# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=- Functions by sections.
# .------..------..------..------..------..------..------..------..------.
# |F.--. ||U.--. ||N.--. ||C.--. ||T.--. ||I.--. ||O.--. ||N.--. ||S.--. |
# | :(): || (\/) || :(): || :/\: || :/\: || (\/) || :/\: || :(): || :/\: |
# | ()() || :\/: || ()() || :\/: || (__) || :\/: || :\/: || ()() || :\/: |
# | '--'F|| '--'U|| '--'N|| '--'C|| '--'T|| '--'I|| '--'O|| '--'N|| '--'S|
# `------'`------'`------'`------'`------'`------'`------'`------'`------'
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=- 

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This function only changes the central month to season.
# ***** INPUT 
# *month_cent: central month of CPT's prediction.

# *****  OUTPUT
# name of season.
central_month <- function(month_cent){
  
  ini_m <- str_sub(month.abb, 1, 1)
  season <- paste0(ini_m, lead(ini_m),lead(ini_m, n = 2) )
  # season <- glue::glue('{ini_m}{lead(ini_m)}{lead(ini_m, n = 2)}')
  season <- case_when(season == 'NDNA' ~ 'NDJ', season == 'DNANA' ~ 'DJF', TRUE ~ as.character(season)) 
  season <- tibble(cent = c(2:12, 1), season)
  
  # season_cent <- season[month_cent]
  season_cent <- season %>% filter(cent == month_cent) %>% .$season
  
  return(season_cent)
}
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 1. Fix february: depends if leap year it's true or false.
# ***** These functions are used in the resampling function (F.to.resampling). ***** 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# (F.to.resampling). It's use when leap == FALSE (this function add a row in each february with 28 days). 
add_29_day <- function(to_change){
  
  Dato_C <- to_change %>%  
    nest(-year) %>% 
    mutate(data = purrr::map(.x = data, .f = function(.x){
      data_add <- bind_rows(.x, .x %>% sample_n(size = 1) %>% mutate(day = 29)) 
      return(data_add)})) %>% 
    unnest %>% 
    dplyr::select(day, month,  year, prec,  t_max,  t_min,  sol_rad)
  return(Dato_C)}

# (F.to.resampling). It's use when leap == TRUE (this function delete a row in each february with 29 days). 
less_29_day <- function(to_change){
  
  Dato_C <- to_change %>% 
    nest(-year) %>% 
    mutate(data = purrr::map(.x = data, .f = function(.x){
      data_less <- .x %>% slice(-n())
      return(data_less)})) %>% 
    unnest %>% 
    dplyr::select(day, month,  year, prec,  t_max,  t_min,  sol_rad) 
  return(Dato_C)}

# (F.to.resampling). This function organize the february data.
change_Leap <- function(leap_forecast, feb_data){
  
  data_to_change <- feb_data %>% 
    mutate(leap = leap_year(year)) %>% 
    nest(-leap)
  
  if (leap_forecast == TRUE) { # if year_forecast == TRUE (all days need to have 29 days).
    data_to_change <- data_to_change %>% 
      mutate(data = purrr::map_if(.x = data, .p = leap == FALSE , .f = add_29_day))
  } else {
    data_to_change <- data_to_change %>% 
      mutate(data = purrr::map_if(.x = data, .p = leap ==  TRUE, .f = less_29_day))
  }
  
  data_to_change <- data_to_change %>% 
    unnest %>% 
    dplyr::select(-leap) %>%  
    arrange(year) 
  
  return(data_to_change) }

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 2. Organize Probability, monthly data and daily data. 
# ***** These functions are used in the resampling function (F.to.resampling). ***** 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# (F.to.resampling). .1. Put in the probabily object start month of season and end month. 
season_to_months <-  function(season){
  
  all_seasons <-  paste0(str_sub(month.abb, 1, 1), lead(str_sub(month.abb, 1, 1)),
                         lead(lead(str_sub(month.abb, 1, 1), n = 1))) %>% 
    tibble(x = . ) %>% 
    mutate(x = ifelse(x == 'NDNA', 'NDJ', ifelse(x == 'DNANA', 'DJF', x))) %>% 
    mutate(start_month = 1:12, end_month = c(3:12, 1, 2))
  
  all_seasons <- all_seasons %>% 
    filter(str_detect(x, as.character(season)) ) %>% 
    dplyr::select(-x)
  
  return(all_seasons)}

# (F.to.resampling). .2.This function organize and classify monthly data by category for one season.
do_organize_data <- function(Season, xi, data, Intial_year, last_year){
  
  month_ini <- xi %>% 
    dplyr::select(start_month) %>% 
    unique() %>% 
    as.numeric()
  
  month_end <- xi %>% 
    dplyr::select(end_month) %>% 
    unique() %>% 
    as.numeric()
  
  if(Season == 'NDJ'){
    new_data <- data %>%
      filter(month %in% c(11,12,1)) %>% 
      mutate(year_M = ifelse(month == 1, year, year+1)) %>% 
      filter(year_M >= (Intial_year + 1), year_M < (last_year +1 ))%>%
      group_by(year_M) %>% 
      summarise(prec = sum(prec)) %>% 
      mutate(year = year_M - 1) %>% 
      dplyr::select(year, prec)
    
  } else if(Season == 'DJF'){
    new_data <- data %>%
      filter(month %in% c(12,1,2)) %>% 
      mutate(year_M = ifelse(month %in% 1:2, year, year+1)) %>% 
      filter(year_M >= (Intial_year + 1), year_M < (last_year +1 ))  %>%
      group_by(year_M) %>% 
      summarise(prec = sum(prec)) %>% 
      mutate(year = year_M - 1) %>% 
      dplyr::select(year, prec)
    
  } else{
    new_data <-  data %>%
      filter(between(month, month_ini, month_end)) %>%
      group_by(year) %>%
      summarise(prec = sum(prec))%>% 
      dplyr::select(year, prec)
  }
  
  # Quantiles of monthly averages are generated ... (be careful how they were generated). 
  quantile <- quantile(new_data$prec, probs = c(0.33, 0.66))
  
  # Classification of the monthly series ...
  new_data <-  new_data %>% 
    # mutate(condtion = case_when( prec < quantile[1] ~  'below', prec > quantile[2] ~ 'above' ,TRUE ~ 'normal')  ) %>%
    mutate(condtion = ifelse(prec < quantile[1], 'below', ifelse(prec > quantile[2], 'above', 'normal')) ) %>%
    nest(-condtion)
  
  return(new_data)}

# (F.to.resampling). .3. This function create 100 samples category (only name).
sample_category <- function(Prob){
  
  # Does the re-sampling of the categories...
  Type_cat <- tibble( id = 1:100) %>% 
    mutate(sample_cat = purrr::map(.x = id, .f = function(.x){
      sample_n(Prob,  size = 1, weight = Prob) }))
  
  return(Type_cat)}

# =-=-=-=-=
# (F.to.resampling). .4. This function dependent of the category, we do the year sample.
year_function <- function(base_cat, mothly_data){
  
  by_cat <- function(cat, mothly_data){
    # cat <- base_cat %>% filter(row_number() < 2 ) %>% unnest %>% select( Type)
    
    mothly_data <- mothly_data %>% 
      filter(condtion == cat$Type) %>% ####################################
      unnest %>% 
      sample_n(size = 1) %>% 
      dplyr::select(-prec)
    
    return(mothly_data)}
  
  year_sample <- base_cat %>% 
    mutate(sample =  purrr::map(.x = sample_cat, .f = by_cat, mothly_data = mothly_data)) %>% 
    # dplyr::select(-sample_cat) %>%
    unnest %>% 
    dplyr::select( -Type, -Prob)
  
  return(year_sample)}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
# 3. Daily data
# ***** These functions are used in the resampling function (F.to.resampling). ***** 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
# (F.to.resampling). This function extract daily data using sample year. 
day_sample <- function(Season, cat, data, Intial_year, last_year){
  
  month_ini <- cat %>% 
    dplyr::select(start_month) %>% 
    unique() %>% 
    as.numeric()
  
  month_end <- cat %>% 
    dplyr::select(end_month) %>% 
    unique() %>% 
    as.numeric()
  
  # Filter by season data serie.
  if(Season == 'NDJ'){
    Daily_filter <- data %>%
      filter(month %in% c(11,12,1)) %>% 
      mutate(year_M = ifelse(month == 1, year, year+1)) %>% 
      filter(year_M >= (Intial_year + 1), year_M < (last_year + 1))%>%
      mutate(year = year_M - 1) %>% 
      dplyr::select(-year_M)
    
  } else if(Season == 'DJF'){
    Daily_filter <- data %>%
      filter(month %in% c(12,1,2)) %>% 
      mutate(year_M = ifelse(month %in% 1:2, year, year+1)) %>% 
      filter(year_M >= (Intial_year + 1), year_M < (last_year +1 ))%>%
      mutate(year = year_M - 1) %>% 
      dplyr::select(-year_M)
    
  } else{
    Daily_filter <-  data %>%
      filter(between(month, month_ini, month_end)) 
  }
  
  Daily_data <- cat %>% 
    dplyr::select(-start_month, -end_month) %>% 
    mutate(daily_data = purrr::map(.x = year, .f = function(.x){
      Daily_filter %>% filter(year == .x)})) %>% 
    dplyr::select(-year)
}

# (F.to.resampling). This function return a tibble with daily sceneries min and max. 
Find_Summary <- function(daily_by_season){
  # Only the monthly grouping is done.
  monthly <- daily_by_season %>% 
    group_by(year) %>% 
    summarise(monthly = sum(prec)) 
  
  # the minimum and maximum precitation is extracted.
  Min_Max <-  monthly %>% 
    arrange(monthly) %>% 
    slice(c(1,n())) %>% 
    mutate(Type = c('min', 'max')) %>% 
    dplyr::select(-monthly)
  
  Lenght <-  daily_by_season %>% 
    filter(year %in% Min_Max$year) %>% 
    count(id) %>% 
    filter(row_number() == 1) %>% 
    dplyr::select(n) %>% 
    as.numeric
  
  Indicators <-  daily_by_season %>% 
    filter(year %in% Min_Max$year) %>% 
    dplyr::select(-id) %>% 
    unique %>%
    mutate(Type = rep(Min_Max$Type, each = Lenght )) %>% 
    nest(-Type)
  
  return(Indicators)}


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
# 4. Resampling...
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
# ***** INPUTs 
# * data: daily station data.
# * CPT_prob: CPT's probability after use central_month function and organize data.
# * year_forecast: year of resampling.

# *****  OUTPUT
# Object with resampling for a station, 
# which includes resampling, summaries and resampled years (escenario_a).

# ***** Note: This function don't save files.
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
# 4. Resampling
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
resampling <-  function(data, CPT_prob, year_forecast){
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # 1. Fix february: depends if leap year it's true or false.
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  season1 <- CPT_prob %>% dplyr::select(Season) %>% unique() %>% filter(row_number() == 1) %>% .$Season
  
  year_f_leap <- ifelse(season1 %in% c('ASO', 'SON', 'OND', 'NDJ', 'DJF'), year_forecast + 1, year_forecast)
  
  # Create a new data (with standard february).
  data <- data %>% 
    mutate(month_P = month) %>% 
    nest(-month_P) %>% 
    mutate(data = purrr::map_if(.x = data ,.p = month_P == 2 ,
                                .f = change_Leap, leap_forecast = leap_year(year_f_leap))) %>% 
    dplyr::select(data) %>% 
    unnest %>% 
    arrange(year)
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
  # =-=-=-=-=  Do years (start year and end year)...
  
  Intial_year <- data %>% dplyr::select(year) %>%  unique %>% slice(1) %>% as.numeric()
  
  last_year <- data %>% dplyr::select(year) %>% unique %>% slice(n()) %>% as.numeric()
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # 2. Organize Probability, monthly data and daily data. 
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  # Add start_month and end_month. 
  Times <- CPT_prob %>% 
    nest(-Season) %>% 
    mutate(Times = purrr::map(.x = Season, .f = season_to_months)) %>% 
    unnest(Times) %>% 
    unnest() %>% 
    nest(-Season)
  
  # In this part we create a new variable with monthly data classify.
  Times <- Times %>% 
    rename(xi = data) %>% 
    mutate(month_data = purrr::map2(.x = Season, .y = xi, 
                                    .f = do_organize_data, data = data, 
                                    Intial_year = Intial_year, last_year = last_year))
  
  # This function do the 100 category samples.   
  Times <- Times %>% mutate(cat = purrr::map(.x = xi,.f = sample_category))
  
  # =-=-=-=-=-=-=-=-
  # This function do the year sample depends of the sample category.
  Times <- Times %>% mutate(cat = purrr::map2(.x = cat, .y = month_data, .f = year_function))
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  Base_years <- Times %>% 
    mutate(order = paste0(letters[1:2], '.',Season)) %>% 
    dplyr::select(order, cat) %>% 
    unnest %>%
    dplyr::select(order, year) %>% 
    nest(-order) %>%
    spread(key = order, value = data) %>%
    unnest %>% 
    set_names(paste0(letters[1:2], '.',  Times$Season)) %>% 
    cbind(id = 1:100, .)
  
  # This function extract daily data using sample year.  
  daily_data <- Times %>% 
    mutate(daily_data = purrr::map2(.x = Season, .y = cat, .f = day_sample, 
                                    data = data, Intial_year = Intial_year, 
                                    last_year = last_year)) %>% 
    dplyr::select(Season, daily_data)
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
  data_to_esc <-  daily_data %>% 
    unnest %>% 
    dplyr::select(-condtion) %>% 
    nest(-id) %>%  #filter(row_number() == 19) %>% dplyr::select(data) %>% unnest() %>% unnest()
    # mutate(data = purrr::map(.x = data, .f = function(.x){ .x  %>% unnest() %>% unnest()})) %>%
    mutate(data = purrr::map(.x = data, .f = function(.x){ .x  %>% unnest()})) %>%
    unnest() 
  
  # add extra
  months <- data_to_esc %>% dplyr::select(month) %>% unique()
  cond_change <- isTRUE(sum(months > 7) > 0 & sum(months < 6) > 0) == TRUE
  
  Escenaries <-  data_to_esc %>%
    mutate(year = year_forecast) %>% 
    mutate(year = ifelse(cond_change == TRUE & month < 6, year + 1, year))  %>%
    # mutate(year = ifelse(Season %in% c('NDJ', 'DJF') & month == 1, year + 1, ifelse(Season == 'DJF' & month == 2, year + 1, year))) %>%
    dplyr::select(-Season) %>% 
    nest(-id) 
  
  # Here was Find_Summary function
  # In this part we create sceneries with min and max years 
  # (building from aggregate precipitation).
  Esc_Type <- data_to_esc %>%
    nest(-Season) %>%
    mutate(Summary = purrr::map(.x = data, .f = Find_Summary)) %>% 
    dplyr::select(-data) %>% 
    unnest() %>% 
    unnest %>% 
    arrange(Type) %>%
    mutate(year = year_forecast) %>% 
    mutate(year = ifelse(cond_change == TRUE & month < 6, year + 1, year)) %>% 
    dplyr::select(-Season) %>% 
    nest(-Type)
  
  # This object is the mix with 3 data set (sceneries, sample years and sceneries types).
  All_data <- bind_cols( Escenaries %>% mutate(Row = 'a') %>% nest(-Row),
                         Base_years %>% mutate(Row1 = 'a') %>% nest(-Row1) %>% rename(Base_years = data)) %>% 
    bind_cols(Esc_Type %>% mutate(Row2 = 'a') %>% nest(-Row2) %>% rename(Esc_Type = data) ) %>% 
    dplyr::select(-Row1, -Row2)
  # dplyr::select(-Row)
  
  return(All_data)}


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#  5. Function to save all files from resampling.  
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# ***** INPUT 
# * station: name of the station. -- >ID
# * Esc_all: resampling function output. 100 escenaries
# * path_out: path for save files. 

# *****  OUTPUT
# This function save resampling files, summary files and escenario_a (years resampled). 

# ***** Note: This function save files.
function_to_save <- function(station, Esc_all, path_out){
  
  # Daily sceneries (generated with resampling).
  
  Esc_C <- Esc_all %>%
    dplyr::select(data) %>% 
    unnest(cols = data) %>% 
    mutate(data = purrr::map(.x = data, .f = function(x){mutate(x, day = as.integer(day), month = as.integer(month), year = as.integer(year))}))%>% 
    mutate(file_name = paste0(path_out, '/',station, '/', station, '_escenario_', id, '.csv')) 
  
  # Creation of the data folder (where the results will be saved). 
  # ifelse(dir.exists(glue::glue('{path_out}{station}')) == FALSE, dir.create(glue::glue('{path_out}{station}')), 'ok')
  ifelse(dir.exists(paste0(path_out, '/',station)) == FALSE, 
         dir.create(paste0(path_out, '/',station)), 'ok')
  
  
  # Creation of the data folder (where the results will be saved). 
  # ifelse(dir.exists(glue::glue('{path_out}summary')) == FALSE, dir.create(glue::glue('{path_out}summary')), 'ok')
  ifelse(dir.exists(paste0(path_out, '/','summary')) == FALSE, 
         dir.create(paste0(path_out, '/','summary')), 'ok')
  
  # Creation of the data folder (where the results will be saved). 
  # ifelse(dir.exists(glue::glue('{path_out}validation')) == FALSE, dir.create(glue::glue('{path_out}validation')), 'ok')
  ifelse(dir.exists(paste0(path_out, '/', 'validation')) == FALSE, 
         dir.create(paste0(path_out, '/', 'validation')), 'ok')
  
  
  # Save daily sceneries.
  walk2(.x = Esc_C$data, .y = Esc_C$file_name, 
        .f = function(.x, .y){ readr::write_csv(x = .x, path = .y)})
  
  # Save scenarios type. MIn and maximun
  Type_Esc <- Esc_all %>% 
    dplyr::select(Esc_Type) %>% 
    unnest %>% 
    mutate(data = purrr::map(.x = data, .f = function(x){mutate(x, day = as.integer(day), month = as.integer(month), year = as.integer(year))}))%>%
    mutate(file_name = paste0(path_out, '/summary/', station, '_escenario_', Type, '.csv'))
  
  walk2(.x = Type_Esc$data, .y = Type_Esc$file_name, 
        .f = function(.x, .y){ write_csv(x = .x, path = .y)})
  
  # Save resampling years.
  Esc_all %>% 
    dplyr::select(Base_years) %>% 
    unnest %>% 
    mutate_all(.funs = as.integer) %>%
    write_csv(., path = paste0(path_out, '/validation/', station, '_Escenario_A.csv'))
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # summary variables files creation.
  Levels <- Esc_C %>% 
    dplyr::select(data) %>% 
    unnest %>% 
    dplyr::select(month) %>% 
    unique 
  
  summaries <- Esc_C %>% 
    dplyr::select(id, data) %>% 
    unnest %>% 
    mutate(month = factor(month,Levels$month))  %>% 
    group_by(id, month, year) %>% 
    summarise(prec = sum(prec), t_max = mean(t_max), t_min = mean(t_min), sol_rad = mean(sol_rad)) %>% 
    ungroup() %>% 
    dplyr::select(-id) %>%
    group_by(month) %>%
    group_by(year, month) %>%
    summarise(prec_avg = mean(prec), prec_max = max(prec), prec_min = min(prec),
              sol_rad_avg = mean(sol_rad), sol_rad_max = max(sol_rad), sol_rad_min = min(sol_rad),
              t_max_avg = mean(t_max), t_max_max = max(t_max), t_max_min = min(t_max),
              t_min_avg = mean(t_min), t_min_max = max(t_min), t_min_min = min(t_min)) %>%
    ungroup()
  
  summaries <- summaries %>% 
    gather(variable, values, -month, -year) %>% 
    nest(-variable) %>% 
    mutate(data = purrr::map2(.x = variable, .y = data, .f = function(.x, .y){
      
      if(str_detect(.x , 'sol_rad_') == TRUE){
        .y <- .y  %>% 
          set_names(c('year', 'month', str_replace(.x , 'sol_rad_', '')))
      } else{
        .y <- .y  %>% 
          set_names(c('year', 'month', str_extract( .x ,'_[a-z]+') %>% str_replace('_', '')))
      }
      return(.y)})) %>% 
    # mutate(file_name = glue::glue('{path_out}summary/{station}_{variable}.csv'))
    mutate(file_name = paste0(path_out, '/summary/', station, '_', variable, '.csv'))
  
  
  # Aqui se guardan los archivos...
  walk2(.x = summaries$data, .y = summaries$file_name,
        .f = function(.x, .y){write_csv(x = .x, path = .y)})
}



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# .------..------..------..------..------..------..------.
# |R.--. ||E.--. ||S.--. ||U.--. ||L.--. ||T.--. ||S.--. |
# | :(): || (\/) || :/\: || (\/) || :/\: || :/\: || :/\: |
# | ()() || :\/: || :\/: || :\/: || (__) || (__) || :\/: |
# | '--'R|| '--'E|| '--'S|| '--'U|| '--'L|| '--'T|| '--'S|
# `------'`------'`------'`------'`------'`------'`------'

# Aqui segun entiendo estan los archivos de entrada o estaciones. 
#dir_stations <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/Resampling/dir_test/dir_stations'
# en el path_save creo que es donde se guardan el archivo 
#path_save <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/Resampling/dir_test/path_save'
# Meh supongo que aqui guardo las salidas... pero pero peroo...
# Preguntarle a sotelo aqui como esta si con / al final o no?
#path_output <- 'D:/OneDrive - CGIAR/Desktop/USAID-Regional/USAID-REGIONAL/Resampling/dir_test/output/'

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 
# Voy a poner aqui los resultados por si acaso... 

Path_stations <- dir_stations # path for read station daily data and coord data. 
# Prob <- readr::read_csv(glue::glue('{path_save}/probabilities.csv')) # read CPT probabilities. 
Prob <- readr::read_csv(paste0(path_save, '/probabilities.csv'))

# =-=-=-=-=-=-=
# *1. Organization and transformation of CPT's probabilities.
# =-=-=-=-=-=-=
Prob <- Prob %>% 
  dplyr::select(-year) %>% 
  mutate(Season = purrr::map(.x = month, .f = central_month)) %>% 
  dplyr::select(-month) %>% 
  unnest(Season) %>% 
  gather(Type, Prob, -Season, -id) %>% 
  dplyr::select(id, Type, Season, Prob) %>% 
  nest(-id)

# we are reading station daily data, coordinates and join with CPT's probabilities. 
Initial_data <- tibble(id = list.files(Path_stations) %>% str_replace('.csv', ''),
                       path_stations = Path_stations %>% list.files(full.names = TRUE)) %>% 
  filter(!str_detect(id , '_coord')) %>% 
  mutate(coord_path = Path_stations %>% list.files(pattern = '_coord', full.names = TRUE), 
         stations = purrr::map(.x = path_stations, .f =  readr::read_csv), 
         coord = purrr::map(.x = coord_path, .f =  readr::read_csv))  %>%
  dplyr::select(-path_stations, -coord_path) %>%
  right_join(Prob, .) %>%
  rename(CPT_prob = data)

# year of resampling (in this case the current year).
year_forecast <- Sys.Date() %>% year()



# =-=-=-=-=-=-=-= we are doing resampling for all data sets 
Resam <- Initial_data %>% 
  mutate(Escenaries = purrr::map2(.x = stations, .y = CPT_prob, 
                                  .f = resampling, year_forecast = year_forecast))



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
# # Save all files from resampling. 
#purrr::map2(.x = Resam$id, .y = Resam$Escenaries, .f = function_to_save, path_out = path_output)


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# If you want put NASA-Power and Chirp data before daily-resampling sceneries. 
# Run from this point. 

# .------..------..------..------..------..------..------..------..------..------.
# |N.--. ||A.--. ||S.--. ||A.--. ||&.--. ||C.--. ||H.--. ||I.--. ||R.--. ||P.--. |
# | :(): || (\/) || :/\: || (\/) || :(): || :/\: || :/\: || (\/) || :(): || :/\: |
# | ()() || :\/: || :\/: || :\/: || ()() || :\/: || (__) || :\/: || ()() || (__) |
# | '--'N|| '--'A|| '--'S|| '--'A|| '--'&|| '--'C|| '--'H|| '--'I|| '--'R|| '--'P|
# `------'`------'`------'`------'`------'`------'`------'`------'`------'`------'

# This section run for all locations with the same dates.  
# =-=-=-=-=-=-=
# Functions. 
# =-=-=-=-=-=-=

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 0. Function to download Chirp data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This function 
# INPUT
# ini.date start date to download.
# end.date: end date to download.
# year_to: resampling year. 
# path_Chirp: path to save raster files. 
# no_cores = # cores to use in parallel. 


#######Disable from here . up2021


## OUTPUT: save chirp raster layers. 
#download_data_chirp <- function(ini.date, end.date, year_to, path_Chirp, no_cores){
  
#  fechas <- seq(as.Date(ini.date), as.Date(end.date), "days") %>% str_replace_all("-", ".")
#  #urls <- paste("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRP/daily/",year_to,"/chirp.",fechas,".tif",sep="")
#  urls <- paste("https://data.chc.ucsb.edu/products/CHIRP/daily/",year_to,"/chirp.",fechas,".tif",sep="")
#  file <- basename(urls)
#  path_Chirp_all <- paste0(path_Chirp,"/",file)
  
#  if(.Platform$OS.type == "unix") {cl <- makeCluster(no_cores, type = "FORK")}
#  cl <- makeCluster(no_cores)
#  clusterMap(cl, download.file, url = urls, destfile = path_Chirp_all, mode = "wb", 
#             .scheduling = 'dynamic')
  
#  stopCluster(cl)
#  return("CHIRPS data downloaded!") }
#
#
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
## 1. Function to extract NASA POWER daily data 
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
## This function 
## INPUT
## * data : daily - station data. 
## * special_data: this contains (lat: latitude of the station / site of interest, 
## lon: longitud of the station / site of interest, year_to: actual year, month_to: actual month).
#
## OUTPUT: NASA series. 
## It could be possible NASA API in some case sometimes don't work. 
#download_data_nasa <- function(data, special_data){
#  # data <- Cerete
#  # special_data <- tibble(lat, lon, year_to, month_to)
#  options(timeout = 120)
#  lat <- special_data$lat
#  lon <- special_data$lon
#  year_to <- special_data$year_to
#  month_to <- special_data$month_to
#  
#  
#  #json_file <- paste0("https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?&request=execute&identifier=SinglePoint&parameters=ALLSKY_SFC_SW_DWN,T2M_MAX,T2M_MIN&startDate=19830101&endDate=",format(Sys.Date(),"%Y%m%d"),"&userCommunity=AG&tempAverage=DAILY&outputList=ASCII&lat=",lat,"&lon=",lon)
#  json_file <- paste0("https://power.larc.nasa.gov/api/temporal/daily/point?start=20200101&end=",format(Sys.Date(),"%Y%m%d"),"&latitude=",lat,"&longitude=",lon, "&community=ag&parameters=ALLSKY_SFC_SW_DWN,T2M_MAX,T2M_MIN&header=true&time-standard=lst")
#  print(json_file)
#  # Esta mostrando un error que no conozco.
#  json_data <- jsonlite::fromJSON(json_file)
#  
#  
#  data_nasa <- json_data$properties$parameter %>% 
#        map(bind_cols) %>%
#        map2(.y = names(.),
#             ~pivot_longer(.x, cols = everything(), values_to = .y, names_to = "date") %>%
#                 mutate(date = lubridate::ymd(date))) %>%
#        reduce(left_join, by = "date") %>% setNames(c("date", "sol_rad", "t_max", "t_min")) %>%
#        mutate(year_n = year(date), month = month(date), day = day(date)) %>% dplyr::select(date, year_n, month, day, t_min, t_max, sol_rad)%>%na_if(-999)
#  
#  
#  
#  # Join observed and NASA data. 
#  all_data <- right_join( data %>% 
#                            filter(year %in% unique(data_nasa$year_n) ) %>% dplyr::select(-prec),
#                          data_nasa %>% 
#                            filter(year_n %in% unique(data$year)) %>% 
#                            purrr::set_names(c('dates', 'year', 'month', 'day', 't_min_N', 't_max_N', 'sol_rad_N')))
#  
#  
#  # Bias between observed data and NASA data. 
#  mean_less <- all_data %>% 
#    summarise(mean_max = mean(t_max-t_max_N, na.rm = TRUE), 
#              mean_min = mean(t_min - t_min_N, na.rm = TRUE),
#              mean_sol_rad = mean(sol_rad - sol_rad_N, na.rm = TRUE))
#  
#  # data full with mean NASA. 
#  nasa_data_dw <- data_nasa %>% 
#    filter(year_n == year_to, month == month_to) %>% 
#    mutate(t_max = t_max + mean_less$mean_max, 
#           t_min = t_min + mean_less$mean_min,
#           sol_rad = sol_rad + mean_less$mean_sol_rad) %>% 
#    mutate(t_max = ifelse(is.na(t_max), mean(t_max, na.rm = TRUE), t_max),
#           t_min = ifelse(is.na(t_min), mean(t_min, na.rm = TRUE), t_min),
#           sol_rad = ifelse(is.na(sol_rad), mean(sol_rad, na.rm = TRUE), sol_rad))
#  
#  return(nasa_data_dw)}
#
#
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
## 2. Function to extract Chirp data and Join with NASA POWER DATA. 
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
## ***** INPUT 
## * data:  path for save files.
## * special_data: data from Chirp and nasa for each station.
## *****  OUTPUT
##  This return resampling scenaries join with satellite data. 
##
## ***** Note: This function save files.
#Join_extract <- function(data, special_data, path_Chirp){
#  # Download Nasa data. 
#  # data<-data_to_replace$stations[[33]]
#  # special_data<-data_to_replace$data[[33]]
#  
#  nasa_data <- download_data_nasa(data, special_data)
#  
#  # Extract Chirp data 
#  # monthly <- ifelse(month_to < 10, glue::glue('0{month_to}'), month_to)
#  monthly <- ifelse(month_to < 10, paste0('0', month_to), month_to)
#  
#  chirp_data <- list.files(path_Chirp, full.names = TRUE, pattern = '.tif') %>%
#    stack %>% 
#    raster::extract(., data.frame(x= special_data$lon,y= special_data$lat)) %>% # arreglar aqui 
#    t() %>% 
#    as_tibble() %>% 
#    set_names('prec') %>% 
#    mutate(names = list.files(path_Chirp,  pattern = '.tif') %>% str_replace('.tif', ''), # arreglar aqui 
#           day = names %>% str_sub(. , nchar(names) - 1, nchar(names)) %>% as.numeric()) %>% 
#    dplyr::select(-names)
#  
#  # Join Chirp and NASA data. 
#  final_month <- right_join(chirp_data, nasa_data) %>% 
#    dplyr::select(day, month, year_n, prec, t_max, t_min, sol_rad) %>% 
#    rename(year = year_n)
#  
#  return(final_month)}
#
## data<-data_to_replace$stations[[1]]
## special_data<-data_to_replace$data[[1]]
#
## Join_extract(data = data_to_replace$stations[[33]], special_data = data_to_replace$data[[33]], path_Chirp = path_Chirp)
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
## 3. Function to add 
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
## ***** INPUT 
## * path:  path for save files.
## * Satellite: data from Chirp and nasa for each station.
## *****  OUTPUT
##  This return resampling scenaries join with satellite data. 
##
## ***** Note: This function save files.
#complete_data <-  function(path, Satellite){
  
#  complete_data <- list.files(path = path, pattern = 'escenario_', full.names = TRUE) %>% 
#    as_tibble() %>% 
#    mutate(data = purrr::map(.x = value, .f = read_csv), 
#           complete_data = purrr::map(.x = data, .f = function(.x){bind_rows(Satellite, .x) })) %>% 
#    dplyr::select(complete_data)
  
#  return(complete_data)}
#
#
###############################################################################
## ***** INPUT 
## * data: resampling data after used complete_data function.
## * path:  path for save files.
## *****  OUTPUT
## This function save replace resampling files, summary files and escenario_a (years resampled). 
#
## ***** Note: This function save files.
#
#function_replace <- function(data, path){
  
#  replace_data <- data %>% 
#    mutate(id = 1:100, 
#           path = list.files(path, pattern = '.csv', full.names = TRUE)) 
  
  # file.remove(list.files(path, pattern = '.csv', full.names = TRUE))
  
  # Save new daily sceneries.
#  walk2(.x = replace_data$complete_data, .y = replace_data$path,
#        .f = function(.x, .y){ write_csv(x = .x, path = .y)})
#}
#
## .------..------..------..------..------..------..------.
## |R.--. ||E.--. ||S.--. ||U.--. ||L.--. ||T.--. ||S.--. |
## | :(): || (\/) || :/\: || (\/) || :/\: || :/\: || :/\: |
## | ()() || :\/: || :\/: || :\/: || (__) || (__) || :\/: |
## | '--'R|| '--'E|| '--'S|| '--'U|| '--'L|| '--'T|| '--'S|
## `------'`------'`------'`------'`------'`------'`------'
#
##---------------------------------------------------------------------------------#
##---------------- From this point we download data. ------------------------------#
##---------------------------------------------------------------------------------#
#
## For now don't modify.
#numberOfDays <- function(date) {
#  m <- format(date, format="%m")
  
#  while (format(date, format="%m") == m) {
#    date <- date + 1
#  }
  
#  return(as.integer(format(date - 1, format="%d")))
#}
#
## I guess this is for fix december...
#if (substring(Sys.Date(),6,7) == "01"){
#  substr_month <- "12"
#  substr_year <- year(Sys.Date()) - 1
#} else {
#  substr_month <- str_pad(as.numeric(substring(Sys.Date(),6,7))-1,2,pad = "0")
#  substr_year <- year(Sys.Date())
#}
#
#
#
#
## -----------
#ini.date <- paste0(substr_year,"-",substr_month,"-01") %>%  as.Date()
## -----------
#end.date <- paste0(substr_year,"-",substr_month,"-",numberOfDays(ini.date)) %>% as.Date()
## -----------
#path_Chirp <- path_output
## -----------
#
## year_to...
#if (substring(Sys.Date(),6,7) == "01"){
#  year_to = as.numeric(format(Sys.Date(),"%Y"))-1
#  month_to = 12
#} else {
#  year_to = format(Sys.Date(),"%Y")
#  month_to = as.numeric(format(Sys.Date(),"%m"))-1
#}
#
## =-=-=-= --------------------------------------------
## =-=-=-=-= Change this parameter for run in parallel. 
#no_cores <- as.numeric(Sys.getenv("N_CORES"))
#
## =-=-= Here we download Chirps data (This download is only done once). 
#download_data_chirp(ini.date, end.date, year_to, path_Chirp, no_cores)
#
#
## =-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=...
#### Here the whole file is made. 
#data_to_replace <- Initial_data %>% 
#  dplyr::select(id, coord) %>% 
#  unnest(coord) %>% 
#  # mutate(path = glue::glue('{path_output}{id}'), year_to, month_to) %>%
#  mutate(path = paste0(path_output, '/',id), year_to, month_to) %>%
#  nest(special_data = c(lat, lon, year_to, month_to)) %>% 
#  right_join(., Initial_data) %>% 
#  dplyr::select(-CPT_prob) 
#
#
#
#
#data_to_replace1 %>% mutate(satellite_data = purrr::map2(.x = stations, .y = data, .f = Join_extract, path_output)) 
#
## data_to_replace %>% dplyr::select(id, satellite_data) %>% filter(id == '5a200e4575c44204941f06db') %>% dplyr::select(-id)%>% unnest()
#
#data_to_replace <- data_to_replace %>%
#  dplyr::select(-stations, -data) %>% 
#  mutate(path = paste0(path_output, '/',id), 
#         complete_data = purrr::map2(.x = path, .y = satellite_data, .f = complete_data))
#
#
#map2(.x = data_to_replace$complete_data, .y = data_to_replace$path, .f = function_replace)
#
#
## This remove chirp files.
#file.remove(list.files(path_output,pattern = ".tif",full.names=T))
#