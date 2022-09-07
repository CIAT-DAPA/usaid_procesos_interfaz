### Set Model - DSSAT API -  Aclimate - EDACaP--->> 
# Author: Rodriguez-Espinoza J. Mesa-Diez J.
# https://github.com/jrodriguez88/
# 2022



# Crea directorios de simulacion - 1 carpeta por cada configuracion 
make_dir_run <- function(dir_run_main, sim_number){
  
  
#  require(stringr)
  dir <- paste0(dir_run_main, sim_number, '/')
#  dir <- stringr::str_replace(dir, "ñ", "n")
  
  if (!dir.exists(dir)) { 
    
    dir.create(dir, showWarnings = F, recursive = TRUE, mode = "777")
    # system('chmod 777 *.*')
    # paste0(dir_base, region, '/', cultivar,  '/', select_day)
    
  }
  
  return(paste0(dir))
}


# Funcion copia inputs base en directorio de simulacion de cada setups
copy_inputs <- function(dir_inputs_setup, dir_inputs_soil, dir_inputs_cultivar, crop, dir_run){
  
  CR <- tibble(
    crop_name = c("rice", "maize", "barley", "sorghum", "wheat", "bean", "fababean", "teff"),
    CR = c("RI", "MZ", "BA", "SG", "WH", "BN", "FB",  "TF")) %>% filter(crop_name==crop)%>%
    pull(CR)
  
  
  gen_files <- list.files(dir_inputs_cultivar, full.names = T, pattern = "ECO|SPE|CUL") %>%
    str_subset(CR)
  
  
  setting_files <- list.files(dir_inputs_setup, full.names = T, pattern = "csv")
  
  soil_files <- list.files(dir_inputs_soil, full.names = T, pattern = ".SOL$")
    
  
#  dir_files <- list.files(dir_inputs, full.names = T)
  file.copy(c(gen_files, setting_files, soil_files), dir_run)
  
#  map2(.x = c("*.SPE", "*.ECO", "*.CUL"), 
#       .y = paste0("standard", c("*.SPE", "*.ECO", "*.CUL")), 
#       ~file.rename(
#         from = list.files(dir_run, pattern = .x, full.names = T), 
#         to = paste0(dir_run, .y)))
        
  
}


# Lee informacion de geolocalizacion
# puede agregarse en un solo archivo - setups
load_coordinates <- function(dir_inputs_run){
  
  frame_list <- function(data){
    
    setNames(split(data[,2], seq(nrow(data))), data[,1])
    
  }
  
  
  print(paste0("dir_inputs_run: ", dir_inputs_run))
#  require(readr)
  coordenadas <- read_csv(paste0(dir_inputs_run,'coordenadas.csv'), show_col_types = F) %>%
    as.data.frame() %>%
    frame_list()
  
}


# Function to write Crop names/model/extension into DSSAT format
crop_name_setup <- function(id_name, crop){
  
  base_tb <- tibble(
    crop_name = c("rice", "maize", "barley", "sorghum", "wheat", "bean", "fababean", "teff"),
    CR = c("RI", "MZ", "BA", "SG", "WH", "BN", "FB",  "TF"),
    model = c(paste0(c("RI", "MZ", "BA", "SG", "WH"), "CER"), rep("CRGRO", 2), "TFAPS"))
  
  cul <- base_tb %>% 
    dplyr::filter(crop_name %in% all_of(crop)) %>%
    mutate(crop_name =  toupper(crop_name),
           ext = paste0(id_name, ".", CR, "X"))
  
  return(cul)
  
  
}


# Crea archivo batch en el directorio especifico de la simulacion
write_batch_aclimate <- function(crop, xfile, treatments_number, filename){
  
  batchfile <- rbind(
    rbind(
      # Batchfile headers            
      paste0("$BATCH(", toupper(crop), ")"),            
      "!",            
      cbind(sprintf("%6s %89s %6s %6s %6s %6s", "@FILEX", "TRTNO", "RP", "SQ", "OP", 
                    "CO"))),            
    cbind(sprintf("%6s %83s %6i %6i %6i %6i",            
                  paste0(xfile),
                  1:treatments_number,  # Variable for treatment number            
                  1,  # Default value for RP element            
                  0,  # Default value for SQ element            
                  1,  # Default value for OP element            
                  0)))  # Default value for CO element 
  
  # Write the batch file to the selected folder  
  write(batchfile, file = filename, append = F)
  

  
}


# convert date to DSSAT format 
date_for_dssat <- function(date) {
  stopifnot(class(date)=="Date")
#  stopifnot(require(lubridate))
  
  yr <- str_sub(year(date), -2)
  doy <- yday(date)
  
  paste0(yr, sprintf("%.3d", doy))
  
}


# Write Fertilizer table
create_fert_dssat <- function(urea, dap, apps_dap = c(1, 40), urea_split = c(1/3, 2/3), dap_split = c(1, 0)){
  
  
  base_tb <- bind_rows(tibble(dap = apps_dap, fert = "dap", value = dap * dap_split) %>% 
                         dplyr::filter(value > 0),
                       tibble(dap = apps_dap, fert= "urea", value = urea * urea_split) %>% 
                         dplyr::filter(value > 0)) 
  
  

  fert_to_N <-function(fert, amount){
    
    if(fert == "dap"){
      N = amount*0.18
    } else if(fert == "urea"){
      N = amount*0.46
    } else {
      message("No detected Fertilizer")
      N = -99
    }
    
    return(N)
  }
  
  fert_to_P <-function(fert, amount){
    
    if(fert == "dap"){
      P = amount*0.46
    } else if(fert == "urea"){
      P = -99
    } else {
      message("No detected Fertilizer")
      P = -99
    }
    
    return(P)
  }
  
  # AP001    Broadcast, not incorporated            
  # AP002    Broadcast, incorporated
  
  # FE005    Urea
  # FE006    Diammonium phosphate (DAP)     
  # FE028    NPK - urea  
  
  base_tb <- base_tb %>% 
    mutate(N = map2_dbl(fert, value, fert_to_N),
           P = map2_dbl(fert, value, fert_to_P),
           FMCD = case_when(fert == "dap" ~ "FE006",
                            fert == "urea" ~"FE005",
                            TRUE  ~ NA_character_),
           FACD = case_when(dap < 5 ~ "AP002",
                            dap > 15 ~ "AP001",
                            TRUE ~ NA_character_),
           FDEP = case_when(dap < 5 ~ 5,
                            dap > 15 ~ 1,
                            TRUE ~ NA_real_))
  
  
  # De acuerdo a las recomendaciones: 2 aplicaciones,
  # 1 app: (nps) + 1(urea)/3  -- Incorporated
  # 2 app: 2(urea)/3          --  No incorporated
  
  #*FERTILIZERS (INORGANIC)
  #@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME
  # 1     1 FE006 AP002     5    10    20   -99   -99   -99   -99 fertApp
  # 1     1 FE005 AP002     5    30   -99   -99   -99   -99   -99 fertApp
  # 1    40 FE005 AP001     1    10    30    10   -99   -99   -99 fertApp
  
  FDATE <- base_tb$dap
  FMCD <-  base_tb$FMCD
  FACD <-  base_tb$FACD
  FDEP <-  base_tb$FDEP 
  FAMN <-  round(base_tb$N)
  FAMP <-  round(base_tb$P)
  FAMK <-  -99
  FAMC <-  -99 
  FAMO <-  -99
  FOCD <-  -99
  FERNAME <- "AgroClimR"
  #  
  #  fertilizer <- data.frame(F = 1, FDATE, FMCD, FACD, FDEP, FAMN, FAMP, FAMK,
  #                           FAMC, FAMO, FOCD, FERNAME)
  
  
  
  
  fertilizer <- tibble(F = 1, FDATE, FMCD, FACD, FDEP, FAMN, FAMP, FAMK,
                       FAMC, FAMO, FOCD, FERNAME)
  
  
  return(fertilizer)
  
}

#c(FALSE , "auto", "fertapp")

get_fertilizer <- function(crop, planting_details, dir_inputs_setup, lat, long){
  
  fert_option <- planting_details$FERT
    
  
  
  if(fert_option == "NO"){
    NULL
  } else if(fert_option == "YES"){
    
    urea <- dplyr::select(planting_details, contains("urea")) %>% 
      pull(1) %>% as.numeric()
    
    dap <- dplyr::select(planting_details, contains("dap")) %>% 
      pull(1) %>% as.numeric()
    
    return(create_fert_dssat(urea, dap))
      
    
    } else if(fert_option == "fertapp"){
    
    downloaded_tif <- get_geoserver_data(crop, "fertilizer", country = "et", format = "geotiff", outpath = dir_inputs_setup)
    
    fertApp_data <- extract_raster_geos(downloaded_tif, lat, long) %>%
      pivot_wider(names_from = file, values_from = V1) %>%
      set_names(c("nps", "urea"))
    
    return(convert_FertApp_dssat(fertApp_data$nps, fertApp_data$urea))
    
  } else {message("No detected data")}
  
  
}

