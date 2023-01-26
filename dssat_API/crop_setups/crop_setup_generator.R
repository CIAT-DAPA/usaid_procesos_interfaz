### Crop setup generator - DSSAT Aclimate - EDACaP
# Author: Rodriguez-Espinoza J. 
# https://github.com/jrodriguez88/
# 2022


library(tidyverse)
library(data.table)
library(jsonlite)
library(raster)
library(furrr)

## INPUTS

## Working directory -  --> Source file location 

path_script <- getwd()
setwd(path_script)

crop <- "maize"
base_data <- read_csv("base_data/setups/base_weather_soil.csv")
DEM_ETH <- getData('alt', country = 'ETH')
soil_file <- "base_data/soil/SOIL.SOL"
id_cultivar <- read_csv("base_data/setups/id_cutivars.csv") %>% dplyr::filter(crop == all_of(crop)) 
freq_sim <- 3
out_path <- "output/"



create_setups_aclimate <- function(crop, base_data, DEM_ETH, soil_file, id_cultivar, freq_sim = 3, out_path = "output/", ncores = 4){



#base_data %>% select(soil_id, soil_name) %>% distinct()

#load("set_final_edacap.RData")
#set_final_edacap_wheat$pd_file
#names(set_final_edacap_wheat)


### table of soils
raw_file <- soil_file %>% read_lines()
target_line <- raw_file %>% str_detect(pattern = "@SITE") %>% which()

id_soil <- raw_file[target_line -1 ] %>% map(~str_split(.x, pattern = " ")) %>% 
  map(~.x %>% map_chr(1)) %>% map_chr(1) %>%  enframe(name = NULL, value = "id_soil")






#id_info <- target_line %>% map(~fread(soil_file, skip = .x, nrows = 1)) #%>% 
#                                 dplyr::select(V1:V5)  %>% 
#                                 mutate(across(.fns = as.character))) %>% 
#  bind_rows() %>% set_names(c("site", "country", "lat", "lon", "scs"))

## set to extract soil data from SOIL.SOL
nlines <- c((target_line-1) %>% diff, (length(raw_file)+1) - (tail(target_line, 1)-1)) 
#leng_tb <- id_info %>% map_dbl(length)


plan(multisession, workers = ncores)


list_soils <-  future_map2((target_line-2), (nlines-1), ~read_lines(file = soil_file, skip = .x, n_max = .y))


# List of EDACaP Soils



soil_final <- list_soils %>% set_names(pull(id_soil)) %>% 
  enframe(name = "id_soil", value = "data_soil")


#Cultivar

cultivar_list <- id_cultivar %>% 
    mutate(cultivar = map2(var_cul, cul_name, ~c(.x, .y ))) %>%
  dplyr::select(-c(var_cul, cul_name))



base_data <- base_data %>% 
  dplyr::select(id = ws_id, 
                latitude = ws_lat, 
                longitude = ws_lon, 
                id_soil = Soil_name, 
                soil_name, 
                id_soil_final = soil_id) %>% 
  mutate(crop = crop) 

### Suggested order ID's names 
#// 0 = Weather station
#// 1 = Cultivar
#// 2 = Soil
#// 3 = Days


## Calculate elevation - 

alt <- map2(base_data$latitude, base_data$longitude, ~raster::extract(DEM_ETH, data.frame(x= .y, y= .x))) 


base2 <- base_data %>% left_join(soil_final) %>%
  mutate(elevation = unlist(alt)) %>% 
  left_join(cultivar_list) 



#######

###Function to create coordinate data frame - 

cordinate_tb <- function(lat, lon, elev, id_soil, cultivar){
  
  var <- cultivar[1]
  var_name <- cultivar[2]
  
  tibble(name = c("lat", "long", "elev", "id_soil", "var_cul", "cul_name"),
         value = c(lat, lon, elev, id_soil, var, var_name))
  
}

### Final dataframe setups
set_final <- base2  %>% mutate(id_set = paste(id, id_cultivar_final, id_soil_final, freq_sim , sep = "_"),
         path_folder = paste0(out_path, id_set)) %>%
  mutate(pd_file = pmap(list(latitude, longitude, elevation, id_soil, cultivar), .f = cordinate_tb))

CR <- tibble(
  crop_name = c("rice", "maize", "barley", "sorghum", "wheat", "bean", "fababean", "teff"),
  CR = c("RI", "MZ", "BA", "SG", "WH", "BN", "FB",  "TF")) %>% filter(crop_name==crop) %>%
  pull(CR)


tt <- set_final

gen_files <- list.files("base_data/crop/", full.names = T, pattern = "ECO|SPE|CUL") %>%
  str_subset(CR)

setting_files <- list.files("base_data/setups/", full.names = T, pattern = "planting_details.csv")

# soil_files <- list.files(dir_inputs_soil, full.names = T, pattern = ".SOL$")

dir.create(out_path)  
map(tt$path_folder, dir.create)    

future_map(tt$path_folder, ~file.copy(c(gen_files, setting_files), .x))

future_map2(tt$data_soil, tt$path_folder, ~write_lines(x = .x, file = paste0(.y, "/SOIL.SOL")))

future_map2(tt$pd_file, tt$path_folder, ~write_csv(x = .x, file = paste0(.y, "/coordenadas.csv")))




folder_created <- list.files(out_path)


repeated_data <- set_final %>% nest(-id_set) %>%
  mutate(n_rep = map_dbl(data, nrow)) %>% 
  filter(n_rep > 1) %>% 
  unnest(data)

message(paste0(length(folder_created), " Settings-Folders created in ", out_path))

write_csv(repeated_data, "repeated_data.csv")
closeAllConnections()

}

#tictoc::tic()
create_setups_aclimate(crop, base_data, DEM_ETH, soil_file, id_cultivar)
#tictoc::toc()
#test 1 .. 1 core sequential
#979.01 sec elapsed

#tictoc::toc()
#362.04 sec elapsed

#unlink(out_path, recursive = T)






