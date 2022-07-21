### Test Crop module - DSSAT - API - Aclimate,  EDACaP
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022

# DSSAT Version 4.8
# Crops tested : c("rice", "maize", "barley", "sorghum", "wheat", "bean", "fababean", "teff")

##Settings:
# Number of Climate Scenaries: 99 ( max allow by DSSAT x file)
# 11 Planting dates (30 days - 1 sim/3 days) ..first planting date = 15 days after first day forecast (climate scenaries)
# 1 Soil - DSSAT ID
# irri <- F  --  Rainfed mode
# fert_in <- NULL - No fertilization

library(profvis)
library(bench)
library(tictoc)



tictoc::tic()


### Wheat
id <- "5a7e2e6a57d7f316c8bc514a_59b024a0b74a4a10f487eaa6_5b3edfe7b16a0d2edc1107e4_1"
crop <- "wheat"
cultivar <- c("AW0071","Yecora_Rojo")
soil <- "IB00000001"
#irri <- T
#fert <-  "fertapp"#  TRUE = auto, FALSE = not fertilized  
source("00_run_dssat_aclimate.R")


### Barley
id <- "5a7e2e6a57d7f316c8bc514a_59b024a0b74a4a10f487eaa6_5b3edfe7b16a0d2edc1107e4_2"
crop <- "barley"
cultivar <- c( "IB0030", "Maris Badger")
soil <- "IB00000001"
irri <- T
fert <-  "auto"
source("00_run_dssat_aclimate.R")

### Maize
id <- "5a7e2e6a57d7f316c8bc514a_59b024a0b74a4a10f487eaa6_5b3edfe7b16a0d2edc1107e4_3"
crop <- "maize"
cultivar <- c( "990002", "MEDIUM SEASON")
soil <- "IB00000001"
source("00_run_dssat_aclimate.R")

### Sorghum
id <- "5a7e2e6a57d7f316c8bc514a_59b024a0b74a4a10f487eaa6_5b3edfe7b16a0d2edc1107e4_4"
crop <- "sorghum"
cultivar <- c( "990004", "W.AFRICAN")
soil <- "IB00000001"
source("00_run_dssat_aclimate.R")

### Rice
id <- "5a7e2e6a57d7f316c8bc514a_59b024a0b74a4a10f487eaa6_5b3edfe7b16a0d2edc1107e4_5"
crop <- "rice"
cultivar <- c( "IB0115",  "IR 64*")
soil <- "IB00000001"
source("00_run_dssat_aclimate.R")

### Beans
id <- "5a7e2e6a57d7f316c8bc514a_59b024a0b74a4a10f487eaa6_5b3edfe7b16a0d2edc1107e4_6"
crop <- "bean"
cultivar <- c( "990005", "Meso Amer. Hab.1")
soil <- "IB00000001"
source("00_run_dssat_aclimate.R")

### Faba Beans
id <- "5a7e2e6a57d7f316c8bc514a_59b024a0b74a4a10f487eaa6_5b3edfe7b16a0d2edc1107e4_7"
crop <- "fababean"
cultivar <- c( "CORD01", "ALAME LD170 1.2g")
soil <- "IB00000001"
source("00_run_dssat_aclimate.R")

### Teff
id <- "5a7e2e6a57d7f316c8bc514a_59b024a0b74a4a10f487eaa6_5b3edfe7b16a0d2edc1107e4_8"
crop <- "teff"
cultivar <- c("IB0304", "Wajera (local)")
soil <- "IB00000001"
source("00_run_dssat_aclimate.R")

tictoc::toc()

#> tictoc::toc()
#556.36 sec elapsed


run_crop_dssat <- function(path, crop, cultivar, soil){
  
  setwd(path)
  
  crop <- crop
  cultivar <- cultivar
  soil <- soil
  
  source("00_run_dssat_aclimate.R")
  
  
}

run_crop_dssat(path = "dssat_API/", crop = "wheat", cultivar = c("AW0071","Yecora_Rojo"),  soil = "IB00000001")

### Workflow Profile - 
#profvis({
#  crop <- "wheat"
#  cultivar <- c("AW0071","Yecora_Rojo")
#  soil <- "IB00000001"
#  source("00_run_dssat_aclimate.R")}
#)


### Test analysis

library(ggplot2)
library(plotly)

sim_data <- list.files("outputs/", full.names = T) %>% map(read_csv) %>%
  bind_rows() %>% rename(crop = weather_station)


sim_data %>% 
  ggplot() +
  geom_line(aes(x = start, avg )) +
  geom_line(aes(start, conf_lower), color = "red") +
  geom_line(aes(start, conf_upper), color = "lightgreen")+
  facet_grid(measure ~crop, scales = "free") +
  theme_bw() +
  labs(
    x = "Date",
    y = NULL, 
  )




plot_ly(
    data = sim_data %>% filter(measure =="yield_0"),
    x = ~factor(start),
    color = ~ crop,
    type="box",
    lowerfence = ~ min,
    q1 = ~ quar_1,
    median = ~ median,
    q3 = ~ quar_3,
    upperfence = ~ max) %>%
    layout(
      yaxis = list(exponentformat = "SI",type="log",title = "Yield - kg/ha"),
      xaxis = list(title = "Date"),
      boxmode = "group")





























