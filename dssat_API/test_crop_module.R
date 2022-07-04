# Test module

tictoc::tic()

### Wheat
crop <- "wheat"
cultivar <- c("AW0071","Yecora_Rojo")
source("00_run_dssat_aclimate.R")

### Barley
crop <- "barley"
cultivar <- c( "IB0030", "Maris Badger")
source("00_run_dssat_aclimate.R")

### Maize
crop <- "maize"
cultivar <- c( "990002", "MEDIUM SEASON")
source("00_run_dssat_aclimate.R")

### Sorghum
crop <- "sorghum"
cultivar <- c( "990004", "W.AFRICAN")
source("00_run_dssat_aclimate.R")

### Rice
crop <- "rice"
cultivar <- c( "IB0115",  "IR 64*")
source("00_run_dssat_aclimate.R")


### Beans
crop <- "bean"
cultivar <- c( "990005", "Meso Amer. Hab.1")
source("00_run_dssat_aclimate.R")

### Teff
crop <- "teff"
cultivar <- c("IB0304", "Wajera (local)")
source("00_run_dssat_aclimate.R")

tictoc::toc()


### Test analysis

library(ggplot2)
library(plotly)


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


sim_data <- list.files("outputs/", full.names = T) %>% map(read_csv) %>%
  bind_rows() %>% rename(crop = weather_station)

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
      yaxis = list(exponentformat = "SI",type="log",title = "TEST"),
      xaxis = list(title = "Date"),
      boxmode = "group")





























