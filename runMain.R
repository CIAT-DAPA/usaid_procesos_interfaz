# Librerias y prerequisitos: 
#   . gunzip
#   . R librarys: fun, lubridate, reshape, string
library(funr)
library(lubridate)
library(reshape)
library(stringr)
library(trend)
library(data.table)
library(tidyverse)
library(magrittr)
library(lazyeval)
library(foreach)

# dirCurrent <- paste0(get_script_path(), "/", sep = "", collapse = NULL)
dirCurrent <- "C:/USAID/procesos_interfaz/"

## Variables paquete forecast
dirForecast <- paste0(dirCurrent, "prediccionClimatica/", sep = "", collapse = NULL)
dirInputs <- paste0(dirCurrent, "inputs/", sep = "", collapse = NULL)
dirOutputs <- paste0(dirCurrent, "outputs/", sep = "", collapse = NULL)
dirPrediccionInputs <- paste0(dirInputs, "prediccionClimatica/", sep = "", collapse = NULL)
dirPrediccionOutputs <- paste0(dirOutputs, "prediccionClimatica/", sep = "", collapse = NULL)
forecastAppDll <- paste0("dotnet ", dirCurrent, "forecast_app/CIAT.DAPA.USAID.Forecast.ForecastApp.dll -out ", sep = "", collapse = NULL)
dir_save <- paste0(dirPrediccionInputs, "descarga", sep = "", collapse = NULL)
dir_response <- paste0(dirPrediccionInputs, "estacionesMensuales", sep = "", collapse = NULL)
dir_stations <- paste0(dirPrediccionInputs, "dailyData", sep = "", collapse = NULL)
path_save <- paste0(dirPrediccionOutputs, "probForecast", sep = "", collapse = NULL)
path_output <- paste0(dirPrediccionOutputs, "resampling", sep = "", collapse = NULL)
path_output_sum <- paste0(path_output, "/summary", sep = "", collapse = NULL)
dir_dssat <- 'C:/DSSAT46/'  ## its necessary to have the parameters .CUL, .ECO, .SPE Updated for running (calibrated the crop (Maize))

## Variables modelo Maiz
dirModeloMaiz <- paste0(dirCurrent, "modeloMaiz/", sep = "", collapse = NULL)
dirModeloMaizInputs <- paste0(dirInputs, "cultivo/maiz/", sep = "", collapse = NULL)
dirModeloMaizOutputs <-paste0(dirOutputs, "cultivo/maiz/", sep = "", collapse = NULL)
dir_run <- paste0(dirModeloMaizOutputs, "run/", sep = "", collapse = NULL)
dir_soil <- paste0(dirModeloMaiz, "soils/CC.SOL", sep = "", collapse = NULL)

## Variables paquete arroz
dirModeloArroz <- paste0(dirCurrent, "modeloMaiz/", sep = "", collapse = NULL)
dirModeloArrozInputs <- paste0(dirInputs, "cultivo/arroz/", sep = "", collapse = NULL)
dirModeloArrozOutputs <-paste0(dirOutputs, "cultivo/arroz/", sep = "", collapse = NULL)

pathConstruct <- function(dirConstruct)
  {
  if (file.exists(file.path(dirConstruct))){
    unlink(file.path(dirConstruct), recursive = TRUE, force = TRUE)
    cat (paste0('\n... directorio "',dirConstruct,'" eliminado\n'))
    dir.create(file.path(dirConstruct))
    cat (paste0('... directorio "',dirConstruct,'" creado\n\n'))
    }
    else {
      dir.create(file.path(dirConstruct))
      cat (paste0('\n... directorio "',dirConstruct,'" creado\n\n'))
    }
  }

pathConstruct(dirInputs)
pathConstruct(dirOutputs)
pathConstruct(dirPrediccionInputs)
pathConstruct(dirPrediccionOutputs)
pathConstruct(dir_save)
pathConstruct(path_save)
pathConstruct(path_output)
pathConstruct(path_output_sum)
pathConstruct(dirModeloMaizInputs)
pathConstruct(dirModeloMaizOutputs)
pathConstruct(dir_run)
pathConstruct(dirModeloArrozInputs)
pathConstruct(dirModeloArrozOutputs)

CMDdirInputs <- paste0(gsub("/","\\\\",dirInputs), "\\\"")
try(system(paste0(forecastAppDll,"-s \"prec\" -p \"",CMDdirInputs," -start 1981 -end 2013"), intern = TRUE, ignore.stderr = TRUE))
try(system(paste0(forecastAppDll,"-wf -p \"",CMDdirInputs," -name \"daily\""), intern = TRUE, ignore.stderr = TRUE))
try(system(paste0(forecastAppDll,"-fs -p \"",CMDdirInputs), intern = TRUE, ignore.stderr = TRUE))
try(system(paste0(forecastAppDll,"-fs -p \"",CMDdirInputs), intern = TRUE, ignore.stderr = TRUE))
cat("\n")

runPrediccion <- source(paste(dirForecast,'01_prediccion.R', sep = "", collapse = NULL))

# runRemuestreo <- source(paste(dirForecast,'02_remuestreo.R', sep = "", collapse = NULL))

## Modelo maiz
data_m_all = list.files(dir_response,full.names = T)
data_d_all = list.files(dir_stations,full.names = T)

station_month_names = gsub('.csv','',list.files(dir_response))
station_names = gsub('.csv','',list.files(dir_stations))

for(x in 1:length(data_m_all)){
  for(y in 1:length(data_d_all)){
    region <- station_month_names[x]
    name_csv <- paste0(region, "_", station_names[y], ".csv", sep = "", collapse = NULL)
    cat(paste("\n\n Ejecutando modelo Maiz para region: \"", region, "\", estacion: \"", station_names[x], "\"\n", sep = ""))
    dir_climate <- paste0(path_output, "/", station_names[x], sep = "", collapse = NULL)
    # runModeloMaiz <- source(paste(dirModeloMaiz,'call_functions.R', sep = "", collapse = NULL))
  }
}
cat("\n\n... modelo Maiz finalizado\n")



