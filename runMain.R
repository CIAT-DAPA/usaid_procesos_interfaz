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

dirCurrent <- paste(get_script_path(), "/", sep = "", collapse = NULL)
#dirCurrent <- "C:/USAID/packages/"

## Variables paquete forecast
dirForecast <- paste(dirCurrent, "prediccionClimatica/", sep = "", collapse = NULL)
dirForecastInputs <- paste(dirForecast, "inputs/", sep = "", collapse = NULL)
dirForecastOutputs <-paste(dirForecast, "outputs/", sep = "", collapse = NULL)
forecastAppDll <- paste("dotnet ", dirCurrent, "forecast_app/CIAT.DAPA.USAID.Forecast.ForecastApp.dll -out ", sep = "", collapse = NULL)
dir_save <- paste(dirForecastInputs, "descarga", sep = "", collapse = NULL)
dir_response <- paste(dirForecastInputs, "estacionesMensuales", sep = "", collapse = NULL)
dir_stations <- paste(dirForecastInputs, "dailyData", sep = "", collapse = NULL)
path_prob <- paste(dirForecastOutputs, "probForecast", sep = "", collapse = NULL)
path_output <- paste(dirForecastOutputs, "resampling", sep = "", collapse = NULL)

dir_dssat <- 'C:/DSSAT46/'  ## its necessary to have the parameters .CUL, .ECO, .SPE Updated for running (calibrated the crop (Maize))

## Variables paquete maiz
dirModeloMaiz <- paste(dirCurrent, "modeloMaiz/", sep = "", collapse = NULL)
path_functions <- dirModeloMaiz
dir_run <- paste(dirModeloMaiz, "run/", sep = "", collapse = NULL)
dir_soil <- paste(dirModeloMaiz, "soils/CC.SOL", sep = "", collapse = NULL)

## ITERAR ESCENARIOS *************************************
## ITERAR ESCENARIOS *************************************
## ITERAR ESCENARIOS *************************************
dir_climate <- paste(path_output, format.Date(Sys.Date(),"/%Y%m%d/Escenarios_58504f6a006cb93ed40eec8c"), sep = "", collapse = NULL)
dir_outMaiz <- paste(dirModeloMaiz, "out/", sep = "", collapse = NULL)


## Variables paquete arroz
dirModeloArroz <- paste(dirCurrent, "modeloMaiz/", sep = "", collapse = NULL)

if (!file.exists(file.path(dir_save))){
  cat ('\n... directorio "descarga" creado\n\n\n')
  dir.create(file.path(dir_save))
}

if (!file.exists(file.path(path_prob))){
  cat ('... directorio "probForecast" creado\n\n\n')
  dir.create(file.path(path_prob))
}

if (!file.exists(file.path(path_output))){
  cat ('... directorio "path_output" creado\n\n\n')
  dir.create(file.path(path_output))
}

if (!file.exists(file.path(dir_run))){
  cat ('... directorio "dir_run" creado\n\n\n')
  dir.create(file.path(dir_run))
}

if (!file.exists(file.path(dir_outMaiz))){
  cat ('... directorio "dir_outMaiz" creado\n\n\n')
  dir.create(file.path(dir_outMaiz))
}

CMDdirForecastInputs <- paste0(gsub("/","\\\\",dirForecastInputs), "\\\"")
try(system(paste0(forecastAppDll,"-s \"prec\" -p \"",CMDdirForecastInputs," -start 1981 -end 2013"), intern = TRUE, ignore.stderr = TRUE))
#try(system(paste0(forecastAppDll,"-wf -p \"",CMDdirForecastInputs," -name \"daily\""), intern = TRUE, ignore.stderr = TRUE))
#try(system(paste0(forecastAppDll,"-fs -p \"",CMDdirForecastInputs), intern = TRUE, ignore.stderr = TRUE))


# runPrediccion <- source(paste(dirForecast,'01_prediccion.R', sep = "", collapse = NULL))

# runRemuestreo <- source(paste(dirForecast,'02_remuestreo.R', sep = "", collapse = NULL))

runModeloMaiz <- source(paste(dirModeloMaiz,'call_functions.R', sep = "", collapse = NULL))

