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
dirCultivosInputs <-paste0(dirInputs, "cultivos/", sep = "", collapse = NULL)
dirCultivosOutputs <-paste0(dirOutputs, "cultivos/", sep = "", collapse = NULL)

## Variables modelo Maiz
dirModeloMaiz <- paste0(dirCurrent, "modeloMaiz/", sep = "", collapse = NULL)
dirModeloMaizInputs <- paste0(dirInputs, "cultivos/maiz/", sep = "", collapse = NULL)
dirModeloMaizOutputs <-paste0(dirOutputs, "cultivos/maiz/", sep = "", collapse = NULL)
dir_run <- paste0(dirModeloMaizOutputs, "run/", sep = "", collapse = NULL)
dir_soil <- paste0(dirModeloMaiz, "soils/CC.SOL", sep = "", collapse = NULL)

## Variables paquete arroz
dirModeloArroz <- paste0(dirCurrent, "modeloMaiz/", sep = "", collapse = NULL)
dirModeloArrozInputs <- paste0(dirInputs, "cultivos/arroz/", sep = "", collapse = NULL)
dirModeloArrozOutputs <-paste0(dirOutputs, "cultivos/arroz/", sep = "", collapse = NULL)

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
pathConstruct(dirCultivosOutputs)
# pathConstruct(dirModeloMaizInputs)
pathConstruct(dirModeloMaizOutputs)
pathConstruct(dir_run)
# pathConstruct(dirModeloArrozInputs)
pathConstruct(dirModeloArrozOutputs)

CMDdirInputs <- paste0(gsub("/","\\\\",dirPrediccionInputs), "\\\"")
try(system(paste0(forecastAppDll,"-s \"prec\" -p \"",CMDdirInputs," -start 1981 -end 2013"), intern = TRUE, ignore.stderr = TRUE))
try(system(paste0(forecastAppDll,"-wf -p \"",CMDdirInputs," -name \"daily\""), intern = TRUE, ignore.stderr = TRUE))
CMDdirInputs <- paste0(gsub("/","\\\\",dirInputs), "\\\"")
try(system(paste0(forecastAppDll,"-fs -p \"",CMDdirInputs), intern = TRUE, ignore.stderr = TRUE))
cat("\n")

runPrediccion <- source(paste(dirForecast,'01_prediccion.R', sep = "", collapse = NULL))

runRemuestreo <- source(paste(dirForecast,'02_remuestreo.R', sep = "", collapse = NULL))

## Modelo maiz
setups = list.dirs(dirModeloMaizInputs,full.names = T)

for(x in 2:length(setups)){
  setSplit <- strsplit(setups[x],"/")
  longName <- setSplit[[1]][length(set_split[[1]])]
  longNameSplit <- strsplit(longName,"_")

  hashStation <- longNameSplit[[1]][1]
  hashCrop <- longNameSplit[[1]][2]
  hashSoil<- longNameSplit[[1]][3]
  hashDayRange <- longNameSplit[[1]][4]

  cat(paste("\n\n Ejecutando modelo Maiz para estacion: \"", hashStation, "\" cultivar: \"", hashCrop, "\" suelo: \"", hashSoil, "\" rango de dias: \"", hashDayRange, "\"\n", sep = ""))
  name_csv <- paste0(longName, ".csv", sep = "", collapse = NULL)
  cat(name_csv)
  dir_climate <- paste0(path_output, "/", hashStation, sep = "", collapse = NULL)
  cat(dir_climate)
  # runModeloMaiz <- source(paste(dirModeloMaiz,'call_functions.R', sep = "", collapse = NULL))
  }

cat("\n\n... modelo Maiz finalizado\n")



