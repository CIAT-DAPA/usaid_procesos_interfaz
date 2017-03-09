# Librerias y prerequisitos: 
#   . gunzip
#   . R librarys
# DSSAT
# Oriza
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

## DIRECTORIO PRINCIPAL
# dirCurrent <- paste0(get_script_path(), "/", sep = "", collapse = NULL)
dirCurrent <- "C:/USAID/usaid_procesos_interfaz/"

## Variables globales paquete forecast
dirForecast <- paste0(dirCurrent, "prediccionClimatica/", sep = "", collapse = NULL)
dirInputs <- paste0(dirCurrent, "inputs/", sep = "", collapse = NULL)
dirOutputs <- paste0(dirCurrent, "outputs/", sep = "", collapse = NULL)
dirPrediccionInputs <- paste0(dirInputs, "prediccionClimatica/", sep = "", collapse = NULL)
dirPrediccionOutputs <- paste0(dirOutputs, "prediccionClimatica/", sep = "", collapse = NULL)
forecastAppDll <- paste0("dotnet ", dirCurrent, "forecast_app/CIAT.DAPA.USAID.Forecast.ForecastApp.dll ", sep = "", collapse = NULL)
dir_save <- paste0(dirPrediccionInputs, "descarga", sep = "", collapse = NULL)
dir_response <- paste0(dirPrediccionInputs, "estacionesMensuales", sep = "", collapse = NULL)
dir_stations <- paste0(dirPrediccionInputs, "dailyData", sep = "", collapse = NULL)
path_save <- paste0(dirPrediccionOutputs, "probForecast", sep = "", collapse = NULL)
path_output <- paste0(dirPrediccionOutputs, "resampling", sep = "", collapse = NULL)
path_output_sum <- paste0(path_output, "/summary", sep = "", collapse = NULL)
dir_dssat <- 'C:/DSSAT46/'  ## its necessary to have the parameters .CUL, .ECO, .SPE Updated for running (calibrated the crop (Maize))
dirCultivosInputs <-paste0(dirInputs, "cultivos/", sep = "", collapse = NULL)
dirCultivosOutputs <-paste0(dirOutputs, "cultivos/", sep = "", collapse = NULL)

## Variables globales modelo Maiz
dirModeloMaiz <- paste0(dirCurrent, "modeloMaiz/", sep = "", collapse = NULL)
dirModeloMaizInputs <- paste0(dirInputs, "cultivos/maiz/", sep = "", collapse = NULL)
dirModeloMaizOutputs <-paste0(dirOutputs, "cultivos/maiz/", sep = "", collapse = NULL)

## Variables globales paquete arroz
dirModeloArroz <- paste0(dirCurrent, "modeloArroz/", sep = "", collapse = NULL)
dirModeloArrozInputs <- paste0(dirInputs, "cultivos/arroz/", sep = "", collapse = NULL)
dirModeloArrozOutputs <-paste0(dirOutputs, "cultivos/arroz/", sep = "", collapse = NULL)

# Directorio salidas permanentes que se van a almacenar mes a mes
dirResults <- paste0(dirCurrent,"results")

if (!file.exists(file.path(dirResults))){
  dir.create(file.path(dirResults))
  cat (paste0('\n... directorio "',dirResults,'" creado\n\n'))}


# Funcion para borra y crear directorios
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
## Construyendo directorios de entrada y salida
pathConstruct(dirInputs)
pathConstruct(dirOutputs)
# predicion climatica
pathConstruct(dirPrediccionInputs)
pathConstruct(dirPrediccionOutputs)
pathConstruct(dir_save)
pathConstruct(path_save)
pathConstruct(path_output)
pathConstruct(path_output_sum)
# directorio de salida para los modelos
pathConstruct(dirCultivosOutputs)
# maiz
pathConstruct(dirModeloMaizOutputs)
# arroz
pathConstruct(dirModeloArrozOutputs)

## Descargando entradas desde la base de datos
CMDdirInputs <- paste0(gsub("/","\\\\",dirPrediccionInputs), "\\\"")
try(system(paste0(forecastAppDll,"-out -s \"prec\" -p \"",CMDdirInputs," -start 1981 -end 2013"), intern = TRUE, ignore.stderr = TRUE))
try(system(paste0(forecastAppDll,"-out -wf -p \"",CMDdirInputs," -name \"daily\""), intern = TRUE, ignore.stderr = TRUE))
CMDdirInputs <- paste0(gsub("/","\\\\",dirInputs), "\\\"")
try(system(paste0(forecastAppDll,"-out -fs -p \"",CMDdirInputs), intern = TRUE, ignore.stderr = TRUE))

# Funcion corrida moledos de cultivos (maiz y arroz)
runCrop <- function(crop, setups) {
  for(i in 2:length(setups)){
    setSplit <- strsplit(setups[i],"/")
    longName <- setSplit[[1]][length(setSplit[[1]])]
    longNameSplit <- strsplit(longName,"_")
    
    hashStation <- longNameSplit[[1]][1]
    hashCrop <- longNameSplit[[1]][2]
    hashSoil<- longNameSplit[[1]][3]
    hashDayRange <- longNameSplit[[1]][4]

    dir_climate <- paste0(path_output, "/", hashStation, sep = "", collapse = NULL)
    region <- hashStation
    
    cat(paste("\n\n Ejecutando modelo ", crop, " para estacion: \"", hashStation, "\" cultivar: \"", hashCrop, "\" suelo: \"", hashSoil, "\" rango de dias: \"", hashDayRange, "\"\n", sep = ""))
    
    if (crop == 'maiz'){
      name_csv <- paste0(longName, ".csv", sep = "", collapse = NULL)
      dir_parameters <- paste0(dirModeloMaizInputs, longName, "/", sep = "", collapse = NULL)
      dir_soil <- paste0(dirModeloMaizInputs, longName, "/SOIL.SOL", sep = "", collapse = NULL)
      dir_run <- paste0(dirModeloMaizOutputs, longName, "/run/", sep = "", collapse = NULL)
      pathConstruct(paste0(dirModeloMaizOutputs, longName, sep = "", collapse = NULL))
      out_dssat <- paste0(dirModeloMaizOutputs, longName, '/out_dssat', sep = "", collapse = NULL)
      pathConstruct(out_dssat)
      pathConstruct(dir_run)
      runModeloMaiz <- source(paste(dirModeloMaiz,'call_functions.R', sep = "", collapse = NULL))
      cat(crop)
    }
    if (crop == 'arroz'){
      dir_run <- paste0(dirModeloArrozOutputs, longName, "/run/", sep = "", collapse = NULL)
      cultivar<- hashCrop
      dir_parameters <- paste0(dirModeloArrozInputs, longName, "/", sep = "", collapse = NULL)
      #runModeloArroz <- source(paste(dirModeloArroz,'call_functions.R', sep = "", collapse = NULL))
      cat(crop)
    }   
  }
}

# Corrida Prediccion
runPrediccion <- source(paste(dirForecast,'01_prediccion.R', sep = "", collapse = NULL))

# Corrida Remuestreo
runRemuestreo <- source(paste(dirForecast,'02_remuestreo.R', sep = "", collapse = NULL))

## Corrida Modelo maiz
setups <- list.dirs(dirModeloMaizInputs,full.names = T)
runCrop('maiz', setups)

## Corrida Modelo arroz
setups <- list.dirs(dirModeloArrozInputs,full.names = T)
runCrop('arroz', setups)

# Escribiendo salidas en la base de datos
CMDdirOutputs <- paste0(gsub("/","\\\\",dirOutputs), "\\\"")
try(system(paste0(forecastAppDll,"-in -fs -cf 0.5 -p \"",CMDdirOutputs), intern = TRUE, ignore.stderr = TRUE))

