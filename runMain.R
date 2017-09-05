# Librerias y prerequisitos: 
#   . gunzip
#   . R librarys
#   . CPT_batch
#   . DSSAT
#   . Oriza
library(corpcor)
library(data.table)
library(foreach)
library(funr)
library(lazyeval)
library(parallel)
library(pcaPP)
library(R.utils)
library(raster)
library(rebus)
library(reshape)
library(rgdal)
library(stringr)
library(tidyverse)
library(trend)

# Function erase and make folder
pathConstruct <- function(dirConstruct)
  {
  if (file.exists(file.path(dirConstruct))){
    unlink(file.path(dirConstruct), recursive = TRUE, force = TRUE)
    cat (paste0('\n... folder "',dirConstruct,'" deleted\n'))
    dir.create(file.path(dirConstruct))
    cat (paste0('... folder "',dirConstruct,'" created\n\n'))
    }
    else {
      dir.create(file.path(dirConstruct))
      cat (paste0('\n... folder "',dirConstruct,'" created\n\n'))
    }
  }

# Function run model process (maize and rice)
runCrop <- function(crop, setups) {
  
  # crop <-'maiz'
  # crop <- 'arroz'
  # i = 2 
  
  for(i in 2:length(setups)){
    
    setSplit <- strsplit(setups[i],"/")
    
    longName <- setSplit[[1]][length(setSplit[[1]])]
    longNameSplit <- strsplit(longName,"_")

    hashStation <- longNameSplit[[1]][1]
    hashCrop <- longNameSplit[[1]][2]
    hashSoil <- longNameSplit[[1]][3]
    hashDayRange <- longNameSplit[[1]][4]

    dir_climate <- paste0(path_output, "/", hashStation, sep = "", collapse = NULL)
    region <- hashStation

    cat(paste("\n\n Crop:", crop, ", station: \"", hashStation, "\", species: \"", hashCrop, "\", soil: \"", hashSoil, "\", rango of days: \"", hashDayRange, "\"\n", sep = ""))
    
    if (crop == 'maiz'){
      print(longName)
      name_csv <- paste0(longName, ".csv", sep = "", collapse = NULL)
      print(name_csv)
      dir_parameters <- paste0(dirModeloMaizInputs, longName, "/", sep = "", collapse = NULL)
      dir_soil <- paste0(dirModeloMaizInputs, longName, "/SOIL.SOL", sep = "", collapse = NULL)
      dir_run <- paste0(dirModeloMaizOutputs, longName, "/run/", sep = "", collapse = NULL)
      pathConstruct(paste0(dirModeloMaizOutputs, longName, sep = "", collapse = NULL))
      out_dssat <- paste0(dirModeloMaizOutputs, longName, '/out_dssat', sep = "", collapse = NULL)
      pathConstruct(out_dssat)
      pathConstruct(dir_run)
      runModeloMaiz <- source(paste(dirModeloMaiz,'call_functions.R', sep = "", collapse = NULL), echo = F, local = T)
    }
    
    if (crop == 'arroz'){
      dir_run <- paste0(dirModeloArrozOutputs, longName, "/run/", sep = "", collapse = NULL)
      cultivar<- hashCrop
      name_csv <- paste0(longName, ".csv", sep = "", collapse = NULL)
      dir_parameters <- paste0(dirModeloArrozInputs, longName, "/", sep = "", collapse = NULL)
      runModeloArroz <- source(paste(dirModeloArroz,'call_functions.R', sep = "", collapse = NULL), local = T, echo = T)
      cat(crop)
    }   
  }
}

## MAIN PATH
# dirCurrent <- paste0(get_script_path(), "/", sep = "", collapse = NULL)
dirCurrent <- "C:/usaid_procesos_interfaz/"

  # forecastAppDll app
  forecastAppDll <- paste0("dotnet ", dirCurrent, "forecast_app/CIAT.DAPA.USAID.Forecast.ForecastApp.dll ", sep = "", collapse = NULL)

  ## Global variables Forecast module
  dirForecast <- paste0(dirCurrent, "prediccionClimatica/", sep = "", collapse = NULL)

  ## Global variables maize model module
  # dir_dssat <- 'C:/DSSAT46/'  ## its necessary to have the parameters .CUL, .ECO, .SPE Updated for running (calibrated the crop (Maize))
  dirModeloMaiz <- paste0(dirCurrent, "modeloMaiz/", sep = "", collapse = NULL)

  ## Global variables rice model module
  dirModeloArroz <- paste0(dirCurrent, "modeloArroz/", sep = "", collapse = NULL)

  # INPUTS variables
  dirInputs <- paste0(dirCurrent, "inputs/", sep = "", collapse = NULL)
    # Input variables Forecast module
    dirPrediccionInputs <- paste0(dirInputs, "prediccionClimatica/", sep = "", collapse = NULL)
      dir_save <- paste0(dirPrediccionInputs, "descarga", sep = "", collapse = NULL)
      dir_response <- paste0(dirPrediccionInputs, "estacionesMensuales", sep = "", collapse = NULL)
      dir_stations <- paste0(dirPrediccionInputs, "dailyData", sep = "", collapse = NULL)
    dirCultivosInputs <-paste0(dirInputs, "cultivos/", sep = "", collapse = NULL)
    # Input variables maize model module
    dirModeloMaizInputs <- paste0(dirInputs, "cultivos/maiz/", sep = "", collapse = NULL)
    # Input variables rice model module
    dirModeloArrozInputs <- paste0(dirInputs, "cultivos/arroz/", sep = "", collapse = NULL)

  # OUTPUTS variables
  dirOutputs <- paste0(dirCurrent, "outputs/", sep = "", collapse = NULL)
    # Output variables Forecast module
    dirPrediccionOutputs <- paste0(dirOutputs, "prediccionClimatica/", sep = "", collapse = NULL)
      path_save <- paste0(dirPrediccionOutputs, "probForecast", sep = "", collapse = NULL)
      path_output <- paste0(dirPrediccionOutputs, "resampling", sep = "", collapse = NULL)
        path_output_sum <- paste0(path_output, "/summary", sep = "", collapse = NULL)
    dirCultivosOutputs <-paste0(dirOutputs, "cultivos/", sep = "", collapse = NULL)
    # Output variables maize model module
    dirModeloMaizOutputs <-paste0(dirOutputs, "cultivos/maiz/", sep = "", collapse = NULL)
    # Output variables rice model module
    dirModeloArrozOutputs <-paste0(dirOutputs, "cultivos/arroz/", sep = "", collapse = NULL)

  # Output permanent folder
  dirResults <- paste0(dirCurrent,"results")

  if (!file.exists(file.path(dirResults))){
    dir.create(file.path(dirResults))
    cat (paste0('\n... folder "',dirResults,'" created\n\n'))}

## ********************** Making inputs and outputs folders  **************************************
# INPUTS
pathConstruct(dirInputs)                        # ./inputs/
  pathConstruct(dirPrediccionInputs)            # ./inputs/prediccionClimatica/
    pathConstruct(dir_save)                     # ./inputs/prediccionClimatica/descarga
# OUTPUTS
pathConstruct(dirOutputs)                       # ./outputs/
  pathConstruct(dirPrediccionOutputs)           # ./outputs/prediccionClimatica/
    pathConstruct(path_save)                    # ./outputs/prediccionClimatica/probForecas
    pathConstruct(path_output)                  # ./outputs/prediccionClimatica/resampling
      pathConstruct(path_output_sum)            # ./outputs/prediccionClimatica/resampling/summary
  # Outoputs crop model
  pathConstruct(dirCultivosOutputs)             # ./outputs/cultivos/
    # Maize
    pathConstruct(dirModeloMaizOutputs)         # ./outputs/cultivos/maiz/
    # Rice
    pathConstruct(dirModeloArrozOutputs)        # ./outputs/cultivos/arroz/
## ************************************************************************************************

## Download initial parameters from interface database
CMDdirInputs <- paste0(gsub("/","\\\\",dirPrediccionInputs), "\\\"")
try(system(paste0(forecastAppDll,"-out -cpt -p \"",CMDdirInputs), intern = TRUE, ignore.stderr = TRUE))
try(system(paste0(forecastAppDll,"-out -s \"prec\" -p \"",CMDdirInputs," -start 1981 -end 2013"), intern = TRUE, ignore.stderr = TRUE))
try(system(paste0(forecastAppDll,"-out -wf -p \"",CMDdirInputs," -name \"daily\""), intern = TRUE, ignore.stderr = TRUE))
try(system(paste0(forecastAppDll,"-out -co -p \"",CMDdirInputs," -name \"daily\""), intern = TRUE, ignore.stderr = TRUE))
CMDdirInputs <- paste0(gsub("/","\\\\",dirInputs), "\\\"")
try(system(paste0(forecastAppDll,"-out -fs -p \"",CMDdirInputs), intern = TRUE, ignore.stderr = TRUE))

# Prediction process
runPrediccion <- source(paste(dirForecast,'01_prediccion.R', sep = "", collapse = NULL))

# Resampling process
runRemuestreo <- source(paste(dirForecast,'02_remuestreo.R', sep = "", collapse = NULL))

## Maize crop model process
setups <- list.dirs(dirModeloMaizInputs,full.names = T)
runCrop('maiz', setups)

## Rice crop model process
setups <- list.dirs(dirModeloArrozInputs,full.names = T)
runCrop('arroz', setups)

# Upload proccess results to interface database
# CMDdirOutputs <- paste0(gsub("/","\\\\",dirOutputs), "\\\"")
# try(system(paste0(forecastAppDll,"-in -fs -cf 0.5 -p \"",CMDdirOutputs), intern = TRUE, ignore.stderr = TRUE))


# try(system(paste0(forecastAppDll,"-out -usr -p \"",CMDdirOutputs), intern = TRUE, ignore.stderr = TRUE))
