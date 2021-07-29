# =====================================================================
# For compatibility with Rscript.exe: 
# =====================================================================
if(length(.libPaths()) == 1){
  # We're in Rscript.exe
  possible_lib_paths <- file.path(Sys.getenv(c('USERPROFILE','R_USER')),
                                  "R","win-library",
                                  paste(R.version$major,
                                        substr(R.version$minor,1,1),
                                        sep='.'))
  indx <- which(file.exists(possible_lib_paths))
  if(length(indx)){
    .libPaths(possible_lib_paths[indx[1]])
  }
  # CLEAN UP
  rm(indx,possible_lib_paths)
}
# =====================================================================

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
library(lubridate)
library(mailR)
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
library(rjson)

# Function erase and make folder
pathConstruct <- function(dirConstruct)
  {
  setwd(dirCurrent)
  if (file.exists(file.path(dirConstruct))){
    unlink(file.path(dirConstruct), recursive = FALSE, force = TRUE)
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
    tryCatch(
      {
    
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
          #out_dssat <- paste0(dirModeloMaizOutputs, longName, '/out_dssat', sep = "", collapse = NULL)
          #pathConstruct(out_dssat)
          pathConstruct(dir_run)
          runModeloMaiz <- source(paste(dirModeloMaiz,'call_functions.R', sep = "", collapse = NULL), echo = F, local = T)
          unlink(file.path(paste0(dirModeloMaizOutputs, longName, sep = "", collapse = NULL)), recursive = TRUE, force = TRUE)
        }
    
        if (crop == 'arroz'){
          dir_run <- paste0(dirModeloArrozOutputs, longName, "/run/", sep = "", collapse = NULL)
          cultivar<- hashCrop
          name_csv <- paste0(longName, ".csv", sep = "", collapse = NULL)
          dir_parameters <- paste0(dirModeloArrozInputs, longName, "/", sep = "", collapse = NULL)
          runModeloArroz <- source(paste(dirModeloArroz,'call_functions.R', sep = "", collapse = NULL), local = T, echo = F)
          #unlink(file.path(paste0(dirModeloArrozOutputs, longName, sep = "", collapse = NULL)), recursive = TRUE, force = TRUE)
        }   

        if (crop == 'frijol'){
          print(longName)
          name_csv <- paste0(longName, ".csv", sep = "", collapse = NULL)
          print(name_csv)
          dir_parameters <- paste0(dirModeloFrijolInputs, longName, "/", sep = "", collapse = NULL)
          dir_soil <- paste0(dirModeloFrijolInputs, longName, "/SOIL.SOL", sep = "", collapse = NULL)
          dir_run <- paste0(dirModeloFrijolOutputs, longName, "/run/", sep = "", collapse = NULL)
          pathConstruct(paste0(dirModeloFrijolOutputs, longName, sep = "", collapse = NULL))
          out_dssat <- paste0(dirModeloFrijolOutputs, longName, '/out_dssat', sep = "", collapse = NULL)
          pathConstruct(out_dssat)
          pathConstruct(dir_run)
          runModeloFrijol <- source(paste(dirModeloFrijol,'call_functions.R', sep = "", collapse = NULL), echo = F, local = T)
          unlink(file.path(paste0(dirModeloFrijolOutputs, longName, sep = "", collapse = NULL)), recursive = TRUE, force = TRUE)
        }
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
        }

  }
  

## MAIN PATH
start.time <- Sys.time()

#dirCurrent <- paste0(get_script_path(), "/", sep = "", collapse = NULL)
#dirCurrent <- "C:/usaid_procesos_interfaz/"
dirCurrent <- "/forecast/workdir/usaid_procesos_interfaz/"

  # forecastAppDll app - App de consola que se conecta a la base de datos
  forecastAppDll <- paste0("dotnet ", dirCurrent, "forecast_app/CIAT.DAPA.USAID.Forecast.ForecastApp.dll ", sep = "", collapse = NULL)

  ## Global variables Forecast module
  dirForecast <- paste0(dirCurrent, "prediccionClimatica/", sep = "", collapse = NULL)

  ## Global variables maize model module
  dir_dssat <- 'C:/DSSAT46/'  ## its necessary to have the parameters .CUL, .ECO, .SPE Updated for running (calibrated the crop (Maize))
  dirModeloMaiz <- paste0(dirCurrent, "modeloMaiz/", sep = "", collapse = NULL)

  ## Global variables rice model module
  dirModeloArroz <- paste0(dirCurrent, "modeloArroz/", sep = "", collapse = NULL)
  
  ## Global variables Frijol model module
  dir_dssat <- 'C:/DSSAT46/'  ## its necessary to have the parameters .CUL, .ECO, .SPE Updated for running (calibrated the crop (Frijol))
  dirModeloFrijol <- paste0(dirCurrent, "modeloFrijol/", sep = "", collapse = NULL)

  # INPUTS variables
  dirInputs <- paste0(dirCurrent, "inputs/", sep = "", collapse = NULL)
    # Input variables Forecast module
    dirPrediccionInputs <- paste0(dirInputs, "prediccionClimatica/", sep = "", collapse = NULL)
      dir_save <- paste0(dirPrediccionInputs, "descarga", sep = "", collapse = NULL)
      dir_runCPT <- paste0(dirPrediccionInputs, "run_CPT", sep = "", collapse = NULL)
      dir_response <- paste0(dirPrediccionInputs, "estacionesMensuales", sep = "", collapse = NULL)
      dir_stations <- paste0(dirPrediccionInputs, "dailyData", sep = "", collapse = NULL)
    dirCultivosInputs <-paste0(dirInputs, "cultivos/", sep = "", collapse = NULL)
    # Input variables Maize model module
    dirModeloMaizInputs <- paste0(dirInputs, "cultivos/maiz/", sep = "", collapse = NULL)
    # Input variables rice model module
    dirModeloArrozInputs <- paste0(dirInputs, "cultivos/arroz/", sep = "", collapse = NULL)
    # Input variables frijol model module
    dirModeloFrijolInputs <- paste0(dirInputs, "cultivos/frijol/", sep = "", collapse = NULL)

  # OUTPUTS variables
  dirOutputs <- paste0(dirCurrent, "outputs/", sep = "", collapse = NULL)
    # Output variables Forecast module
    dirPrediccionOutputs <- paste0(dirOutputs, "prediccionClimatica/", sep = "", collapse = NULL)
      path_save <- paste0(dirPrediccionOutputs, "probForecast", sep = "", collapse = NULL)
      path_rasters <- paste0(dirPrediccionOutputs, "raster", sep = "", collapse = NULL)
      path_output <- paste0(dirPrediccionOutputs, "resampling", sep = "", collapse = NULL)
        path_output_sum <- paste0(path_output, "/summary", sep = "", collapse = NULL)
    dirCultivosOutputs <-paste0(dirOutputs, "cultivos/", sep = "", collapse = NULL)
    # Output variables maize model module
    dirModeloMaizOutputs <-paste0(dirOutputs, "cultivos/maiz/", sep = "", collapse = NULL)
    # Output variables rice model module
    dirModeloArrozOutputs <-paste0(dirOutputs, "cultivos/arroz/", sep = "", collapse = NULL)
    # Output variables frijol model module
    dirModeloFrijolOutputs <-paste0(dirOutputs, "cultivos/frijol/", sep = "", collapse = NULL)
    
  # Output permanent folder
  dirResults <- paste0(dirCurrent,"results")

  # if (!file.exists(file.path(dirResults))){
  #   dir.create(file.path(dirResults))
  #   cat (paste0('\n... folder "',dirResults,'" created\n\n'))}

## ********************** Making inputs and outputs folders  **************************************
# INPUTS
pathConstruct(dirInputs)                        # ./inputs/
  pathConstruct(dirPrediccionInputs)            # ./inputs/prediccionClimatica/
    pathConstruct(dir_save)                     # ./inputs/prediccionClimatica/descarga
    pathConstruct(dir_runCPT)                   # ./inputs/prediccionClimatica/run_CPT
# OUTPUTS
pathConstruct(dirOutputs)                       # ./outputs/
  pathConstruct(dirPrediccionOutputs)           # ./outputs/prediccionClimatica/
    pathConstruct(path_save)                    # ./outputs/prediccionClimatica/probForecas
    pathConstruct(path_rasters)                 # ./outputs/prediccionClimatica/raster
    pathConstruct(path_output)                  # ./outputs/prediccionClimatica/resampling
      pathConstruct(path_output_sum)            # ./outputs/prediccionClimatica/resampling/summary
  # Outoputs crop model
  pathConstruct(dirCultivosOutputs)             # ./outputs/cultivos/
    # Maize
    pathConstruct(dirModeloMaizOutputs)         # ./outputs/cultivos/maiz/
    # Rice
    pathConstruct(dirModeloArrozOutputs)        # ./outputs/cultivos/arroz/
    # Frijol
    pathConstruct(dirModeloFrijolOutputs)        # ./outputs/cultivos/frijol/
    
## ************************************************************************************************

## Download initial parameters from interface database
setwd(paste0(dirCurrent,"/forecast_app"))
CMDdirInputs <- paste0(gsub("/","\\\\",dirPrediccionInputs), "\\\"")
try(system(paste0(forecastAppDll,"-out -cpt -p \"",CMDdirInputs), intern = TRUE, ignore.stderr = TRUE))
try(system(paste0(forecastAppDll,"-out -s \"prec\" -p \"",CMDdirInputs," -start 1982 -end 2013"), intern = TRUE, ignore.stderr = TRUE))
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

## Frijol crop model process
setups <- list.dirs(dirModeloFrijolInputs,full.names = T)
runCrop('frijol', setups)

# Upload proccess results to interface database
setwd(paste0(dirCurrent,"/forecast_app"))
CMDdirOutputs <- paste0(gsub("/","\\\\",dirOutputs), "\\\"")
try(system(paste0(forecastAppDll,"-in -fs -cf 0.5 -p \"",CMDdirOutputs), intern = TRUE, ignore.stderr = TRUE))
try(system(paste0(forecastAppDll,"-share"), intern = TRUE, ignore.stderr = TRUE))

# Delete cropmodels cache
pathConstruct(dirCultivosOutputs)             # ./outputs/cultivos/

# send mail
try(system(paste0(forecastAppDll,"-out -usr -p \"",CMDdirOutputs), intern = TRUE, ignore.stderr = TRUE))

sender <- "pronosticosaclimate@gmail.com"
#recipients <- readLines(paste0(dirOutputs, "notify/notify.csv"))
recipients <- c("edarague@gmail.com")
email <- send.mail(from = sender,
                   to = recipients,
                   subject="Resultados pronosticos",
                   body = "El proceso ha finalizado con 'exito",
                   smtp = list(host.name = "aspmx.l.google.com", port = 25),
                   authenticate = FALSE,
                   send = FALSE)
email$send() # execute to send email

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken