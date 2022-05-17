# =====================================================================
# Cores to use when the crop models run in parallel. Change this parameter to use more cores.
# Current country for which the forecasts are being run
# Country list with country name as key and objectid as value
no_cores <- as.numeric(Sys.getenv("N_CORES"))
countries_list <- list("COLOMBIA", "ETHIOPIA")
countries_ids <- list("COLOMBIA"="61e59d829d5d2486e18d2ea8", "ETHIOPIA"="61e59d829d5d2486e18d2ea9")
# =====================================================================

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
#   . Oryza
#   . Pycpt NextGen
library(corpcor)
library(data.table)
library(foreach)
library(funr)
library(lazyeval)
library(lubridate)
library(parallel)
library(pcaPP)
library(R.utils)
library(raster)
library(rebus)
library(reshape)
library(rgdal)
library(ncdf4) #Must be installed on the image
library(stringr)
#library(tidyverse)
library(trend)
library(curl)
library(askpass)
library(jsonlite)
library(mime)
library(openssl)
library(R6)
library(sys)
library(httr)

# Function erase and make folder
pathConstruct <- function(dirConstruct)
  {
  
  if (!(file.exists(file.path(dirCurrent)))){
    dir.create(file.path(dirCurrent))
    }
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
  
  mclapply(2:length(setups), function(i) {
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
          unlink(file.path(paste0(dirModeloArrozOutputs, longName, sep = "", collapse = NULL)), recursive = TRUE, force = TRUE)
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
      }, error=function(e){cat(paste("Scenarie: ", longName, "ERROR :"),conditionMessage(e), "\n")})
    
        }, mc.cores = no_cores, mc.preschedule = F)

}

run_oryza_by_setup <- function(setups){
  auth <- "https://oryza.aclimate.org/api/auth"
  process <- "https://oryza.aclimate.org/api/runOryza"

  #authenitcation token
  identify_body <- '{
      "user": "admin",
      "pass": "1234"
      }'
  #currentWd <- getwd()

  make_request_oryza <- function(setup) {
    response <- httr::POST(auth, add_headers('accept-encoding'='gzip, deflate'), body=identify_body, content_type_json(), verbose())
    token <- fromJSON(content(response, "text"))$token
    scenarie <- str_split_fixed(setup, '/', n=8) #current scenarie/setup
    correction <- str_split_fixed(scenarie[8], '_', n=2)
    station <- gsub('/', '', correction[1]) #current climatic station

    #Copying a compressing files for Oryza API
    file.copy(paste0(path_output, "/",station, "/"), dir_oryza_api_inputs_climate, recursive=TRUE)
    file.copy(paste0(dirModeloArrozInputs, scenarie[8],"/"), dir_oryza_api_inputs_setup, recursive=TRUE)
    setwd('/forecast/workdir/oryzaApiInputs/')
    zip(zipfile='inputs', './inputs')

    #Calling Oryza API
    dest <- paste0(dirModeloArrozOutputs, substring(scenarie[8], 2), ".csv")
    cat(paste("enviado", Sys.time()))

   
    httr::POST(process, 
              add_headers('Content-Type'='multipart/form-data', 'access-token'=token, 'accept-encoding'='gzip, deflate'), 
              body = list("fileZip" = upload_file(paste0(dir_oryza_api_inputs, "inputs.zip"))), 
              write_disk(dest, overwrite = TRUE)) #Copying output csv on rice outputs
      

  
    #Sys.sleep(10)
    cat(paste("recibido", Sys.time()))
    #Deleting inputs files to replace
    unlink(paste0(dir_oryza_api_inputs_climate, station), recursive = TRUE)
    unlink(paste0(dir_oryza_api_inputs_setup, scenarie[8]), recursive = TRUE)
    unlink(paste0(dir_oryza_api_inputs, 'inputs.zip'), recursive = TRUE)

  }
  results <- lapply(setups, make_request_oryza)

  #setwd(currentWd)
}

prepareRastersUpload <- function(forecastID) {
  forecastID <- "1"
  trimesters <- list("Jan-Mar"="jfm", "Feb-Apr"="fma", "Mar-May"="mam", "Apr-Jun"="amj", "May-Jul"="mjj", "Jun-Aug"="jja", "Jul-Sep"="jas", "Aug-Oct"="aso", "Sep-Nov"="son", "Oct-Dec"="ond", "Nov-Jan"="ndj", "Dec-Feb"="djf")


  #Writting deterministic raster files (to upload to geoserver)
  for(i in 1:length(nextGenFileName_det)){
    firstModel <- raster(paste0(dir_outputs_nextgen, "/", nextGenFileName_det[i]))
    secondModel <- raster(paste0(dir_outputs_nextgen, "/", nextGenFileName_det[i]))

    #Writting raster files in .tif
    writeRaster(firstModel, paste0(path_rasters, "/", tolower(paste0("seasonal_", currentCountry, "_" ,trimesters[tgts[i]], "_deterministic_", monf, "_", fyr, ".tif"))), overwrite=TRUE)
    writeRaster(secondModel, paste0(path_rasters, "/", tolower(paste0("seasonal_", currentCountry, "_" ,trimesters[tgts[i]], "_deterministic_", monf, "_", fyr, ".tif"))), overwrite=TRUE)

  }

  #Writting probabilistic raster files (to upload to geoserver)

  for(i in 1:length(nextGenFileName_prob)){
    dataNextGenAbove = raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname="Above_Normal")
    dataNextGenBelow = raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname="Below_Normal") # seasonal_country_probabilistic_above_71e59d829d5d2486e18d2aa2_apr_2022_amj.tif
    dataNextGenNormal = raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname="Normal")

    #Writting raster files in .tif
    writeRaster(dataNextGenAbove, paste0(path_rasters, "/", tolower(paste0("seasonal_country_probabilistic_above", "_" ,trimesters[tgts[i]], "_probabilistic_above_", monf, "_", fyr, ".tif"))), overwrite=TRUE)
    writeRaster(dataNextGenBelow, paste0(path_rasters, "/", tolower(paste0("seasonal_country_probabilistic_below", currentCountry, "_" ,trimesters[tgts[i]], "_probabilistic_below_", monf, "_", fyr, ".tif"))), overwrite=TRUE)
    writeRaster(dataNextGenNormal, paste0(path_rasters, "/", tolower(paste0("seasonal_country_probabilistic_normal", currentCountry, "_" ,trimesters[tgts[i]], "_probabilistic_normal_", monf, "_", fyr, ".tif"))), overwrite=TRUE)

  }

}

uploadRasterFiles <- function() {

  setwd(dir_upload_raster)
  system(paste0("python import.py ", "aclimate_", objectIdCurrentCountry))
  unlink(paste0(dir_oryza_api_inputs_climate, station), recursive = TRUE)
  unlink(paste0(dir_oryza_api_inputs_setup, scenarie[8]), recursive = TRUE)
  unlink(paste0(dir_oryza_api_inputs, 'inputs.zip'), recursive = TRUE)
}

#Python upload/import function---
#Country name, filepath
#Main function
#Import function if exists
#Creates store if not exist and imports data


## MAIN PATH
start.time <- Sys.time()
#dirCurrent <- "/forecast/workdir/usaid_procesos_interfaz/"
dirCurrent <- "/forecast/usaid_procesos_interfaz/"

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

  #Script that upload files to the Geoserver file system
  dir_python_folder <- paste0(dirCurrent, "python/")
  dir_pycpt_scripts <- paste0(dir_python_folder, "PyCPT/")
  dir_upload_raster <- paste0(dir_python_folder, "UploadMosaics/")
  
  #Common directory to send data to Oryza API
  dir_oryza_api_inputs <- "/forecast/workdir/oryzaApiInputs/"
  dir_oryza_api_inputs_zip <- "/forecast/workdir/oryzaApiInputs/inputs/"
  dir_oryza_api_inputs_climate <- paste0(dir_oryza_api_inputs_zip, "climate/")
  dir_oryza_api_inputs_setup <- paste0(dir_oryza_api_inputs_zip, "setups/")


##ProbForecats files lists for merging (For importation proccess)
metrics_list <- list()
probabilities_list <- list()

for(c in countries_list){
  currentCountry <- c
    #Checks country for avoid conlicts
    maize_name_by_country <- if(currentCountry=="COLOMBIA") "maiz" else "maize"

    dirCurrent <- paste0("/forecast/workdir/", currentCountry, "/")
    # INPUTS variables
    dirInputs <- paste0(dirCurrent, "inputs/", sep = "", collapse = NULL)
      # Input variables Forecast module
      dirPrediccionInputs <- paste0(dirInputs, "prediccionClimatica/", sep = "", collapse = NULL)
        dir_save <- paste0(dirPrediccionInputs, "descarga", sep = "", collapse = NULL)
        dir_runCPT <- paste0(dirPrediccionInputs, "run_CPT", sep = "", collapse = NULL)
        dir_response <- paste0(dirPrediccionInputs, "estacionesMensuales", sep = "", collapse = NULL)
        dir_stations <- paste0(dirPrediccionInputs, "dailyData", sep = "", collapse = NULL)
        dir_inputs_nextgen <- paste0(dirPrediccionInputs, "NextGen", sep = "", collapse = NULL)
      dirCultivosInputs <-paste0(dirInputs, "cultivos/", sep = "", collapse = NULL)
      # Input variables Maize model module
      dirModeloMaizInputs <- paste0(dirInputs, "cultivos/", maize_name_by_country, "/", sep = "", collapse = NULL)
      # Input variables rice model module
      dirModeloArrozInputs <- paste0(dirInputs, "cultivos/arroz/", sep = "", collapse = NULL)
      # Input variables frijol model module
      dirModeloFrijolInputs <- paste0(dirInputs, "cultivos/frijol/", sep = "", collapse = NULL)

    # OUTPUTS variables
    dirUnifiedOutputs <- paste0("/forecast/workdir/", "unified_outputs", "/")
    #Outputs NExtGen
    dir_outputs_nextgen <- paste0("/forecast/PyCPT/iri-pycpt/", currentCountry, "/output/")
    dirOutputs <- paste0(dirCurrent, "outputs/", sep = "", collapse = NULL)
      # Output variables Forecast module
      dirPrediccionOutputs <- paste0(dirOutputs, "prediccionClimatica/", sep = "", collapse = NULL)
      dirNextGen <- paste0(dirOutputs, "NextGen/", sep = "", collapse = NULL)
        path_save <- paste0(dirPrediccionOutputs, "probForecast", sep = "", collapse = NULL)
        path_rasters <- paste0(dirPrediccionOutputs, "raster", sep = "", collapse = NULL)
        path_output <- paste0(dirPrediccionOutputs, "resampling", sep = "", collapse = NULL)
          path_output_sum <- paste0(path_output, "/summary", sep = "", collapse = NULL)
      dirCultivosOutputs <-paste0(dirOutputs, "cultivos/", sep = "", collapse = NULL)
      # Output variables maize model module
      dirModeloMaizOutputs <-paste0(dirOutputs, "cultivos/", maize_name_by_country, "/", sep = "", collapse = NULL)
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
      pathConstruct(dir_inputs_nextgen)           # ./inputs/prediccionClimatica/NextGen
      #Oryza API
      pathConstruct(dir_oryza_api_inputs)         # ./workdir/oryzaApiInputs/
      pathConstruct(dir_oryza_api_inputs_zip)     # ./workdir/oryzaApiInputs/inputs
      pathConstruct(dir_oryza_api_inputs_climate) # ./workdir/oryzaApiInputs/inputs/climate
      pathConstruct(dir_oryza_api_inputs_setup)   # ./workdir/oryzaApiInputs/inputs/setups
  # OUTPUTS
  pathConstruct(dirUnifiedOutputs)                # /unified_outputs/
  pathConstruct(dirOutputs)                       # ./outputs/
    pathConstruct(dirPrediccionOutputs)           # ./outputs/prediccionClimatica/
    pathConstruct(dirNextGen)                      # ./outputs/NextGen/
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
  scriptsDir <- "/forecast/usaid_procesos_interfaz/"
  setwd(paste0(scriptsDir, "forecast_app"))
  #CMDdirInputs <- paste0(gsub("/","\\\\",dirPrediccionInputs), "\\\"")
  CMDdirInputs <- dirPrediccionInputs
  CMDdirCropsInputs <- dirInputs
  objectIdCurrentCountry <- countries_ids[currentCountry]
  pyCPTMonth <- "5"
  dotnet_cmd <- c(paste0(forecastAppDll,"-out -cpt -p \"",CMDdirInputs, "\" -c \"", objectIdCurrentCountry, "\""),
                paste0(forecastAppDll,"-out -s \"prec\" -p \"",CMDdirInputs,"\" -start 1982 -end 2013 -c \"", objectIdCurrentCountry, "\""),
                paste0(forecastAppDll,"-out -wf -p \"",CMDdirInputs,"\" -name \"daily\" -c \"", objectIdCurrentCountry, "\""),
                paste0(forecastAppDll,"-out -co -p \"",CMDdirInputs,"\" -name \"daily\" -c \"", objectIdCurrentCountry, "\""),
                paste0(forecastAppDll,"-out -fs -p \"",CMDdirCropsInputs, "\" -c \"", objectIdCurrentCountry, "\""),
                paste0(forecastAppDll, "-out -py -p \"", dir_inputs_nextgen, "\" -c \"", objectIdCurrentCountry, "\" -m \"", pyCPTMonth, "\""),
                paste0(forecastAppDll, "-out -pyco -p \"", dir_inputs_nextgen, "\" -c \"", objectIdCurrentCountry, "\""))

  print(dotnet_cmd)

  try(system(dotnet_cmd[1], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[2], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[3], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[4], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[5], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[6], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[7], intern = TRUE, ignore.stderr = TRUE)) 


  # Prediction process
  if(currentCountry=="COLOMBIA"){
  source(paste(dirForecast,'01_prediccion.R', sep = "", collapse = NULL)) 
  } else {
  source(paste(dir_pycpt_scripts, 'PyCPT_from_R.r', sep = "", collapse = NULL))
  }

  # Resampling process
  runRemuestreo <- source(paste(dirForecast,'02_remuestreo.R', sep = "", collapse = NULL))

  # Dowloading and final joining data process
  runJoinFinalData <- source(paste(dirForecast,'03_join_wth_final.R', sep = "", collapse = NULL))

  ## Maize crop model process
  setups <- list.dirs(dirModeloMaizInputs,full.names = T)
  # Deletes the first empty directory when running in parallel. This due to some errors that occur when running in parallel and not sequential
  #setups <- if(no_cores > 1) setups[-1] else setups
  runCrop("maiz", setups)


  ## Rice crop model process
  setups <- list.dirs(dirModeloArrozInputs,full.names = T)
  setups <- if(no_cores > 1) setups[-1] else setups
  run_oryza_by_setup(setups)
  

  ## Frijol crop model process
  #setups <- list.dirs(dirModeloFrijolInputs,full.names = T)
  #runCrop('frijol', setups)

  ## Copying outputs for importation
  file.copy(dirOutputs, dirUnifiedOutputs, recursive=TRUE)

  ##saving .csv probForecast for merging
  metrics_list[[length(metrics_list) +1 ]] <- read_csv(paste0(path_save, "/metrics.csv"))
  probabilities_list[[length(probabilities_list) +1 ]] <- read_csv(paste0(path_save, "/probabilities.csv"))

}
##Merging probForecast files
probForecastUnifiedDir <- paste0(dirUnifiedOutputs, "outputs/prediccionClimatica/probForecast/")
write_csv(bind_rows(metrics_list), paste0(probForecastUnifiedDir, "metrics.csv"))
write_csv(bind_rows(probabilities_list), paste0(probForecastUnifiedDir, "probabilities.csv"))

# Upload proccess results to interface database
setwd(paste0(scriptsDir, "forecast_app"))
CMDdirOutputs <- paste0(dirUnifiedOutputs, "outputs/") #paste0(gsub("/","\\\\",dirOutputs), "\\\"")
try(system(paste0(forecastAppDll,"-in -fs -cf 0.5 -p \"",CMDdirOutputs, "\""), intern = TRUE, ignore.stderr = TRUE))

#Import raster to Geoserver process


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
