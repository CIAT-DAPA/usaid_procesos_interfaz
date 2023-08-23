# =====================================================================
# Cores to use when the crop models run in parallel. Change this parameter to use more cores.
no_cores <- as.numeric(Sys.getenv("N_CORES"))
#countries_list <- list("COLOMBIA", "ETHIOPIA", "ANGOLA")

countries_ids <- list("COLOMBIA" = "61e59d829d5d2486e18d2ea8", "ETHIOPIA" = "61e59d829d5d2486e18d2ea9", "ANGOLA" = "62a739250dd05810f0e2938d", "GUATEMALA"="636c0813e57f2e6ac61394e6", 
"MALAWI"="641c820e4fb2a6438cc670e7", "TANZANIA"="641c82214fb2a6438cc670eb", "ZAMBIA"="641c82304fb2a6438cc670ee")

#Taking arguments from cmd
args <- commandArgs(trailingOnly = TRUE)
#Current country for which the forecasts are being run. Mandatory argument
currentCountry <- args[1]
#Optional arguments:
extract_input_data <- if(is.na(args[2])) FALSE else TRUE
import_data_to_db <- if(is.na(args[3])) FALSE else TRUE

# =====================================================================

# =====================================================================
# For compatibility with Rscript.exe:
# =====================================================================
if (length(.libPaths()) == 1) {
  # We're in Rscript.exe
  possible_lib_paths <- file.path(
    Sys.getenv(c("USERPROFILE", "R_USER")),
    "R", "win-library",
    paste(R.version$major,
      substr(R.version$minor, 1, 1),
      sep = "."
    )
  )
  indx <- which(file.exists(possible_lib_paths))
  if (length(indx)) {
    .libPaths(possible_lib_paths[indx[1]])
  }
  # CLEAN UP
  rm(indx, possible_lib_paths)
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
library(doParallel)
library(pcaPP)
library(R.utils)
library(raster)
library(rebus)
library(reshape)
library(rgdal)
library(ncdf4)
library(stringr)
library(tidyverse)
library(trend)
library(curl)
library(askpass)
library(jsonlite)
library(mime)
library(openssl)
library(R6)
library(sys)
library(httr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(readr)
library(magrittr)


# Function erase and make folder
pathConstruct <- function(dirConstruct) {
  if (!(file.exists(file.path(dirCurrent)))) {
    dir.create(file.path(dirCurrent))
  }
  setwd(dirCurrent)

  if (file.exists(file.path(dirConstruct))) {
    unlink(file.path(dirConstruct), recursive = FALSE, force = TRUE)
    cat(paste0('\n... folder "', dirConstruct, '" deleted\n'))
    dir.create(file.path(dirConstruct))
    cat(paste0('... folder "', dirConstruct, '" created\n\n'))
  } else {
    dir.create(file.path(dirConstruct))
    cat(paste0('\n... folder "', dirConstruct, '" created\n\n'))
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
        setSplit <- strsplit(setups[i], "/")

        longName <- setSplit[[1]][length(setSplit[[1]])]
        longNameSplit <- strsplit(longName, "_")

        hashStation <- longNameSplit[[1]][1]
        hashCrop <- longNameSplit[[1]][2]
        hashSoil <- longNameSplit[[1]][3]
        hashDayRange <- longNameSplit[[1]][4]

        dir_climate <- paste0(path_output, "/", hashStation, sep = "", collapse = NULL)
        region <- hashStation

        cat(paste("\n\n Crop:", crop, ", station: \"", hashStation, "\", species: \"", hashCrop, "\", soil: \"", hashSoil, "\", rango of days: \"", hashDayRange, "\"\n", sep = ""))

        if (crop == "maiz") {
          print(longName)
          name_csv <- paste0(longName, ".csv", sep = "", collapse = NULL)
          print(name_csv)
          dir_parameters <- paste0(dirModeloMaizInputs, longName, "/", sep = "", collapse = NULL)
          dir_soil <- paste0(dirModeloMaizInputs, longName, "/SOIL.SOL", sep = "", collapse = NULL)
          dir_run <- paste0(dirModeloMaizOutputs, longName, "/run/", sep = "", collapse = NULL)
          pathConstruct(paste0(dirModeloMaizOutputs, longName, sep = "", collapse = NULL))
          # out_dssat <- paste0(dirModeloMaizOutputs, longName, '/out_dssat', sep = "", collapse = NULL)
          # pathConstruct(out_dssat)
          pathConstruct(dir_run)
          runModeloMaiz <- source(paste(dirModeloMaiz, "call_functions.R", sep = "", collapse = NULL), echo = F, local = T)
          unlink(file.path(paste0(dirModeloMaizOutputs, longName, sep = "", collapse = NULL)), recursive = TRUE, force = TRUE)
        }

        if (crop == "arroz") {
          dir_run <- paste0(dirModeloArrozOutputs, longName, "/run/", sep = "", collapse = NULL)
          cultivar <- hashCrop
          name_csv <- paste0(longName, ".csv", sep = "", collapse = NULL)
          dir_parameters <- paste0(dirModeloArrozInputs, longName, "/", sep = "", collapse = NULL)
          runModeloArroz <- source(paste(dirModeloArroz, "call_functions.R", sep = "", collapse = NULL), local = T, echo = F)
          unlink(file.path(paste0(dirModeloArrozOutputs, longName, sep = "", collapse = NULL)), recursive = TRUE, force = TRUE)
        }

        if (crop == "frijol") {
          print(longName)
          name_csv <- paste0(longName, ".csv", sep = "", collapse = NULL)
          print(name_csv)
          dir_parameters <- paste0(dirModeloFrijolInputs, longName, "/", sep = "", collapse = NULL)
          dir_soil <- paste0(dirModeloFrijolInputs, longName, "/SOIL.SOL", sep = "", collapse = NULL)
          dir_run <- paste0(dirModeloFrijolOutputs, longName, "/run/", sep = "", collapse = NULL)
          pathConstruct(paste0(dirModeloFrijolOutputs, longName, sep = "", collapse = NULL))
          out_dssat <- paste0(dirModeloFrijolOutputs, longName, "/out_dssat", sep = "", collapse = NULL)
          pathConstruct(out_dssat)
          pathConstruct(dir_run)
          runModeloFrijol <- source(paste(dirModeloFrijol, "call_functions.R", sep = "", collapse = NULL), echo = F, local = T)
          unlink(file.path(paste0(dirModeloFrijolOutputs, longName, sep = "", collapse = NULL)), recursive = TRUE, force = TRUE)
        }
      },
      error = function(e) {
        cat(paste("Scenarie: ", longName, "ERROR :"), conditionMessage(e), "\n")
      }
    )
  }, mc.cores = no_cores, mc.preschedule = F)
}

prepare_setups_api_oryza <- function(setups) {

  # Preparing inputs for parallelization
  lapply(2:length(setups), function(i) {
    scenarie <- str_split_fixed(setups[i], "/", n = 8) # current scenarie/setup
    correction <- str_split_fixed(scenarie[8], "_", n = 2)
    station <- gsub("/", "", correction[1]) # current climatic station

    # Copying a compressing files for Oryza API
    file.copy(paste0(path_output, "/", station, "/"), dir_oryza_api_inputs_climate, recursive = TRUE)
    file.copy(paste0(dirModeloArrozInputs, scenarie[8], "/"), dir_oryza_api_inputs_setup, recursive = TRUE)

    setwd(dir_oryza_api_inputs)
    zip(zipfile = "inputs", "./inputs")

    currentFolder <- paste0(dir_oryza_api_inputs, substring(scenarie[8], 2), "/")
    dir.create(currentFolder)
    file.copy("inputs.zip", currentFolder, recursive = TRUE)

    # Deleting inputs files to replace
    unlink(paste0(dir_oryza_api_inputs_climate, station), recursive = TRUE)
    unlink(paste0(dir_oryza_api_inputs_setup, scenarie[8]), recursive = TRUE)
    unlink(paste0(dir_oryza_api_inputs, "inputs.zip"), recursive = TRUE)
  })# , mc.cores = no_cores, mc.preschedule = F)

  # This return TRUE indicates that the process is done
  return(TRUE)
}

run_oryza_by_setup <- function() {
  auth <- "https://oryza.aclimate.org/api/v1/login"
  process <- "https://oryza.aclimate.org/api/v1/run"
  #auth <- "http://127.0.0.1:5000/api/v1/login"
  #process <- "http://localhost:5000/api/v1/run"

  # authenitcation token
  identify_body <- '{
      "user": "aclimate_forecast",
      "password": "p*M$3Hp,X*h_ME@-s"
      }'
  # identify_body <- '{
  #     "user": "forecast",
  #     "password": "My11v3S3cret4"
  #     }'
  # currentWd <- getwd()
  
  response <- httr::POST(auth, add_headers("accept-encoding" = "deflate"), body = identify_body, content_type_json(), verbose())
  token <- fromJSON(content(response, "text"))$token

  inputsList <- list.files(dir_oryza_api_inputs)
  inputsList <- head(inputsList, -1) # To ignore inputs folder
  make_request_oryza <- function(currentInputFolder) {
    # CSV name
    csvName <- paste0(currentInputFolder, ".csv")
    # Making curl post request (problems with httr in this part)
    req <- paste0(
      'curl -X POST "', process, '" -H "accept: */*"', " -H ", '"x-access-token: ', token, '"', " -H ", '"Content-Type: multipart/form-data"',
      " -F ", '"inputs=@', paste0(dir_oryza_api_inputs, currentInputFolder, "/inputs.zip"), ';type=application/x-zip-compressed"',
      ' -o "', csvName, '"'
    )

    # Sending request
    cat(paste("send request at: ", Sys.time()))
    system(req)
    cat(paste("received at: ", Sys.time()))
    # Deleting used inputs (storage control)
    #unlink(paste0(dir_oryza_api_inputs, currentInputFolder), recursive = TRUE)
  }
  setwd(dirModeloArrozOutputs)
  # no_cores = 3 in server
  #results <- mclapply(inputsList, make_request_oryza, mc.cores = 3, mc.preschedule = F)
  results <- mclapply(1:length(inputsList), function(i) {
    make_request_oryza(inputsList[i])
    #Sys.sleep(2)

  }, mc.cores = 3, mc.preschedule = F)
}

################################### Working on wheat (this is not the final version of this function)
runDssatModule <- function(crop){
  cul_file <- ''
  if(crop == "maize"){
    cul_file <- 'MZCER048'
  } else if(crop=="wheat"){
    cul_file <- 'WHCER048'
  }

  dirCurrentCropInputs <- paste0(dirInputs, "cultivos/", if (currentCountry == "COLOMBIA" && crop == "maize") "maiz" else crop, "/", sep = "", collapse = NULL)

  ## Wheat setups
  setups <- list.dirs(dirCurrentCropInputs, full.names = T)
  #setups <- setups[1:4]
  setwd(dir_dssat_api)
  
  tryCatch(
    {
      source("00_run_dssat_edacap.r")
      source("dssat_scripts/01_load_inputs_setting_dssat.R")
      source("dssat_scripts/02_climate_functions_dssat.R")
      source("dssat_scripts/03_connect_georserver.R")
      source("dssat_scripts/03_create_xfile_dssat.R")
      source("dssat_scripts/04_run_dssat_model.R")
      source("dssat_scripts/05_get_outputs_dssat.R")
      source("dssat_scripts/06_stress_risk.R")
      source("dssat_scripts/07_land_preparation.R")
      source("dssat_scripts/08_phenological_phases.R")
      source("dssat_scripts/09_hazard_indicators_count_days.R")
      source("dssat_scripts/10_hazards_indicators_water_balance.R")

    },
    error = function(e) {
      
    }
  )

  tictoc::tic()
  mclapply(2:length(setups), function(i) {
    
    #tictoc::tic()
    current_conf <- setups[i]
    id <- gsub("/", "", str_split_fixed(current_conf, "/", n = 8)) # current scenarie/setup
    correction <- str_split_fixed(id[8], "_", n = 2)
    station <- gsub("/", "", correction[1]) # current climatic station
    id <- id[8]

    # Set up run paths
    current_dir_inputs_climate <- paste0(path_output, "/", station, "/")
    current_setup_dir <- paste0(dirCurrentCropInputs, id, "/")
    # if(currentCountry=="COLOMBIA" || (currentCountry=="ETHIOPIA" && crop == "wheat")){

      # skip_lines <- ifelse(currentCountry == "COLOMBIA", 2, ifelse(crop=="maize", 5, 4))
      # skip_cul <- read_lines(paste0(current_conf, "/", cul_file, ".CUL")) %>% str_detect("@VAR#") %>% which() +skip_lines
      # culFile <- read_lines(paste0(current_conf, "/", cul_file, ".CUL"))[skip_cul[1]]
      # cultivar <- strsplit(culFile, " ", fixed=T)
      # cultivar <- c(cultivar[[1]][1], cultivar[[1]][2])

      skip_cul <- readLines(paste0(current_conf, "/", cul_file, ".CUL"), warn=FALSE)
      excluded_characters <- c("^!", "^@", "^\\*")
      culFile <- skip_cul[!grepl(paste(excluded_characters, collapse = "|"), skip_cul)]
      cultivar <- strsplit(culFile, " ", fixed=T)
      cultivar <- c(cultivar[[1]][1], cultivar[[1]][2])
      
      skip_soil <- read_lines(paste0(current_conf, "/SOIL.SOL")) %>% str_detect("@SITE") %>% which() -1
      soilFile <- read_lines(paste0(current_conf, "/SOIL.SOL"))[skip_soil[1]]
      soil <- strsplit(soilFile, " ", fixed=T)
      soil <- substring(soil[[1]][1], 2)

      tryCatch({
        #tictoc::tic()
        run_crop_dssat(id, crop, current_dir_inputs_climate, current_setup_dir, 45, soil, cultivar)
        #tictoc::toc()

      }, error = function(e) {
        dir_outputs <- paste0(dirOutputs, "cultivos/", if (currentCountry == "COLOMBIA" && crop == "maize") "maiz" else crop, "/", sep = "", collapse = NULL)
        cat(conditionMessage(e))
        system(paste0("rm -R ",dir_outputs, id,"/*"))
      })
      
      # tictoc::toc()

    # } else {

    #   run_crop_dssat(id, crop, current_dir_inputs_climate, current_setup_dir, 45)
    #   #tictoc::toc()
    # }
    
    
  
  }, mc.cores = 8, mc.preschedule = F)
  tictoc::toc()

}

#This function merge all metrics and probabilities csv in one in order to import into the database
prepared_metrics_and_probabilities_csv <- function(){
  ## ProbForecats files lists for merging (For importation proccess)
  metrics_list <- list()
  probabilities_list <- list()

  ##
  for (c in seq(1:length(countries_ids))) {
    ## saving .csv probForecast for merging
    
    if(file.exists(paste0("/forecast/workdir/", names(countries_ids)[c], "/outputs/prediccionClimatica/probForecast", "/metrics.csv"))){
      metrics_list[[length(metrics_list) + 1]] <- read_csv(paste0("/forecast/workdir/", names(countries_ids)[c], "/outputs/prediccionClimatica/probForecast", "/metrics.csv"))
    }
    
    if(file.exists(paste0("/forecast/workdir/", names(countries_ids)[c], "/outputs/prediccionClimatica/probForecast", "/probabilities.csv"))){
      probabilities_list[[length(probabilities_list) + 1]] <- read_csv(paste0("/forecast/workdir/", names(countries_ids)[c], "/outputs/prediccionClimatica/probForecast", "/probabilities.csv"))
    }
    
    ## Copying outputs in common directory for importation into db process
    ## This copy must be done when a country has all its outputs finished
    file.copy(paste0("/forecast/workdir/", names(countries_ids)[c], "/outputs/"), dirUnifiedOutputs, recursive = TRUE)

  }

  ## Merging probForecast files
  probForecastUnifiedDir <- paste0(dirUnifiedOutputs, "outputs/prediccionClimatica/probForecast/")
  metricsMerged <- bind_rows(metrics_list)
  # Replacing NA values
  metricsMerged[is.na(metricsMerged)] <- 0
  write_csv(metricsMerged, paste0(probForecastUnifiedDir, "metrics.csv"))
  write_csv(bind_rows(probabilities_list), paste0(probForecastUnifiedDir, "probabilities.csv"))

}

## MAIN PATH
start.time <- Sys.time()
# dirCurrent <- "/forecast/workdir/usaid_procesos_interfaz/"
dirCurrent <- "/forecast/usaid_procesos_interfaz/"
dirWorkdir <- "/forecast/workdir/"
#dirWorkdir <- "D:/forecast_process/workdir/"
# forecastAppDll app - App de consola que se conecta a la base de datos
forecastAppDll <- paste0("dotnet ", dirCurrent, "forecast_app/CIAT.DAPA.USAID.Forecast.ForecastApp.dll ", sep = "", collapse = NULL)

## Global variables Forecast module
dirForecast <- paste0(dirCurrent, "prediccionClimatica/", sep = "", collapse = NULL)

## Global variables maize model module
dir_dssat <- "C:/DSSAT46/" ## its necessary to have the parameters .CUL, .ECO, .SPE Updated for running (calibrated the crop (Maize))
dirModeloMaiz <- paste0(dirCurrent, "modeloMaiz/", sep = "", collapse = NULL)

## Global variables rice model module
dirModeloArroz <- paste0(dirCurrent, "modeloArroz/", sep = "", collapse = NULL)

## Global variables Frijol model module
dir_dssat <- "C:/DSSAT46/" ## its necessary to have the parameters .CUL, .ECO, .SPE Updated for running (calibrated the crop (Frijol))
dirModeloFrijol <- paste0(dirCurrent, "modeloFrijol/", sep = "", collapse = NULL)

# Script that upload files to the Geoserver file system
dir_python_folder <- paste0(dirCurrent, "python/")
dir_pycpt_scripts <- paste0(dir_python_folder, "PyCPT/")
dir_upload_raster_script <- paste0(dir_python_folder, "UploadMosaics/src")
dir_upload_raster_layers <- paste0(dir_python_folder, "UploadMosaics/data/layers")

# Common directory to send data to Oryza API
dir_oryza_api_inputs <- paste0(dirWorkdir, "oryzaApiInputs/")
dir_oryza_api_inputs_zip <- paste0(dir_oryza_api_inputs, "inputs/")
dir_oryza_api_inputs_climate <- paste0(dir_oryza_api_inputs_zip, "climate/")
dir_oryza_api_inputs_setup <- paste0(dir_oryza_api_inputs_zip, "setups/")

# Commom directories DSSAT API
dir_dssat_api <- "/forecast/usaid_procesos_interfaz/dssat_API/"
# Common directories prepare observed data
dir_prepare_observed_data <- paste0(dirForecast,"prepareObservedData/")
#country iso
country_iso <- ifelse(currentCountry == "COLOMBIA", "co", ifelse(currentCountry == "ETHIOPIA", "et", ifelse(currentCountry == "ANGOLA", "ao", "gt")))
# Checks country for avoid conlicts
maize_name_by_country <- if (currentCountry == "COLOMBIA") "maiz" else "maize"
dirCurrent <- paste0(dirWorkdir, currentCountry, "/")
#Forecast Log
dirLogs <- paste0(dirCurrent,"logs/")
# INPUTS variables
dirInputs <- paste0(dirCurrent, "inputs/", sep = "", collapse = NULL)
# Input variables Forecast module
dirPrediccionInputs <- paste0(dirInputs, "prediccionClimatica/", sep = "", collapse = NULL)
dir_save <- paste0(dirPrediccionInputs, "descarga", sep = "", collapse = NULL)
dir_runCPT <- paste0(dirPrediccionInputs, "run_CPT", sep = "", collapse = NULL)
dir_response <- paste0(dirPrediccionInputs, "estacionesMensuales", sep = "", collapse = NULL)
dir_stations <- paste0(dirPrediccionInputs, "dailyData", sep = "", collapse = NULL)
dir_inputs_nextgen <- paste0(dirInputs, "NextGen/", sep = "", collapse = NULL)
dirCultivosInputs <- paste0(dirInputs, "cultivos/", sep = "", collapse = NULL)
# Input variables Maize model module
dirModeloMaizInputs <- paste0(dirInputs, "cultivos/", maize_name_by_country, "/", sep = "", collapse = NULL)
# Input variables rice model module
dirModeloArrozInputs <- paste0(dirInputs, "cultivos/arroz/", sep = "", collapse = NULL)
# Input variables rice model module
dirModeloWheatInputs <- paste0(dirInputs, "cultivos/wheat/", sep = "", collapse = NULL)
# Input variables frijol model module
dirModeloFrijolInputs <- paste0(dirInputs, "cultivos/frijol/", sep = "", collapse = NULL)
# OUTPUTS variables
dirUnifiedOutputs <- paste0(dirWorkdir, "unified_outputs", "/")
# Outputs NExtGen
dir_outputs_nextgen_seasonal <- paste0("/forecast/PyCPT/iri-pycpt/", paste0(currentCountry, "_seasonal"), "/output/")
dir_outputs_nextgen_subseasonal <- paste0("/forecast/PyCPT/iri-pycpt/", paste0(currentCountry, "_subseasonal"), "/output/")
dirOutputs <- paste0(dirCurrent, "outputs/", sep = "", collapse = NULL)
# Output variables Forecast module
dirPrediccionOutputs <- paste0(dirOutputs, "prediccionClimatica/", sep = "", collapse = NULL)
dirNextGen <- paste0(dirOutputs, "NextGen/", sep = "", collapse = NULL)
#Guatemala ETL rasters dir
dir_rasters_categories_guate <- paste0(dirPrediccionOutputs, "rasterCategories/", sep = "", collapse = NULL)
path_save <- paste0(dirPrediccionOutputs, "probForecast", sep = "", collapse = NULL)
path_rasters <- paste0(dirPrediccionOutputs, "raster", sep = "", collapse = NULL)
path_output <- paste0(dirPrediccionOutputs, "resampling", sep = "", collapse = NULL)
path_output_observed_data <- paste0(dirPrediccionOutputs, "observedData/", sep = "", collapse = NULL)
path_output_sum <- paste0(path_output, "/summary", sep = "", collapse = NULL)
dirCultivosOutputs <- paste0(dirOutputs, "cultivos/", sep = "", collapse = NULL)
# Output variables maize model module
dirModeloMaizOutputs <- paste0(dirOutputs, "cultivos/", maize_name_by_country, "/", sep = "", collapse = NULL)
# Output variables rice model module
dirModeloArrozOutputs <- paste0(dirOutputs, "cultivos/arroz/", sep = "", collapse = NULL)
# Output variables dssat wheat  module
dirModeloWheatOutputs <- paste0(dirOutputs, "cultivos/wheat/", sep = "", collapse = NULL)
# Output variables frijol model module
dirModeloFrijolOutputs <- paste0(dirOutputs, "cultivos/frijol/", sep = "", collapse = NULL)
# Output permanent folder
dirResults <- paste0(dirCurrent, "results")
# if (!file.exists(file.path(dirResults))){
#   dir.create(file.path(dirResults))
#   cat (paste0('\n... folder "',dirResults,'" created\n\n'))}
## ********************** Making inputs and outputs folders  **************************************
# INPUTS
pathConstruct(dirInputs) # ./inputs/
pathConstruct(dirLogs) # ./logs/
pathConstruct(dirPrediccionInputs) # ./inputs/prediccionClimatica/
pathConstruct(dir_save) # ./inputs/prediccionClimatica/descarga
pathConstruct(dir_runCPT) # ./inputs/prediccionClimatica/run_CPT
pathConstruct(dir_inputs_nextgen) # ./inputs/prediccionClimatica/NextGen
# Oryza API
pathConstruct(dir_oryza_api_inputs) # ./workdir/oryzaApiInputs/
pathConstruct(dir_oryza_api_inputs_zip) # ./workdir/oryzaApiInputs/inputs
pathConstruct(dir_oryza_api_inputs_climate) # ./workdir/oryzaApiInputs/inputs/climate
pathConstruct(dir_oryza_api_inputs_setup) # ./workdir/oryzaApiInputs/inputs/setups
# OUTPUTS
pathConstruct(dirUnifiedOutputs) # /unified_outputs/
pathConstruct(dirOutputs) # ./outputs/
pathConstruct(dirPrediccionOutputs) # ./outputs/prediccionClimatica/
pathConstruct(dir_rasters_categories_guate) # ./outputs/rasterCategpries/
pathConstruct(dirNextGen) # ./outputs/NextGen/
pathConstruct(path_save) # ./outputs/prediccionClimatica/probForecas
pathConstruct(path_output_observed_data) # ./outputs/prediccionClimatica/observedData
pathConstruct(path_rasters) # ./outputs/prediccionClimatica/raster
pathConstruct(path_output) # ./outputs/prediccionClimatica/resampling
pathConstruct(path_output_sum) # ./outputs/prediccionClimatica/resampling/summary
# Outoputs crop model
pathConstruct(dirCultivosOutputs) # ./outputs/cultivos/
# Maize
pathConstruct(dirModeloMaizOutputs) # ./outputs/cultivos/maiz/
# Rice
pathConstruct(dirModeloArrozOutputs) # ./outputs/cultivos/arroz/
# Wheat
pathConstruct(dirModeloWheatOutputs) # ./outputs/cultivos/wheat/
# Frijol
pathConstruct(dirModeloFrijolOutputs) # ./outputs/cultivos/frijol/
## ************************************************************************************************
## Download initial parameters from interface database
scriptsDir <- "/forecast/usaid_procesos_interfaz/"
setwd(paste0(scriptsDir, "forecast_app"))
# CMDdirInputs <- paste0(gsub("/","\\\\",dirPrediccionInputs), "\\\"")
CMDdirInputs <- dirPrediccionInputs
CMDdirCropsInputs <- dirInputs
objectIdCurrentCountry <- countries_ids[currentCountry]
pyCPTMonth <- month(Sys.Date())
dotnet_cmd <- c(
  paste0(forecastAppDll, "-out -cpt -p \"", CMDdirInputs, "\" -c \"", objectIdCurrentCountry, "\""),
  paste0(forecastAppDll, "-out -s \"prec\" -p \"", CMDdirInputs, "\" -start 1982 -end 2013 -c \"", objectIdCurrentCountry, "\""),
  paste0(forecastAppDll, "-out -wf -p \"", CMDdirInputs, "\" -name \"daily\" -c \"", objectIdCurrentCountry, "\""),
  paste0(forecastAppDll, "-out -co -p \"", CMDdirInputs, "\" -name \"daily\" -c \"", objectIdCurrentCountry, "\""),
  paste0(forecastAppDll, "-out -fs -p \"", CMDdirCropsInputs, "\" -c \"", objectIdCurrentCountry, "\""),
  paste0(forecastAppDll, "-out -py -p \"", dir_inputs_nextgen, "\" -c \"", objectIdCurrentCountry, "\" -m \"", pyCPTMonth, "\""),
  paste0(forecastAppDll, "-out -sub -p \"", dir_inputs_nextgen, "\" -c \"", objectIdCurrentCountry, "\" -m \"", pyCPTMonth, "\""),
  paste0(forecastAppDll, "-out -pyco -p \"", dir_inputs_nextgen, "\" -c \"", objectIdCurrentCountry, "\"")
)
print(dotnet_cmd)

if(extract_input_data){
  try(system(dotnet_cmd[1], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[2], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[3], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[4], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[5], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[6], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[7], intern = TRUE, ignore.stderr = TRUE))
  try(system(dotnet_cmd[8], intern = TRUE, ignore.stderr = TRUE))

}

#Downloading observed data for prepare climate scenaries in crops setups
if (currentCountry == "COLOMBIA" || currentCountry == "ETHIOPIA") {
   source(paste0(dir_prepare_observed_data, "downloadObservedData.R"))
   downloadObservedData(dir_stations, format(strptime(as.character(Sys.Date()), "%Y-%m-%d"),"%d/%m/%Y" ), path_output_observed_data, currentCountry)
}
# Prediction process
if (currentCountry == "COLOMBIA" || currentCountry == "ANGOLA" || currentCountry == "MALAWI" || currentCountry == "ZAMBIA" || currentCountry == "TANZANIA") {
  Sys.setenv(CPT_BIN_DIR = "/forecast/models/CPT/15.5.10/bin/")
  #source(paste(dirForecast, "01_prediccion.R", sep = "", collapse = NULL))
  source(paste(dirForecast, "01_aclimate_cpt.R", sep = "", collapse = NULL))
  } else if (currentCountry == "ETHIOPIA"){
  Sys.setenv(CPT_BIN_DIR = "/forecast/models/CPT/17.6.1/bin/")
  source(paste(dirForecast, "PyCPT_seasonal_outputs_process_R.r", sep = "", collapse = NULL))
  source(paste(dirForecast, "PyCPT_subseasonal_outputs_process_R.r", sep = "", collapse = NULL))
}

#Rasters upload
if (currentCountry == "GUATEMALA" || currentCountry == "ETHIOPIA") {
  # Import rasters to Geoserver
  source(paste0(dirForecast, "raster_upload.r"))
  uploadRasterFiles()

}

# Resampling process
runRemuestreo <- source(paste(dirForecast, "02_remuestreo.R", sep = "", collapse = NULL))
# Dowloading and final joining data process
runJoinFinalData <- source(paste(dirForecast, "03_join_wth_final.R", sep = "", collapse = NULL))

#new dssat module
if (currentCountry == "ETHIOPIA") {
  crop <- "wheat"
  runDssatModule(crop)
  crop <- "maize"
  runDssatModule(crop)
  
}

## Rice crop model process
if (currentCountry == "COLOMBIA") {
  ## Maize crop model process
  crop <- "maize"
  runDssatModule("maize")

  setups <- list.dirs(dirModeloArrozInputs, full.names = T)
  # Preparing inputs files
  if (prepare_setups_api_oryza(setups)) {
    # Making post request to oryza api
    run_oryza_by_setup()
  }
}

#Importation into db process
if(import_data_to_db){
  prepared_metrics_and_probabilities_csv()

  # Upload proccess results to interface database
  setwd(paste0(scriptsDir, "forecast_app"))
  CMDdirOutputs <- paste0(dirUnifiedOutputs, "outputs/") # paste0(gsub("/","\\\\",dirOutputs), "\\\"")
  #try(system(paste0(forecastAppDll, "-in -fs -cf 0.5 -p \"", CMDdirOutputs, "\""), intern = TRUE, ignore.stderr = TRUE))
  try(system(paste0(forecastAppDll, "-in -fs -cf 0.5 -p \"", CMDdirOutputs, "\"", " -frid \"", "648202f6a0488e3540a59e4e", "\""), intern = TRUE, ignore.stderr = TRUE))

}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
