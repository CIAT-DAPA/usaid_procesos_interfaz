# pixel+0 - above
# pixel+100 - normal
# pixel+200 - below
# reclassification function for dominant raster file
rc <- function(x1, x2, x3) {
#   ifelse( x1 > x2, ifelse( x1 > x3, 0, 1), ifelse(x2 > x3, 2, 1) )
    ifelse( x1 > x2, ifelse( x1 > x3, x1, x3+200), ifelse(x2 > x3, x2+100, x3+200) )
}

get_seasons <- function(){

  seasons <- c(paste0(month(month(Sys.Date()), label=TRUE), "-", month(month(Sys.Date() %m+% months(2)), label=TRUE)),
  paste0(month(month(Sys.Date() %m+% months(3)), label=TRUE), "-", month(month(Sys.Date() %m+% months(5)), label=TRUE))
  )
  return(seasons)

}

get_season_years <- function(month, year){
  if(month==9 | month==10 | month==11) {
    return(c(as.numeric(year), as.numeric(year)+1))
  } else if (month==12) {
    return(c(as.numeric(year)+1, as.numeric(year)+1))
  } else {
    return(c(as.numeric(year), as.numeric(year)))
  }
}


prepareRastersUploadGuate <- function(ru_forecast_type, paths_prob) {
  require(readr)
  #forecastID <- read_csv(paste0(dirUnifiedOutputs, "outputs/", "forecast.csv")) 
  #forecastID <- forecastID$forecast_id
  monf <- month(month(Sys.Date()), label=TRUE)
  monthsNumber <- list("Jan-Mar" = 02, "Feb-Apr" = 03, "Mar-May" = 04, "Apr-Jun" = 05, "May-Jul" = 06, "Jun-Aug" = 07, "Jul-Sep" = 08, "Aug-Oct" = 09, "Sep-Nov" = 10, "Oct-Dec" = 11, "Nov-Jan" = 12, "Dec-Feb" = 01)
  trimesters <- list("Jan-Mar" = "jfm", "Feb-Apr" = "fma", "Mar-May" = "mam", "Apr-Jun" = "amj", "May-Jul" = "mjj", "Jun-Aug" = "jja", "Jul-Sep" = "jas", "Aug-Oct" = "aso", "Sep-Nov" = "son", "Oct-Dec" = "ond", "Nov-Jan" = "ndj", "Dec-Feb" = "djf")
  
  fyr <- year(Sys.Date())
  tgts <- get_seasons()
  years <- get_season_years(month(Sys.Date()), fyr)
 

    #Seasonal
    if (ru_forecast_type == "seasonal"){
    # if(!is.null(paths_det)){
    #   # Writting deterministic raster files (to upload to geoserver)
    #   for (i in 1:length(paths_det)) {
    #     det <- raster(paste0(datadir, "/", paths_det[i]))
    #     monthFormat <- if (monthsNumber[tgts[i]] < 10) paste0("0", monthsNumber[tgts[i]]) else monthsNumber[tgts[i]]
    #     # Writting raster files in .tif
    #     writeRaster(det, paste0(dir_upload_raster_layers, "/deterministic/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_deterministic_", years[i], monthFormat, ".tif"))), overwrite = TRUE)
    #   }

    # }

      # Writting probabilistic and dominant raster files (to upload to geoserver)
      for (i in 1:2) {
        
        dataNextGenAbove <- raster(paste0(paths_prob,"above",i, ".tiff")) 
        dataNextGenBelow <- raster(paste0(paths_prob,"below",i, ".tiff")) 
        dataNextGenNormal <- raster(paste0(paths_prob,"normal",i, ".tiff"))

        monthFormat <- if (monthsNumber[tgts[i]] < 10) paste0("0", monthsNumber[tgts[i]]) else monthsNumber[tgts[i]]

        ## Set Coordinate Reference System (CRS) ##
        # crs(dataNextGenAbove) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        # crs(dataNextGenBelow) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        # crs(dataNextGenNormal) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

        # Writting probabilistic raster files in .tif
        writeRaster(dataNextGenAbove, paste0(dir_upload_raster_layers, "/above/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_above_", years[i], monthFormat, ".tif"))), overwrite = TRUE)
        writeRaster(dataNextGenNormal, paste0(dir_upload_raster_layers, "/normal/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_normal_", years[i], monthFormat, ".tif"))), overwrite = TRUE)
        writeRaster(dataNextGenBelow, paste0(dir_upload_raster_layers, "/below/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_below_", years[i], monthFormat, ".tif"))), overwrite = TRUE)

        # Writting dominant raster files in .tif
        dominantRasterFile <- overlay(stack(dataNextGenAbove, dataNextGenNormal, dataNextGenBelow), fun=rc)
        writeRaster(dominantRasterFile, paste0(dir_upload_raster_layers, "/seasonal_dominant/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_dominant_", years[i], monthFormat, ".tif"))), overwrite = TRUE)
        #writeRaster(dominantRasterFile, "/forecast/workdir/GUATEMALA/outputs/prediccionClimatica/rasterCategories/seasonal_gt_dec_mam_dominant_202304.tiff")

      }
    }
    
  # Copying raster in path_rasters (backup)
  # system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/above/ "), paste0(path_rasters, "/")))
  # system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/below/ "), paste0(path_rasters, "/")))
  # system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/normal/ "), paste0(path_rasters, "/")))
  # system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/deterministic/ "), paste0(path_rasters, "/")))
  # system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/seasonal_dominant/ "), paste0(path_rasters, "/")))
  # system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/subseasonal_dominant/ "), paste0(path_rasters, "/")))
}


# Some of the variables used in this function come from /prediccionClimatica/PyCPT_sub/seasonal_outputs_process_R.r
prepareRastersUpload <- function(ru_forecast_type) {
  ##Donwloading ETH shape for clipping the rasters
  countrySHP <- raster::getData('GADM', country='ETH', level=1) 
  fyr <- year(Sys.Date())
  tgts <- get_seasons()
  years <- get_season_years(month(Sys.Date()), fyr)

    #Seasonal
    if (ru_forecast_type == "seasonal"){
       # Writting deterministic raster files (to upload to geoserver)
      for (i in 1:length(nextGenFileName_det)) {
        det <- raster(paste0(dir_outputs_nextgen_seasonal, "/", nextGenFileName_det[i]))
        monthFormat <- if (monthsNumber[tgts[i]] < 10) paste0("0", monthsNumber[tgts[i]]) else monthsNumber[tgts[i]]
        # Writting raster files in .tif
        writeRaster(raster::mask(det,countrySHP), paste0(dir_upload_raster_layers, "/deterministic/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_deterministic_", years[i], monthFormat, ".tif"))), overwrite = TRUE)
      }

      # Writting probabilistic and dominant raster files (to upload to geoserver)
      for (i in 1:length(nextGenFileName_prob)) {
        dataNextGenAbove <- raster(paste0(dir_outputs_nextgen_seasonal, "/", nextGenFileName_prob[i]), varname = "Above_Normal")
        dataNextGenNormal <- raster(paste0(dir_outputs_nextgen_seasonal, "/", nextGenFileName_prob[i]), varname = "Normal")
        dataNextGenBelow <- raster(paste0(dir_outputs_nextgen_seasonal, "/", nextGenFileName_prob[i]), varname = "Below_Normal")
        monthFormat <- if (monthsNumber[tgts[i]] < 10) paste0("0", monthsNumber[tgts[i]]) else monthsNumber[tgts[i]]

        # Writting probabilistic raster files in .tif
        writeRaster(raster::mask(dataNextGenAbove,countrySHP), paste0(dir_upload_raster_layers, "/above/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_above_", years[i], monthFormat, ".tif"))), overwrite = TRUE)
        writeRaster(raster::mask(dataNextGenNormal,countrySHP), paste0(dir_upload_raster_layers, "/normal/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_normal_", years[i], monthFormat, ".tif"))), overwrite = TRUE)
        writeRaster(raster::mask(dataNextGenBelow,countrySHP), paste0(dir_upload_raster_layers, "/below/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_below_", years[i], monthFormat, ".tif"))), overwrite = TRUE)

        # Writting dominant raster files in .tif
        dominantRasterFile <- overlay(stack(dataNextGenAbove, dataNextGenNormal, dataNextGenBelow), fun=rc)
        writeRaster(raster::mask(dominantRasterFile,countrySHP), paste0(dir_upload_raster_layers, "/seasonal_dominant/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_dominant_", years[i], monthFormat, ".tif"))), overwrite = TRUE)
        

      }
    
    #Subseasonal
    } else {
       # Writting deterministic raster files (to upload to geoserver)
      for (i in 1:length(nextGenFileName_det_sub)) {
        det <- raster(paste0(dir_outputs_nextgen_subseasonal, "/", nextGenFileName_det_sub[i]))
        monthFormat <- if (month(Sys.Date()) < 10) paste0("0", month(Sys.Date())) else month(Sys.Date())
        writeRaster(raster::mask(det,countrySHP), paste0(dir_upload_raster_layers, "/deterministic/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_deterministic_", fyr, monthFormat, i, ".tif"))), overwrite = TRUE)
        
      }
      # Writting probabilistic raster files (to upload to geoserver)
      for (i in 1:length(nextGenFileName_prob_sub)) {
        dataNextGenAbove <- raster(paste0(dir_outputs_nextgen_subseasonal, "/", nextGenFileName_prob_sub[i]), varname = "Above_Normal")
        dataNextGenBelow <- raster(paste0(dir_outputs_nextgen_subseasonal, "/", nextGenFileName_prob_sub[i]), varname = "Below_Normal")
        dataNextGenNormal <- raster(paste0(dir_outputs_nextgen_subseasonal, "/", nextGenFileName_prob_sub[i]), varname = "Normal")
        monthFormat <- if (month(Sys.Date()) < 10) paste0("0", month(Sys.Date())) else month(Sys.Date())

        # Writting raster files in .tif
        writeRaster(raster::mask(dataNextGenAbove,countrySHP), paste0(dir_upload_raster_layers, "/above/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_above_", fyr, monthFormat, i, ".tif"))), overwrite = TRUE)
        writeRaster(raster::mask(dataNextGenBelow,countrySHP), paste0(dir_upload_raster_layers, "/below/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_below_", fyr, monthFormat, i, ".tif"))), overwrite = TRUE)
        writeRaster(raster::mask(dataNextGenNormal,countrySHP), paste0(dir_upload_raster_layers, "/normal/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_normal_", fyr, monthFormat, i, ".tif"))), overwrite = TRUE)

        # Writting dominant raster files in .tif
        dominantRasterFile <- overlay(stack(dataNextGenAbove, dataNextGenNormal, dataNextGenBelow), fun=rc)
        writeRaster(raster::mask(dominantRasterFile,countrySHP), paste0(dir_upload_raster_layers, "/subseasonal_dominant/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_dominant_", fyr, monthFormat, i, ".tif"))), overwrite = TRUE)
      }
    }

  # Copying raster in path_rasters (backup)
  system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/above/ "), paste0(path_rasters, "/")))
  system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/below/ "), paste0(path_rasters, "/")))
  system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/normal/ "), paste0(path_rasters, "/")))
  system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/deterministic/ "), paste0(path_rasters, "/")))
  system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/seasonal_dominant/ "), paste0(path_rasters, "/")))
  system(paste0("cp -R ", paste0(dir_upload_raster_layers, "/subseasonal_dominant/ "), paste0(path_rasters, "/")))
}

uploadRasterFiles <- function() {
  tryCatch(
    {
      setwd(dir_upload_raster_script)

      #ru - raster upload
      ru_workspace <- paste0("aclimate_", country_iso)

      if(currentCountry == "GUATEMALA"){
        ru_forecast_type <- "seasonal"
        paths_prob <- "/forecast/workdir/GUATEMALA/outputs/prediccionClimatica/rasterCategories/"
        prepareRastersUploadGuate(ru_forecast_type, paths_prob)
        system(paste("python import.py", ru_workspace, ru_forecast_type))
      }

      if(currentCountry == "ETHIOPIA"){
        ru_forecast_type <- "seasonal"
        prepareRastersUpload(ru_forecast_type)
        system(paste("python import.py", ru_workspace, ru_forecast_type))

        ru_forecast_type <- "subseasonal"
        prepareRastersUpload(ru_forecast_type)
        system(paste("python import.py", ru_workspace, ru_forecast_type))

      }
    },
    error = function(e) {
      cat("Error while uploading rasters files: ", conditionMessage(e), "\n")
    }
  )
}
