# pixel+0 - above
# pixel+100 - normal
# pixel+200 - below
# reclassification function for dominant raster file
rc <- function(x1, x2, x3) {
#   ifelse( x1 > x2, ifelse( x1 > x3, 0, 1), ifelse(x2 > x3, 2, 1) )
    ifelse( x1 > x2, ifelse( x1 > x3, x1, x3+200), ifelse(x2 > x3, x2+100, x3+200) )
}

# Some of the variables used in this function come from /prediccionClimatica/PyCPT_sub/seasonal_outputs_process_R.r
prepareRastersUpload <- function(ru_forecast_type) {
  require(readr)
  forecastID <- read_csv(paste0(dirUnifiedOutputs, "outputs/", "forecast.csv")) 
  forecastID <- forecastID$forecast_id

    #Seasonal
    if (ru_forecast_type == "seasonal"){
       # Writting deterministic raster files (to upload to geoserver)
      for (i in 1:length(nextGenFileName_det)) {
        det <- raster(paste0(datadir, "/", nextGenFileName_det[i]))
        monthFormat <- if (monthsNumber[tgts[i]] < 10) paste0("0", monthsNumber[tgts[i]]) else monthsNumber[tgts[i]]
        # Writting raster files in .tif
        writeRaster(det, paste0(dir_upload_raster_layers, "/deterministic/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", forecastID, "_", monf, "_", trimesters[tgts[i]], "_deterministic_", fyr, monthFormat, ".tif"))), overwrite = TRUE)
      }

      # Writting probabilistic and dominant raster files (to upload to geoserver)
      for (i in 1:length(nextGenFileName_prob)) {
        dataNextGenAbove <- raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname = "Above_Normal")
        dataNextGenNormal <- raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname = "Normal")
        dataNextGenBelow <- raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname = "Below_Normal")
        monthFormat <- if (monthsNumber[tgts[i]] < 10) paste0("0", monthsNumber[tgts[i]]) else monthsNumber[tgts[i]]

        # Writting probabilistic raster files in .tif
        writeRaster(dataNextGenAbove, paste0(dir_upload_raster_layers, "/above/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_above_", fyr, monthFormat, ".tif"))), overwrite = TRUE)
        writeRaster(dataNextGenNormal, paste0(dir_upload_raster_layers, "/normal/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_normal_", fyr, monthFormat, ".tif"))), overwrite = TRUE)
        writeRaster(dataNextGenBelow, paste0(dir_upload_raster_layers, "/below/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_below_", fyr, monthFormat, ".tif"))), overwrite = TRUE)

        # Writting dominant raster files in .tif
        dominantRasterFile <- overlay(stack(dataNextGenAbove, dataNextGenNormal, dataNextGenBelow), fun=rc)
        writeRaster(dominantRasterFile, paste0(dir_upload_raster_layers, "/seasonal_dominant/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_", trimesters[tgts[i]], "_dominant_", fyr, monthFormat, ".tif"))), overwrite = TRUE)
        

      }
    }
    #Subseasonal
    else {
       # Writting deterministic raster files (to upload to geoserver)
      for (i in 1:length(nextGenFileName_det_sub)) {
        det <- raster(paste0(datadir, "/", nextGenFileName_det_sub[i]))
        monthFormat <- if (month(Sys.Date()) < 10) paste0("0", month(Sys.Date())) else month(Sys.Date())
        writeRaster(det, paste0(dir_upload_raster_layers, "/deterministic/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_deterministic_", fyr, monthFormat, i, ".tif"))), overwrite = TRUE)
        
      }
      # Writting probabilistic raster files (to upload to geoserver)
      for (i in 1:length(nextGenFileName_prob_sub)) {
        dataNextGenAbove <- raster(paste0(datadir, "/", nextGenFileName_prob_sub[i]), varname = "Above_Normal")
        dataNextGenBelow <- raster(paste0(datadir, "/", nextGenFileName_prob_sub[i]), varname = "Below_Normal")
        dataNextGenNormal <- raster(paste0(datadir, "/", nextGenFileName_prob_sub[i]), varname = "Normal")
        monthFormat <- if (month(Sys.Date()) < 10) paste0("0", month(Sys.Date())) else month(Sys.Date())

        # Writting raster files in .tif
        writeRaster(dataNextGenAbove, paste0(dir_upload_raster_layers, "/above/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_above_", fyr, monthFormat, i, ".tif"))), overwrite = TRUE)
        writeRaster(dataNextGenBelow, paste0(dir_upload_raster_layers, "/below/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_below_", fyr, monthFormat, i, ".tif"))), overwrite = TRUE)
        writeRaster(dataNextGenNormal, paste0(dir_upload_raster_layers, "/normal/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_normal_", fyr, monthFormat, i, ".tif"))), overwrite = TRUE)

        # Writting dominant raster files in .tif
        dominantRasterFile <- overlay(stack(dataNextGenAbove, dataNextGenNormal, dataNextGenBelow), fun=rc)
        writeRaster(dominantRasterFile, paste0(dir_upload_raster_layers, "/subseasonal_dominant/", tolower(paste0(ru_forecast_type, "_", country_iso, "_", monf, "_dominant_", fyr, monthFormat, i, ".tif"))), overwrite = TRUE)
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
      ru_forecast_type <- "seasonal"
      prepareRastersUpload(ru_forecast_type)
      system(paste("python import.py", ru_workspace, ru_forecast_type))

      ru_forecast_type <- "subseasonal"
      prepareRastersUpload(ru_forecast_type)
      system(paste("python import.py", ru_workspace, ru_forecast_type))
    },
    error = function(e) {
      cat("Error while uploading rasters files: ", conditionMessage(e), "\n")
    }
  )
}
