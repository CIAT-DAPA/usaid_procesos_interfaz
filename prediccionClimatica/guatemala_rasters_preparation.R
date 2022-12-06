download_insivumeh_probabilities_scenaries <- function(month, scenarie, file_name){

  #urls <- paste("https://data.chc.ucsb.edu/products/CHIRP/daily/",year_to,"/chirp.",fechas,".tif",sep="") Esta ruta con https pone problemas en el servidor de linux
  urls <- paste0("http://dl.insivumeh.gob.gt/SOURCES/.NextGen/.CPT/.Estacional/.CHIRPS/.Guatemala/.REALTIME/.NextGen/.Forecast/.Probabilities-Categories/C/(",scenarie,")/VALUE/S/751.0/VALUE/X/-92.65/-88.05/RANGEEDGES/Y/13.60167/17.90167/RANGEEDGES/%5BX/Y/%5D/palettecolor.tiff?filename=", file_name, ".tiff")
  file <- basename(urls)
  path_guatemala_rasters_files <- paste0("/forecast/workdir/GUATEMALA/outputs/prediccionClimatica/rasterCategories","/",file_name, ".tif")
    download.file(urls, path_guatemala_rasters_files, mode = "w")

  # mclapply(1:length(urls), function(i) {

  #  }, mc.cores = 2, mc.preschedule = F)

}

for (i in 1:length(nextGenFileName_prob)) {
    # It divides by 100 in orden to have a 0-1 data and not a 1-100
    dataNextGenAbove <- raster(paste0("/forecast/workdir/GUATEMALA/outputs/prediccionClimatica/rasterCategories","/","above", ".tif")) / 250
    dataNextGenBelow <- raster(paste0("/forecast/workdir/GUATEMALA/outputs/prediccionClimatica/rasterCategories","/","below", ".tif"))
    dataNextGenNormal <- raster(paste0("/forecast/workdir/GUATEMALA/outputs/prediccionClimatica/rasterCategories","/","normal", ".tif"))

    # Stack structure in order to extract to create .csv files
    stacksBySeason[[i]] <- stack(dataNextGenBelow, dataNextGenNormal, dataNextGenAbove)
}


# Writing probabilities.csv process
stations_coords <- read.table(paste0(dir_inputs_nextgen, "stations_coords.csv"), head = TRUE, sep = ",")
coords <- data.frame(stations_coords$lon, stations_coords$lat)
names(coords)[1:2] <- c("lon", "lat")

list_Prob_Forec <- list()

for (i in 1:length(stacksBySeason)) {
    stacksBySeasonCurrent <- stack(stacksBySeason[[i]])
    P_forecast_1 <- raster::extract(stacksBySeasonCurrent, coords)

    P_forecast_final <- data.frame(rep(fyr, nrow(coords)), rep(as.numeric(monthsNumber[tgts[i]]), nrow(coords)), stations_coords[, 1], P_forecast_1)
    names(P_forecast_final)[1:6] <- c("year", "month", "id", "below", "normal", "above")

    list_Prob_Forec[[i]] <- P_forecast_final
}

list_Prob_Forec_new <- lapply(list_Prob_Forec, rbind)

list_Prob_Forec_new <- as.data.frame(list_Prob_Forec[[1]])

for (i in 2:length(list_Prob_Forec)) {
    list_Prob_Forec_new <- rbind(list_Prob_Forec_new, as.data.frame(list_Prob_Forec[[i]]))
}
# Writting probabilities csv
write.table(list_Prob_Forec_new, paste0(path_save, "/probabilities.csv"), row.names = FALSE, sep = ",")
