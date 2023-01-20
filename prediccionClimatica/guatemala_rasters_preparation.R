
##Begin Functions
#Get two trimesters (based on current date)
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

download_insivumeh_probabilities_scenaries <- function(month, scenarie, file_name){

  #http://dl.insivumeh.gob.gt/SOURCES/.NextGen/.CPT/.Estacional/.CHIRPS/.Guatemala/.REALTIME/.NextGen/.Forecast/.Probabilities-Categories/C/(Sobre)/VALUE/S/755.0/VALUE/X/-92.41666/-88.11666/RANGEEDGES/Y/13.57528/17.87528/RANGEEDGES/%5BX/Y/%5D/data.tiff?filename=dataSobre20221201T0000.tiff
  urls <- paste0("http://dl.insivumeh.gob.gt/SOURCES/.NextGen/.CPT/.Estacional/.CHIRPS/.Guatemala/.REALTIME/.NextGen/.Forecast/.Probabilities-Categories/C/(",scenarie,")/VALUE/S/755.0/VALUE/X/-92.41666/-88.11666/RANGEEDGES/Y/13.57528/17.87528/RANGEEDGES/%5BX/Y/%5D/data.tiff?filename=", file_name, ".tiff")
  file <- basename(urls)
  path_guatemala_rasters_files <- paste0("/forecast/workdir/GUATEMALA/outputs/prediccionClimatica/rasterCategories","/",file_name, ".tif")
    download.file(urls, path_guatemala_rasters_files, mode = "w")

  # mclapply(1:length(urls), function(i) {

  #  }, mc.cores = 2, mc.preschedule = F)

}

##End functions




####Begin probabilities.csv process
#Reading rasters downloaded
stacksBySeason <- list()
monthsNumber <- list("Jan-Mar" = 02, "Feb-Apr" = 03, "Mar-May" = 04, "Apr-Jun" = 05, "May-Jul" = 06, "Jun-Aug" = 07, "Jul-Sep" = 08, "Aug-Oct" = 09, "Sep-Nov" = 10, "Oct-Dec" = 11, "Nov-Jan" = 12, "Dec-Feb" = 01)
for (i in 1:2) {

    # It divides by 100 in orden to have a 0-1 data and not a 1-100
    dataNextGenAbove <- raster(paste0("/forecast/workdir/GUATEMALA/outputs/prediccionClimatica/rasterCategories","/","above",i, ".tif")) / 100
    dataNextGenBelow <- raster(paste0("/forecast/workdir/GUATEMALA/outputs/prediccionClimatica/rasterCategories","/","below",i, ".tif")) / 100
    dataNextGenNormal <- raster(paste0("/forecast/workdir/GUATEMALA/outputs/prediccionClimatica/rasterCategories","/","normal",i, ".tif")) / 100

    # Stack structure in order to extract to create .csv files
    stacksBySeason[[i]] <- stack(dataNextGenBelow, dataNextGenNormal, dataNextGenAbove)
}

stations_coords <- read.table(paste0(dir_inputs_nextgen, "stations_coords.csv"), head = TRUE, sep = ",")
coords <- data.frame(stations_coords$lon, stations_coords$lat)
names(coords)[1:2] <- c("lon", "lat")
fyr <- year(Sys.Date())
tgts <- get_seasons()
years <- get_season_years(month(Sys.Date()), fyr)

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
####End probabilities.csv process
