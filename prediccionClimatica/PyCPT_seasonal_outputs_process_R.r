
# Reading json config file
setwd(dir_inputs_nextgen) # json files location
inputsPyCPT <- read_json("seasonal_pycpt.json")
inputsPyCPT

region <- paste0(currentCountry, "_seasonal")

spatial_predictors <- paste(inputsPyCPT[[1]]$spatial_predictors, collapse = " ")
spatial_predictors <- gsub(" ", ",", spatial_predictors)
typeof(spatial_predictors)
spatial_predictors

spatial_predictands <- paste(inputsPyCPT[[1]]$spatial_predictands, collapse = " ")
spatial_predictands <- gsub(" ", ",", spatial_predictands)
typeof(spatial_predictands)
spatial_predictands

models <- paste(inputsPyCPT[[1]]$models, collapse = " ")
models <- gsub(" ", ",", models)
models <- gsub("_", "-", models)
typeof(models)
models

obs <- inputsPyCPT[[1]]$obs
typeof(obs)
station <- inputsPyCPT[[1]]$station
typeof(station)
mos <- inputsPyCPT[[1]]$mos
typeof(mos)
predictand <- inputsPyCPT[[1]]$predictand
typeof(predictand)
predictor <- inputsPyCPT[[1]]$predictors
typeof(predictor)

mons <- paste(inputsPyCPT[[1]]$mons, collapse = " ")
mons <- gsub(" ", ",", mons)
typeof(mons)
mons

tgtii <- paste(inputsPyCPT[[1]]$tgtii, collapse = " ")
tgtii <- gsub(" ", ",", tgtii)
typeof(spatial_predictands)
tgtii

tgtff <- paste(inputsPyCPT[[1]]$tgtff, collapse = " ")
tgtff <- gsub(" ", ",", tgtff)
typeof(tgtff)
tgtff

tgts <- c()
for(i in 1:length(inputsPyCPT[[1]]$tgts)){
    tgts <- append(tgts, inputsPyCPT[[1]]$tgts[[i]][[1]])

}
tgts

years <- c()
for(i in 1:length(inputsPyCPT[[1]]$tgts)){
    years <- append(years, inputsPyCPT[[1]]$tgts[[i]][[2]])

}
years


tini <- inputsPyCPT[[1]]$tini
typeof(tini)
tend <- inputsPyCPT[[1]]$tend
typeof(tend)

xmodes_min <- inputsPyCPT[[1]]$xmodes_min
typeof(xmodes_min)
xmodes_max <- inputsPyCPT[[1]]$xmodes_max
typeof(xmodes_max)
ymodes_min <- inputsPyCPT[[1]]$ymodes_min
typeof(ymodes_min)
ymodes_max <- inputsPyCPT[[1]]$ymodes_max
typeof(ymodes_max)
ccamodes_min <- inputsPyCPT[[1]]$ccamodes_min
typeof(ccamodes_min)
ccamodes_max <- inputsPyCPT[[1]]$ccamodes_max
typeof(ccamodes_max)
force_download <- inputsPyCPT[[1]]$force_download
typeof(force_download)
single_models <- inputsPyCPT[[1]]$single_models
typeof(single_models)
forecast_anomaly <- inputsPyCPT[[1]]$forecast_anomaly
typeof(forecast_anomaly)
forecast_spi <- inputsPyCPT[[1]]$forecast_spi
typeof(forecast_spi)
confidence_level <- inputsPyCPT[[1]]$confidence_level
typeof(confidence_level)


setwd(dir_pycpt_scripts)
ru_forecast_type <- "seasonal"
# Running PyCPT
system(paste(
    "python run_main.py", ru_forecast_type, region, spatial_predictors, spatial_predictands,
    models, obs, station, mos, predictand, predictor, mons, tgtii,
    tgtff, tini, tend, xmodes_min, xmodes_max, ymodes_min,
    ymodes_max, ccamodes_min, ccamodes_max, force_download,
    single_models, forecast_anomaly, forecast_spi, confidence_level
))

#get years by season
get_season_years <- function(month, year){
  if(month==9 | month==10 | month==11) {
    return(c(as.numeric(year), as.numeric(year)+1))
  } else if (month==12) {
    return(c(as.numeric(year)+1, as.numeric(year)+1))
  } else {
    return(c(as.numeric(year), as.numeric(year)))
  }
}

quarter_name <- function(central_month) {
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  if (central_month < 1 || central_month > 12) {
    return("Invalid central month. It should be between 1 and 12.")
  }
  
  # Calculate the indices of the months in the quarter
  month1 <- ((central_month - 2) %% 12 + 12) %% 12 + 1
  month2 <- central_month
  month3 <- (central_month %% 12) + 1
  
  # Get the names of the months and format the quarter
  quarter <- paste(months[month1], months[month2], months[month3], sep = "-")
  return(quarter)
}


# Where outputs files of Pycpt are
#dir_outputs_nextgen_seasonal <- dir_outputs_nextgen_seasonal
fyr <- year(Sys.Date()) # Forecast year
dir_outputs_nextgen_seasonal=paste0("/root/EDACaP_Seasonal_fcst/", fyr, "/", tgts, "/53W-30E_to_0S-20N/output")#/EDACaP_Seasonal_fcst/2024/Feb-Apr/53W-30E_to_0S-20N/output/
setwd(dir_outputs_nextgen_seasonal)
dir.create(file.path(dir_outputs_nextgen_seasonal, "nc_files"))

models <- as.character(inputsPyCPT[[1]]$models)
MOS <- mos
PREDICTAND <- predictand
PREDICTOR <- predictor
monf <- inputsPyCPT[[1]]$mons[[1]] # Initialization month
#tgts <- as.character(inputsPyCPT[[1]]$tgts)
mons <- as.character(inputsPyCPT[[1]]$mons)

#nextGenFileName_prob <- paste0("NextGEN_", PREDICTAND, PREDICTOR, "_", MOS, "FCST_P_", tgts, "_", monf, years, ".nc")
nextGenFileName_prob <- paste0("NextGEN_", "probabilistic_", tgts, years, ".nc")
nextGenFileName_det <- paste0("MME_deterministic_forecast_", fyr,".nc")

stacksBySeason <- list()
monthsNumber <- list("Jan-Mar" = 02, "Feb-Apr" = 03, "Mar-May" = 04, "Apr-Jun" = 05, "May-Jul" = 06, "Jun-Aug" = 07, "Jul-Sep" = 08, "Aug-Oct" = 09, "Sep-Nov" = 10, "Oct-Dec" = 11, "Nov-Jan" = 12, "Dec-Feb" = 01)
trimesters <- list("Jan-Mar" = "jfm", "Feb-Apr" = "fma", "Mar-May" = "mam", "Apr-Jun" = "amj", "May-Jul" = "mjj", "Jun-Aug" = "jja", "Jul-Sep" = "jas", "Aug-Oct" = "aso", "Sep-Nov" = "son", "Oct-Dec" = "ond", "Nov-Jan" = "ndj", "Dec-Feb" = "djf")

# Writting probabilistic raster files (to upload to geoserver) and stacking (to create .csv files)
for (i in 1:length(nextGenFileName_prob)) {
    # It divides by 100 in orden to have a 0-1 data and not a 1-100
    dataNextGenAbove <- raster(paste0(dir_outputs_nextgen_seasonal[i], "/", nextGenFileName_prob[i]), varname = "Above_Normal") / 100
    dataNextGenBelow <- raster(paste0(dir_outputs_nextgen_seasonal[i], "/", nextGenFileName_prob[i]), varname = "Below_Normal") / 100
    dataNextGenNormal <- raster(paste0(dir_outputs_nextgen_seasonal[i], "/", nextGenFileName_prob[i]), varname = "Normal") / 100

    # Stack structure in order to extract to create .csv files
    stacksBySeason[[i]] <- stack(dataNextGenBelow, dataNextGenNormal, dataNextGenAbove)
}

# Writing probabilities.csv process
stations_coords <- read.table(paste0(dir_inputs_nextgen, "stations_coords.csv"), head = TRUE, sep = ",")
coords <- data.frame(stations_coords$lon, stations_coords$lat)
names(coords)[1:2] <- c("lon", "lat")
years <- get_season_years(month(Sys.Date()), fyr)

list_Prob_Forec <- list()

for (i in 1:length(stacksBySeason)) {
    stacksBySeasonCurrent <- stack(stacksBySeason[[i]])
    P_forecast_1 <- raster::extract(stacksBySeasonCurrent, coords)

    P_forecast_final <- data.frame(rep(years[i], nrow(coords)), rep(as.numeric(monthsNumber[tgts[i]]), nrow(coords)), 
    stations_coords[, 1], P_forecast_1, rep(quarter_name(as.numeric(monthsNumber[tgts[i]])), nrow(coords)), rep("prec", nrow(coords)))
    names(P_forecast_final)[1:8] <- c("year", "month", "id", "below", "normal", "above", "season", "predictand")

    list_Prob_Forec[[i]] <- P_forecast_final
}

list_Prob_Forec_new <- lapply(list_Prob_Forec, rbind)

list_Prob_Forec_new <- as.data.frame(list_Prob_Forec[[1]])

for (i in 2:length(list_Prob_Forec)) {
    list_Prob_Forec_new <- rbind(list_Prob_Forec_new, as.data.frame(list_Prob_Forec[[i]]))
}
# Writting probabilities csv
list_Prob_Forec_new <- na.omit(list_Prob_Forec_new)
write.table(list_Prob_Forec_new, paste0(path_save, "/probabilities.csv"), row.names = FALSE, sep = ",")


################################ Working on metrics.csv ####################################

ncMetricsFiles <- list()

for (i in 1:length(dir_outputs_nextgen_seasonal)) {

    metric2AFC <- raster(paste0(dir_outputs_nextgen_seasonal[i], "/", "MME_skill_scores.nc"), varname = "2afc")
    metricGROC <- raster(paste0(dir_outputs_nextgen_seasonal[i], "/", "MME_skill_scores.nc"), varname = "generalized_roc")
    metricIgnorance <- raster(paste0(dir_outputs_nextgen_seasonal[i], "/", "MME_skill_scores.nc"), varname = "ignorance")
    metricPearson <- raster(paste0(dir_outputs_nextgen_seasonal[i], "/", "MME_skill_scores.nc"), varname = "pearson")
    metricRPSS <- raster(paste0(dir_outputs_nextgen_seasonal[i], "/", "MME_skill_scores.nc"), varname = "rank_probability_skill_score")
    metricSpearman <- raster(paste0(dir_outputs_nextgen_seasonal[i], "/", "MME_skill_scores.nc"), varname = "spearman")


    # Stack structure in order to extract to create .csv files
    ncMetricsFiles[[i]] <- stack(metric2AFC, metricGROC, metricIgnorance, metricPearson, metricRPSS, metricSpearman)
}

## Extracting values of metrics by coords
metricsCoords <- matrix(NA, ncol = 3 + length(ncMetricsFiles[[1]][1]), nrow = nrow(coords) * length(tgts))

## Years and Months
for (i in 1:length(tgts)) {
    #Limits of each season
    ini <- (nrow(coords) * i) - (nrow(coords) - 1)
    end <- nrow(coords) * i

    #Years
    metricsCoords[ini:end, 1] <- rep(as.numeric(years[i]), nrow(coords))
    #Months
    metricsCoords[ini:end, 2] <- rep(as.numeric(monthsNumber[tgts[i]]), nrow(coords))
}

##Get metrics values for first quarter
metricsSeason1 = raster::extract(ncMetricsFiles[[1]], coords)
##Get metrics values for second quarter
metricsSeason2 = raster::extract(ncMetricsFiles[[2]], coords)
totalMetrics = do.call(rbind, list(metricsSeason1, metricsSeason2))
##Add metrics values of both season to final dataframe
metricsCoords[, 4:9] = totalMetrics

metricsCoords <- as.data.frame(metricsCoords)
names(metricsCoords)[1:ncol(metricsCoords)] <- c("year", "month", "id", "afc2", "groc", "ignorance", "pearson", "rpss", "spearman")

## Adding Ids to final dataframe
totalMonths <- unique(metricsCoords$month)
monthAuxList <- list()
for (i in 1:length(totalMonths)) {
    monthAuxList[[i]] <- subset(metricsCoords, month == totalMonths[i])
    monthAuxList[[i]]$id <- stations_coords$id
}
finalMetricsCsv <- as.data.frame(do.call(rbind, monthAuxList))
# Writting metrics csv
finalMetricsCsv <- na.omit(finalMetricsCsv)
write.table(finalMetricsCsv, paste0(path_save, "/metrics.csv"), row.names = FALSE, sep = ",")

################### end of writting metrics.csv ##########################