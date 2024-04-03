
# Reading json config file
setwd(dir_inputs_nextgen) # json files location
inputsPyCPT <- read_json("new_seasonal_pycpt.json")


region <- paste0(currentCountry, "_seasonal")

predictor_extent <- gsub(" ", ",", paste(inputsPyCPT$predictor_extent, collapse = " "))
#spatial_predictors <- gsub(" ", ",", spatial_predictors)
typeof(predictor_extent)
predictor_extent

predictand_extent <- gsub(" ", ",", paste(inputsPyCPT$predictand_extent, collapse = " "))
typeof(predictand_extent)
predictand_extent

predictor_names <- gsub(" ", ",", paste(inputsPyCPT$predictor_names, collapse = " "))
typeof(predictor_names)
predictor_names

predictand_name <- inputsPyCPT$predictand_name
typeof(predictand_name)
predictand_name

MOS <- inputsPyCPT$MOS

ini_mon <- inputsPyCPT$ini_mon

llow <- gsub(" ", ",", paste(inputsPyCPT$llow, collapse = " "))
llow
lhigh <- gsub(" ", ",", paste(inputsPyCPT$lhigh, collapse = " "))
lhigh

fcst_season <- gsub(" ", ",", paste(inputsPyCPT$fcst_season, collapse = " "))
typeof(fcst_season)

first_year <- inputsPyCPT$first_year
final_year <- inputsPyCPT$final_year

cca_modes <- gsub(" ", ",", paste(inputsPyCPT$cca_modes, collapse = " "))
cca_modes
tailoring <- inputsPyCPT$tailoring
x_eof_modes <- gsub(" ", ",", paste(inputsPyCPT$x_eof_modes, collapse = " "))
x_eof_modes
y_eof_modes <- gsub(" ", ",", paste(inputsPyCPT$y_eof_modes, collapse = " "))
y_eof_modes
scree <- inputsPyCPT$scree

#Calling seasonal script in python
setwd(dir_pycpt_scripts)
ru_forecast_type <- "seasonal"
# Running PyCPT
system(paste(
    "ipython EDACaP_seasonal_v2.ipynb", ini_mon, fcst_season, llow, lhigh,
    MOS, predictor_names, predictand_name, first_year, final_year, predictor_extent, predictand_extent, tailoring,
    cca_modes, x_eof_modes, y_eof_modes, scree
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

#Quarter in Month-Month-Month format
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

# Current years and seasons
fyr <- year(Sys.Date()) # Forecast year
years <- get_season_years(month(Sys.Date()), fyr)
tgts <- c()
for(i in 1:length(inputsPyCPT$fcst_season)){
    tgts <- append(tgts, paste0(inputsPyCPT$fcst_season[i]))

}
monf <- month(month(Sys.Date()), label=TRUE)
monthsNumber <- list("Jan-Mar" = 02, "Feb-Apr" = 03, "Mar-May" = 04, "Apr-Jun" = 05, "May-Jul" = 06, "Jun-Aug" = 07, "Jul-Sep" = 08, "Aug-Oct" = 09, "Sep-Nov" = 10, "Oct-Dec" = 11, "Nov-Jan" = 12, "Dec-Feb" = 01)
trimesters <- list("Jan-Mar" = "jfm", "Feb-Apr" = "fma", "Mar-May" = "mam", "Apr-Jun" = "amj", "May-Jul" = "mjj", "Jun-Aug" = "jja", "Jul-Sep" = "jas", "Aug-Oct" = "aso", "Sep-Nov" = "son", "Oct-Dec" = "ond", "Nov-Jan" = "ndj", "Dec-Feb" = "djf")
  
# Where outputs files of Pycpt are
dir_outputs_nextgen_seasonal=paste0("/root/EDACaP_Seasonal_fcst/", fyr, "/", tgts, "/53W-30E_to_0S-20N/output")#/EDACaP_Seasonal_fcst/2024/Feb-Apr/53W-30E_to_0S-20N/output/
#setwd(dir_outputs_nextgen_seasonal)
#dir.create(file.path(dir_outputs_nextgen_seasonal, "nc_files"))

#Output files names
nextGenFileName_prob <- paste0("NextGEN_", "probabilistic_", tgts, years, ".nc")
nextGenFileName_det <- paste0("MME_deterministic_forecast_", fyr,".nc")

#Getting rasters categories
stacksBySeason <- list()
monthsNumber <- list("Jan-Mar" = 02, "Feb-Apr" = 03, "Mar-May" = 04, "Apr-Jun" = 05, "May-Jul" = 06, "Jun-Aug" = 07, "Jul-Sep" = 08, "Aug-Oct" = 09, "Sep-Nov" = 10, "Oct-Dec" = 11, "Nov-Jan" = 12, "Dec-Feb" = 01)

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