
# Reading json config file
setwd(dir_inputs_nextgen) # json files location
inputsPyCPT <- read_json("new_subseasonal_pycpt.json")


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
    "ipython EDACaP_S2S.ipynb", ini_mon, MOS, predictor_names, predictand_name, predictor_extent, predictand_extent, tailoring,
    cca_modes, x_eof_modes, y_eof_modes, scree
    ))

monf <- paste0(month(month(Sys.Date()), label=TRUE))# Initialization month
mon_fcst_ini <- paste0(monf,1)
#mon_fcst_ini <- paste0(monf,3)
weeks = c(1:4)

fyr <- year(Sys.Date()) # Forecast year
# Where outputs files of Pycpt are
dir_outputs_nextgen_subseasonal = paste0("/root/EDACaP_S2S/", fyr, "/", monf, "/31W-50E_to_-1S-18N/output")
setwd(dir_outputs_nextgen_subseasonal)
dir.create(file.path(dir_outputs_nextgen_subseasonal, "nc_files"))

models <- as.character(inputsPyCPT[[1]]$models)
#models <- c('ECMWF','CFSv2_SubX')
#MOS <- mos
MOS <- 'CCA'
PREDICTAND <- "PRCP"
PREDICTOR <- "PRCP"
nextGenFileName_prob_sub <- paste0("NextGEN_S2S_probablistic_", monf, fyr,"_Week",weeks,".nc")
nextGenFileName_det_sub <- paste0("NextGEN_S2S_deterministic","_", monf,"_Week",weeks,".nc")

stacksBySeason <- list()
monthsNumber <- list("Jan-Mar" = 02, "Feb-Apr" = 03, "Mar-May" = 04, "Apr-Jun" = 05, "May-Jul" = 06, "Jun-Aug" = 07, "Jul-Sep" = 08, "Aug-Oct" = 09, "Sep-Nov" = 10, "Oct-Dec" = 11, "Nov-Jan" = 12, "Dec-Feb" = 01)
trimesters <- list("Jan-Mar" = "jfm", "Feb-Apr" = "fma", "Mar-May" = "mam", "Apr-Jun" = "amj", "May-Jul" = "mjj", "Jun-Aug" = "jja", "Jul-Sep" = "jas", "Aug-Oct" = "aso", "Sep-Nov" = "son", "Oct-Dec" = "ond", "Nov-Jan" = "ndj", "Dec-Feb" = "djf")

# Writting probabilistic raster files (to upload to geoserver) and stacking (to create .csv files)
for (i in 1:length(nextGenFileName_prob_sub)) {
    # It divides by 100 in orden to have a 0-1 data and not a 1-100
    dataNextGenAbove <- raster(paste0(dir_outputs_nextgen_subseasonal, "/", nextGenFileName_prob_sub[i]), varname = "Above_Normal") / 100
    dataNextGenBelow <- raster(paste0(dir_outputs_nextgen_subseasonal, "/", nextGenFileName_prob_sub[i]), varname = "Below_Normal") / 100
    dataNextGenNormal <- raster(paste0(dir_outputs_nextgen_subseasonal, "/", nextGenFileName_prob_sub[i]), varname = "Normal") / 100

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

    P_forecast_final <- data.frame(rep(fyr, nrow(coords)), rep(i, nrow(coords)), rep(as.numeric(month(Sys.Date()), nrow(coords))), stations_coords[, 1], P_forecast_1)
    names(P_forecast_final)[1:7] <- c("year", "week", "month", "id", "below", "normal", "above")

    list_Prob_Forec[[i]] <- P_forecast_final
}

list_Prob_Forec_new <- lapply(list_Prob_Forec, rbind)

list_Prob_Forec_new <- as.data.frame(list_Prob_Forec[[1]])

for (i in 2:length(list_Prob_Forec)) {
    list_Prob_Forec_new <- rbind(list_Prob_Forec_new, as.data.frame(list_Prob_Forec[[i]]))
}
# Writting probabilities csv
list_Prob_Forec_new <- na.omit(list_Prob_Forec_new)
write.table(list_Prob_Forec_new, paste0(path_save, "/probabilities_subseasonal.csv"), row.names = FALSE, sep = ",")

################################ Working on metrics.csv ####################################

nextGenFileName_skills_sub <- paste0("NextGEN_S2S_skillscore_", monf, fyr,"_Week",c(1:4),".nc")
ncMetricsFiles <- list()

for (i in 1:length(nextGenFileName_skills_sub)) {

    metric2AFC <- raster(nextGenFileName_skills_sub, varname = "two_alternative_forced_choice")
    metricGROC <- raster(nextGenFileName_skills_sub, varname = "generalized_roc")
    metricIgnorance <- raster(nextGenFileName_skills_sub, varname = "ignorance")
    metricPearson <-raster(nextGenFileName_skills_sub, varname = "pearson")
    metricRPSS <- raster(nextGenFileName_skills_sub, varname = "rank_probability_skill_score")
    metricSpearman <- raster(nextGenFileName_skills_sub, varname = "spearman")


    # Stack structure in order to extract to create .csv files
    ncMetricsFiles[[i]] <- stack(metric2AFC, metricGROC, metricIgnorance, metricPearson, metricRPSS, metricSpearman)
}


## Extracting values of metrics by coords
metricsCoords <- matrix(NA, ncol = 4 + length(ncMetricsFiles[[1]][1]), nrow = nrow(coords) * length(weeks))

## Year
metricsCoords[, 1] <- rep(fyr, nrow(coords) * length(weeks))

## Month
for (i in 1:length(weeks)) {
    ini <- (nrow(coords) * i) - (nrow(coords) - 1)
    end <- nrow(coords) * i
    metricsCoords[ini:end, 3] <- rep(as.numeric(month(Sys.Date())), nrow(coords))
}

## Weeks
for (i in 1:length(weeks)) {
    ini <- (nrow(coords) * i) - (nrow(coords) - 1)
    end <- nrow(coords) * i
    metricsCoords[ini:end, 2] <- rep(i, nrow(coords))
}
##Get metrics values for each week
metricsWeek1 = raster::extract(ncMetricsFiles[[1]], coords)
metricsWeek2 = raster::extract(ncMetricsFiles[[2]], coords)
metricsWeek3 = raster::extract(ncMetricsFiles[[3]], coords)
metricsWeek4 = raster::extract(ncMetricsFiles[[4]], coords)

totalMetrics = do.call(rbind, list(metricsWeek1, metricsWeek2, metricsWeek3, metricsWeek4))
##Add metrics values of both season to final dataframe
metricsCoords[, 5:10] = totalMetrics

## Naming columns
metricsCoords <- as.data.frame(metricsCoords)
names(metricsCoords)[1:ncol(metricsCoords)] <- c("year", "week", "month", "id", "afc2", "groc", "ignorance", "pearson", "rpss", "spearman")

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
write.table(finalMetricsCsv, paste0(path_save, "/metrics_subseasonal.csv"), row.names = FALSE, sep = ",")

################### end of writting metrics.csv ##########################
