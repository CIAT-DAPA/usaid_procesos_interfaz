
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

tgts <- paste(inputsPyCPT[[1]]$tgts, collapse = " ")
tgts <- gsub(" ", ",", tgts)
typeof(tgts)
tgts

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
    tgtff, tgts, tini, tend, xmodes_min, xmodes_max, ymodes_min,
    ymodes_max, ccamodes_min, ccamodes_max, force_download,
    single_models, forecast_anomaly, forecast_spi, confidence_level
))

# Where outputs files of Pycpt are
datadir <- dir_outputs_nextgen_seasonal
setwd(datadir)
dir.create(file.path(datadir, "nc_files"))

models <- as.character(inputsPyCPT[[1]]$models)
MOS <- mos
PREDICTAND <- predictand
PREDICTOR <- predictor
monf <- inputsPyCPT[[1]]$mons[[1]] # Initialization month
tgts <- as.character(inputsPyCPT[[1]]$tgts)
mons <- as.character(inputsPyCPT[[1]]$mons)

fyr <- year(Sys.Date()) # Forecast year

for (seas in tgts)
{
    #### translate all  output data to netcdf
    for (i in 1:length(models)) {
        # probablistics forecast
        ctl_input <- paste0(datadir, models[i], "_", PREDICTAND, PREDICTOR, "_CCAFCST_P_", seas, "_", monf, fyr, ".ctl")
        nc_output <- paste0(datadir, "nc_files/", models[i], "_", PREDICTAND, PREDICTOR, "_CCAFCST_P_", seas, "_", monf, fyr, ".nc")
        system(paste0("cdo -f nc import_binary ", ctl_input, " ", nc_output))
        # Deterministic forecast
        ctl_input2 <- paste0(datadir, models[i], "_", PREDICTAND, PREDICTOR, "_CCAFCST_mu_", seas, "_", monf, fyr, ".ctl")
        nc_output2 <- paste0(datadir, "nc_files/", models[i], "_", PREDICTAND, PREDICTOR, "_CCAFCST_mu_", seas, "_", monf, fyr, ".nc")
        system(paste0("cdo -f nc import_binary ", ctl_input2, " ", nc_output2))
    }

    system(paste0("cdo --no_history -ensmean  nc_files/*_CCAFCST_P_*.nc NextGEN_", PREDICTAND, PREDICTOR, "_", MOS, "FCST_P_", seas, "_", monf, fyr, ".nc"))
    system(paste0("ncrename -v a,Below_Normal -v b,Normal -v c,Above_Normal  NextGEN_", PREDICTAND, PREDICTOR, "_", MOS, "FCST_P_", seas, "_", monf, fyr, ".nc"))
    system(paste0("cdo --no_history -ensmean  nc_files/*_CCAFCST_mu_*.nc NextGEN_", PREDICTAND, PREDICTOR, "_", MOS, "FCST_mu_", seas, "_", monf, fyr, ".nc"))
    system(paste0("rm -rf ", datadir, "nc_files/*.nc"))
}

nextGenFileName_prob <- paste0("NextGEN_", PREDICTAND, PREDICTOR, "_", MOS, "FCST_P_", tgts, "_", monf, fyr, ".nc")
nextGenFileName_det <- paste0("NextGEN_", PREDICTAND, PREDICTOR, "_", MOS, "FCST_mu_", tgts, "_", monf, fyr, ".nc")

stacksBySeason <- list()
monthsNumber <- list("Jan-Mar" = 02, "Feb-Apr" = 03, "Mar-May" = 04, "Apr-Jun" = 05, "May-Jul" = 06, "Jun-Aug" = 07, "Jul-Sep" = 08, "Aug-Oct" = 09, "Sep-Nov" = 10, "Oct-Dec" = 11, "Nov-Jan" = 12, "Dec-Feb" = 01)
trimesters <- list("Jan-Mar" = "jfm", "Feb-Apr" = "fma", "Mar-May" = "mam", "Apr-Jun" = "amj", "May-Jul" = "mjj", "Jun-Aug" = "jja", "Jul-Sep" = "jas", "Aug-Oct" = "aso", "Sep-Nov" = "son", "Oct-Dec" = "ond", "Nov-Jan" = "ndj", "Dec-Feb" = "djf")

# Writting probabilistic raster files (to upload to geoserver) and stacking (to create .csv files)
for (i in 1:length(nextGenFileName_prob)) {
    # It divides by 100 in orden to have a 0-1 data and not a 1-100
       <- raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname = "Above_Normal") / 100
    dataNextGenBelow <- raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname = "Below_Normal") / 100
    dataNextGenNormal <- raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname = "Normal") / 100

    # Stack structure in order to extract to create .csv files
    stacksBySeason[[i]] <- stack(dataNextGenBelow, dataNextGenNormal, dataNextGenAbove)
}

#0-above
#1-normal
#2-below
# reclassification function
rc <- function(x1, x2, x3) {
#   ifelse( x1 > x2, ifelse( x1 > x3, 0, 1), ifelse(x2 > x3, 2, 1) )
    ifelse( x1 > x2, ifelse( x1 > x3, 2, 0), ifelse(x2 > x3, 1, 0) )
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

################################ Working on metrics.csv ####################################

################# .nc files metrics ##########################
# station location
# loc <- data.frame(
#     lon = c(38.90, 36.5),
#     lat = c(8.25, 7.764)
# )

##### NextGen skill matrix translator
skilmetrics <- c("2AFC", "GROC", "Ignorance", "Pearson", "RPSS", "Spearman")
metrics <- data.frame()
ncMetricsFiles <- list()
for (skill in skilmetrics) {
    for (seas in tgts) {
        ctlinput <- paste0("NextGen_", PREDICTAND, PREDICTOR, "_", MOS, "_", skill, "_", seas, "_", monf, ".ctl")
        ncout <- paste0("NextGen_", PREDICTAND, PREDICTOR, "_", MOS, "_", skill, "_", seas, "_", monf, ".nc")

        system(paste0("cdo -f nc -import_binary ", ctlinput, " ", ncout))
        ncMetricsFiles <- append(ncMetricsFiles, paste0("NextGen_", PREDICTAND, PREDICTOR, "_", MOS, "_", skill, "_", seas, "_", monf, ".nc"))
    }
}
################# .nc files metrics ##########################

################### working on writting metrics.csv ##########################
raster_metrics <- list()

## Organize raster stacks by metric
for (i in seq(from = 0, to = length(ncMetricsFiles), by = length(tgts))) {
    if (i != length(ncMetricsFiles)) {
        temp_raster_list <- list()
        for (j in 1:length(tgts)) {
            temp_raster_list[[j]] <- raster(ncMetricsFiles[[i + j]])
        }
        raster_metrics <- append(raster_metrics, stack(temp_raster_list))
    }
}

## Extracting values of metrics by coords
metricsCoords <- matrix(NA, ncol = 3 + length(raster_metrics), nrow = nrow(coords) * length(tgts))

## Year
metricsCoords[, 1] <- rep(fyr, nrow(coords) * length(tgts))
## Month
for (i in 1:length(tgts)) {
    ini <- (nrow(coords) * i) - (nrow(coords) - 1)
    end <- nrow(coords) * i
    metricsCoords[ini:end, 2] <- rep(as.numeric(monthsNumber[tgts[i]]), nrow(coords))
}

for (i in 1:length(raster_metrics)) {
    metricsCoords[, 3 + i] <- raster::extract(raster_metrics[[i]], coords)
}

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
write.table(finalMetricsCsv, paste0(path_save, "/metrics.csv"), row.names = FALSE, sep = ",")

################### end of writting metrics.csv ##########################
