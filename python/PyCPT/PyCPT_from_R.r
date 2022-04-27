
library("jsonlite")

#Reading json config file
setwd(dir_inputs_nextgen) #json files location
inputsPyCPT <- read_json("Structure ConfigurationPyCpt.json")
inputsPyCPT

region <- currentCountry

spatial_predictors <- paste(inputsPyCPT$spatial_predictors, collapse = " ")
spatial_predictors <- gsub(" ", ",", spatial_predictors)
spatial_predictors

spatial_predictands <- paste(inputsPyCPT$spatial_predictands, collapse = " ")
spatial_predictands <- gsub(" ", ",", spatial_predictands)
spatial_predictands

models <- paste(inputsPyCPT$models, collapse = " ")
models <- gsub(" ", ",", models)
models <- gsub("_", "-", models)
models

obs <- inputsPyCPT$obs
station <- inputsPyCPT$station
mos <- inputsPyCPT$mos
predictand <- inputsPyCPT$predictand
predictor <- inputsPyCPT$predictors

mons <- paste(inputsPyCPT$mons, collapse = " ")
mons <- gsub(" ", ",", mons)
mons

tgtii <- paste(inputsPyCPT$tgtii, collapse = " ")
tgtii <- gsub(" ", ",", tgtii)
tgtii

tgtff <- paste(inputsPyCPT$tgtff, collapse = " ")
tgtff <- gsub(" ", ",", tgtff)
tgtff

tgts <- paste(inputsPyCPT$tgts, collapse = " ")
tgts <- gsub(" ", ",", tgts)
tgts

tini <- inputsPyCPT$tini
tend <- inputsPyCPT$tend

xmodes_min <- inputsPyCPT$xmodes_min
xmodes_max <- inputsPyCPT$xmodes_max
ymodes_min <- inputsPyCPT$ymodes_min
ymodes_max <- inputsPyCPT$ymodes_max
ccamodes_min <- inputsPyCPT$ccamodes_min
ccamodes_max <- inputsPyCPT$ccamodes_max
force_download <- inputsPyCPT$force_download
single_models <- inputsPyCPT$single_models
forecast_anomaly <- inputsPyCPT$forecast_anomaly
forecast_spi <- inputsPyCPT$forecast_spi
confidence_level <- inputsPyCPT$confidence_level
ind_exec <- inputsPyCPT$ind_exec

setwd(dir_pycpt_scripts)
#Running PyCPT
system(paste("python run_main.py", region, spatial_predictors, spatial_predictands, 
             models, obs, station, mos, predictand, predictor, mons, tgtii, 
             tgtff, tgts, tini, tend, xmodes_min, xmodes_max, ymodes_min, 
             ymodes_max, ccamodes_min, ccamodes_max, force_download, 
             single_models, forecast_anomaly, forecast_spi, confidence_level, 
             ind_exec))

datadir <- dir_outputs_nextgen
setwd(datadir)
dir.create(file.path(datadir,"nc_files"))

models=as.character(inputsPyCPT$models)
MOS = mos
PREDICTAND = predictand
PREDICTOR = predictor
monf = 'Apr'	# Initialization month 
tgts = as.character(inputsPyCPT$tgts)
mons = as.character(inputsPyCPT$mons)

fyr=2022	# Forecast year

for (seas in tgts)
{
  #seas=tgts[1]
	#### translate all  output data to netcdf
	for (i in 1:length(models)) {
	  #probablistics forecast
	  ctl_input = paste0(datadir,models[i],"_",PREDICTAND,PREDICTOR,"_CCAFCST_P_",seas,"_",monf,fyr,".ctl")
	  nc_output = paste0(datadir,"nc_files/",models[i],"_",PREDICTAND,PREDICTOR,"_CCAFCST_P_",seas,"_",monf,fyr,".nc")
	  system(paste0("cdo -f nc import_binary ", ctl_input, " ", nc_output))
	  #Deterministic forecast
	  ctl_input2 = paste0(datadir,models[i],"_",PREDICTAND,PREDICTOR,"_CCAFCST_mu_",seas,"_",monf,fyr,".ctl")
	  nc_output2 = paste0(datadir,"nc_files/",models[i],"_",PREDICTAND,PREDICTOR,"_CCAFCST_mu_",seas,"_",monf,fyr,".nc")
	  system(paste0("cdo -f nc import_binary ", ctl_input2, " ", nc_output2))
	  
	}

	system(paste0("cdo --no_history -ensmean  nc_files/*_CCAFCST_P_*.nc NextGEN_",PREDICTAND,PREDICTOR,"_",MOS,"FCST_P_",seas,"_",monf,fyr,".nc"))
	system(paste0("ncrename -v a,Below_Normal -v b,Normal -v c,Above_Normal  NextGEN_",PREDICTAND,PREDICTOR,"_",MOS,"FCST_P_",seas,"_",monf,fyr,".nc"))
	system(paste0("cdo --no_history -ensmean  nc_files/*_CCAFCST_mu_*.nc NextGEN_",PREDICTAND,PREDICTOR,"_",MOS,"FCST_mu_",seas,"_",monf,fyr,".nc"))
	system(paste0("rm -rf ",datadir,"nc_files/*.nc"))
}

nextGenFileName_prob <- paste0("NextGEN_",PREDICTAND,PREDICTOR,"_",MOS,"FCST_P_",tgts,"_",monf,fyr,".nc")
nextGenFileName_det <- paste0("NextGEN_",PREDICTAND,PREDICTOR,"_",MOS,"FCST_mu_",tgts,"_",monf,fyr,".nc")

stacksBySeason <- list()
monthsNumber <- list("Jan-Mar"=2, "Feb-Apr"=3, "Mar-May"=4, "Apr-Jun"=5, "May-Jul"=6, "Jun-Aug"=7, "Jul-Sep"=8, "Aug-Oct"=9, "Sep-Nov"=10, "Oct-Dec"=11, "Nov-Jan"=12, "Dec-Feb"=1)
trimesters <- list("Jan-Mar"="jfm", "Feb-Apr"="fma", "Mar-May"="mam", "Apr-Jun"="amj", "May-Jul"="mjj", "Jun-Aug"="jja", "Jul-Sep"="jas", "Aug-Oct"="aso", "Sep-Nov"="son", "Oct-Dec"="ond", "Nov-Jan"="ndj", "Dec-Feb"="djf")

#Writting probabilistic raster files (to upload to geoserver) and stacking (to create .csv files) 
for(i in 1:length(nextGenFileName_prob)){
  dataNextGenAbove = raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname="Above_Normal")
  dataNextGenBelow = raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname="Below_Normal")
  dataNextGenNormal = raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname="Normal")

  #Writting raster files in .tif
  writeRaster(dataNextGenAbove, paste0(path_rasters, "/", tolower(paste0("seasonal_", currentCountry, "_" ,trimesters[tgts[i]], "_probabilistic_above_", monf, "_", fyr, ".tif"))), overwrite=TRUE)
  writeRaster(dataNextGenBelow, paste0(path_rasters, "/", tolower(paste0("seasonal_", currentCountry, "_" ,trimesters[tgts[i]], "_probabilistic_below_", monf, "_", fyr, ".tif"))), overwrite=TRUE)
  writeRaster(dataNextGenNormal, paste0(path_rasters, "/", tolower(paste0("seasonal_", currentCountry, "_" ,trimesters[tgts[i]], "_probabilistic_normal_", monf, "_", fyr, ".tif"))), overwrite=TRUE)

  #Stack structure in order to extract to create .csv files
  stacksBySeason [[i]] = stack(dataNextGenBelow, dataNextGenNormal, dataNextGenAbove)
}
#Writting deterministic raster files (to upload to geoserver)
for(i in 1:length(nextGenFileName_det)){
  firstModel <- raster(paste0(datadir, "/", nextGenFileName_det[i]))
  secondModel <- raster(paste0(datadir, "/", nextGenFileName_det[i]))

  #Writting raster files in .tif
  writeRaster(firstModel, paste0(path_rasters, "/", tolower(paste0("seasonal_", currentCountry, "_" ,trimesters[tgts[i]], "_deterministic_", monf, "_", fyr, ".tif"))), overwrite=TRUE)
  writeRaster(secondModel, paste0(path_rasters, "/", tolower(paste0("seasonal_", currentCountry, "_" ,trimesters[tgts[i]], "_deterministic_", monf, "_", fyr, ".tif"))), overwrite=TRUE)


}

# Writing probabilities.csv process
stations_coords <- read.table(paste0(dirPrediccionInputs, "NextGenPycptData/Ethiopia/stations_coords.csv"), head=TRUE, sep=",")
coords <- data.frame(stations_coords$lon, stations_coords$lat)
names (coords)[1:2] =c("lon", "lat")

list_Prob_Forec = list()

for( i in 1: length(stacksBySeason)){

  stacksBySeasonCurrent = stack(stacksBySeason[[i]])
  P_forecast_1= extract(stacksBySeasonCurrent, coords)

  P_forecast_final = data.frame(rep(fyr, nrow(coords)), rep(as.numeric(monthsNumber[tgts[i]]), nrow(coords)),stations_coords[,1],P_forecast_1) ##ciclo 018000912345
  names(P_forecast_final)[1:6]=c("year", "month", "id", "below", "normal", "above")

  list_Prob_Forec [[i]] = P_forecast_final 
}

list_Prob_Forec_new = lapply(list_Prob_Forec, rbind)#list_Prob_Forec [[1]]

list_Prob_Forec_new = as.data.frame(list_Prob_Forec[[1]])

for (i in 2:length(list_Prob_Forec)){

list_Prob_Forec_new = rbind(list_Prob_Forec_new, as.data.frame(list_Prob_Forec[[i]]))

}
#Writting probabilities csv
write.table(list_Prob_Forec_new, paste0(path_save, "/probabilities.csv"), row.names=FALSE, sep=",")


##Extracting a writing raster files
for(i in 1: length(stacksBySeason)){
  writeRaster(det, "prueba.tif") 
}

# system(paste0("cdo --no_history -ensmean  nc_files/*_CCAFCST_P_*.nc seasonal_", region, "_", trimesters[seas],PREDICTOR,"_",MOS,"FCST_P_",seas,"_",monf,fyr,".nc"))
# 	system(paste0("ncrename -v a,Below_Normal -v b,Normal -v c,Above_Normal  NextGEN_",PREDICTAND,PREDICTOR,"_",MOS,"FCST_P_",seas,"_",monf,fyr,".nc"))
# 	system(paste0("cdo --no_history -ensmean  nc_files/*_CCAFCST_mu_*.nc NextGEN_",PREDICTAND,PREDICTOR,"_",MOS,"FCST_mu_",seas,"_",monf,fyr,".nc"))




