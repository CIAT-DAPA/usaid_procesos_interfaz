
#Reading json config file
setwd(dir_inputs_nextgen) #json files location
inputsPyCPT <- read_json("inputsPycpt.json")
inputsPyCPT

region <- currentCountry


spatial_predictors <- paste(inputsPyCPT[[1]][[1]]$spatial_predictors, collapse = " ")
spatial_predictors <- gsub(" ", ",", spatial_predictors)
typeof(spatial_predictors)
spatial_predictors

spatial_predictands <- paste(inputsPyCPT[[1]][[1]]$spatial_predictands, collapse = " ")
spatial_predictands <- gsub(" ", ",", spatial_predictands)
typeof(spatial_predictands)
spatial_predictands

models <- paste(inputsPyCPT[[1]][[1]]$models, collapse = " ")
models <- gsub(" ", ",", models)
models <- gsub("_", "-", models)
typeof(models)
models

obs <- inputsPyCPT[[1]][[1]]$obs
typeof(obs)
station <- inputsPyCPT[[1]][[1]]$station
typeof(station)
mos <- inputsPyCPT[[1]][[1]]$mos
typeof(mos)
predictand <- inputsPyCPT[[1]][[1]]$predictand
typeof(predictand)
predictor <- inputsPyCPT[[1]][[1]]$predictors
typeof(predictor)

mons <- paste(inputsPyCPT[[1]][[1]]$mons, collapse = " ")
mons <- gsub(" ", ",", mons)
typeof(mons)
mons

tgtii <- paste(inputsPyCPT[[1]][[1]]$tgtii, collapse = " ")
tgtii <- gsub(" ", ",", tgtii)
typeof(spatial_predictands)
tgtii

tgtff <- paste(inputsPyCPT[[1]][[1]]$tgtff, collapse = " ")
tgtff <- gsub(" ", ",", tgtff)
typeof(tgtff)
tgtff

tgts <- paste(inputsPyCPT[[1]][[1]]$tgts, collapse = " ")
tgts <- gsub(" ", ",", tgts)
typeof(tgts)
tgts

tini <- inputsPyCPT[[1]][[1]]$tini
typeof(tini)
tend <- inputsPyCPT[[1]][[1]]$tend
typeof(tend)

xmodes_min <- inputsPyCPT[[1]][[1]]$xmodes_min
typeof(xmodes_min)
xmodes_max <- inputsPyCPT[[1]][[1]]$xmodes_max
typeof(xmodes_max)
ymodes_min <- inputsPyCPT[[1]][[1]]$ymodes_min
typeof(ymodes_min)
ymodes_max <- inputsPyCPT[[1]][[1]]$ymodes_max
typeof(ymodes_max)
ccamodes_min <- inputsPyCPT[[1]][[1]]$ccamodes_min
typeof(ccamodes_min)
ccamodes_max <- inputsPyCPT[[1]][[1]]$ccamodes_max
typeof(ccamodes_max)
force_download <- inputsPyCPT[[1]][[1]]$force_download
typeof(force_download)
single_models <- inputsPyCPT[[1]][[1]]$single_models
typeof(single_models)
forecast_anomaly <- inputsPyCPT[[1]][[1]]$forecast_anomaly
typeof(forecast_anomaly)
forecast_spi <- inputsPyCPT[[1]][[1]]$forecast_spi
typeof(forecast_spi)
confidence_level <- inputsPyCPT[[1]][[1]]$confidence_level
typeof(confidence_level)


setwd(dir_pycpt_scripts)
#Running PyCPT
system(paste("python run_main.py", region, spatial_predictors, spatial_predictands, 
             models, obs, station, mos, predictand, predictor, mons, tgtii, 
             tgtff, tgts, tini, tend, xmodes_min, xmodes_max, ymodes_min, 
             ymodes_max, ccamodes_min, ccamodes_max, force_download, 
             single_models, forecast_anomaly, forecast_spi, confidence_level))

datadir <- dir_outputs_nextgen
setwd(datadir)
dir.create(file.path(datadir,"nc_files"))

models=as.character(inputsPyCPT[[1]][[1]]$models)
MOS = mos
PREDICTAND = predictand
PREDICTOR = predictor
monf = 'May'	# Initialization month 
tgts = as.character(inputsPyCPT[[1]][[1]]$tgts)
mons = as.character(inputsPyCPT[[1]][[1]]$mons)

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

#Writting probabilistic raster files (to upload to geoserver) and stacking (to create .csv files) 
for(i in 1:length(nextGenFileName_prob)){
  dataNextGenAbove = raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname="Above_Normal")
  dataNextGenBelow = raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname="Below_Normal")
  dataNextGenNormal = raster(paste0(datadir, "/", nextGenFileName_prob[i]), varname="Normal")

  #Stack structure in order to extract to create .csv files
  stacksBySeason [[i]] = stack(dataNextGenBelow, dataNextGenNormal, dataNextGenAbove)
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

################################ Working on metrics.csv ####################################

#station location
loc = data.frame(lon = c(38.90,36.5),
                 lat=c(8.25, 7.764))

#setwd("/forecast/PyCPT/iri-pycpt/Ethiopia/output/")
##### NextGen skill matrix translator
skilmetrics <- c("2AFC","GROC","Ignorance","Pearson","RPSS","Spearman")
MOS='CCA'
PREDICTAND='PRCP'
PREDICTOR='PRCP'
tgts=c('May-Jul', 'Aug-Oct')
#lismonf='May'	# Initialization month 
metrics <- data.frame()
ncMetricsFiles <- list()
for (skill in skilmetrics){
	for (seas in tgts){
		
	ctlinput <- paste0("NextGen_",PREDICTAND,PREDICTOR,"_",MOS,"_",skill,"_", seas,"_",monf,".ctl")
	ncout <- paste0("NextGen_",PREDICTAND,PREDICTOR,"_",MOS,"_",skill,"_", seas,"_",monf,".nc")	

	system(paste0("cdo -f nc -import_binary ", ctlinput, " ", ncout))
	ncMetricsFiles <- append(ncMetricsFiles, paste0("NextGen_",PREDICTAND,PREDICTOR,"_",MOS,"_",skill,"_", seas,"_",monf,".nc"))
	  
	}
}

#ncMetricsFiles <- paste0("NextGen_",PREDICTAND,PREDICTOR,"_",MOS,"_",skilmetrics,"_", tgts,"_",monf,".nc")

list_Prob_Forec = list()
raster_metrics = list()


#rastersMetrics <- lapply(ncMetricsFiles, raster)

## Organize raster stacks by metric
for(i in seq(from=0, to=length(ncMetricsFiles), by=length(tgts))){
	
	#i=0
	if(i!=length(ncMetricsFiles)){
		temp_raster_list <- list()
		for(j in 1: length(tgts)){
			temp_raster_list[[j]] = raster(ncMetricsFiles[[i+j]])
			print(j)
			
		}
		raster_metrics = append(raster_metrics, stack(temp_raster_list))

	}

}

metricsCoords = lapply(raster_metrics, extract(raster_metrics, coords))
for(i in length(raster_metrics)){

  P_forecast_final = data.frame(rep(fyr, nrow(coords)), rep(as.numeric(monthsNumber[tgts[i]]), nrow(coords)),stations_coords[,1],P_forecast_1) ##ciclo 018000912345
  names(P_forecast_final)[1:6]=c("year", "month", "id", "below", "normal", "above")

  list_Prob_Forec [[i]] = P_forecast_final 

}



list_Prob_Forec_new = lapply(listMetricsForec, rbind)#list_Prob_Forec [[1]]

list_Prob_Forec_new = as.data.frame(listMetricsForec[[1]])

for (i in 2:length(listMetricsForec)){

list_Prob_Forec_new = rbind(list_Prob_Forec_new, as.data.frame(listMetricsForec[[i]]))

}
#Writting probabilities csv
write.table(list_Prob_Forec_new, paste0(path_save, "/probabilities.csv"), row.names=FALSE, sep=",")