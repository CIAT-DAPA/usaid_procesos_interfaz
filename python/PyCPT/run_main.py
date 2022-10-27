from PyCPT_seasonal import *
from PyCPT_subseasonal import *
import sys

ru_forecast_type = sys.argv[1]
print(ru_forecast_type)
if ru_forecast_type == 'seasonal':
        
    region = sys.argv[2]
    # Setting format
    spatial_predictors_string = sys.argv[3].split(",")
    spatial_predictors = []
    for i in range(0, len(spatial_predictors_string)):
        spatial_predictors.append(int(spatial_predictors_string[i]))

    print(type(spatial_predictors), spatial_predictors)

    spatial_predictands_string = sys.argv[4].split(",")
    spatial_predictands = []
    for i in range(0, len(spatial_predictands_string)):
        spatial_predictands.append(int(spatial_predictands_string[i]))

    print(type(spatial_predictands), spatial_predictands)
    models = sys.argv[5].split(",")
    print(type(models), models)
    obs = sys.argv[6]
    print(type(obs), obs)
    station = True if sys.argv[7] == 'TRUE' else False
    print(type(station), station)
    mos = sys.argv[8]
    print(type(mos), mos)
    predictand = sys.argv[9]
    print(type(predictand), predictand)
    predictor = sys.argv[10]
    print(type(predictor), predictor)
    mons = sys.argv[11].split(",")
    print(type(mons), mons)
    tgtii = sys.argv[12].split(",")
    print(type(tgtii), tgtii)
    tgtff = sys.argv[13].split(",")
    print(type(tgtff), tgtff)
    # tgts_j = sys.argv[14].split(",")
    # print(type(tgts_j), tgts_j)
    tini = int(sys.argv[14])
    print(type(tini), tini)
    tend = int(sys.argv[15])
    print(type(tend), tend)
    xmodes_min = int(sys.argv[16])
    print(type(xmodes_min), xmodes_min)
    xmodes_max = int(sys.argv[17])
    print(type(xmodes_max), xmodes_max)
    ymodes_min = int(sys.argv[18])
    print(type(ymodes_min), ymodes_min)
    ymodes_max = int(sys.argv[19])
    print(type(ymodes_max), ymodes_max)
    ccamodes_min = int(sys.argv[20])
    print(type(ccamodes_min), ccamodes_min)
    ccamodes_max = int(sys.argv[21])
    print(type(ccamodes_max), ccamodes_max)
    force_download = True if sys.argv[22] == 'TRUE' else False
    print(type(force_download), force_download)
    single_models = True if sys.argv[23] == 'TRUE' else False
    print(type(single_models), single_models)
    forecast_anomaly = True if sys.argv[24] == 'TRUE' else False
    print(type(forecast_anomaly), forecast_anomaly)
    forecast_spi = True if sys.argv[25] == 'TRUE' else False
    print(type(forecast_spi), forecast_spi)
    confidence_level = int(sys.argv[26])
    print(type(confidence_level), confidence_level)

    run_pycpt_seasonal(region, spatial_predictors, spatial_predictands, models, obs, station,
            mos, predictand, predictor, mons, tgtii, tgtff, tini, tend,
            xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min,
            ccamodes_max, force_download, single_models, forecast_anomaly,
            forecast_spi, confidence_level)

else:

    region = sys.argv[2]
    # Setting format
    spatial_predictors_string = sys.argv[3].split(",")
    spatial_predictors = []
    for i in range(0, len(spatial_predictors_string)):
        spatial_predictors.append(int(spatial_predictors_string[i]))

    print(type(spatial_predictors), spatial_predictors)

    spatial_predictands_string = sys.argv[4].split(",")
    spatial_predictands = []
    for i in range(0, len(spatial_predictands_string)):
        spatial_predictands.append(int(spatial_predictands_string[i]))

    print(type(spatial_predictands), spatial_predictands)
    models = sys.argv[5].split(",")
    print(type(models), models)
    obs = sys.argv[6]
    print(type(obs), obs)
    station = True if sys.argv[7] == 'TRUE' else False
    print(type(station), station)
    mos = sys.argv[8]
    print(type(mos), mos)
    predictand = sys.argv[9]
    print(type(predictand), predictand)
    predictor = sys.argv[10]
    print(type(predictor), predictor)
    mons = sys.argv[11].split(",")
    print(type(mons), mons)
    tgtii = sys.argv[12].split(",")
    print(type(tgtii), tgtii)
    tgtff = sys.argv[13].split(",")
    print(type(tgtff), tgtff)
    tgts = sys.argv[14].split(",")
    print(type(tgts), tgts)
    tini = int(sys.argv[15])
    print(type(tini), tini)
    tend = int(sys.argv[16])
    print(type(tend), tend)
    xmodes_min = int(sys.argv[17])
    print(type(xmodes_min), xmodes_min)
    xmodes_max = int(sys.argv[18])
    print(type(xmodes_max), xmodes_max)
    ymodes_min = int(sys.argv[19])
    print(type(ymodes_min), ymodes_min)
    ymodes_max = int(sys.argv[20])
    print(type(ymodes_max), ymodes_max)
    ccamodes_min = int(sys.argv[21])
    print(type(ccamodes_min), ccamodes_min)
    ccamodes_max = int(sys.argv[22])
    print(type(ccamodes_max), ccamodes_max)
    force_download = True if sys.argv[23] == 'TRUE' else False
    print(type(force_download), force_download)
    single_models = True if sys.argv[24] == 'TRUE' else False
    print(type(single_models), single_models)
    forecast_anomaly = True if sys.argv[25] == 'TRUE' else False
    print(type(forecast_anomaly), forecast_anomaly)
    forecast_spi = True if sys.argv[26] == 'TRUE' else False
    print(type(forecast_spi), forecast_spi)
    confidence_level = int(sys.argv[27])
    print(type(confidence_level), confidence_level)

    run_pycpt_subseasonal(region, spatial_predictors, spatial_predictands, models, obs, station,
            mos, predictand, predictor, mons, tgtii, tgtff, tgts, tini, tend,
            xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min,
            ccamodes_max, force_download, single_models, forecast_anomaly,
            forecast_spi, confidence_level)

