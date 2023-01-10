#!/usr/bin/env python
# coding: utf-8

# # PyCPT Version 2
# 
# This is an example of a PyCPT Version 2 seasonal climate forecasting workflow. This notebook can be adapted to suit your exact needs through modifications of the code. This notebook uses PyCPT v2 utilities to 
# 
# 1. download data from the IRI Data Library (through the CPT-DL python library) 
# 2. Run bias-correction using the IRI Climate Predictability Tool (through its companion python library, CPT-CORE) 
# 3. Plot skills scores and spatial loadings
# 4. Produce a multi-model ensemble forecast by taking the simple average of the bias-corrected members
# 5. Plots skill scores, deterministic forecasts, probabilistic forecasts, and exceedance probabilities for this NextGen MME forecast. 
# 
# PyCPT Version 2 was primarily designed and implemented by Kyle Hall
# 
import cptdl as dl 
import cptio as cio 
import cptcore as cc 
import cptextras as ce 
import xarray as xr 
import datetime as dt 
from pathlib import Path 
import matplotlib.pyplot as plt 
import cartopy.crs as ccrs
import numpy as np
import cartopy.feature as cartopyFeature
from cartopy.io.shapereader import Reader
from matplotlib.offsetbox import AnchoredText
from dateutil.relativedelta import relativedelta
import calendar
import os

# Function to prepare forecast season for EDACaP
def months_short(inputdate):
    return inputdate.strftime('%B')[0:3]



                
# Create a feature for States/Admin 1 regions at 1:50m from Natural Earth
states_provinces = cartopyFeature.NaturalEarthFeature(category='cultural',name='admin_0_countries',scale='50m', facecolor='none')
fname = '/forecast/PyCPT/iri-pycpt/eth_shp/gadm41_ETH_1.shp'
shape_feature = cartopyFeature.ShapelyFeature(Reader(fname).geometries(),
                                    ccrs.PlateCarree(), facecolor='none',linewidth=0.5, linestyle='-', zorder=5, alpha=0.8)

current_date = dt.date.today()
fcst_date = dt.datetime(current_date.year, current_date.month,1)
season1_start = current_date + relativedelta(months=0)
season1_end = current_date + relativedelta(months=2)
season2_start = current_date + relativedelta(months=3)
season2_end = current_date + relativedelta(months=5)
# Target seasons and related parameters
target_season = [str(months_short(season1_start)+'-' + months_short(season1_end)) +
            '', '' + str(months_short(season2_start)+'-' + months_short(season2_end))]
tgtii=['0.5','2.5']  #S: start for the DL
tgtff=['3.5','5.5']   #S: end for the DL
mons = [current_date.strftime('%B')[0:3] + '','' + current_date.strftime('%B')[0:3]]
# for ii in target_season: 
    
SOURCE = 'EDACaP-IRI NextGEN fcst:'
LICENSE = str(current_date.year) + ' ' + target_season[0]
text = AnchoredText('\u00A9 {}; Season: {}'
                        ''.format(SOURCE, LICENSE),
                        loc=4, prop={'size': 10}, frameon=False)
###### Loop for each target season
for ii in range(len(target_season)): 
        
    # #### Define Working directory for each season  
    caseDir = "pycpt_EDACaP_" + target_season[ii] + "_start_" + mons[ii]
    # #### Parameters - This cell defines the parameters of your analysis
    case_directory = Path.home() / "Desktop" / caseDir
    case_directory.mkdir(exist_ok=True, parents=True)
    MOS='CCA' # must be one of 'CCA', 'PCR', or "None"
    predictor_names = ['SEAS5.PRCP', 'CFSv2.PRCP', 'CanSIPSIC3.PRCP']
    predictand_name = 'UCSB.PRCP'
    # use dl.observations.keys() to see all options for predictand 
    # and dl.hindcasts.keys() to see all options for predictors
    # make sure your first_year & final_year are compatible with 
    # your selections for your predictors and predictands 
    download_args = { 
    #'fdate': dt.datetime(2022, 5, 1),  
    'fdate': dt.datetime(current_date.year, current_date.month,1),  
    'first_year': 1982, 
    'final_year': 2016, 
    'predictor_extent': {
        'east': 28,
        'west': 52, 
        'north': 20, 
        'south': 0
    }, 
    'predictand_extent': {
        'east': 32,
        'west': 49, 
        'north': 16, 
        'south': 3
    }, 
    #'lead_low': 1.5,
    'lead_low': tgtii[ii],  
    #'lead_high': 4.5, 
    'lead_high': tgtff[ii],  
    #'target': 'Jun-Sep',
    'target': target_season[ii],  
    'filetype': 'cptv10.tsv'
    }
    cpt_args = { 
        'transform_predictand': None,  # transformation to apply to the predictand dataset - None, 'Empirical', 'Gamma'
        'tailoring': None,  # tailoring None, Anomaly, StdAnomaly, or SPI (SPI only available on Gamma)
        'cca_modes': (1,3), # minimum and maximum of allowed CCA modes 
        'x_eof_modes': (1,8), # minimum and maximum of allowed X Principal Componenets 
        'y_eof_modes': (1,6), # minimum and maximum of allowed Y Principal Components 
        'validation': 'crossvalidation', # the type of validation to use - crossvalidation, retroactive, or doublecrossvalidation
        'drymask': True, #whether or not to use a drymask of -999
        'scree': True, # whether or not to save % explained variance for eof modes
        'crossvalidation_window': 5,  # number of samples to leave out in each cross-validation step 
        'synchronous_predictors': True, # whether or not we are using 'synchronous predictors'
    }
    force_download = True
    #extracting domain boundaries and create house keeping
    domain = download_args['predictor_extent']
    e,w,n,s = domain.values()
    domainFolder = str(w)+"W-" + str(e)+"E" +'_to_'+ str(s)+"S-" + str(n)+"N"

    domainDir = Path("/forecast/PyCPT/iri-pycpt/") / caseDir / domainFolder
    domainDir.mkdir(exist_ok=True, parents=True)

    dataDir = Path("/forecast/PyCPT/iri-pycpt/") / caseDir / domainFolder / "data"
    dataDir.mkdir(exist_ok=True, parents=True)

    figDir = Path("/forecast/PyCPT/iri-pycpt/") / caseDir / domainFolder / "figures"
    figDir.mkdir(exist_ok=True, parents=True)

    outputDir =Path("/forecast/PyCPT/iri-pycpt/") / caseDir / domainFolder / "output"
    outputDir.mkdir(exist_ok=True, parents=True)

    # Uncomment the following line & change the config filepath to save this configuration: 
    config_file = ce.save_configuration(caseDir+'.config', download_args, cpt_args, MOS, predictor_names, predictand_name )
    # Uncomment the following line & change the config filepath to load an existing configuration: 
    #MOS, download_args, cpt_args, predictor_names, predictand_name = ce.load_configuration('test1.config')
    # #### Download Observations
    if not Path(dataDir / '{}.nc'.format(predictand_name)).is_file() or force_download:
        Y = dl.download(dl.observations[predictand_name], dataDir / (predictand_name +'.tsv'), **download_args, verbose=True, use_dlauth=False)
        Y = getattr(Y, [i for i in Y.data_vars][0])
        Y.to_netcdf(dataDir / '{}.nc'.format(predictand_name))
    else:
        Y = xr.open_dataset(dataDir / '{}.nc'.format(predictand_name))
        Y = getattr(Y, [i for i in Y.data_vars][0])
    # #### Download Hindcast Data
    # download training data 
    hindcast_data = []
    for model in predictor_names: 
        if not Path(dataDir / (model + '.nc')).is_file() or force_download:
            X = dl.download(dl.hindcasts[model],dataDir / ( model+'.tsv'), **download_args, verbose=True, use_dlauth=False)
            X = getattr(X, [i for i in X.data_vars][0])
            X.name = Y.name
            X.to_netcdf(dataDir / '{}.nc'.format(model))
        else:
            X = xr.open_dataset(dataDir / (model + '.nc'))
            X = getattr(X, [i for i in X.data_vars][0])
            X.name = Y.name
        hindcast_data.append(X)
    # #### Download Forecast Data
    # download forecast data 
    forecast_data = []
    for model in predictor_names: 
        if not Path(dataDir / (model + '_f.nc')).is_file() or force_download:
            F = dl.download(dl.forecasts[model], dataDir / (model+'_f.tsv'), **download_args, verbose=True, use_dlauth=False)
            F = getattr(F, [i for i in F.data_vars][0])
            F.name = Y.name
            F.to_netcdf(dataDir / (model + '_f.nc'))
        else:
            F = xr.open_dataset(dataDir / (model + '_f.nc'))
            F = getattr(F, [i for i in F.data_vars][0])
            F.name = Y.name
        forecast_data.append(F)
    # #### Perform Analysis 
    hcsts, fcsts, skill, pxs, pys = [], [], [], [], []
    for i, model_hcst in enumerate(hindcast_data):
        
        
        if str(MOS).upper() == 'CCA':
            
            # fit CCA model between X & Y and produce real-time forecasts for F 
            cca_h, cca_rtf, cca_s, cca_px, cca_py = cc.canonical_correlation_analysis(model_hcst, Y, \
            F=forecast_data[i], **cpt_args, cpt_kwargs={"interactive": False})
            # fit CCA model again between X & Y, and produce in-sample probabilistic hindcasts 
            # this is using X in place of F, with the year coordinates changed to n+100 years
            # because CPT does not allow you to make forecasts for in-sample data
            cca_h, cca_f, cca_s, cca_px, cca_py = cc.canonical_correlation_analysis(model_hcst, Y, \
            F=ce.redate(model_hcst), **cpt_args)
            
            cca_h = xr.merge([cca_h, ce.redate(cca_f.probabilistic, yeardelta=-100), ce.redate(cca_f.prediction_error_variance, yeardelta=-100)])
            
            # use the in-sample probabilistic hindcasts to perform probabilistic forecast verification
            # warning - this produces unrealistically optimistic values 
            cca_pfv = cc.probabilistic_forecast_verification(cca_h.probabilistic, Y, **cpt_args)
            cca_s = xr.merge([cca_s, cca_pfv])
            hcsts.append(cca_h)
            fcsts.append(cca_rtf)
            skill.append(cca_s.where(cca_s > -999, other=np.nan))
            pxs.append(cca_px)
            pys.append(cca_py)
            
        elif str(MOS).upper() == 'PCR':
            
            # fit PCR model between X & Y and produce real-time forecasts for F 
            pcr_h, pcr_rtf, pcr_s, pcr_px = cc.principal_components_regression(model_hcst, Y, F=forecast_data[i], **cpt_args)
            
            # fit PCR model again between X & Y, and produce in-sample probabilistic hindcasts 
            # this is using X in place of F, with the year coordinates changed to n+100 years
            # because CPT does not allow you to make forecasts for in-sample data
            pcr_h, pcr_f, pcr_s, pcr_px = cc.principal_components_regression(model_hcst, Y, F=ce.redate(model_hcst), **cpt_args)
            pcr_h = xr.merge([pcr_h, ce.redate(pcr_f.probabilistic, yeardelta=-100), ce.redate(pcr_f.prediction_error_variance, yeardelta=-100)])
            
            # use the in-sample probabilistic hindcasts to perform probabilistic forecast verification
            # warning - this produces unrealistically optimistic values 
            pcr_pfv = cc.probabilistic_forecast_verification(pcr_h.probabilistic, Y, **cpt_args)
            pcr_s = xr.merge([pcr_s, pcr_pfv])
            hcsts.append(pcr_h)
            fcsts.append(pcr_rtf)
            skill.append(pcr_s.where(pcr_s > -999, other=np.nan))
            pxs.append(pcr_px)
        else:
            # simply compute deterministic skill scores of non-corrected ensemble means 
            nomos_skill = cc.deterministic_skill(model_hcst, Y, **cpt_args)
            skill.append(nomos_skill.where(nomos_skill > -999, other=np.nan))
            
        # choose what data to export here (any of the above results data arrays can be saved to netcdf)
        if str(MOS).upper() == 'CCA':
            cca_h.to_netcdf(outputDir /  (predictor_names[i] + '_crossvalidated_cca_hindcasts.nc'))
            cca_rtf.to_netcdf(outputDir / (predictor_names[i] + '_realtime_cca_forecasts.nc'))
            cca_s.to_netcdf(outputDir / (predictor_names[i] + '_skillscores_cca.nc'))
            cca_px.to_netcdf(outputDir / (predictor_names[i] + '_cca_x_spatial_loadings.nc'))
            cca_py.to_netcdf(outputDir / (predictor_names[i] + '_cca_y_spatial_loadings.nc'))
        elif str(MOS).upper() == 'PCR':
            pcr_h.to_netcdf(outputDir /  (predictor_names[i] + '_crossvalidated_pcr_hindcasts.nc'))
            pcr_rtf.to_netcdf(outputDir / (predictor_names[i] + '_realtime_pcr_forecasts.nc'))
            pcr_s.to_netcdf(outputDir / (predictor_names[i] + '_skillscores_pcr.nc'))
            pcr_px.to_netcdf(outputDir / (predictor_names[i] + '_pcr_x_spatial_loadings.nc'))
        else: 
            nomos_skill.to_netcdf(outputDircase_directory / (predictor_names[i] + '_nomos_skillscores.nc'))
            
        
    # # Multi-Model Ensemble
    ensemble = ['SEAS5.PRCP', 'CFSv2.PRCP',  'CanSIPSIC3.PRCP']
    ### Do not modify below
    det_fcst = []
    det_hcst = []
    pr_fcst = []
    pr_hcst = []
    pev_fcst = []
    pev_hcst = []
    for model in ensemble:
        assert model in predictor_names, "all members of the nextgen ensemble must be in predictor_names - {} is not".format(model)
        ndx = predictor_names.index(model)
        
        det_fcst.append(fcsts[ndx].deterministic)
        det_hcst.append(hcsts[ndx].deterministic)
        pr_fcst.append(fcsts[ndx].probabilistic)
        pr_hcst.append(hcsts[ndx].probabilistic)
        pev_fcst.append(fcsts[ndx].prediction_error_variance)
        pev_hcst.append(hcsts[ndx].prediction_error_variance)
    det_fcst = xr.concat(det_fcst, 'model').mean('model')
    det_hcst = xr.concat(det_hcst, 'model').mean('model')
    pr_fcst = xr.concat(pr_fcst, 'model').mean('model')
    pr_hcst = xr.concat(pr_hcst, 'model').mean('model')
    pev_fcst = xr.concat(pev_fcst, 'model').mean('model')
    pev_hcst = xr.concat(pev_hcst, 'model').mean('model')
    det_hcst.attrs['missing'] = hcsts[0].attrs['missing']
    det_hcst.attrs['units'] = hcsts[0].attrs['units']
    pr_hcst.attrs['missing'] = hcsts[0].attrs['missing']
    pr_hcst.attrs['units'] = hcsts[0].attrs['units']
    nextgen_skill_deterministic = cc.deterministic_skill(det_hcst, Y, **cpt_args)
    nextgen_skill_probabilistic = cc.probabilistic_forecast_verification(pr_hcst, Y, **cpt_args)
    nextgen_skill = xr.merge([nextgen_skill_deterministic, nextgen_skill_probabilistic])
    nextgen_skill.to_netcdf(outputDir /  (MOS + '_nextgen_skill.nc'))
    pr_fcst.to_netcdf(outputDir /  ('NEXTGen_' + MOS + '_ensemble_probabilisticForecast.nc'))
    det_fcst.to_netcdf(outputDir /  ('NEXTGen_' + MOS + '_ensemble_DeterministicForecast.nc'))
##### Construct MME Flexible Forecasts for each station
# import pandas as pd
# from scipy.stats import norm, t
#EDACaP has 16k virtual station, one virtual station/kebele: 
#Here we can generate Flexible forecast for each Keble's 
# edacap_stations = pd.read_csv('EDACaP_kebele_cropSimStaions.csv', names=('STNID', 'LAT', 'LON'))
# for jj in edacap_stations: 
    
#     # choose a gridpoint within the predictand domain to plot the forecast and climatological
#     # probability of exceedance and PDF curves 
#     point_latitude = edacap_stations.LAT[jj]
#     point_longitude = edacap_stations.LON[jj]
#     # if 'isPercentile is True, the threshold is a percentile (e.g., 0.5)
#     # else in the unit of the predictand (e.g., mm, degC, ...)
#     threshold = 0.5
#     isPercentile = True
#     ## DO NOT modify below
#     # Define transformer based on transform_predictand setting
#     if MOS =='CCA':
#         if str(cpt_args['transform_predictand']).upper() == 'GAMMA':
#             transformer = ce.GammaTransformer()
#         elif str(cpt_args['transform_predictand']).upper() == 'EMPIRICAL':
#             transformer = ce.EmpiricalTransformer()
#         else:
#             transformer = None
#     elif MOS == 'PCR':
#         if str(cpt_args['transform_predictand']).upper() == 'GAMMA':
#             transformer = ce.GammaTransformer()
#         elif str(cpt_args['transform_predictand']).upper() == 'EMPIRICAL':
#             transformer = ce.EmpiricalTransformer()
#         else:
#             transformer = None
#     else:
#         print('FLEX FORECASTS NOT POSSIBLE WITHOUT MOS')
                
#     # if the transformer is not none, then we used a y-transform in cpt
#     # therefore we have received a prediction error variance file in "units" of (standard normal deviates)^2
#     # and need to transform the forecast mean, in order to calculate probability of exceedance
#     if MOS in ['CCA', 'PCR']:
#         if transformer is not None:
#             # we need to normalize the forecast mean here, using the same method as CPT
#             transformer.fit(Y.expand_dims({'M':[0]}))
#             fcst_mu = transformer.transform(det_fcst.expand_dims({'M':[0]}))
#         else:
#             fcst_mu = det_fcst
#         if isPercentile:
#             if transformer is None:
#                 # if the user provided a percentile theshold, rather than an actual value
#                 # and also used no transformation / normalization, 
#                 # then we also need to compute the theshold as an actual value
#                 threshold = Y.quantile(threshold, dim='T').drop('quantile')
#             else:
#                 # if the user used a transformation and gave a percentile threshold, 
#                 # we we can set the threshold using the cumulative distribution function 
#                 # for the normal distribution N(0, 1)- since thats what the Y data has 
#                 # been transformed to
#                 threshold = xr.ones_like(fcst_mu).where(~np.isnan(fcst_mu), other=np.nan) * norm.cdf(threshold)
#         else:
#             if transformer is None:
#                 # if the user did not use a transform, and also did not use a percentile for a threshold,
#                 # we can just use the value directly. but it must be expanded to a 2D datatype
#                 threshold = xr.ones_like(fcst_mu).where(~np.isnan(fcst_mu), other=np.nan) * threshold 
#             else: 
#                 # if the user used a transformation, but gave a full value and NOT a percentile, 
#                 # we must use the transformation that CPT used to transform the threshold onto 
#                 # the normal distribution at N(0, 1)
#                 threshold = xr.ones_like(fcst_mu).where(~np.isnan(fcst_mu), other=np.nan) * threshold 
#                 threshold = transformer.transform(threshold)
#         def _xr_tsf(thrs, loc1, scale1, dof1=1):
#             return t.sf(thrs, dof1, loc=loc1, scale=scale1)
#         ntrain = Y.shape[list(Y.dims).index('T')]
#         fcst_scale = np.sqrt( (ntrain -2)/ntrain * pev_fcst )
#         # if we transformed the forecast data, we should transform the actual Y data to match
#         if transformer is not None:
#             Y2 = transformer.transform(Y.expand_dims({'M':[0]})).fillna(Y.min('T')) * xr.ones_like(Y.mean('T')).where(~np.isnan(Y.mean('T')), other=np.nan)
#             Y2_fill = xr.where(~np.isfinite(Y2), 0, Y2)
#             Y2 = xr.where(np.isfinite(Y2), Y2, Y2_fill)
#         else:
#             Y2 = Y
#         # here we calculate the climatological mean and variance
#         climo_var =  Y2.var('T') # xr.ones_like(fcst_mu).where(~np.isnan(fcst_mu), other=np.nan) if transformer is not None else
#         climo_mu =  Y2.mean('T') # xr.ones_like(fcst_mu).where(~np.isnan(fcst_mu), other=np.nan) if transformer is not None else
#         climo_scale = np.sqrt( (ntrain -2)/ntrain * climo_var )
#         # we calculate here, the probability of exceedance by taking 1 - t.cdf()
#         # after having transformed the forecast mean to match the units of the 
#         # prediction error variance, if necessary.
#         exceedance_prob = xr.apply_ufunc( _xr_tsf, threshold, fcst_mu, fcst_scale, input_core_dims=[['X', 'Y'], ['X', 'Y'], ['X', 'Y']], output_core_dims=[['X', 'Y']],keep_attrs=True, kwargs={'dof1':ntrain})
        
#     ### Plot Flexible MME Forecasts
#     # plot exceedance probability map
#     cmap=plt.get_cmap('RdBu_r', 11)
#     # setting up canvas on which to draw
#     fig = plt.figure(figsize=(10,10))
#     gs0 = gridspec.GridSpec(4, 1, figure=fig)
#     gs00 = gridspec.GridSpecFromSubplotSpec(5, 5, subplot_spec=gs0[:3])
#     gs11 = gridspec.GridSpecFromSubplotSpec(1, 2, subplot_spec=gs0[3])
#     gs01 = gridspec.GridSpecFromSubplotSpec(5, 5, subplot_spec=gs11[0])
#     gs02 = gridspec.GridSpecFromSubplotSpec(5, 5, subplot_spec=gs11[1])
#     map_ax = fig.add_subplot(gs00[:,:], projection = ccrs.PlateCarree())
#     cdf_ax = fig.add_subplot(gs01[:,:])     
#     pdf_ax = fig.add_subplot(gs02[:,:])     
#     # plot the map
#     art = exceedance_prob.transpose('Y', 'X', ...).plot(cmap=cmap,  ax=map_ax, vmin=0, vmax=1) 
#     map_ax.scatter([point_longitude], [point_latitude], marker='x', s=100, color='red', transform=ccrs.PlateCarree())
#     coasts = art.axes.coastlines()
#     art.axes.add_feature(cartopyFeature.BORDERS)
#     art.axes.add_feature(shape_feature, edgecolor='gray')
#     title = map_ax.set_title('(a) Probabilities of Exceedance')
#     # point calculations - select the nearest point to the lat/lon the user wanted to plot curves
#     point_threshold = float(threshold.sel(**{'X':point_longitude, 'Y':point_latitude}, method='nearest').values)
#     point_fcst_scale = float(fcst_scale.sel(**{'X':point_longitude, 'Y':point_latitude}, method='nearest').values)
#     point_climo_scale = float(climo_scale.sel(**{'X':point_longitude, 'Y':point_latitude}, method='nearest').values)
#     point_fcst_mu = float(fcst_mu.sel(**{'X':point_longitude, 'Y':point_latitude}, method='nearest').values)
#     point_climo_mu = float(climo_mu.sel(**{'X':point_longitude, 'Y':point_latitude}, method='nearest').values)
#     point_climo = np.squeeze(Y2.sel(**{'X':point_longitude, 'Y':point_latitude}, method='nearest').values)
#     point_climo.sort()
#     if transformer is not None:
#         point_climo_mu_nontransformed = float(Y.mean('T').sel(**{'X':point_longitude, 'Y':point_latitude}, method='nearest').values)
#         point_climo_std_nontransformed = float(Y.std('T').sel(**{'X':point_longitude, 'Y':point_latitude}, method='nearest').values)
#     x = point_climo 
#     x1 =np.linspace(x.min(), x.max(), 1000)
#     cprobth =  sum(x >= point_threshold) / x.shape[0]  #round(t.sf(point_threshold, ntrain, loc=point_climo_mu, scale=point_climo_scale),2)
#     fprobth = round(t.sf(point_threshold, ntrain, loc=point_fcst_mu, scale=point_fcst_scale),2)
#     # POE plot
#     cdf_ax.plot(x, [ sum(x >= x[i]) / x.shape[0] for i in range(x.shape[0]) ],'g-', lw=2, marker='x', alpha=0.8, label='clim (empirical)')
#     cdf_ax.plot(x1, t.sf(x1, ntrain, loc=point_fcst_mu, scale=point_fcst_scale),'r-',  lw=1, alpha=0.8, label='fcst')
#     cdf_ax.plot(x1, norm.sf(x1, loc=point_climo_mu, scale=point_fcst_scale),'b-', lw=1, alpha=0.8, label='clim (fitted)')
#     cdf_ax.plot(point_threshold, fprobth,'ok')
#     cdf_ax.plot(point_threshold, cprobth,'ok')
#     cdf_ax.axvline(x=point_threshold, color='k', linestyle='--')
#     cdf_ax.set_title(' (b) Point Probabilities of Exceedance')
#     cdf_ax.set_xlabel(Y.name.upper())
#     cdf_ax.set_ylabel('Probability (%)')
#     cdf_ax.legend(loc='best', frameon=False)
#     # PDF plot
#     fpdf=t.pdf(x1, ntrain, loc=point_fcst_mu, scale=np.sqrt(point_fcst_scale))
#     pdf_ax.plot(x1, norm.pdf(x1, loc=point_climo_mu, scale =point_climo_scale), 'b-', alpha=0.8, label='clim (fitted)') # clim pdf in blue
#     pdf_ax.plot(x1, fpdf, 'r-',  alpha=0.8, label='fcst') # fcst PDF in red
#     pdf_ax.hist(point_climo, density=True, histtype='step', label='clim (empirical)') # want this in GREEN
#     pdf_ax.axvline(x=point_threshold, color='k', linestyle='--')
#     pdf_ax.legend(loc='best', frameon=False)
#     pdf_ax.set_title('(c) Point Probability Density Functions')
#     pdf_ax.set_xlabel(Y.name.upper())
#     pdf_ax.set_ylabel('')
#     if transformer is not None:
#         newticks = [-2, -1, 0, 1, 2]
#         pdf_ax.set_xticks(newticks, [round(i * point_climo_std_nontransformed + point_climo_mu_nontransformed, 2) for i in newticks], rotation=0)
#         cdf_ax.set_xticks(newticks, [round(i * point_climo_std_nontransformed + point_climo_mu_nontransformed, 2) for i in newticks], rotation=0)
#     # save plot
#     figName = MOS + '_' + edacap_stations.STNID[jj] + '_flexForecast_probExceedence.png'
#     plt.savefig(Path.home() / "Desktop" / caseDir / domainFolder / "figures" / figName, bbox_inches='tight') 
        