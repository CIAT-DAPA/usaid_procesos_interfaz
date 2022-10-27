#!/usr/bin/env python
# coding: utf-8

# <div>
# <img src="https://repository-images.githubusercontent.com/142679292/954e7180-2ef6-11ea-802d-5765d8448bd8" width="150" style="float: left;"/></div>
#
# ## sv1.9.2 (5 Nov 2020)
# ## Produce seasonal (un)calibrated forecasts and assess associated skill
#
#
# #### Requires:
# CPTv16.5.2+
#
# #### Authors:
# Ángel G. Muñoz (agmunoz@iri.columbia.edu), Andrew W. Robertson (awr@iri.columbia.edu), Cuihua Li (OCP), Simon J. Mason (simon@iri.columbia.edu), Kyle Hall (IRI)
#
# #### Acknowledgements:
# Rémi Cousin (IRI) for key Ingrid code development and support.
# Xandre Chourio (IRI) for Windows and code support, and testing.
# James Doss-Gollin (Columbia Water Center) developed the original download Python functions.
# Part of the effort to develop PyCPT is funded by the Columbia World Project "Adapting Agriculture to Climate Today, for Tomorrow" (ACToday), and NOAA MAPP's projects NA18OAR4310275 (Muñoz) and NA16OAR4310145 (Robertson).
#
# #### Version log
# * This version now reads station data from the DL, and can run CPT in an automated way with it, but still doesn't have all plot functionality. --AGM 13 Sep 2019
# * Started simplifying functions, wrote readGrADSctl function; added functions to create the NextGen files for det skill assessment and plotting --AGM, Sep 2019
# * Fixed bug with plotting functions when selecting a subset of the seasons, and added start time for forecast file in CPT script -- AGM, July 1st 2019
# * Added VQ and UQ from CFSv2. User can now select the seasons to visualize in the skill and EOF maps. Fixed bug related to coordinate selection in CHIRPS, TRMM and CPC. -- AGM, June 13th 2019
# * First Notebook seasonal version -- AGM, May 7th 2019
# * Several PyCPT sub-seasonal versions (through v1.2) --see logs in that version 2018-present
# * First iPython sub-seasonal version (Jupyter Notebook) -- AWR, 24 Jun 2018
# * First similar version (BASH for CFSv2) by Muñoz and Chourio for the OLE2 -- 12 Dec 2010
#
# #### Version log
# See version.log in Github
#
# #### Workflow
# <div>
# <img src="https://bitbucket.org/repo/X5dkaz8/images/133964354-image.png" width="700"/>
# </div>
#
# #### Input:
# * Predictor is rainfall from S2S ECMWF (no realtime), or NCEP CFSv2 (realtime), or SubX GEFS (realtime)
# * Predictand is either rainfall total or frequency of rainy days from TRMM (3B42 v7) or CPC Unified or IMD
#
# #### Output:
# * Several skill maps for assessment of deterministic forecast AND forecast maps, in the output folder.
# * CPT scripts used to assess skill, in the scripts folder.
# * Downloaded input files, in the input folder.
#
# #### Notes:
# 1. Old data in the input folder is deleted at the beginning of the process!
#
# ### Namelist section

# In[1]:
import os
import sys
import xarray as xr
import numpy as np
import pandas as pd
import subprocess
from pycpt_functions_seasonal import *
from pycpt_dictionary import dic_sea, dic_sea_elr
from scipy.stats import t
import cartopy.crs as ccrs
from cartopy.feature import NaturalEarthFeature, LAND, COASTLINE
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
import datetime
from dateutil.relativedelta import relativedelta
import calendar


def months_short(inputdate):
    return inputdate.strftime('%B')[0:3]


def run_pycpt_seasonal(region, spatial_predictors, spatial_predictands, models, obs, station, mos, predictand, predictor, mons, tgtii, tgtff, tini, tend,
              xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min, ccamodes_max, force_download, single_models, forecast_anomaly, forecast_spi,
              confidence_level):

    current_date = datetime.date.today()
    # Work name (name of the work folder; e.g., one word followed by "_seasonal":)
    # work='ENACTS_BD_T2M'
    work = region  # 'Colombia_seasonal'

    # Model (choose one, a subset or all models:
    # CanSIPSv2*, CMC1-CanCM3, CMC2-CanCM4, COLA-RSMAS-CCSM3, COLA-RSMAS-CCSM4*,
    # GFDL-CM2p1, GFDL-CM2p5-FLOR-A06*, GFDL-CM2p5-FLOR-B01*, NASA-GEOSS2S*, NCAR-CESM1, NCEelo2P-CFSv2*)
    # The ones with "*" are producing operation#al forecasts, the others are frozen.
    # CanSIPSv2 forecasts are ONLY AVAILABLE after Aug 2019!!!!
    # EU(for 2020 forecast)=['EU-C3S-UKMO-GloSea5GC2S15','EU-C3S-DWD-GCFS2p0','EU-C3S-CMCC-SPS3']
    # NMME_models=['CanSIPSv2','COLA-RSMAS-CCSM4','NASA-GEOSS2S','NCEP-CFSv2']
    models = models
    # ['COLA-RSMAS-CCSM4','NASA-GEOSS2S','NCEP-CFSv2']
    # EU_models=['EU-C3S-ECMWF-SEAS5','EU-C3S-MeteoFrance-System7','EU-C3S-UKMO-GloSea6GC2S600','EU-C3S-DWD-GCFS2p1','EU-C3S-CMCC-SPS3p5']
    # models=['NCEP-CFSv2','CanSIPSv2']
    # models=['NCEP-CFSv2','NASA-GEOSS2S']
    # Obs (choose between CPC-CMAP-URD, CHIRPS, TRMM, CPC, Chilestations)
    # obs='ENACTS-BD'
    obs = obs
    station = station
    # MOS method (choose between None, PCR, CCA)
    MOS = mos
    # MOS='PCR'
    # MOS='None'
    # Predictand (choose between PRCP, RFREQ)
    # PREDICTAND='TMEAN'
    PREDICTAND = predictand  # 'PRCP'
    # Predictor (choose between GCM's PRCP, VQ, UQ)
    # VQ and UQ only works with models=['NCEP-CFSv2']
    # PREDICTOR='T2M'
    # UQ VQ: for desired horizontal moisture fluxes in the U (Zonal) and V (Meridional) directions at a geopotential height of P (user-definable: 700, 850, 925, etc.).
    pressure = '850'
    # (Tip: change your work foldername after changing the pressure)
    # PREDICTOR='T2M'
    PREDICTOR = predictor  # 'PRCP'
    season1_start = current_date + relativedelta(months=0)
    season1_end = current_date + relativedelta(months=2)
    season2_start = current_date + relativedelta(months=3)
    season2_end = current_date + relativedelta(months=5)
    # Target seasons and related parameters
    # If more targets, increase the arrays accordingly
    # mons = mons  # ['Feb']
    mons = [current_date.strftime('%B')[0:3] + '',
            ''+current_date.strftime('%B')[0:3]]

    #mons=['May','May']
    tgtii = tgtii  # S: start for the DL
    tgtff = tgtff  # S: end for the DL
    # tgtii=['1.5','2.5']  #S: start for the DL
    # tgtff=['1.5','2.5']   #S: end for the DL
    # tgtii=['1.5','2.5']  #S: start for the DL
    # tgtii='1.5'  #S: start for the DL
    # tgtff='3.5'   #S: end for the DL
    # for now, just write the target period (for DL)
    # ['Mar-May'] #'Aug-Oct','Mar-May','Apr-Jun','May-Jul']   #Needs to be any of the seasons computed.
    #tgts = tgts
    tgts = [str(months_short(season1_start)+'-' + months_short(season1_end)) +
            '', '' + str(months_short(season2_start)+'-' + months_short(season2_end))]
    #tgts = ['May-Jul','Aug-Oct']
    monss = tgts
    # tgts=['Jan','Feb']
    #tgts =['Jan-Mar']
    # Start and end of the training period: (must be >1982 for NMME models. Because of CanSIPSv2, probably end in 2018)
    tini = tini
    tend = tend
    xmodes_min = xmodes_min
    xmodes_max = xmodes_max
    ymodes_min = ymodes_min
    ymodes_max = ymodes_max
    ccamodes_min = ccamodes_min
    ccamodes_max = ccamodes_max
    # Forecast date
    #monf = 'May'  # todays_date.month  # Initialization month
    monf = current_date.strftime('%B')[0:3]
    # fyr = 2021  # Forecast year
    fyr = current_date.year  # Forecast year
    # for s in range(len(tgts)):
    #     #tgts_j=["Mon-Mon", "year", "Mon-Mon", "year"]
    #     if(s == 1 and tgts_j[1] != tgts_j[3]):
    #         fyr = int(tgts_j[3])
            
    #     else:
    #         fyr = current_date.year

    # Switches:
    # force download of data files, even if they already exist locally
    force_download = force_download
    single_models = single_models  # Switch Single_model plots on or off
    # Switch to plot deterministic forecast anomaly maps.
    forecast_anomaly = forecast_anomaly
    # Switch to plot deterministic forecast precipitation maps as a standardized precipitation index (SPI).
    forecast_spi = forecast_spi
    # Confidence level (%) for deterministic forecast confidence interval (-/+ confidence_level) maps.
    confidence_level = confidence_level
    # # ########Spatial domain for predictor
    # nla1=30 	# Northernmost latitude
    # sla1=4 	# Southernmost latitude
    # wlo1=95 	# Westernmost longitude
    # elo1=115 	# Easternmost longitude
    # # Spatial domain for predictand
    # nla2=30 	# Northernmost latitude
    # sla2=4 	# Southernmost latitude
    # wlo2=95 	# Westernmost longitude
    # elo2=115 	# Easternmost longitude
    # # Spatial domain for predictand
    # nla2=7 	# Northernmost latitude
    # sla2=24 	# Southernmost latitude
    # wlo2=100 	# Westernmost longitude
    # elo2=110 	# Easternmost longitude
    # ########Spatial domain for predictor
    # nla1=35 	# Northernmost latitude
    # sla1=15 	# Southernmost latitude
    # wlo1=80 	# Westernmost longitude
    # elo1=100 	# Easternmost longitude
    # Spatial domain for predictand
    # nla2=28 	# Northernmost latitude
    # sla2=20 	# Southernmost latitude
    # wlo2=87 	# Westernmost longitude
    # elo2=94 	# Easternmost longitude
    # ########Spatial domain for predictor
    nla1 = spatial_predictors[0]  # 20 	# Northernmost latitude
    sla1 = spatial_predictors[1]  # -1 	# Southernmost latitude
    wlo1 = spatial_predictors[2]  # 28 	# Westernmost longitude
    elo1 = spatial_predictors[3]  # 50 	# Easternmost longitude
    # Spatial domain for predictand
    nla2 = spatial_predictands[0]  # 12 	# Northernmost latitude
    sla2 = spatial_predictands[1]  # -4 	# Southernmost latitude
    wlo2 = spatial_predictands[2]  # -78 	# Westernmost longitude
    elo2 = spatial_predictands[3]  # -66 	# Easternmost longitude
    # Want to add a topo background to the domain plots?
    use_topo = True
    map_color = 'WindowsCPT'  # set to "WindowsCPT" for CPT colorscheme
    colorbar_option = True
    use_ocean = True
    # In[4]:
    # Some folder and file options:
    # Working directory --it should exist!!!
    #workdir = '/mnt/c/Users/Patri/OneDrive/Columbia_Academics/S2S4/iri-pycpt/'
    #workdir = '/Users/kjhall/Projects/iri-pycpt/'
    #workdir = '/home/cli/iri-pycpt23-git/'
    workdir = '/forecast/PyCPT/iri-pycpt'
    # location in which the current .ipynb is located, for saving Jupyter notebooks
    savedir = workdir
    # PATH to CPT root directory
    # cptdir='/mnt/c/Users/Patri/Downloads/CPT/16.5.8/'
    # cptdir='/Users/kjhall/CPT/16.5.8/'
    cptdir = '/forecast/models/CPT/17.6.1/bin/'
    #workdir = '/home/cli/iri-pycpt3-git/'
    # PATH to CPT root directory
    # cptdir='/Users/agmunoz/Documents/Angel/CPT/CPT/16.2.4/'
    # cptdir='/software/centos7/x86_64/CPT/16.5.4/bin/'

    print("PyCPT folder is:")
    # %cd $workdir
    os.chdir(os.path.join(workdir))
    os.mkdir(os.path.join(workdir, work))

    print("Python libraries loaded")
    print("Now in the work folder:", os.getcwd())

    os.chdir(os.path.join(workdir, work))
    workdir = os.getcwd()

    (rainfall_frequency, threshold_pctle, wetday_threshold, obs_source, hdate_last, mpref, L,
    ntrain, fprefix, x_offset, y_offset) = setup_params(PREDICTOR, PREDICTAND, obs, MOS, tini, tend)

    print("Creating working folders, if not already there...")
    print("Work directory is:")
    # %cd $workdir
    os.chdir(os.path.join(workdir))
    os.mkdir('input')
    os.mkdir('input/noMOS')
    os.mkdir('output')
    os.mkdir('scripts')
    os.system('rm -Rf scripts/*')
    os.mkdir('output/figures')
    # Set up CPT environment
    os.environ["CPT_BIN_DIR"] = cptdir
    print("CPT environment loaded...")
    print("CPT version is "+str(wetday_threshold))
    # ## Check if domains are ok --if you don't like them, go back to the namelist and modify them
    print("Training period: "+str(tini)+"-"+str(tend))
    print("Forecast initialized on: "+monf+" "+str(fyr))
    # Plot geographical domains
    pltdomain(wlo1, elo1, nla1, sla1, wlo2, elo2, nla2, sla2, use_topo)

    # # Download data if necessary, and run CPT
    # If downloadling data from several models, this section might take a while to be done
    for model in models:
        print('')
        print('')
        print('\033[1m----Starting process for '+model+'----\033[0;0m')
        for mo in range(len(mons)):
            mon = mons[mo]
            tar = tgts[mo]
            tgti = tgtii[mo]
            tgtf = tgtff[mo]
            print("New folder:")
            # %cd $workdir/input
            os.chdir(os.path.join(workdir, 'input'))

            print('Preparing CPT files for \033[1m'+model+'\033[0;0m. Target: \033[1m' +
                tar+'\033[0;0m - Initialization \033[1m'+mon+'\033[0;0m...')
            PrepFiles(fprefix, PREDICTAND, threshold_pctle, tini, tend, wlo1, wlo2, elo1, elo2, sla1, sla2, nla1, nla2, tgti, tgtf, mon, monf,
                    fyr, os, wetday_threshold, tar, model, obs, obs_source, hdate_last, force_download, station, dic_sea, dic_sea_elr, pressure, MOS)
            print("New folder:")
            # %cd $workdir/scripts
            os.chdir(os.path.join(workdir, 'scripts'))
            CPTscript(model, PREDICTAND, mon, monf, fyr, tini, tend, nla1, sla1, wlo1, elo1, nla2, sla2, wlo2, elo2, fprefix, mpref, tar,
                    ntrain, MOS, station, xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min, ccamodes_max, forecast_anomaly, forecast_spi)
            print('Executing CPT for \033[1m'+model+'\033[0;0m. Target: \033[1m' +
                tar+'\033[0;0m - Initialization \033[1m'+mon+'\033[0;0m...')
            try:
                subprocess.check_output(cptdir+'CPT.x < params > CPT_log_'+model +
                                        '_'+tar+'_'+mon+'.txt', stderr=subprocess.STDOUT, shell=True)
            except subprocess.CalledProcessError as e:
                print(e.output.decode())
                raise
            print('----------------------------------------------')
            print('Calculations for Target: \033[1m'+tar +
                '\033[0;0m - Initialization \033[1m'+mon+'\033[0;0m completed!')
            print('See output folder, and check scripts/CPT_log_' +
                model+'_'+tar+'_'+mon+'.txt for log')
            print('\033[1mQuick error report from CPT (if any):\033[0;0m')
            with open('CPT_log_'+model+'_'+tar+'_'+mon+'.txt', "r") as fp:
                for line in lines_that_contain("Error:", fp):
                    print(line)
            print('----------------------------------------------')
            print('----------------------------------------------')
    print('')
    print('')
    print('')
    print('\033[1mPROCESS COMPLETED \033[0;0m')

    # ## Skill Analysis

    models

    # Choose metrics (options are: 'Pearson','Spearman','2AFC','RocAbove','RocBelow'):
    # met=('Ignorance','RPSS','GROC')
    met = ('Pearson', 'Spearman', 'RPSS', 'GROC')
    #monss=['Jul-Sep']   #Needs to be any of the seasons computed.#
    print("Present folder:")
    os.chdir(os.path.join(workdir, 'output'))
    # Skill scores loop
    plt.rcParams.update({'font.size': 10})
    for ime in met:
        pltmap(models, PREDICTAND, ime, wlo2, elo2, sla2, nla2, fprefix, mpref, tgts, mons,
            monss, map_color, colorbar_option, use_ocean, x_offset=x_offset, y_offset=y_offset)
        plt.savefig(('figures/Skill-Models-'+obs+'-' +
                    MOS+'-' + ime + '.pdf'), dpi=300)
        print('')
        print("\033[1m"+ime+"\033[0;0m")
        # plt.close()
        plt.show()
        # print('')
    # # Single Model Probabilistic Forecasts
    # Author: Patric
    if single_models and MOS != 'None':
        print("Present folder:")

        os.chdir(os.path.join(workdir))

        plt_single_probabilistic(models, PREDICTOR, PREDICTAND, wlo2,
                                elo2, sla2, nla2, fprefix, mpref, tgts, mon, fyr, use_ocean)
    elif single_models and MOS not in ['CCA', 'PCR']:
        print('PROBABILISTIC FORECASTS ONLY AVAILABLE IF MOS=CCA or MOS=PCR')
    elif MOS in ['CCA', 'PCR'] and not single_models:
        print('SET single_models=True TO PLOT INDIVIDUAL GCM DETERMINISTIC FORECASTS')
    else:
        pass
    # # Single Model Deterministic Forecast and Confidence Intervals
    # Author: Cuihua / Patric
    if single_models and MOS != 'None':
        print("Present folder:")
        # %cd $workdir
        os.chdir(os.path.join(workdir))
        plt_single_deterministic_and_confidenceIntervals(
            models, PREDICTOR, PREDICTAND, wlo2, elo2, sla2, nla2, fprefix, mpref, tgts, mon, fyr, use_ocean, confidence_level, forecast_spi)
    elif single_models and MOS not in ['CCA', 'PCR']:
        print('FORECASTS ONLY AVAILABLE IF MOS=CCA or MOS=PCR')
    elif MOS in ['CCA', 'PCR'] and not single_models:
        print('SET single_models=True TO PLOT INDIVIDUAL GCM DETERMINISTIC FORECASTS')
    else:
        pass

    # ## Multi-model ensemble: NextGen
    # Indicate models to be included in the NextGen ensemble:
    # models=['EU-C3S-ECMWF-SEAS5','EU-C3S-MeteoFrance-System7','EU-C3S-UKMO-GloSea5GC2S15']
    models
    MOS

    print('\033[1m----Assessing skill for NextGen----\033[0;0m')
    model = 'NextGen'
    NEXTGEN_MOS = 'None'  # we're changing here to no MOS to assess skill of NextGen
    print('Using the following model(s) in the NextGen cocktail: \n\n'+str(models))
    print('')
    print('')
    print('')
    for mo in range(len(mons)):
        mon = mons[mo]
        tar = tgts[mo]
        tgti = tgtii[mo]
        tgtf = tgtff[mo]
        print("New folder:")
        # %cd $workdir/input
        os.chdir(os.path.join(workdir, 'input'))
        print('Preparing CPT files for \033[1m'+model+'\033[0;0m. Target: \033[1m' +
            tar+'\033[0;0m - Initialization \033[1m'+mon+'\033[0;0m...')
        NGensemble(models, fprefix, PREDICTOR, PREDICTAND, mpref,
                'FCST_xvPr', tar, mon, tgti, tgtf, monf, fyr)

        print("New folder:")
        os.chdir(os.path.join(workdir, 'scripts'))
        # We run CPT with a NoMOS configuration, BUT producing a MOS=mpref (e.g., CCA) file!
        CPTscript(model, PREDICTAND, mon, monf, fyr, tini, tend, nla1, sla1, wlo1, elo1, nla2, sla2, wlo2, elo2, fprefix, mpref, tar, ntrain,
                NEXTGEN_MOS, station, xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min, ccamodes_max, forecast_anomaly, forecast_spi)
        print('Executing CPT for \033[1m'+model+'\033[0;0m. Target: \033[1m' +
            tar+'\033[0;0m - Initialization: \033[1m'+mon+'\033[0;0m...')
        try:
            subprocess.check_output(cptdir+'CPT.x < params > CPT_log_'+model +
                                    '_'+tar+'_'+mon+'.txt', stderr=subprocess.STDOUT, shell=True)
        except subprocess.CalledProcessError as e:
            print(e.output.decode())
            raise
        print('----------------------------------------------')
        print('Calculations completed for \033[1m'+tar +
            '\033[0;0m - Initialization: \033[1m'+mon+'\033[0;0m')
        print('See output folder, and check scripts/CPT_log_' +
            model+'_'+tar+'_'+mon+'.txt for errors')
        print('\033[1mQuick error report from CPT (if any):\033[0;0m')
        with open('CPT_log_'+model+'_'+tar+'_'+mon+'.txt', "r") as fp:
            for line in lines_that_contain("Error:", fp):
                print(line)
            print('----------------------------------------------')
            print('----------------------------------------------')
        print('Generating FlexForecast files for \033[1m'+model+'\033[0;0m. Target: \033[1m' +
            tar+'\033[0;0m - Initialization: \033[1m'+mon+'\033[0;0m...')
        NGensemble(models, fprefix, PREDICTOR, PREDICTAND, mpref,
                'FCST_mu', tar, mon, tgti, tgtf, monf, fyr)
        NGensemble(models, fprefix, PREDICTOR, PREDICTAND, mpref,
                'FCST_var', tar, mon, tgti, tgtf, monf, fyr)
        print('----------------------------------------------')
    print('')
    print('')
    print('')
    print('\033[1mPROCESS COMPLETED \033[0;0m')

    # Choose metrics (options are: 'Pearson','Spearman','2AFC','RocAbove','RocBelow','Ignorance','RPSS','GROC'):
    met = ('Pearson', 'Spearman', 'RPSS', 'GROC')
    # monss=['Jul-Sep']#Aug-Oct','Mar-May','Apr-Jun','Sep-Nov']   #Needs to be any of the seasons computed.
    #models=['NextGen','CanSIPSv2', 'COLA-RSMAS-CCSM4', 'GFDL-CM2p5-FLOR-A06', 'GFDL-CM2p5-FLOR-B01', 'NASA-GEOSS2S', 'NCEP-CFSv2']
    print("Present folder:")
    # %cd $workdir/output
    os.chdir(os.path.join(workdir, 'output'))
    # Skill scores loop
    plt.rcParams.update({'font.size': 10})
    for ime in met:
        pltmap(['NextGen'], PREDICTAND, ime, wlo2, elo2, sla2, nla2, fprefix, mpref, tgts, mons,
            monss, map_color, colorbar_option, use_ocean, x_offset=x_offset, y_offset=y_offset)
        plt.savefig(('figures/Skill-NextGen-'+obs+'-' +
                    MOS+'-' + ime + '.pdf'), dpi=300)
        print('')
        print("\033[1m"+ime+"\033[0;0m")
        plt.show()
        # print('')
    # In[23]:
    print("Present folder:")
    # %cd $workdir
    os.chdir(os.path.join(workdir))
    plt_ng_deterministic(models, PREDICTOR, PREDICTAND, wlo2,
                        elo2, sla2, nla2, fprefix, mpref, tgts, mon, fyr, use_ocean)
    if MOS != "None":
        plt_ng_probabilistic(models, PREDICTOR, PREDICTAND, wlo2,
                            elo2, sla2, nla2, fprefix, mpref, tgts, mon, fyr, use_ocean)
    else:
        print('Probababilistic Forecasts only available when MOS=CCA or MOS=PCR')
