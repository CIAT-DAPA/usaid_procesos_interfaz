#!/usr/bin/env python
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

def month_short(inputdate):
    return inputdate.strftime('%B')[0:3]


def run_pycpt_seasonal(region, spatial_predictors, spatial_predictands, models, obs, station, mos, predictand, predictor, mons, tgtii, tgtff, tini, tend,
              xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min, ccamodes_max, force_download, single_models, forecast_anomaly, forecast_spi,
              confidence_level):

    current_date = datetime.date.today()
    seas1_s = current_date + relativedelta(month=0)
    seas1_e = current_date + relativedelta(month=2)
    seas2_s = current_date + relativedelta(month=3)
    seas2_e = current_date + relativedelta(month=5) 

    # forecast target month 
    monsf = [current_date.strftime('%B')[0:3] + '','' + current_date.strftime('%B')[0:3]]

    fcst_seas = [str(month_short(seas1_s)) + '-' + str(month_short(seas1_e)) + '','' +
                str(month_short(seas2_s)) + '-' + str(month_short(seas2_e))]

    tgtii_s=['1.5', '3.5']  #S: start for the DL
    tgtff_e=['3.5', '5.5']   #S: end for the DL

    #for ii in length(fcst_seas)
    #ii=0

    ########Target seasons and related parameters
    #mons =monsf[ii]
    #mons=['Dec']
    #tgtii=tgtii_s[ii]  #S: start for the DL
    #tgtff=tgtff_e[ii]   #S: end for the DL
    #tgts =fcst_seas[ii] #'Aug-Oct','Mar-May','Apr-Jun','May-Jul']   #Needs to be any of the seasons computed.
    #monss = tgts 

    ########Forecast date  
    #monf=monsf[0]	# Initialization month 
    #fyr=2021	# Forecast year



    ########Work name (name of the work folder; e.g., one word followed by "_seasonal":)
    #work='ENACTS_BD_T2M'
    work=region

    ########Model (choose one, a subset or all models: 
    #models=['COLA-RSMAS-CCSM4','NASA-GEOSS2S', 'CanSIPSv2', 'NCEP-CFSv2', 'EU-C3S-ECMWF-SEAS5']
    models = models

    ########Obs (choose between CPC-CMAP-URD, CHIRPS, TRMM, CPC, Chilestations)
    obs=obs
    station=station

    ########MOS method (choose between None, PCR, CCA)
    MOS=mos

    ########Predictand (choose between PRCP, RFREQ)
    PREDICTAND=predictand

    ########Predictor (choose between GCM's PRCP, VQ, UQ)
    pressure='850'  # UQ VQ: for desired horizontal moisture fluxes in the U (Zonal) and V (Meridional) directions at a geopotential height of P (user-definable: 700, 850, 925, etc.). 
    PREDICTOR=predictand

    ########Target seasons and related parameters
    #mons =['Nov']
    mons = [current_date.strftime('%B')[0:3] + '',
           ''+current_date.strftime('%B')[0:3]]
    tgtii=['1.5']  #S: start for the DL
    tgtff=['3.5']   #S: end for the DL
    #tgtii=['1.5','2.5']  #S: start for the DL
    #tgtff=['1.5','2.5']   #S: end for the DL

    #for now, just write the target period (for DL)
    #tgts =['Dec-Feb'] #'Aug-Oct','Mar-May','Apr-Jun','May-Jul']   #Needs to be any of the seasons computed.
    tgts = [str(month_short(seas1_s)) + '-' + str(month_short(seas1_e)) + '','' +
                str(month_short(seas2_s)) + '-' + str(month_short(seas2_e))]

    monss = tgts 
    #Start and end of the training period: (must be >1982 for NMME models. Because of CanSIPSv2, probably end in 2018)
    tini = tini
    tend = tend


    xmodes_min = xmodes_min
    xmodes_max = xmodes_max
    ymodes_min = ymodes_min
    ymodes_max = ymodes_max
    ccamodes_min = ccamodes_min
    ccamodes_max = ccamodes_max


    ########Forecast date  
    #monf='Nov'	# Initialization month
    monf = current_date.strftime('%B')[0:3] 
    fyr=2021	# Forecast year

    ########Switches:
    force_download = force_download   #force download of data files, even if they already exist locally
    single_models = single_models     #Switch Single_model plots on or off
    forecast_anomaly=forecast_anomaly # Switch to plot deterministic forecast anomaly maps.
    forecast_spi = forecast_spi # Switch to plot deterministic forecast precipitation maps as a standardized precipitation index (SPI). 
    confidence_level=confidence_level # Confidence level (%) for deterministic forecast confidence interval (-/+ confidence_level) maps.


    # ########Spatial domain for predictor: Ethiopia Domain
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
    use_topo=True
    map_color='WindowsCPT' #set to "WindowsCPT" for CPT colorscheme
    colorbar_option=True
    use_ocean=False

    ########Some folder and file options:
    # Working directory --it should exist!!!
    workdir = '/forecast/PyCPT/iri-pycpt'
    savedir = workdir  # location in which the current .ipynb is located, for saving Jupyter notebooks
    # PATH to CPT root directory
    cptdir = '/forecast/models/CPT/17.6.1/bin/'
    
    print("PyCPT folder is:")
    get_ipython().run_line_magic('cd', '$workdir')
    get_ipython().system('mkdir -p $work')


    #### End of namelist section  (do not modify anything below this line)
    ###############################################################################################################################
    # #### Load Libraries


    print("Python libraries loaded")
    print("Now in the work folder:")
    get_ipython().run_line_magic('cd', '$work')
    workdir = os.getcwd()
    (rainfall_frequency,threshold_pctle,wetday_threshold,obs_source,hdate_last,mpref,L,ntrain,fprefix, x_offset, y_offset)=setup_params(PREDICTOR,PREDICTAND,obs,MOS,tini,tend)


    print("Creating working folders, if not already there...")
    print("Work directory is:")
    get_ipython().run_line_magic('cd', '$workdir')
    get_ipython().system('mkdir -p input')
    get_ipython().system('mkdir -p input/noMOS')
    get_ipython().system('mkdir -p output')
    get_ipython().system('mkdir -p scripts')
    get_ipython().system('rm -Rf scripts/*')
    get_ipython().system('mkdir -p output/figures')

    # Set up CPT environment
    os.environ["CPT_BIN_DIR"] = cptdir
    print("CPT environment loaded...")
    #print("CPT version is "+str(wetday_threshold))


    # ## Check if domains are ok --if you don't like them, go back to the namelist and modify them

    print("Training period: "+str(tini)+"-"+str(tend))
    print("Forecast initialized on: "+monf+" "+str(fyr))

    #Plot geographical domains
    #pltdomain(wlo1,elo1,nla1,sla1,wlo2,elo2,nla2,sla2,use_topo)


    # # Download data if necessary, and run CPT

    #If downloadling data from several models, this section might take a while to be done
    for model in models:
        print('')
        print('')
        print('\033[1m----Starting process for '+model+'----\033[0;0m')
        for mo in range(len(mons)):
            mon=mons[mo]
            tar=tgts[mo]
            tgti=tgtii[mo]
            tgtf=tgtff[mo]
            print("New folder:")
            get_ipython().run_line_magic('cd', '$workdir/input')

            print('Preparing CPT files for \033[1m'+model+'\033[0;0m. Target: \033[1m'+tar+'\033[0;0m - Initialization \033[1m'+mon+'\033[0;0m...')        
            PrepFiles(fprefix, PREDICTAND, threshold_pctle, tini, tend, wlo1, wlo2,elo1, elo2, sla1, sla2, nla1, nla2, tgti, tgtf, mon, monf, fyr, os, wetday_threshold, tar, model, obs, obs_source, hdate_last, force_download, station, dic_sea, dic_sea_elr, pressure, MOS)
            print("New folder:")
            get_ipython().run_line_magic('cd', '$workdir/scripts')

            CPTscript(model,PREDICTAND, mon,monf, fyr, tini,tend,nla1,sla1,wlo1,elo1,nla2,sla2,wlo2,elo2,fprefix,mpref,tar,ntrain,MOS,station, xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min, ccamodes_max, forecast_anomaly, forecast_spi)

            print('Executing CPT for \033[1m'+model+'\033[0;0m. Target: \033[1m'+tar+'\033[0;0m - Initialization \033[1m'+mon+'\033[0;0m...')
            try:
                subprocess.check_output(cptdir+'CPT.x < params > CPT_log_'+model+'_'+tar+'_'+mon+'.txt',stderr=subprocess.STDOUT, shell=True)
            except subprocess.CalledProcessError as e:
                print(e.output.decode())
                raise
            print('----------------------------------------------')
            print('Calculations for Target: \033[1m'+tar+'\033[0;0m - Initialization \033[1m'+mon+'\033[0;0m completed!')

            print('See output folder, and check scripts/CPT_log_'+model+'_'+tar+'_'+mon+'.txt for log')
            print('\033[1mQuick error report from CPT (if any):\033[0;0m')
            with open('CPT_log_'+model+'_'+tar+'_'+mon+'.txt', "r") as fp:
                for line in lines_that_contain("Error:", fp):
                    print (line)
            print('----------------------------------------------')
            print('----------------------------------------------')

    print('')
    print('')
    print('')
    print('\033[1mPROCESS COMPLETED \033[0;0m')

    ## Skill Analysis

    models

    #####Choose metrics (options are: 'Pearson','Spearman','2AFC','RocAbove','RocBelow'):
    #met=('Ignorance','RPSS','GROC')
    met=('Pearson','Spearman','2AFC','RPSS','GROC')
    #monss=['Jul-Sep']   #Needs to be any of the seasons computed.#

    print("Present folder:")
    get_ipython().run_line_magic('cd', '$workdir/output')

    # Skill scores loop
    plt.rcParams.update({'font.size': 10})
    for ime in met:
        pltmap(models,PREDICTAND,ime,wlo2,elo2,sla2,nla2,fprefix,mpref,tgts,mons,monss,map_color, colorbar_option, use_ocean, x_offset=x_offset, y_offset=y_offset)   
        plt.savefig(('figures/Skill-Models-'+obs+'-'+MOS+'-' + ime + '.pdf'), dpi=300)
        print('')
        print("\033[1m"+ime+"\033[0;0m")
        #plt.close()
        #plt.show()
        print('')



    # ## Multi-model ensemble: NextGen

    #Indicate models to be included in the NextGen ensemble:
    #models=['EU-C3S-ECMWF-SEAS5','EU-C3S-MeteoFrance-System7','EU-C3S-UKMO-GloSea5GC2S15']

    mon, MOS

    print('\033[1m----Assessing skill for NextGen----\033[0;0m')
    model='NextGen'
    NEXTGEN_MOS='None'   #we're changing here to no MOS to assess skill of NextGen
    print('Using the following model(s) in the NextGen cocktail: \n\n'+str(models))
    print('')
    print('')
    print('')

    for mo in range(len(mons)):
        mon=mons[mo]
        tar=tgts[mo]
        tgti=tgtii[mo]
        tgtf=tgtff[mo]
        print("New folder:")
        get_ipython().run_line_magic('cd', '$workdir/input')

        print('Preparing CPT files for \033[1m'+model+'\033[0;0m. Target: \033[1m'+tar+'\033[0;0m - Initialization \033[1m'+mon+'\033[0;0m...')
        NGensemble(models,fprefix,PREDICTOR,PREDICTAND,mpref,'FCST_xvPr',tar,mon,tgti,tgtf,monf,fyr)
        
        print("New folder:")
        get_ipython().run_line_magic('cd', '$workdir/scripts')

        #We run CPT with a NoMOS configuration, BUT producing a MOS=mpref (e.g., CCA) file!
        CPTscript(model,PREDICTAND, mon,monf, fyr, tini,tend,nla1,sla1,wlo1,elo1,nla2,sla2,wlo2,elo2,fprefix,mpref,tar,ntrain,NEXTGEN_MOS,station, xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min, ccamodes_max, forecast_anomaly, forecast_spi)

        print('Executing CPT for \033[1m'+model+'\033[0;0m. Target: \033[1m'+tar+'\033[0;0m - Initialization: \033[1m'+mon+'\033[0;0m...')
        try:
            subprocess.check_output(cptdir+'CPT.x < params > CPT_log_'+model+'_'+tar+'_'+mon+'.txt',stderr=subprocess.STDOUT, shell=True)
        except subprocess.CalledProcessError as e:
            print(e.output.decode())
            raise
        print('----------------------------------------------')
        print('Calculations completed for \033[1m'+tar+'\033[0;0m - Initialization: \033[1m'+mon+'\033[0;0m')
        print('See output folder, and check scripts/CPT_log_'+model+'_'+tar+'_'+mon+'.txt for errors')
        print('\033[1mQuick error report from CPT (if any):\033[0;0m')
        with open('CPT_log_'+model+'_'+tar+'_'+mon+'.txt', "r") as fp:
            for line in lines_that_contain("Error:", fp):
                print (line)
            print('----------------------------------------------')
            print('----------------------------------------------')
        print('Generating FlexForecast files for \033[1m'+model+'\033[0;0m. Target: \033[1m'+tar+'\033[0;0m - Initialization: \033[1m'+mon+'\033[0;0m...')
        NGensemble(models,fprefix,PREDICTOR,PREDICTAND,mpref,'FCST_mu',tar,mon,tgti,tgtf,monf,fyr)    
        NGensemble(models,fprefix,PREDICTOR,PREDICTAND,mpref,'FCST_var',tar,mon,tgti,tgtf,monf,fyr) 
        print('----------------------------------------------')

    print('')
    print('')
    print('')
    print('\033[1mPROCESS COMPLETED \033[0;0m')



    #####Choose metrics (options are: 'Pearson','Spearman','2AFC','RocAbove','RocBelow','Ignorance','RPSS','GROC'):
    met=('Spearman','RPSS','GROC','2AFC')
    #monss=['Jul-Sep']#Aug-Oct','Mar-May','Apr-Jun','Sep-Nov']   #Needs to be any of the seasons computed.

    #models=['NextGen','CanSIPSv2', 'COLA-RSMAS-CCSM4', 'GFDL-CM2p5-FLOR-A06', 'GFDL-CM2p5-FLOR-B01', 'NASA-GEOSS2S', 'NCEP-CFSv2']

    print("Present folder:")
    get_ipython().run_line_magic('cd', '$workdir/output')

    # Skill scores loop
    plt.rcParams.update({'font.size': 10})
    for ime in met:
        pltmap(['NextGen'],PREDICTAND,ime,wlo2,elo2,sla2,nla2,fprefix,mpref,tgts,mons,monss,map_color, colorbar_option, use_ocean, x_offset=x_offset, y_offset=y_offset)   
        plt.savefig(('figures/Skill-NextGen-'+obs+'-'+MOS+'-' + ime + '.pdf'), dpi=300)
        print('')
        print("\033[1m"+ime+"\033[0;0m")
        #plt.show()
        print('')

    print("Present folder:")
    get_ipython().run_line_magic('cd', '$workdir')
    plt_ng_deterministic(models,PREDICTOR,PREDICTAND,wlo2,elo2,sla2,nla2,fprefix,mpref,tgts, mon, fyr, use_ocean)
    if MOS != "None":
        plt_ng_probabilistic(models,PREDICTOR,PREDICTAND,wlo2,elo2,sla2,nla2,fprefix,mpref,tgts, mon, fyr, use_ocean)
    else: 
        print('Probababilistic Forecasts only available when MOS=CCA or MOS=PCR')


    # ### Flexible Forecasts

    flexformat_threshold = 0.5 #threshold for the flexible format map --here is just a scalar (only one target season)
    ffthres_ispctl=True  #If True, flex format threshold is a percentile: 0.001 - 0.999

    print("Folder:")
    get_ipython().run_line_magic('cd', '$workdir/output')
    plt.rcParams.update({'font.size': 12})
    pltmapff(['NextGen'],PREDICTOR, PREDICTAND,flexformat_threshold,ffthres_ispctl,ntrain,wlo2,elo2,sla2,nla2,fprefix,mpref,monf,fyr,mons,tgts,x_offset=x_offset, y_offset=y_offset)
    plt.savefig("ProbFcst_Flex.pdf", dpi=300)

