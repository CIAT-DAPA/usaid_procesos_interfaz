
import os
import sys
import xarray as xr
import numpy as np
import pandas as pd
import subprocess
from pycpt_functions import *
from pycpt_dictionary import dic_s2s, dic_s2s_elr
from scipy.stats import t

import cartopy.crs as ccrs
from cartopy.feature import NaturalEarthFeature, LAND, COASTLINE
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
import calendar
import datetime


print("Python libraries loaded")
print("Now in the work directory:")

def run_pycpt_subseasonal(region, spatial_predictors, spatial_predictands, models, obs, station, mos, predictand, predictor, mons, tgtii, tgtff, tini, tend,
              xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min, ccamodes_max, force_download, single_models, forecast_anomaly, forecast_spi,
              confidence_level):

    current_date = datetime.date.today()
    #=================================================================================
    #         Case name (name of the case/experiment, try one word)
    #---------------------------------------------------------------------------------
    case=region

    #---------------------------------------------------------------------------------
    #                       Some folder and file options:
    #---------------------------------------------------------------------------------
    # Working directory --it should exist!!! (this is the place where the .py functions, notebooks, and .IRIDLAUTH is)
    workdir = '/forecast/PyCPT/iri-pycpt'
    # PATH to CPT root directory
    cptdir='/forecast/models/CPT/17.7.4/bin/'

    #---------------------------------------------------------------------------------
    #  Individual Models (choose between ECMWF,ECMWFrt, CFSv2, GEFS, CFSv2_SubX, GEPS6, ESRL)
    #                     MME :: (CFSv2_SUbX GEFS ESRL)
    #---------------------------------------------------------------------------------
    # Choose any model from the list and add any number of models

    #models = ['CFSv2_SubX', 'GEFS','ESRL']
    models = models

    # models to plot for individual models skill
    modeli=[models[0]] # choose any model from the list
    model=models
    #---------------------------------------------------------------------------------
    #     Obs (choose between CHIRPS, TRMM, CPC, IMD1deg, IMDp25deg)
    #---------------------------------------------------------------------------------
    obs=obs

    #---------------------------------------------------------------------------------
    #          MOS method (choose between None, PCR, CCA and ELR)
    #---------------------------------------------------------------------------------
    MOS=mos
    #MOS='PCR'
    if MOS=='ELR':
        from sklearn.linear_model import LogisticRegression

    #----------------------------------------------------------------------------------------------------------
    #                                       Forecast date  
    #  If ECMWF, it needs to be a Monday or a Thursday! CFSv2: any day; GEFS,ESRL: Wednesdays GEPS6: Thrusday
    #.                             SubX MME should start on Wednesday 
    #----------------------------------------------------------------------------------------------------------

    mon=current_date.strftime('%B')[0:3] 	# Forecast month 
    #mon="Sep"

    fyr=current_date.year  	# Forecast year

    fday=1 	# Forecast day  (Monday and Thursday in ECMWF model; yesterday in CFSv2: real time)
    #----------------------------------------------------------------------------------------------------
    #                          Traning Season (One month or Three MOnth)
    #----------------------------------------------------------------------------------------------------
    training_season=mon
    #training_season='May-Jul'  # with *mon* in the middle, e.g., 'May-Jul' if mon='Jun'

    xmodes_min = xmodes_min
    xmodes_max = xmodes_max
    ymodes_min = ymodes_min
    ymodes_max = ymodes_max
    ccamodes_min = ccamodes_min
    ccamodes_max = ccamodes_max

    #----------------------------------------------------------------------------------------------------
    #                                           Switches:
    #----------------------------------------------------------------------------------------------------
    #force download of data files, even if they already exist locally
    force_download = force_download

    # Rainfall frequency switch 
    rainfall_frequency = False  #False gives total rainfall for forecast periods

    wetday_threshold = 3 #WET day threshold (mm) --only used if rainfall_frequency is True!
    threshold_pctle = False    #False for threshold in mm; Note that if True then if counts DRY days!!!

    #----------------------------------------------------------------------------------------------------
    #                              Spatial domain for predictor
    #----------------------------------------------------------------------------------------------------

    # Spatial domain for predictor

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
    #-------------------------------------------------------------------------------------------------------------
    #  Forecast lead interval  -- This counts using L, the lead time; e.g., ECMWF L=0..46 (NOT calendar days)
    #-------------------------------------------------------------------------------------------------------------
    nwk=4 # Number of weeks to process (leads)
    # Lists for looping over lead times -- ALL these arrays should have the same dimension (see nwk above), as they correspond

    wk =   [1       ,2       ,3       ,34       ,4       ]    # week-lead number label (e.g., corresponding to week1, week2, week3, week4, week1-4/month 1)
    wknam= ['Week 1','Week 2','Week 3','Week 3-4','Month 1']  #naming the weeks (for plots) --can use a different language here
    day1 = [1       ,7       ,14      ,14      ,21        ]   # first lead day of target weeks 
    day2 = [7       ,14      ,21      ,28      ,28       ]    # last lead day of target weeks 

    # ECMWF - first day is day 0, 0000Z accumulated rainfall; specify day1=1 for week 1
    # GEFS/GEPS - first day is day 0.5 (daily average rainfall rate); specify day1=0 for week 1
    # CFSv2 - first day is day 1, 0000Z accumulated rainfall over the first day; specify day1=1 for week 1 
    #-------------------------------------------------------------------------------------------------------------
    # Want to add a topo background to the domain plots?
    #-------------------------------------------------------------------------------------------------------------
    use_topo=True
    map_color='WindowsCPT' #set to "WindowsCPT" for CPT colorscheme
    colorbar_option=True
    use_ocean=False

    #-------------------------------------------------------------------------------------------------------------
    #                                     Observation dataset URLs
    #-------------------------------------------------------------------------------------------------------------
    #           Naming of output files
    #-------------------------------------------
    if rainfall_frequency:
        fprefix = 'RFREQ'
    else:
        fprefix = 'PRCP'
    #-------------------------------------------
    if obs == 'TRMM':
        obs_source = 'SOURCES/.NASA/.GES-DAAC/.TRMM_L3/.TRMM_3B42/.v7/.daily/.precipitation/X/0./1.5/360./GRID/Y/-50/1.5/50/GRID'
        obsclimo_source = ''
        hdate_last = 2014
    elif obs == 'CPC':
        obs_source = 'SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.extREALTIME/.rain/X/0./.5/360./GRID/Y/-90/.5/90/GRID'
        obsclimo_source = ''
        hdate_last = 2018
    elif obs == 'CHIRPS':
        obs_source = 'SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p25/.prcp/X/-180./.5/180./GRID/Y/-90/.5/90/GRID'
        obsclimo_source = 'home/.mbell/.UCSB/.v2p0/.daily-improved/.global/.0p25/.climatology/.pc9514/.prcp/X/-180./.5/180./GRID/Y/-90/.5/90/GRID'
        obsclimo2_source = 'SOURCES/.ECMWF/.S2S/.climatologies/.observed/.CHIRPS/.prcpSmooth/X/-180./.5/180./GRID/Y/-90/.5/90/GRID'
        hdate_last = 2018
    elif obs == 'IMD1deg':
        obs_source = 'SOURCES/.IMD/.NCC1-2005/.v4p0/.rf'
        obsclimo_source = ''
        hdate_last = 2015
    elif obs == 'IMDp25deg':
        obs_source = 'SOURCES/.IMD/.RF0p25/.gridded/.daily/.v1901-2015/.rf'
        obsclimo_source = ''
        hdate_last = 2016
    else:
        print ("Obs option is invalid")

    #------------------------------------------
    #         MOS-dependent parameters
    #------------------------------------------
    if MOS=='None':
        mpref='noMOS'
    elif MOS=='CCA':
        mpref='CCA'
    elif MOS=='PCR':
        mpref='PCR'
    elif MOS=='ELR':
        mpref='ELRho'
    #else:
    #    print ("MOS option is invalid")

    #---------------------------------------------
    #             S2S Database key
    #---------------------------------------------
    print("PyCPT folder is:")
    os.chdir(os.path.join(workdir))
    os.mkdir(os.path.join(workdir, case))
    # %cd $workdir
    # !mkdir -p $case
    with open(workdir+'/.IRIDLAUTH') as file:
        authkey = file.read() 

        #%cd $case
    os.chdir(os.path.join(workdir, case))
    workdir = os.getcwd()

    if rainfall_frequency:
        print('Predictand is Rainfall Frequency; wet day threshold = '+str(wetday_threshold)+' mm')
    else:
        print('Predictand is Rainfall Total (mm)')

    print("")
    print("Creating input, output and script folders, if not already there...")
    #print("Work directory is:")
    #%cd $workdir
    os.chdir(os.path.join(workdir))
    os.mkdir('input')
    os.mkdir('input/noMOS')
    os.mkdir('output')
    os.mkdir('scripts')
    os.system('rm -Rf scripts/*')
    os.mkdir('output/figures')
    # Set up CPT environment
    os.environ["CPT_BIN_DIR"] = cptdir
    print("")
    print("CPT environment loaded. Using this CPT version:")
    print(cptdir)

    #Print calendars
    print('Forecast date is '+str(fday)+' '+mon+' '+str(fyr))
    print('')
    c=calendar.TextCalendar(calendar.MONDAY)
    print(c.formatmonth(fyr,list(calendar.month_abbr).index(mon)))
    if list(calendar.month_abbr).index(mon)+1==13:
        print(c.formatmonth(fyr+1,1))
    else:
        print(c.formatmonth(fyr,list(calendar.month_abbr).index(mon)+1))

    # #Plot domains
    # pltdomain(wlo1,elo1,nla1,sla1,wlo2,elo2,nla2,sla2)

    from urllib import parse
    GEPShdate1 = 'dummy_string'

    for model in models:
        ########Model-dependent parameters
        print('Model::',model)
        hstep =0
        nlag=0
        #-------------------------------------------------------------------------------------------------------
        # Hstep : use all starts in the trainng period with this daily step between them 
        # ntrain : Number of samples that are used in trainig 
        # nlag: length of the lagged ensemble in days,Only relevent for CFSv2 for other it is a dummy variable to pass
        # lit: Initial training period for retroactive forecasts (in timesteps; e.g., 80 for ECMWF, 28 for CFSv2)
        # liti:Update interval for retroactive forecasts (in timesteps; e.g., 10)
        #---------------------------------------------------------------------------------------------------------
        if model=='CFSv2':
            hstep = 7 
            nlag  = 3  
            ntrain= 55 
            lit =  28
            liti = 10
        elif model=='ECMWF' or model=='ECMWFrt':
            ntrain= 144 
            lit =  70
            liti = 20
        elif model=='GEFS':
            ntrain= 78 
            lit =  35
            liti = 10
        elif model=='GEPS6':
            ntrain= 80 
            GEPShdate1 = '0000 4 Jun' # first hindcast date each year in training season
            # Must be a THURSDAY in the forecast year since GEPS is an on-the-fly model
            GEPShdate1 = parse.quote(GEPShdate1)
            lit =  40
            liti = 10
        elif model=='CFSv2_SubX':
            hstep = 7 
            nlag  = 3  
            ntrain= 78  
            lit =  35
            liti = 10

        elif model=='ESRL': 
            ntrain= 78 
            lit =  35
            liti = 10
        else:
            print ("Model option is invalid")
        
        
        for L in range(nwk):
            nday=day2[L]-day1[L]	# Length of target period (days) 
            print("New folder:")
            #%cd $workdir/input
            os.chdir(os.path.join(workdir, 'input'))
            
            PrepFiles(rainfall_frequency, threshold_pctle, wlo1, wlo2,elo1, elo2,sla1, sla2,nla1, nla2, day1[L], day2[L], fday, nday, fyr, mon, os, authkey, wk[L], wetday_threshold, nlag, training_season, hstep, model, obs_source, obsclimo_source, obsclimo2_source, hdate_last, GEPShdate1, force_download, mpref, MOS, dic_s2s, dic_s2s_elr)
            print("New folder:")
            #%cd $workdir/scripts
            os.chdir(os.path.join(workdir, 'scripts'))

            CPTscript(model,mon,fday,lit,liti,wk[L],nla1,sla1,wlo1,elo1,nla2,sla2,wlo2,elo2,fprefix,mpref,training_season,ntrain,rainfall_frequency,MOS, xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min, ccamodes_max)

            print('Executing CPT for Week '+str(wk[L])+'... This might take a while... (want to grab a tea or a coffee?)')
            try:
                    subprocess.check_output(cptdir+'CPT.x < params > CPT_log_'+model+training_season+'_ini'+mon+str(fday)+'_'+str(fyr)+'_wk'+str(wk[L])+'.txt',stderr=subprocess.STDOUT, shell=True)
            except subprocess.CalledProcessError as e:
                    print(e.output.decode())
                    raise
            print('\033[1m----------------------------------------------\033[0;0m')
            print('\033[1mWeek '+str(wk[L])+' calculations completed!\033[0;0m')
            print('\033[1mSee output folder, and check scripts/CPT_log_'+model+training_season+'_ini'+mon+str(fday)+'_'+str(fyr)+'_wk'+str(wk[L])+'.txt for log\033[0;0m')
            print('\033[1mQuick error report from CPT (if any):\033[0;0m')
            with open('CPT_log_'+model+training_season+'_ini'+mon+str(fday)+'_'+str(fyr)+'_wk'+str(wk[L])+'.txt', "r") as fp:
                    for line in lines_that_contain("Error:", fp):
                        print (line)
            print('\033[1m----------------------------------------------\033[0;0m')
            print('----------------------------------------------')

        print('')
        print('')
        print('')
        print('\033[1mPROCESS COMPLETED \033[0;0m')

    #     neofs_plot = 3

    # print("Present folder:")
    # #%cd $workdir/output
    # os.chdir(os.path.join(workdir, 'output'))
    # print('')
    # print('')

    # if MOS=='None':
    #     print("To compute EOFs you need to choose MOS=PCR or MOS=CCA")
    # else:
    #     # EOF loop
    #     plt.rcParams.update({'font.size': 11})
    #     for imod in range(neofs_plot):
    #         for L in range(nwk):
    #             plteofs_eofts(models,imod,neofs_plot,wlo1,elo1,sla1,nla1,wlo2,elo2,sla2,nla2,fprefix,mpref,training_season,nwk, wk[L], wknam[L], map_color, colorbar_option, use_ocean)
    #             plt.show()

    # nccas_plot=2

    # print("Present folder:")
    # #%cd $workdir/output
    # os.chdir(os.path.join(workdir, 'output'))
    # print('')
    # print('')

    # if MOS=='None':
    #     print("To compute CCAs you need to choose MOS=CCA")  
    # else:
    #     # CCA loop
    #     for imod in range(nccas_plot):
    #         for L in range(nwk):    
    #             plt.rcParams.update({'font.size': 11})
    #             #pltccas_ccats(models,imod,nccas_plot,wlo1,elo1,sla1,nla1,wlo2,elo2,sla2,nla2,fprefix,mpref,training_season,nwk, wk[L], map_color, colorbar_option, use_ocean)
    #             plt.show()

    # model = models[0]
    # print("New folder:")
    # #%cd $workdir/output
    # os.chdir(os.path.join(workdir, 'output'))

    # # Skill scores loop
    # plt.rcParams.update({'font.size': 10})
    # #for ime in ('Pearson','Spearman','RocAbove','RocBelow','Ignorance','RPSS','GROC'):
    # for ime in ('Spearman','2AFC','RocAbove','RocBelow','Ignorance','RPSS','GROC'):
    #     pltmap(model,ime,wlo2,elo2,sla2,nla2,fprefix,mpref,training_season, mon, fday, nwk, wk) 
    #     plt.savefig(('figures/Skill-'+model+'-'+obs+'-'+MOS+'-' + ime + '.pdf'), dpi=300)
    #     plt.show()
    #     print('----------------------------------------------')
    #     print('')
        
    # print('')
    # print('')
    # print('')
    # print('\033[1mPROCESS COMPLETED \033[0;0m')

    # print("Folder:")
    # #%cd $workdir/output
    # os.chdir(os.path.join(workdir, 'output'))

    # #Desired scores: 
    # #score=('Spearman','2AFC','RocAbove','RocBelow','Ignorance','RPSS','GROC')
    # score=('Spearman','2AFC','RocAbove','RocBelow')
    # #Coordinates of the box:
    # lon1=35 #lon of upper left point of geographical sub-domain
    # lat1=5  #lat of upper left point of geographical sub-domain
    # lon2=40 #lon of lower right point of geographical sub-domain
    # lat2=10  #lat of lower right point of geographical sub-domain
    # # Skill scores loop
    # df=skilltab(model,score,wknam,lon1,lat1,lat2,lon2,wlo2,elo2,sla2,nla2,fprefix,mpref,training_season,mon,fday,nwk,wk)
    # df

    # modeli=['ECMWF']
    # model='ECMWF'
    # print(model)
    # print("Folder:")
    # ##--------------------------------------------------------------------------------------------------------------------
    # #%cd $workdir/output
    # # os.chdir(os.path.join(workdir, 'output'))

    # # plt.rcParams.update({'font.size': 10})
    # # pltmap(model,mpref+'FCST_V',wlo2,elo2,sla2,nla2,fprefix,mpref,training_season, mon, fday, nwk, wk)
    # # plt.savefig('figures/Deterministic-'+model+'-'+obs+'-'+MOS+'.pdf', dpi=300)
    # # print('----------------------------------------------')
    # # print('')
    # # plt.rcParams.update({'font.size': 11})
    # #pltmapProb(model,wlo2,elo2,sla2,nla2,fprefix,mpref,training_season, mon, fday, nwk, wk)
    # #%cd $workdir 
    # ##--------------------------------------------------------------------------------------------------------------------
    # os.chdir(os.path.join(workdir))
    # plt_ng_probabilistic(modeli,wlo2,elo2,sla2,nla2,fprefix,mpref,training_season,wk,nwk,fday,mon,fyr, use_ocean)
    # #%cd $workdir/output
    # os.chdir(os.path.join(workdir, 'output'))
    # plt.savefig('../output/figures/ProbFcst'+model+'-'+obs+'-'+MOS+'.pdf', dpi=300)

    # #threshold for the flexible format map (in mm/week -- remember: fcsts are in ANOMALIES!):
    # flexformat_threshold = [0.5,0.5,0.5,0.5] #array (we need one threshold per forecast week)
    # ffthres_ispctl=True  #If True, flex format threshold is a percentile: 0.001 - 0.999

    # print("Folder:")
    # #%cd $workdir/output
    # os.chdir(os.path.join(workdir, 'output'))
    # plt.rcParams.update({'font.size': 12})
    # if mpref=='noMOS' and fprefix=='PRCP':
    #     pltmapff(flexformat_threshold,ffthres_ispctl,ntrain,wlo1,elo1,sla1,nla1,fprefix,mpref,training_season,mon,fday,nwk,wk)
    # else:
    #     pltmapff(model,flexformat_threshold,ffthres_ispctl,ntrain,wlo2,elo2,sla2,nla2,fprefix,mpref,training_season,mon,fday,nwk,wk)
    # plt.savefig('figures/ProbFcst-Flexplt'+model+'-'+obs+'-'+MOS+'.pdf', dpi=300)

    # #threshold for the flexible format map (in mm/week -- remember: fcsts are in ANOMALIES!):
    # flexformat_threshold = [0.5,0.5,0.5,0.5] #array (we need one threshold per week to forecast)
    # ffthres_ispctl=True  #If True, flex format threshold is a percentile: 0.001 - 0.999
    # #Location coordinates:
    # lon=38
    # lat=9.
    
    # print("Folder:")
    # #%cd $workdir/output
    # os.chdir(os.path.join(workdir, 'output'))
    # plt.rcParams.update({'font.size': 12})
    # pltprobff(model,flexformat_threshold,ffthres_ispctl,ntrain,lon,lat,wlo2,elo2,sla2,nla2,fprefix,mpref,training_season,mon,fday,nwk,wk)
    # plt.savefig('figures/ProbExceed-'+model+'-'+obs+'-'+MOS+'.pdf', dpi=300)