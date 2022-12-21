#This is PyCPT_functions_seasonal.py (version1.9) -- 2 Dec 2020
#Authors: AG Muñoz (agmunoz@iri.columbia.edu) and Andrew W. Robertson (awr@iri.columbia.edu)
#Notes: be sure it matches version of PyCPT
#Requires: CPTv16.5.2+
#Log:

#* Started simplifying functions, wrote readGrADSctl function; added functions to create the NextGen files for det skill assessment and plotting --AGM, Sep 2019
#* Fixed bug with plotting functions when selecting a subset of the seasons, and added start time for forecast file in CPT script -- AGM, July 1st 2019
#* Added VQ and UQ from CFSv2. User can now select the seasons to visualize in the skill and EOF maps. Fixed bug related to coordinate selection in CHIRPS, TRMM and CPC. -- AGM, June 13th 2019
#* First Notebook seasonal version -- AGM, May 7th 2019
#* Several PyCPT sub-seasonal versions (through v1.2) --see logs in that version 2018-present
#* First iPython sub-seasonal version (Jupyter Notebook) -- AWR, 24 Jun 2018
#* First similar version (BASH for CFSv2) by Muñoz and Chourio for the OLE2 -- 12 Dec 2010

#To Do: (as June 8th, 2019 -- AGM)
#	+ ELR proceedure is not reproducing results obtained in R or Matlab
#	+ Simplify download functions: just one function, with the right arguments and dictionaries.
#	+ Check Hindcasts and Forecast_RFREQ
import os
import sys
import platform
import warnings
import struct
import xarray as xr
import numpy as np
import pandas as pd
from copy import copy
from scipy.stats import t
from scipy.stats import invgamma
from scipy import stats
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.ticker as ticker
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from matplotlib.ticker import Formatter, MaxNLocator
from matplotlib.colors import LinearSegmentedColormap
import cartopy.crs as ccrs
from cartopy import feature
from cartopy.io.shapereader import Reader
from cartopy.feature import ShapelyFeature
from cartopy.mpl.geoaxes import GeoAxes
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
from cartopy.mpl.ticker import LatitudeFormatter, LongitudeFormatter
import fileinput
import numpy.ma as ma
from netCDF4 import Dataset
from sklearn.linear_model import LogisticRegression
from subprocess import call
import re
from datetime import datetime as dt


warnings.filterwarnings("ignore")

def lines_that_equal(line_to_match, fp):
	return [line for line in fp if line == line_to_match]

def lines_that_contain(string, fp):
	return [line for line in fp if string in line]

def lines_that_start_with(string, fp):
	return [line for line in fp if line.startswith(string)]

def lines_that_end_with(string, fp):
	return [line for line in fp if line.endswith(string)]

def exceedprob(x,dof,lo,sc):
	return t.sf(x, dof, loc=lo, scale=sc)*100

def writeGrads(fcsttype, filename, models, predictor, predictand, mpref, tgt, mon, fyr, monf):
	if fcsttype == 'FCST_Obs':
		lats, longs, data, years = read_forecast('deterministic', models[0], predictor, predictand, mpref, tgt, mon, fyr, filename='../input/NextGen'+'_'+predictor+ '_' + tgt+'_ini'+monf+'.tsv', converting_tsv=True)
	else:
		lats, longs, data, years = read_forecast('deterministic', models[0], predictor, predictand, mpref, tgt, mon, fyr, filename='../output/NextGen'+'_'+predictor+predictand+'_'+mpref+fcsttype+'_'+tgt+'_'+monf+str(fyr)+'.tsv', converting_tsv=True)
	W, XD = len(longs), longs[1] - longs[0]
	H, YD = len(lats), lats[0] - lats[1]
	T = len(years)
	f=open('../output/NextGen' +'_'+predictor+predictand+'_'+mpref+fcsttype+ '_' +tgt+'_'+monf+str(fyr)+'.ctl','w')
	f.write('DSET {}\n'.format('./output/NextGen' +'_'+predictor+predictand+'_'+mpref+fcsttype+ '_' +tgt+'_'+monf+str(fyr)+'.dat'))
	f.write('TITLE {}\n'.format('NextGen_{}'.format(fcsttype)))
	f.write('UNDEF -999.000000\n')
	f.write('OPTIONS yrev sequential little_endian\n')
	f.write('XDEF {} LINEAR {} {}\n'.format(W, longs[0], XD))
	f.write('YDEF {} LINEAR {} {}\n'.format(H, lats[-1], YD))
	f.write('TDEF {} LINEAR 1{}{} 1yr\n'.format(T, tgt[0:3], years[0]))
	f.write('ZDEF 1 LINEAR 1 1\n')
	f.write('VARS 1\n')
	f.write('\ta\t0\t99\t{}                   unitless\n'.format(predictand))
	f.write('ENDVARS')
	f.close()
	print('Wrote {}'.format('../output/NextGen' +'_'+predictor+predictand+'_'+mpref+fcsttype+ '_' +tgt+'_'+monf+str(fyr)+'.ctl'))

	f=open('../output/NextGen'+'_'+predictor+predictand+'_'+mpref+fcsttype+ '_' +tgt+'_'+monf+str(fyr)+'.dat','wb')
	for t in range(T):
		data[t][np.isnan(data[t])] = -999.000
		f.write(struct.pack('i', int(W*H*np.dtype('float32').itemsize)))
		for i in range(H):
			for j in range(W):
				f.write(struct.pack('f', float(data[t][i][j])))
		f.write(struct.pack('i', int(W*H*np.dtype('float32').itemsize)))
	f.close()
	print('Wrote {}'.format('../output/NextGen' +'_'+predictor+predictand+'_'+mpref+fcsttype + '_' +tgt+'_'+monf+str(fyr)+'.dat'))


class MidpointNormalize(colors.Normalize):
    def __init__(self, vmin=None, vmax=None, midpoint=None, clip=False):
        midpoint = midpoint
        colors.Normalize.__init__(self, vmin, vmax, clip)

    def __call__(self, value, clip=None):
        # I'm ignoring masked values and all kinds of edge cases to make a
        # simple example...
        x, y = [vmin, midpoint, vmax], [0, 0.5, 1]
        return np.ma.masked_array(np.interp(value, x, y))

def discrete_cmap(N, base_cmap=None):
	"""Create an N-bin discrete colormap from the specified input map"""
	# Note that if base_cmap is a string or None, you can simply do
	#    return plt.cm.get_cmap(base_cmap, N)
	# The following works for string, None, or a colormap instance:
	base = plt.cm.get_cmap(base_cmap)
	color_list = base(np.linspace(0, 1, N))
	cmap_name = base.name + str(N)
	#base.set_bad(color='white')
	#return base.from_list(cmap_name, color_list, N)
	return LinearSegmentedColormap.from_list(cmap_name, color_list, N) #perceptually uniform colormaps

def make_cmap( map_color='bwr', N=11, continuous=True):
	if map_color == 'WindowsCPT':
		colors = [(238, 43, 51), (255, 57, 67),(253, 123, 91),(248, 175, 123),(254, 214, 158),(252, 239, 188),(255, 254, 241),(244, 255,255),(187, 252, 255),(160, 235, 255),(123, 210, 255),(89, 179, 238),(63, 136, 254),(52, 86, 254)]
		colors = [ (colors[i][0] / 255.0, colors[i][1] / 255.0, colors[i][2] / 255.0) for i in range(len(colors))]
		colors.reverse()
		if str(continuous) == "continuous":
			return LinearSegmentedColormap.from_list( "CPT", colors)
		else:
			return LinearSegmentedColormap.from_list( "CPT", colors, N=N)
	else:
		map_color = 'bwr' #override user input
		if str(continuous) == 'continuous':
			return plt.get_cmap(map_color)
		else:
			return plt.get_cmap(map_color, N)

def make_cmap_blue(x):
	colors = [(244, 255,255),
	(187, 252, 255),
	(160, 235, 255),
	(123, 210, 255),
	(89, 179, 238),
	(63, 136, 254),
	(52, 86, 254)]
	colors = [ (colors[i][0] / 255.0, colors[i][1] / 255.0, colors[i][2] / 255.0) for i in range(len(colors))]
	#colors.reverse()
	return LinearSegmentedColormap.from_list( "matlab_clone", colors, N=x)

def replaceAll(file,searchExp,replaceExp):
    for line in fileinput.input(file, inplace=1):
        if searchExp in line:
            line = line.replace(searchExp,replaceExp)
        sys.stdout.write(line)

def setup_params(PREDICTOR,PREDICTAND,obs,MOS,tini,tend):
	"""PyCPT setup"""
	# Predictor switches
	if PREDICTOR=='PRCP' or PREDICTOR=='UQ' or PREDICTOR=='VQ' or PREDICTOR=='UA' or PREDICTOR=='VA' or PREDICTOR=='T2M' or PREDICTOR=='TMAX' or PREDICTOR=='TMIN' or PREDICTOR=='SST':
		rainfall_frequency = False  #False uses total rainfall for forecast period, True uses frequency of rainy days
		threshold_pctle = False
		wetday_threshold = -999 #WET day threshold (mm) --only used if rainfall_frequency is True!
	elif PREDICTOR=='RFREQ':
		rainfall_frequency = True  #False uses total rainfall for forecast period, True uses frequency of rainy days
		wetday_threshold = 3 #WET day threshold (mm) --only used if rainfall_frequency is True!
		threshold_pctle = False    #False for threshold in mm; Note that if True then it counts DRY days!!!

	if PREDICTAND=='RFREQ':
		print('Predictand is Rainfall Frequency; wet day threshold = '+str(wetday_threshold)+' mm')
	if PREDICTAND=='PRCP':
		print('Predictand is Rainfall Total (mm)')
	if PREDICTAND=='userdef':
		print('Predictand is a misterious field known by the user :P')

	########Observation dataset URLs
	hdate_last = 2015  #some arbitrary year --it gets updated below
	if obs == 'CPC-CMAP-URD':
	    obs_source = 'SOURCES/.Models/.NMME/.CPC-CMAP-URD/prate'
	    hdate_last = 2010
	elif obs == 'TRMM':
	    obs_source = 'SOURCES/.NASA/.GES-DAAC/.TRMM_L3/.TRMM_3B42/.v7/.daily/.precipitation/X/-180./1.5/180./GRID/Y/-50/1.5/50/GRID'
	    hdate_last = 2014
	elif obs == 'CPC':
	    obs_source = 'SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.extREALTIME/.rain/X/-180./1.5/180./GRID/Y/-90/1.5/90/GRID'
	    hdate_last = 2020
	elif obs == 'CHIRPS':
	    obs_source = 'SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p25/.prcp/30/mul'
	    hdate_last = 2020
	elif obs == 'Chilestations':
	    obs_source = 'home/.xchourio/.ACToday/.CHL/.prcp'
	    hdate_last = 2019
	elif obs == 'ENACTS-BD':
		obs_source = 'SOURCES/.Bangladesh/.BMD/.monthly/'
	elif obs == 'userdef':
	    obs_source = ''
	else:
	    print ("Obs option is invalid")

	if obs == 'ENACTS-BD':
		x_offset, y_offset = 0.6, 0.4
	else: 
		x_offset, y_offset =0.0, 0.0
	########MOS-dependent parameters
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

	L=['1'] #lead for file name (TO BE REMOVED --requested by Xandre)
	ntrain= tend-tini+1 # length of training period
	fprefix = PREDICTOR
	return rainfall_frequency,threshold_pctle,wetday_threshold,obs_source,hdate_last,mpref,L,ntrain,fprefix,x_offset, y_offset

def plt_single_deterministic(models,PREDICTOR,PREDICTAND,loni,lone,lati,late,fprefix,mpref,tgts, mon, fyr, use_ocean):
	cbar_loc, fancy = 'bottom', True
	nmods=len(models)
	nsea=len(tgts)
	# First all of the data is extracted from the output folder.
	# This results in a nested list for example of the dimension (1, 2, dim(lon), dim(lat))
	# of which 1 would be the season, 2 the number of models and the rest the dimension of the data.
	#The following print statements are for testing, and do not affect the functionality of the function

	list_det_by_season = [[] for i in range(nsea)]
	for i in range(nmods):
		for j in range(nsea):
			if platform.system() == "Windows":
				plats, plongs, av, years = read_forecast_bin('deterministic', models[i], PREDICTOR, PREDICTAND, mpref, tgts[j], mon, fyr )
			else:
				plats, plongs, av, years = read_forecast('deterministic', models[i], PREDICTOR, PREDICTAND, mpref, tgts[j], mon, fyr )
			#for kl in range(av.shape[0]):
			list_det_by_season[j].append(av[0])
	#print("shape of av", np.shape(av))
	#print('shape of av[0]', np.shape(av[0]))
	#print("shape of av[0]", av.shape[0])
			#if platform.system() == "Windows":
			#	dlats, dlongs, av = read_forecast_bin('deterministic', models[i], PREDICTAND, mpref, tgts[j], mon, fyr )
			#else:
			#	dlats, dlongs, av = read_forecast('deterministic', models[i], PREDICTAND, mpref, tgts[j], mon, fyr )
			#list_det_by_season[j].append(av[0])
	#The following print statements are for testing, and do not affect the functionality of the function
	#list_all_array = np.asarray(list_det_by_season)
	#print("shape of big list with all the data: ", np.shape(list_all_array))

	detfcst_by_season = []
	for k in range(nmods):
		detfcst_by_season.append([])
		for j in range(nsea):
			d_array = np.asarray(list_det_by_season[j][k])
			#print("Shape of d_array", np.shape(d_array))
			#print("k",k)
			#print("j",j)
			#d_nanmean = np.nanmean(d_array, axis=0)
			detfcst_by_season[k].append(d_array)
	#detfcst_by_season_ar = np.asarray(detfcst_by_season)
	#print('Shape of big list after manip.:', np.shape(detfcst_by_season_ar))
		
		#print(type(detfcst_by_season))
		#print(np.shape(detfcst_by_season))
		#print(np.shape(d_array))
		#print(type(d_array))
		
		
		
	fig = plt.figure(figsize=(nsea*13, nmods*10))
	gs = fig.add_gridspec(nrows=nmods, ncols=nsea,hspace=0)
	axs = gs.subplots(sharex=False,sharey=False, subplot_kw={'projection': ccrs.PlateCarree()})
	#print(nmods)
	#print(nsea)
	#print(axs)
	#print(type(axs))

	if nmods == 1 and nsea == 1: 
		axs = [[axs]]
	elif nmods > 1 and nsea == 1: 
		axs = [  [ axs[i] ] for i in range(nmods)]
	elif nsea > 1 and nmods == 1: 
		axs = [[ axs[i] for i in range(nsea)]]
	else: 
		pass 
	#print(type(axs))
	#print("axs shape is ", np.shape(np.asarray(axs)))
	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
	#				name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')

	for i in range(nmods):
		for j in range(nsea):
			current_cmap = plt.get_cmap('BrBG') ## can choose 'nipy_spectral' for higher contrast if desired
			current_cmap.set_under('white', 0.0)

			current_cmap_copper = plt.get_cmap('YlOrRd', 9)
			current_cmap_binary = plt.get_cmap('Greens', 4)
			current_cmap_ylgn = make_cmap_blue(9)

			lats, longs = plats, plongs

			axs[i][j].set_extent([longs[0],longs[-1],lats[0],lats[-1]], ccrs.PlateCarree())

			axs[i][j].add_feature(states_provinces, edgecolor='black')
			if str(use_ocean) == "True":
				axs[i][j].add_feature(feature.OCEAN)
			axs[i][j].add_feature(feature.LAND)
			axs[i][j].add_feature(states_provinces, edgecolor='black')
			#ax[i][j].add_feature(feature.COASTLINE)
			pl=axs[i][j].gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				linewidth=1, color='gray', alpha=0.5, linestyle=(0,(2,4)))
			pl.xlabels_top = False
			pl.ylabels_left = True
			pl.ylabels_right = False
			#pl.xlabels_bottom = False
			#if i == nmods - 1: change so long vals in every plot
			pl.xlabels_bottom = True
			pl.xformatter = LONGITUDE_FORMATTER
			pl.yformatter = LATITUDE_FORMATTER
			pl.xlabel_style = {'size': 8}#'rotation': 'vertical'}

			axs[i][j].set_ybound(lower=lati, upper=late)

			if j == 0:
				axs[i][j].text(-0.25, 0.5, models[i],rotation='vertical', verticalalignment='center', horizontalalignment='center', transform=axs[i][j].transAxes)

			#labels = ['Rainfall (mm)', 'Probability (%)']
			#axs[i][j].set_title(tgts[j])


			#fancy probabilistic
			#print(pbn[i][j])
			#print(np.shape(pbn))
			#print(i)
			#print(j)
			
			CS_det = axs[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), detfcst_by_season[i][j],
					#norm=MidpointNormalize(midpoint=0.),
					cmap=current_cmap)
			#print(i,j)
			#print('model index',i,'season index',j,detfcst_by_season[i][j],'\n')
	axs[0][int(nsea/2)].set_title(tgts[j])
	if cbar_loc == 'left':
		#fancy deterministic cb left
		axins_det = inset_axes(axs[-1][int(nsea/2)], width="5%",  height="100%", loc='center left', bbox_to_anchor=(-0.25, 0., 1, 1),bbox_transform=axs[-1][int(nsea/2)].transAxes, borderpad=0.1 )
		cbar_ldet = fig.colorbar(CS_det, ax=axs[-1][int(nsea/2)], cax=axins_det,  orientation='vertical', pad=0.02)
		cbar_ldet.set_label(labels[i]) #, rotation=270)\
		axins_det.yaxis.tick_left()
	else:
		#fancy deterministic cb bottom
		axins_det = inset_axes(axs[-1][int(nsea/2)],width="100%",  height="5%",  loc='lower center',bbox_to_anchor=(-0.1, -0.15, 1.1, 1), bbox_transform=axs[-1][int(nsea/2)].transAxes,borderpad=0.1 )
		cbar_bdet = fig.colorbar(CS_det, ax=axs[-1][int(nsea/2)],  cax=axins_det, orientation='horizontal', pad = 0.02)#, ticks=bounds)
		cbar_bdet.set_label("Rainfall (mm)")
	fig.savefig('./output/figures/Single_Deterministic_RealtimeForecasts.png', dpi=500, bbox_inches='tight')

def plt_single_deterministic_and_confidenceIntervals(models,PREDICTOR,PREDICTAND,loni,lone,lati,late,fprefix,mpref,tgts, mon, fyr, use_ocean, confidence_level, forecast_spi):
	cbar_loc, fancy = 'bottom', True
	nmods=len(models)
	nsea=len(tgts)
	# First all of the data is extracted from the output folder.
	# This results in a nested list for example of the dimension (1, 2, dim(lon), dim(lat))
	# of which 1 would be the season, 2 the number of models and the rest the dimension of the data.
	#The following print statements are for testing, and do not affect the functionality of the function

	list_det_by_season = [[] for i in range(nsea)]
	for i in range(nmods):
		for j in range(nsea):
			if platform.system() == "Windows":
				plats, plongs, av, years = read_forecast_bin('deterministic', models[i], PREDICTOR, PREDICTAND, mpref, tgts[j], mon, fyr )
			else:
				plats, plongs, av, years = read_forecast('deterministic', models[i], PREDICTOR, PREDICTAND, mpref, tgts[j], mon, fyr )
			#for kl in range(av.shape[0]):
			list_det_by_season[j].append(av[0])
	#print("shape of av", np.shape(av))
	#print('shape of av[0]', np.shape(av[0]))
	#print("shape of av[0]", av.shape[0])
			#if platform.system() == "Windows":
			#       dlats, dlongs, av = read_forecast_bin('deterministic', models[i], PREDICTAND, mpref, tgts[j], mon, fyr )
			#else:
			#       dlats, dlongs, av = read_forecast('deterministic', models[i], PREDICTAND, mpref, tgts[j], mon, fyr )
			#list_det_by_season[j].append(av[0])
	#The following print statements are for testing, and do not affect the functionality of the function
	#list_all_array = np.asarray(list_det_by_season)
	#print("shape of big list with all the data: ", np.shape(list_all_array))

	detfcst_by_season = []
	upper_detfcst_by_season = []
	lower_detfcst_by_season = []
	#confidence_level(%)=95
	#z_score=stats.norm.ppf(1-(1-0.95)/2) 
	z_score=stats.norm.ppf(1-(1-confidence_level/100.)/2)
	for k in range(nmods):
		detfcst_by_season.append([])
		upper_detfcst_by_season.append([])
		lower_detfcst_by_season.append([])
		for j in range(nsea):
			d_array = np.asarray(list_det_by_season[j][k])
			#Read grads binary file size H, W  --it assumes all files have the same size
			with open('./output/'+models[k]+'_'+fprefix+PREDICTAND+'_'+mpref+'FCST_mu_'+tgts[j]+'_'+mon+str(fyr)+'.ctl', "r") as fp:
				for line in lines_that_contain("XDEF", fp):
					W = int(line.split()[1])
					XD= float(line.split()[4])
			with open('./output/'+models[k]+'_'+fprefix+PREDICTAND+'_'+mpref+'FCST_mu_'+tgts[j]+'_'+mon+str(fyr)+'.ctl', "r") as fp:
				for line in lines_that_contain("YDEF", fp):
					H = int(line.split()[1])
					YD= float(line.split()[4])

			#Read variance
			f=open('./output/'+models[k]+'_'+fprefix+PREDICTAND+'_'+mpref+'FCST_var_'+tgts[j]+'_'+mon+str(fyr)+'.dat','rb')
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			#Now we read the field
			A=np.fromfile(f,dtype='float32',count=numval)
			vari = np.transpose(A.reshape((W, H), order='F'))
			vari[vari<0.]=np.nan #only positive values
			up_d_array = d_array+z_score*np.sqrt(vari)
			lo_d_array = d_array-z_score*np.sqrt(vari)

			#print("max(var)=",np.nanmax(vari))
			#print("max(sqrt(var))=",np.nanmax(np.sqrt(vari)))
			#print("Shape of d_array", np.shape(d_array))
			#print("Shape of up_d_array", np.shape(up_d_array))
			#print("Shape of lo_d_array", np.shape(lo_d_array))
			#print("k",k)
			#print("j",j)
			#d_nanmean = np.nanmean(d_array, axis=0)
			detfcst_by_season[k].append(d_array)
			upper_detfcst_by_season[k].append(up_d_array)
			lower_detfcst_by_season[k].append(lo_d_array)
	#detfcst_by_season_ar = np.asarray(detfcst_by_season)
	#print('Shape of big list after manip.:', np.shape(detfcst_by_season_ar))

		#print(type(detfcst_by_season))
		#print(np.shape(detfcst_by_season))
		#print(np.shape(d_array))
		#print(type(d_array))

	vmin0 = np.nanmin(np.array([lower_detfcst_by_season,detfcst_by_season,upper_detfcst_by_season]))
	vmax0 = np.nanmax(np.array([lower_detfcst_by_season,detfcst_by_season,upper_detfcst_by_season]))
	#print('vmin0=',vmin0)
	#print('vmax0=',vmax0)

	vmin2=-1*(abs(vmin0)+abs(vmax0))/2.
	vmax2=(abs(vmin0)+abs(vmax0))/2.
	#vmin1 = np.nanmin(detfcst_by_season)
	#vmax1 = np.nanmax(detfcst_by_season)
	#print('vmin=',vmin1)
	#print('vmax=',vmax1)
	vmin1 = -1*(abs(np.nanmin(detfcst_by_season))+abs(np.nanmax(detfcst_by_season)))/2.
	vmax1 = (abs(np.nanmin(detfcst_by_season))+abs(np.nanmax(detfcst_by_season)))/2.
	#print('vmin=',vmin1)
	#print('vmax=',vmax1)
	#print('vmin2=',vmin2)
	#print('vmax2=',vmax2)
	for k in range(nsea):

		#fig = plt.figure(figsize=(nsea*13, nmods*10))
		fig = plt.figure(figsize=(20, 15))
		gs = fig.add_gridspec(nrows=nmods, ncols=3,wspace=0.2,hspace=0.3)
		axs = gs.subplots(sharex=False,sharey=False, subplot_kw={'projection': ccrs.PlateCarree()})
		#print(nmods)
		#print(nsea)
		#print(axs)
		#print(type(axs))

		if nmods == 1: 
			axs = [[ axs[i] for i in range(3)]]
		#print(axs)
		#print(type(axs))
		#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
		states_provinces = feature.NaturalEarthFeature(
			category='cultural',
		#       name='admin_1_states_provinces_shp',
			name='admin_0_countries',
			scale='10m',
			facecolor='none')

		for i in range(nmods):
			for j in range(3):
				current_cmap = plt.get_cmap('BrBG') ## can choose 'nipy_spectral' for higher contrast if desired
				current_cmap.set_under('white', 0.0)

				current_cmap_copper = plt.get_cmap('YlOrRd', 9)
				current_cmap_binary = plt.get_cmap('Greens', 4)
				current_cmap_ylgn = make_cmap_blue(9)

				lats, longs = plats, plongs

				axs[i][j].set_extent([longs[0],longs[-1],lats[0],lats[-1]], ccrs.PlateCarree())

				axs[i][j].add_feature(states_provinces, edgecolor='black')
				if str(use_ocean) == "True":
					axs[i][j].add_feature(feature.OCEAN)
				axs[i][j].add_feature(feature.LAND)
				axs[i][j].add_feature(states_provinces, edgecolor='black')
				#ax[i][j].add_feature(feature.COASTLINE)
				pl=axs[i][j].gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
					linewidth=1, color='gray', alpha=0.5, linestyle=(0,(2,4)))
				pl.xlabels_top = False
				pl.ylabels_left = True
				pl.ylabels_right = False
				#pl.xlabels_bottom = False
				#if i == nmods - 1: change so long vals in every plot
				pl.xlabels_bottom = True
				pl.xformatter = LONGITUDE_FORMATTER
				pl.yformatter = LATITUDE_FORMATTER
				pl.xlabel_style = {'size': 8}#'rotation': 'vertical'}

				axs[i][j].set_ybound(lower=lati, upper=late)

				if j == 0:
					axs[i][j].text(-0.25, 0.5, models[i],rotation='vertical', verticalalignment='center', horizontalalignment='center', transform=axs[i][j].transAxes)

				#labels = ['Rainfall (mm)', 'Probability (%)']
				#axs[i][j].set_title(tgts[j])


				#fancy probabilistic
				#print(pbn[i][j])
				#print(np.shape(pbn))
				#print(i)
				#print(j)

				if j == 0:
					lower_detfcst_by_seasonik=lower_detfcst_by_season[i][k]
					lower_detfcst_by_seasonik[np.where(lower_detfcst_by_seasonik<vmin2)]=vmin2
					CS_det = axs[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), lower_detfcst_by_seasonik, vmin=vmin2,vmax=vmax2,cmap=current_cmap)

					title_string="Lower ("+str(confidence_level)+"%)"
					axs[i][j].set_title(title_string)

				elif j == 1:
					detfcst_by_seasonik=detfcst_by_season[i][k]
					detfcst_by_seasonik[np.where(detfcst_by_seasonik<vmin1)]=vmin1
					CS_det = axs[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), detfcst_by_seasonik, vmin=vmin1,vmax=vmax1,cmap=current_cmap)
					axs[i][j].set_title("Forecast Mean")

				else:
					upper_detfcst_by_seasonik=upper_detfcst_by_season[i][k]
					upper_detfcst_by_seasonik[np.where(upper_detfcst_by_seasonik>vmax2)]=vmax2
					CS_det = axs[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), upper_detfcst_by_seasonik, vmin=vmin2,vmax=vmax2,cmap=current_cmap)

					title_string="Upper ("+str(confidence_level)+"%)"
					axs[i][j].set_title(title_string)

				axins_det = inset_axes(axs[i][j],width="100%",  height="5%",  loc='lower center',borderpad=-5 )
				cbar_bdet = fig.colorbar(CS_det, ax=axs[i][j],  cax=axins_det, orientation='horizontal', pad = 0.02)#, ticks=bounds)
				if forecast_spi:
					cbar_bdet.set_label("SPI")
				else:
					cbar_bdet.set_label("Rainfall (mm)")

				#print(i,j)
				#print('model index',i,'season index',j,detfcst_by_season[i][j],'\n')
		fig.suptitle(tgts[k], fontsize=18)
		#axs[0][int(3/2)].set_title(tgts[j])
			#fancy deterministic cb bottom
			#axins_det = inset_axes(axs[-1][int(nsea/2)],width="100%",  height="5%",  loc='lower center',bbox_to_anchor=(-0.1, -0.15, 1.1, 1), bbox_transform=axs[-1][int(nsea/2)].transAxes,borderpad=0.1 )
			#cbar_bdet = fig.colorbar(CS_det, ax=axs[-1][int(nsea/2)],  cax=axins_det, orientation='horizontal', pad = 0.02)#, ticks=bounds)
			#if forecast_spi:
			#	cbar_bdet.set_label("SPI")
			#else:
			#	cbar_bdet.set_label("Rainfall (mm)")
	fig.savefig('./output/figures/Single_Deterministic_RealtimeForecasts.png', dpi=500, bbox_inches='tight')


def plt_single_probabilistic(models, PREDICTOR, PREDICTAND, loni, lone, lati, late, fprefix, mpref, tgts, mon, fyr, use_ocean):
	# This function plots probabilistic models for single models
	# It is similar to the function plt_ng_probabilistic, 
	# but is tailored for multiple subplots of different models.
	#cbar_loc, fancy = 'bottom', True
	nmods=len(models)
	nsea=len(tgts)
	# First all of the data is extracted from the output folder.
	# This results in a nested list for example of the dimension (1, 3, 2, dim(lat), dim(lon))
	# of which 1 would be the season, 3 the class, 2 the number of models and the rest the dimension of the data.

	list_probabilistic_by_season = [ [[], [], []] for i in range(nsea) ]
	list_det_by_season = [[] for i in range(nsea)]
	for i in range(nmods):
		for j in range(nsea):
			if platform.system() == "Windows":
				plats, plongs, av, years = read_forecast_bin('probabilistic', models[i], PREDICTOR, PREDICTAND, mpref, tgts[j], mon, fyr )
			else:
				plats, plongs, av, years = read_forecast('probabilistic', models[i], PREDICTOR, PREDICTAND, mpref, tgts[j], mon, fyr )
			for kl in range(av.shape[0]):
				list_probabilistic_by_season[j][kl].append(av[kl])
			#if platform.system() == "Windows":
			#	dlats, dlongs, av = read_forecast_bin('deterministic', models[i], PREDICTAND, mpref, tgts[j], mon, fyr )
			#else:
			#	dlats, dlongs, av = read_forecast('deterministic', models[i], PREDICTAND, mpref, tgts[j], mon, fyr )
			#list_det_by_season[j].append(av[0])
	#The following print statements are for testing, and do not affect the functionality of the function
	#list_all_array = np.asarray(list_probabilistic_by_season)
	#print("shape of big list with all the data: ", np.shape(list_all_array))

	pbn, pn, pan = [],[],[]
	for k in range(nmods):
		pbn.append([])
		pn.append([])
		pan.append([])
		for j in range(nsea):
			p_bn_array = np.asarray(list_probabilistic_by_season[j][0][k])
			p_n_array = np.asarray(list_probabilistic_by_season[j][1][k])
			p_an_array = np.asarray(list_probabilistic_by_season[j][2][k])
			#print(np.shape(p_bn_array))

			p_bn = p_bn_array
			p_n = p_n_array
			p_an = p_an_array
			#print(np.shape(p_an))

			max_ndxs = np.argmax(np.asarray([p_bn, p_n, p_an]), axis=0)
			#print(np.shape(max_ndxs))
			p_bn[np.where(max_ndxs!= 0)] = np.nan
			p_n[np.where(max_ndxs!= 1)] = np.nan
			p_an[np.where(max_ndxs!= 2)] = np.nan
			pbn[k].append(p_bn)
			pn[k].append(p_n)
			pan[k].append(p_an)


	fig, axs = plt.subplots(nrows=nmods, ncols=nsea, figsize=(nsea*13, nmods*10), sharex=False,sharey=False, subplot_kw={'projection': ccrs.PlateCarree()})
	#print(nmods)
	#print(nsea)
	#print(axs)
	#print(type(axs))

	if nmods == 1 and nsea == 1: 
		axs = [[axs]]
	elif nmods > 1 and nsea == 1: 
		axs = [  [ axs[i] ] for i in range(nmods)]
	elif nsea > 1 and nmods == 1: 
		axs = [[ axs[i] for i in range(nsea)]]
	else: 
		pass 
	#print(type(axs))
	#print("axs shape is ", np.shape(np.asarray(axs)))
	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
	#				name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')

	for i in range(nmods):
		for j in range(nsea):
			current_cmap = plt.get_cmap('BrBG')
			current_cmap.set_under('white', 0.0)

			current_cmap_copper = plt.get_cmap('YlOrRd', 9)
			current_cmap_binary = plt.get_cmap('Greens', 4)
			current_cmap_ylgn = make_cmap_blue(9)

			lats, longs = plats, plongs

			axs[i][j].set_extent([longs[0],longs[-1],lats[0],lats[-1]], ccrs.PlateCarree())


			axs[i][j].add_feature(states_provinces, edgecolor='black')
			if str(use_ocean) == "True":
				axs[i][j].add_feature(feature.OCEAN)
			axs[i][j].add_feature(feature.LAND)
			#ax[i][j].add_feature(feature.COASTLINE)
			pl=axs[i][j].gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				linewidth=1, color='gray', alpha=0.5, linestyle=(0,(2,4)))
			pl.xlabels_top = False
			pl.ylabels_left = True
			pl.ylabels_right = False
			#pl.xlabels_bottom = False
			#if i == nmods - 1: change so long vals in every plot
			pl.xlabels_bottom = True
			pl.xformatter = LONGITUDE_FORMATTER
			pl.yformatter = LATITUDE_FORMATTER
			pl.xlabel_style = {'size': 8}#'rotation': 'vertical'}


			axs[i][j].set_ybound(lower=lati, upper=late)

			if j == 0:
				axs[i][j].text(-0.25, 0.5, models[i],rotation='vertical', verticalalignment='center', horizontalalignment='center', transform=axs[i][j].transAxes)

			labels = ['Rainfall (mm)', 'Probability (%)']
			axs[i][j].set_title(tgts[j])


			#fancy probabilistic
			#print(pbn[i][j])
			#print(np.shape(pbn))
			#print(i)
			#print(j)
			CS1 = axs[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), pbn[i][j],
				vmin=35, vmax=80,
				#norm=MidpointNormalize(midpoint=0.),
				cmap=current_cmap_copper)
			CS2 = axs[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), pn[i][j],
				vmin=35, vmax=55,
				#norm=MidpointNormalize(midpoint=0.),
				cmap=current_cmap_binary)
			CS3 = axs[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), pan[i][j],
				vmin=35, vmax=80,
				#norm=MidpointNormalize(midpoint=0.),
				cmap=current_cmap_ylgn)

			bounds = [40,45,50,55,60,65,70,75]
			nbounds = [40,45,50]

	#fancy probabilistic cb bottom
	axins_f_bottom = inset_axes(axs[-1][int(nsea/2)],
		width="40%",  # width = 5% of parent_bbox width
		height="5%",  # height : 50%
		loc='lower left',
		bbox_to_anchor=(-0.2, -0.15, 1.2, 1),
		bbox_transform=axs[-1][int(nsea/2)].transAxes,
		borderpad=0.1 )
	axins2_bottom = inset_axes(axs[-1][int(nsea/2)],
		width="20%",  # width = 5% of parent_bbox width
		height="5%",  # height : 50%
		loc='lower center',
		bbox_to_anchor=(-0.0, -0.15, 1, 1),
		bbox_transform=axs[-1][int(nsea/2)].transAxes,
		borderpad=0.1 )
	axins3_bottom = inset_axes(axs[-1][int(nsea/2)],
		width="40%",  # width = 5% of parent_bbox width
		height="5%",  # height : 50%
		loc='lower right',
		bbox_to_anchor=(0, -0.15, 1.2, 1),
		bbox_transform=axs[-1][int(nsea/2)].transAxes,
		borderpad=0.1 )
	cbar_fbl = fig.colorbar(CS1, ax=axs[-1][int(nsea/2)], cax=axins_f_bottom, orientation='horizontal', ticks=bounds)
	cbar_fbl.set_label('BN Probability (%)') #, rotation=270)\

	cbar_fbc = fig.colorbar(CS2, ax=axs[-1][int(nsea/2)],  cax=axins2_bottom, orientation='horizontal', ticks=nbounds)
	cbar_fbc.set_label('N Probability (%)') #, rotation=270)\

	cbar_fbr = fig.colorbar(CS3, ax=axs[-1][int(nsea/2)],  cax=axins3_bottom, orientation='horizontal', ticks=bounds)
	cbar_fbr.set_label('AN Probability (%)') #, rotation=270)\

	fig.savefig('./output/figures/Single_Probabilistic_RealtimeForecasts.png', dpi=500, bbox_inches='tight')


def plt_ng_probabilistic(models,PREDICTOR,PREDICTAND,loni,lone,lati,late,fprefix,mpref,tgts, mon, fyr, use_ocean):
	cbar_loc, fancy = 'bottom', True
	nmods=len(models)
	nsea=len(tgts)
	xdim=1
	#
	list_probabilistic_by_season = [[[], [], []] for i in range(nsea)]
	list_det_by_season = [[] for i in range(nsea)]
	for i in range(nmods):
		for j in range(nsea):
			if platform.system() == "Windows":
				plats, plongs, av, years = read_forecast_bin('probabilistic', models[i], PREDICTOR, PREDICTAND, mpref, tgts[j], mon, fyr )
			else:
				plats, plongs, av, years = read_forecast('probabilistic', models[i], PREDICTOR, PREDICTAND, mpref, tgts[j], mon, fyr )
			for kl in range(av.shape[0]):
				list_probabilistic_by_season[j][kl].append(av[kl])
			#if platform.system() == "Windows":
			#	dlats, dlongs, av = read_forecast_bin('deterministic', models[i], PREDICTAND, mpref, tgts[j], mon, fyr )
			#else:
			#	dlats, dlongs, av = read_forecast('deterministic', models[i], PREDICTAND, mpref, tgts[j], mon, fyr )
			#list_det_by_season[j].append(av[0])

	ng_probfcst_by_season = []
	ng_detfcst_by_season = []
	pbn, pn, pan = [],[],[]
	for j in range(nsea):
		p_bn_array = np.asarray(list_probabilistic_by_season[j][0])
		p_n_array = np.asarray(list_probabilistic_by_season[j][1])
		p_an_array = np.asarray(list_probabilistic_by_season[j][2])

		p_bn = np.nanmean(p_bn_array, axis=0) #average over the models
		p_n = np.nanmean(p_n_array, axis=0)   #some areas are NaN
		p_an = np.nanmean(p_an_array, axis=0) #if they are Nan for All, mark

		all_nan = np.zeros(p_bn.shape)
		for ii in range(p_bn.shape[0]):
			for jj in range(p_bn.shape[1]):
				if np.isnan(p_bn[ii,jj]) and np.isnan(p_n[ii,jj]) and np.isnan(p_an[ii,jj]):
					all_nan[ii,jj] = 1
		missing = np.where(all_nan > 0)

		max_ndxs = np.argmax(np.asarray([p_bn, p_n, p_an]), axis=0)
		p_bn[np.where(max_ndxs!= 0)] = np.nan
		p_n[np.where(max_ndxs!= 1)] = np.nan
		p_an[np.where(max_ndxs!= 2)] = np.nan
		pbn.append(p_bn)
		pn.append(p_n)
		pan.append(p_an)

	fig, ax = plt.subplots(nrows=xdim, ncols=nsea, figsize=(nsea*13, xdim*10), sharex=False,sharey=False, subplot_kw={'projection': ccrs.PlateCarree()})

	if nsea == 1:
		ax = [ax]
	ax = [ax]
	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
#				name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')

	for i in range(xdim):
		for j in range(nsea):
			current_cmap = plt.get_cmap('BrBG')
			current_cmap.set_under('white', 0.0)

			current_cmap_copper = plt.get_cmap('YlOrRd', 9)
			current_cmap_binary = plt.get_cmap('Greens', 4)
			current_cmap_ylgn = make_cmap_blue(9)

			lats, longs = plats, plongs

			ax[i][j].set_extent([longs[0],longs[-1],lats[0],lats[-1]], ccrs.PlateCarree())


			ax[i][j].add_feature(states_provinces, edgecolor='black')
			if str(use_ocean) == "True":
				ax[i][j].add_feature(feature.OCEAN)
			ax[i][j].add_feature(feature.LAND)
			#ax[i][j].add_feature(feature.COASTLINE)
			pl=ax[i][j].gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				  linewidth=1, color='gray', alpha=0.5, linestyle=(0,(2,4)))
			pl.xlabels_top = False
			pl.ylabels_left = True
			pl.ylabels_right = False
			#pl.xlabels_bottom = False
			#if i == nmods - 1: change so long vals in every plot
			pl.xlabels_bottom = True
			pl.xformatter = LONGITUDE_FORMATTER
			pl.yformatter = LATITUDE_FORMATTER
			pl.xlabel_style = {'size': 8}#'rotation': 'vertical'}


			ax[i][j].set_ybound(lower=lati, upper=late)
			titles = ["Deterministic Forecast", "Probabilistic Forecast (Dominant Tercile)"]


			if j == 0:
				ax[i][j].text(-0.25, 0.5, "Probabilistic Forecast (Dominant Tercile)",rotation='vertical', verticalalignment='center', horizontalalignment='center', transform=ax[i][j].transAxes)

			labels = ['Rainfall (mm)', 'Probability (%)']
			ax[i][j].set_title(tgts[j])


			#fancy probabilistic
			CS1 = ax[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), pbn[j],
				vmin=35, vmax=80,
				#norm=MidpointNormalize(midpoint=0.),
				cmap=current_cmap_copper)
			CS2 = ax[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), pn[j],
				vmin=35, vmax=55,
				#norm=MidpointNormalize(midpoint=0.),
				cmap=current_cmap_binary)
			CS3 = ax[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), pan[j],
				vmin=35, vmax=80,
				#norm=MidpointNormalize(midpoint=0.),
				cmap=current_cmap_ylgn)

			bounds = [40,45,50,55,60,65,70,75]
			nbounds = [40,45,50]

			#fancy probabilistic cb bottom
			axins_f_bottom = inset_axes(ax[i][j],
            	width="40%",  # width = 5% of parent_bbox width
               	height="5%",  # height : 50%
               	loc='lower left',
               	bbox_to_anchor=(-0.2, -0.15, 1.2, 1),
               	bbox_transform=ax[i][j].transAxes,
               	borderpad=0.1 )
			axins2_bottom = inset_axes(ax[i][j],
            	width="20%",  # width = 5% of parent_bbox width
               	height="5%",  # height : 50%
               	loc='lower center',
               	bbox_to_anchor=(-0.0, -0.15, 1, 1),
               	bbox_transform=ax[i][j].transAxes,
               	borderpad=0.1 )
			axins3_bottom = inset_axes(ax[i][j],
            	width="40%",  # width = 5% of parent_bbox width
               	height="5%",  # height : 50%
               	loc='lower right',
               	bbox_to_anchor=(0, -0.15, 1.2, 1),
               	bbox_transform=ax[i][j].transAxes,
               	borderpad=0.1 )
			cbar_fbl = fig.colorbar(CS1, ax=ax[i][j], cax=axins_f_bottom, orientation='horizontal', ticks=bounds)
			cbar_fbl.set_label('BN Probability (%)') #, rotation=270)\

			cbar_fbc = fig.colorbar(CS2, ax=ax[i][j],  cax=axins2_bottom, orientation='horizontal', ticks=nbounds)
			cbar_fbc.set_label('N Probability (%)') #, rotation=270)\

			cbar_fbr = fig.colorbar(CS3, ax=ax[i][j],  cax=axins3_bottom, orientation='horizontal', ticks=bounds)
			cbar_fbr.set_label('AN Probability (%)') #, rotation=270)\

	fig.savefig('./output/figures/NG_Probabilistic_RealtimeForecasts.png', dpi=500, bbox_inches='tight')



def plt_ng_deterministic(models,predictor,predictand,loni,lone,lati,late,fprefix,mpref,mons, mon, fyr, use_ocean):
	"""A simple function for ploting the statistical scores

	PARAMETERS
	----------
		fcst_type: either 'deterministic' or 'probabilistic'
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
	"""
	cbar_loc, fancy = 'bottom', True
	nmods=len(models)
	nsea=len(mons)

	xdim = 1
	list_probabilistic_by_season = [[[], [], []] for i in range(nsea)]
	list_det_by_season = [[] for i in range(nsea)]
	for i in range(nmods):
		for j in range(nsea):
			#plats, plongs, av = read_forecast('probabilistic', models[i], predictand, mpref, mons[j], mon, fyr )
			#list_probabilistic_by_season[j][0].append(av[0])
			#list_probabilistic_by_season[j][1].append(av[1])
			#list_probabilistic_by_season[j][2].append(av[2])
			dlats, dlongs, av, years = read_forecast('deterministic', models[i], predictor, predictand, mpref, mons[j], mon, fyr )
			list_det_by_season[j].append(av[0])

	ng_probfcst_by_season = []
	ng_detfcst_by_season = []
	for j in range(nsea):
		d_array = np.asarray(list_det_by_season[j])
		d_nanmean = np.nanmean(d_array, axis=0)
		ng_detfcst_by_season.append(d_nanmean)

	fig, ax = plt.subplots(nrows=xdim, ncols=nsea, figsize=(nsea*13, xdim*10), sharex=True,sharey=True, subplot_kw={'projection': ccrs.PlateCarree()})
	if nsea == 1:
		ax = [ax]
	ax = [ax]



	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
#				name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')
	for i in range(xdim):
		for j in range(nsea):
			current_cmap = plt.get_cmap('BrBG')
			current_cmap.set_bad('white',0.0)
			current_cmap.set_under('white', 0.0)

			lats, longs = dlats, dlongs
			ax[i][j].set_extent([longs[0],longs[-1],lats[0],lats[-1]], ccrs.PlateCarree())


			if str(use_ocean) == "True":
				ax[i][j].add_feature(feature.OCEAN)
			ax[i][j].add_feature(feature.LAND)
			ax[i][j].add_feature(states_provinces)

			pl=ax[i][j].gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				  linewidth=1, color='gray', alpha=0.5, linestyle=(0,(2,4)))
			pl.xlabels_top = False
			pl.ylabels_left = True
			pl.ylabels_right = False
			#pl.xlabels_bottom = False
			#if i == nmods - 1: change so long vals in every plot
			pl.xlabels_bottom = True
			pl.xformatter = LONGITUDE_FORMATTER
			pl.yformatter = LATITUDE_FORMATTER
			ax[i][j].add_feature(states_provinces, edgecolor='black')
			ax[i][j].set_ybound(lower=lati, upper=late)
			pl.xlabel_style = {'size': 8}#'rotation': 'vertical'}

			titles = ["Deterministic Forecast", "Probabilistic Forecast (Dominant Tercile)"]


			if j == 0:
				ax[i][j].text(-0.25, 0.5, "Deterministic Forecast",rotation='vertical', verticalalignment='center', horizontalalignment='center', transform=ax[i][j].transAxes)

			labels = ['Rainfall (mm/month)', 'Probability (%)']
			ax[i][j].set_title(mons[j])

			#fancy deterministic
			var = ng_detfcst_by_season[j]
		#	bounds = [int(xx) for xx in np.linspace(0, np.nanmax(var), 11)]
			CS_det = ax[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), var,
				#norm=MidpointNormalize(midpoint=0.),
				cmap=current_cmap)

			if cbar_loc == 'left':
				#fancy deterministic cb left
				axins_det = inset_axes(ax[i][j], width="5%",  height="100%", loc='center left', bbox_to_anchor=(-0.25, 0., 1, 1),bbox_transform=ax[i][j].transAxes, borderpad=0.1 )
				cbar_ldet = fig.colorbar(CS_det, ax=ax[i][j], cax=axins_det,  orientation='vertical', pad=0.02)
				cbar_ldet.set_label(labels[i]) #, rotation=270)\
				axins_det.yaxis.tick_left()
			else:
				#fancy deterministic cb bottom
				axins_det = inset_axes(ax[i][j],width="100%",  height="5%",  loc='lower center',bbox_to_anchor=(-0.1, -0.15, 1.1, 1), bbox_transform=ax[i][j].transAxes,borderpad=0.1 )
				cbar_bdet = fig.colorbar(CS_det, ax=ax[i][j],  cax=axins_det, orientation='horizontal', pad = 0.02)#, ticks=bounds)
				cbar_bdet.set_label(labels[i])
	fig.savefig('./output/figures/NG_Deterministic_RealtimeForecasts.png', dpi=500, bbox_inches='tight')

def readGrADSctl(models,fprefix,predictand,mpref,id,tar,monf,fyr):
	#Read grads binary file size H, W, T
	with open('../output/'+models[0]+'_'+fprefix+predictand+'_'+mpref+id+'_'+tar+'_'+monf+str(fyr)+'.ctl', "r") as fp:
		for line in lines_that_contain("XDEF", fp):
			W = int(line.split()[1])
			Wi= float(line.split()[3])
			XD= float(line.split()[4])
	with open('../output/'+models[0]+'_'+fprefix+predictand+'_'+mpref+id+'_'+tar+'_'+monf+str(fyr)+'.ctl', "r") as fp:
		for line in lines_that_contain("YDEF", fp):
			H = int(line.split()[1])
			Hi= float(line.split()[3])
			YD= float(line.split()[4])
	with open('../output/'+models[0]+'_'+fprefix+predictand+'_'+mpref+id+'_'+tar+'_'+monf+str(fyr)+'.ctl', "r") as fp:
		for line in lines_that_contain("TDEF", fp):
			T = int(line.split()[1])
			Ti= int((line.split()[3])[-4:])
			TD= 1  #not used
	return (W, Wi, XD, H, Hi, YD, T, Ti, TD)

def PrepFiles(fprefix, predictand, threshold_pctle, tini, tend, wlo1, wlo2,elo1, elo2, sla1, sla2, nla1, nla2, tgti, tgtf, mon, monf, fyr, os, wetday_threshold, tar, model, obs, obs_source, hdate_last, force_download, station, dic_sea, dic_sea_elr, pressure, MOS):

	"""Function to download (or not) the needed files"""
	if fprefix=='RFREQ':
		GetObs_RFREQ(predictand, tini,tend,wlo2, elo2, sla2, nla2, wetday_threshold, threshold_pctle, tar, obs_source, hdate_last, force_download,station, dic_sea)
		print('Obs:rfreq file ready to go')
		print('----------------------------------------------')
#		nday added after nlag for GEFS & CFSv2
		GetHindcasts_RFREQ(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, wetday_threshold, tar, model, force_download, dic_sea)
		#GetHindcasts_RFREQ(wlo1, elo1, sla1, nla1, day1, day2, nday, fyr, mon, os, authkey, wk, wetday_threshold, nlag, training_season, hstep, model, force_download)
		print('Hindcasts file ready to go')
		print('----------------------------------------------')
		#GetForecast_RFREQ(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, authkey, wk, wetday_threshold, nlag, model, force_download)
		GetForecast_RFREQ(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, wetday_threshold, model, force_download, dic_sea)
		print('Forecasts file ready to go')
		print('----------------------------------------------')
	elif fprefix=='T2M':
		GetHindcasts_T2M(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea)
		print('Hindcasts file ready to go')
		print('----------------------------------------------')
		GetObs(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea)
		print('Obs:precip file ready to go')
		print('----------------------------------------------')
		GetForecast_T2M(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea)
		print('Forecasts file ready to go')
		print('----------------------------------------------')
	elif fprefix=='TMAX':
		GetHindcasts_TMAX(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea)
		print('Hindcasts file ready to go')
		print('----------------------------------------------')
		GetObs(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea)
		print('Obs:precip file ready to go')
		print('----------------------------------------------')
		GetForecast_TMAX(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea)
		print('Forecasts file ready to go')
		print('----------------------------------------------')
	elif fprefix=='TMIN':
		GetHindcasts_TMIN(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea)
		print('Hindcasts file ready to go')
		print('----------------------------------------------')
		GetObs(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea)
		print('Obs:precip file ready to go')
		print('----------------------------------------------')
		GetForecast_TMIN(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea)
		print('Forecasts file ready to go')
		print('----------------------------------------------')
	elif fprefix=='UQ':
		GetHindcasts_UQ(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea, pressure)
		print('Hindcasts file ready to go')
		print('----------------------------------------------')
		GetObs(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea)
		print('Obs:precip file ready to go')
		print('----------------------------------------------')
		GetForecast_UQ(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea, pressure)
		print('Forecasts file ready to go')
		print('----------------------------------------------')
	elif fprefix=='VQ':
		GetHindcasts_VQ(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea, pressure)
		print('Hindcasts file ready to go')
		print('----------------------------------------------')
		GetObs(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea)
		print('Obs:precip file ready to go')
		print('----------------------------------------------')
		GetForecast_VQ(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea, pressure)
		print('Forecasts file ready to go')
		print('----------------------------------------------')
	elif fprefix=='UA':
		GetHindcasts_UA(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea, pressure)
		print('Hindcasts file ready to go')
		print('----------------------------------------------')
		GetObs(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea)
		print('Obs:precip file ready to go')
		print('----------------------------------------------')
		GetForecast_UA(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea, pressure)
		print('Forecasts file ready to go')
		print('----------------------------------------------')
	elif fprefix=='SST':
		GetHindcasts_SST(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea)
		print('Hindcasts file ready to go')
		print('----------------------------------------------')
		GetObs(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea)
		print('Obs:precip file ready to go')
		print('----------------------------------------------')
		GetForecast_SST(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea)
		print('Forecasts file ready to go')
		print('----------------------------------------------')
	elif fprefix=='VA':
		GetHindcasts_VA(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea, pressure)
		print('Hindcasts file ready to go')
		print('----------------------------------------------')
		GetObs(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea)
		print('Obs:precip file ready to go')
		print('----------------------------------------------')
		GetForecast_VA(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea, pressure)
		print('Forecasts file ready to go')
		print('----------------------------------------------')
	else:
		#GetHindcasts(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea)
		#print('Hindcasts file ready to go')
		#print('----------------------------------------------')
		#GetObs(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea)
		#print('Obs:precip file ready to go')
		#print('----------------------------------------------')
		#GetForecast(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea)
		#print('Forecasts file ready to go')
		#print('----------------------------------------------')
		if MOS == 'ELR':
			GetHindcasts_elr(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea_elr)
			print('Hindcasts netcdf file ready to go')
			print('----------------------------------------------')
			GetObs_hc_elr(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea_elr)
			print('Obs:precip netcdf file ready to go')
			print('----------------------------------------------')
			GetForecast(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea)
			print('Forecasts file ready to go')
			print('----------------------------------------------')
		else:
			GetHindcasts(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea)
			print('Hindcasts file ready to go')
			print('----------------------------------------------')
			GetObs(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea)
			print('Obs:precip file ready to go')
			print('----------------------------------------------')
			GetForecast(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea)
			print('Forecasts file ready to go')
			print('----------------------------------------------')



def PrepFiles_usrNetcdf(fprefix, predictand, tini,tend, wlo1, wlo2,elo1, elo2, sla1, sla2, nla1, nla2, tgti, tgtf, mon, monf, fyr, tar, infile_predictand, infile_hindcast, infile_forecast):
	"""Function to download (or not) the needed files"""
	readNetCDF_predictand(infile_predictand,outfile, predictand, tini,tend,wlo2, elo2, sla2, nla2, tar)
	print('Obs:precip file ready to go')
	print('----------------------------------------------')

	readNetCDF_Hindcasts(infile_hindcast, outfile, tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, tar)
	print('Hindcasts file ready to go')
	print('----------------------------------------------')

	readNetCDF_Forecast(infile_forecast, outfile, monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1)
	print('Forecasts file ready to go')
	print('----------------------------------------------')

def pltdomain(loni1,lone1,lati1,late1,loni2,lone2,lati2,late2,use_topo):
	"""A simple plot function for the geographical domain

	PARAMETERS
	----------
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
		title: title
	"""
	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
		#name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')

	fig = plt.subplots(figsize=(15,15), subplot_kw=dict(projection=ccrs.PlateCarree()))
	loni = [loni1,loni2]
	lati = [lati1,lati2]
	lone = [lone1,lone2]
	late = [late1,late2]
	title = ['Predictor', 'Predictand']

	for i in range(2):

		ax = plt.subplot(1, 2, i+1, projection=ccrs.PlateCarree())
		ax.set_extent([loni[i],lone[i],lati[i],late[i]], ccrs.PlateCarree())

		# Put a background image on for nice sea rendering.
		if str(use_topo) == "True":
			ax.stock_img()

		ax.add_feature(feature.LAND)
		ax.add_feature(feature.COASTLINE)
		ax.add_feature(feature.OCEAN)
		ax.set_title(title[i]+" domain")
		pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				  linewidth=2, color='gray', alpha=0.5, linestyle='--')
		pl.xlabels_top = False
		pl.ylabels_left = False
		pl.xformatter = LONGITUDE_FORMATTER
		pl.yformatter = LATITUDE_FORMATTER
		pl.xlocator = ticker.MaxNLocator(4)
		pl.ylocator = ticker.MaxNLocator(4)
		ax.add_feature(states_provinces, edgecolor='gray')
	plt.show()

def plteofs_eofts(models,predictor,predictand,mode,M,loni1,lone1,lati1,late1,loni2,lone2,lati2,late2,fprefix,mpref,tgts,mol,mons,map_color, colorbar_option, use_ocean, x_offset=0, y_offset=0):
	"""A simple function for ploting EOFs computed by CPT
	PARAMETERS
	----------
		models: list of models to plot
		predictor: exactly that
		predictand: exactly that
		mode: EOF being visualized
		M: total number of EOFs computed by CPT (max defined in PyCPT is 10)
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
		fprefix:
	"""
	#mol=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
	if mpref=='None':
		print('No EOFs are computed if MOS=None is used')
		return

	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
#                               name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')

	nmods=len(models)
	current_cmap = make_cmap(map_color, continuous=colorbar_option)
	#plt.figure(figsize=(20,10))
	#fig, ax = plt.subplots(figsize=(20,15),sharex=True,sharey=True)
	fig, ax = plt.subplots(figsize=(20,15))
	grid = plt.GridSpec(nmods, 3, wspace=0.2,hspace=0.3)
	tari=tgts[0]
	#model=models[0]
	monn=mol[0]
	nsea=len(mons)
	if mode == 0:
		fig.suptitle(tari +"  (init: "+monn+")", fontsize=18)
	vmi = 0
	vma = 0  #for later use of vmin vmax adjustment
	mm=-1
	for model in models:
		mm=mm+1
		#Read  grid
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFX_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				W = int(line.split()[1])
				XD= float(line.split()[4])
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFX_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				H = int(line.split()[1])
				YD= float(line.split()[4])

		if mpref=='CCA':
			with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFY_'+tari+'_'+monn+'.ctl', "r") as fp:
				for line in lines_that_contain("XDEF", fp):
					Wy = int(line.split()[1])
					XDy= float(line.split()[4])
			with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFY_'+tari+'_'+monn+'.ctl', "r") as fp:
				for line in lines_that_contain("YDEF", fp):
					Hy = int(line.split()[1])
					YDy= float(line.split()[4])
			eofy=np.empty([M,Hy,Wy])  # define array for later use
		# print(  [M,Hy,Wy],     )
		# eofx=np.empty([nmods, nsea, M,H,W])  #define array for later use
		eofx=np.empty([1, nsea, M,H,W])  #define array for later use
		# vmi = 0 
		# vma = 0  #for later use of vmin vmax adjustment 
		k = 0 
		for tar in mons: 
			k = k + 1
			mon=mol[tgts.index(tar)]
			#ax = plt.subplot(nmods+1,nsea, k, projection=ccrs.PlateCarree()) #nmods+obs
			#ax = plt.subplot(nsea,3, k+1, projection=ccrs.PlateCarree()) 
			#ax = plt.subplot(grid[mm, 2], projection=ccrs.PlateCarree())

			if mpref=='CCA':  #skip if there are not predictand EOFs (e.g., PCR)
				ax = plt.subplot(grid[mm, 2], projection=ccrs.PlateCarree())
				ax.set_extent([loni2+x_offset,loni2+Wy*XDy+x_offset,lati2+y_offset,lati2+Hy*YDy+y_offset], ccrs.PlateCarree())
				#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl) 
				f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFY_'+tar+'_'+mon+'.dat','rb')
				#cycle for all time steps  (same approach to read GrADS files as before, but now read T times) 
				#Now we read the field 
				EOFYDATA= []
				good = True
				while (good==True):
					try:
						recl=struct.unpack('i',f.read(4))[0] # number of bites in the record 
						numval=int(recl/np.dtype('float32').itemsize) #this if for each time/EOF stamp 
						A0=np.fromfile(f,dtype='float32',count=numval)
						endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
						EOFYDATA.append(np.transpose(A0.reshape((Wy, Hy), order='F')))
					except:
						good = False
				eofy = np.asarray(EOFYDATA) ## same thing for CCAX 
				eofy[eofy==-999.]=np.nan #nans 
				if  mm == 0:
					if -1*max(np.nanmax(eofy),np.abs(np.nanmin(eofy))) < vmi:
						vmi=-max(np.nanmax(eofy),np.abs(np.nanmin(eofy)))
						vma=-vmi
				CS=plt.pcolormesh(np.linspace(loni2+x_offset, loni2+Wy*XDy+x_offset,num=Wy), np.linspace(lati2+Hy*YDy+y_offset, lati2+y_offset, num=Hy), eofy[mode],
				#vmin=vmi,vmax=vma,
				vmin=-.1,vmax=.1,
				cmap=current_cmap,
				transform=ccrs.PlateCarree())
				#label = 'EOF loadings'
				label = 'EOF Spatial loadings'

				ax.add_feature(feature.LAND)
				ax.add_feature(states_provinces, edgecolor='gray')
				if str(use_ocean) == "True":
					ax.add_feature(feature.OCEAN)
					#tick_spacing = 0.5 
					#ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing)) 

				pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
					  linewidth=2, color='gray', alpha=0., linestyle='--')
				pl.xlabels_top = False
				pl.xlabels_bottom = True
				pl.ylabels_left = True
				pl.ylabels_right = False
				pl.xformatter = LONGITUDE_FORMATTER
				pl.yformatter = LATITUDE_FORMATTER
				pl.xlocator = ticker.MaxNLocator(4)
				pl.ylocator = ticker.MaxNLocator(4)
				lon_formatter = LongitudeFormatter(number_format='.2f') #LongitudeFormatter(degree_symbol='')
				lat_formatter = LatitudeFormatter(number_format='.2f' ) #LatitudeFormatter(degree_symbol='')
				ax.xaxis.set_major_formatter(lon_formatter)
				ax.yaxis.set_major_formatter(lat_formatter)
				ax.set_ybound(lower=lati2, upper=late2)

				#if k<=nsea:
					#ax.set_title(tar)
				#if ax.is_first_col():
				#ax.set_ylabel(model, rotation=90)
				#ax.set_title('Y Spatial Loadings')
				ax.set_title('Y EOF Spatial Loadings')

				#if k==1:
				#ax.text(-0.35,0.5,'Obs',rotation=90,verticalalignment='center', transform=ax.transAxes)


				#plt.autoscale(enable=True)
				plt.subplots_adjust(bottom=0.15, top=0.9)
				#cax = plt.axes([0.69, 0.58, 0.2, 0.03])
				#cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
				cbar = plt.colorbar(CS, orientation='horizontal')
				cbar.set_label(label) #, rotation=270)

		eofx=[]
		#nrow=0
		#for model in models:
		#Read  grid
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFX_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				W = int(line.split()[1])
				XD= float(line.split()[4])
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFX_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				H = int(line.split()[1])
				YD= float(line.split()[4])
		eofx.append([])
		#m_ndx = models.index(model)
		m_ndx = 0
		#nrow=nrow+1 #first model is in row=2 and nrow=1
		for tar in mons:
			s_ndx = mons.index(tar)
			k=k+1
			mon=mol[tgts.index(tar)]
			#ax = plt.subplot(nmods+1,nsea, k, projection=ccrs.PlateCarree()) #nmods+obs
			#ax = plt.subplot(nsea,3, k, projection=ccrs.PlateCarree())
			ax = plt.subplot(grid[mm, 0], projection=ccrs.PlateCarree())
			if mpref=='PCR':
				ax.set_extent([loni1+x_offset,loni1+W*XD+x_offset,lati1+y_offset,lati1+H*YD+y_offset], ccrs.PlateCarree())  #EOF domains will look different between CCA and PCR if X and Y domains are different
			else:
				#ax.set_extent([loni,loni+Wy*XDy,lati,lati+Hy*YDy], ccrs.PlateCarree())
				ax.set_extent([loni1+x_offset,loni1+W*XD+x_offset,lati1+y_offset,lati1+H*YD+y_offset], ccrs.PlateCarree())
				#ax.set_extent([loni2,loni2+Wy*XDy,lati2,lati2+Hy*YDy], ccrs.PlateCarree())

			#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
			states_provinces = feature.NaturalEarthFeature(
				category='cultural',
	#                               name='admin_1_states_provinces_shp',
				name='admin_0_countries',
				scale='10m',
				facecolor='none')

			ax.add_feature(feature.LAND)
			ax.add_feature(states_provinces, edgecolor='gray')
			#ax.add_feature(feature.COASTLINE)
			if str(use_ocean) == "True":
				ax.add_feature(feature.OCEAN)
			#if k == (nrow*nsea)+1:
			ax.text(-0.35,0.5,model,rotation=90,verticalalignment='center', transform=ax.transAxes)
			if mm == 0:
				ax.text(-0.35,1.3,"EOF "+str(mode+1),fontsize=18,fontweight='bold',transform=ax.transAxes)
			#if mode == 0:
			#       ax.text(0.3, 12.0, tar +"  (init: "+monn+")", fontsize=18, ha='center')

			#tick_spacing=0.5
			#ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))

			pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				  linewidth=2, color='gray', alpha=0., linestyle='--')
			pl.xlabels_top = False
			pl.ylabels_left = True
			pl.ylabels_right = False
			pl.xlabels_bottom = True
			pl.xformatter = LONGITUDE_FORMATTER
			pl.yformatter = LATITUDE_FORMATTER
			pl.xlocator = ticker.MaxNLocator(4)
			pl.ylocator = ticker.MaxNLocator(4)
			ax.add_feature(states_provinces, edgecolor='gray')
			lon_formatter = LongitudeFormatter(number_format='.2f') #LongitudeFormatter(degree_symbol='')
			lat_formatter = LatitudeFormatter(number_format='.2f' ) #LatitudeFormatter(degree_symbol='')
			ax.xaxis.set_major_formatter(lon_formatter)
			ax.yaxis.set_major_formatter(lat_formatter)
			ax.set_ybound(lower=lati1, upper=late1)
			#ax.set_ybound(lower=lati2, upper=late2)

			#if ax.is_first_col():
			ax.set_ylabel(model, rotation=90)
			#ax.set_title('X Spatial Loadings')
			ax.set_title('X EOF Spatial Loadings')
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)

			f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFX_'+tar+'_'+mon+'.dat','rb')
			#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
			EOFXDATA = []
			good = True
			ndx = 0
			while good and ndx < M:
				try:
					#Now we read the field
					recl=struct.unpack('i',f.read(4))[0]
					numval=int(recl/np.dtype('float32').itemsize) #this if for each time/EOF stamp
					A0=np.fromfile(f,dtype='float32',count=numval)
					endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
					EOFXDATA.append(np.transpose(A0.reshape((W, H), order='F')))
				except:
					good = False
				ndx += 1
			EOFXDATA = np.asarray(EOFXDATA)
			EOFXDATA[EOFXDATA==-999.]=np.nan #nans
			#if -1*max(np.nanmax(EOFXDATA),np.abs(np.nanmin(EOFXDATA))) < vmi: 
			#       vmi=-max(np.nanmax(EOFXDATA),np.abs(np.nanmin(EOFXDATA)))
			#       vma=-vmi
			eofx[m_ndx].append(EOFXDATA)
			cmap =current_cmap
			CS=plt.pcolormesh(np.linspace(loni1+x_offset, loni1+W*XD+x_offset,num=W), np.linspace(lati1+H*YD+y_offset, lati1+y_offset, num=H),eofx[m_ndx][s_ndx][mode],
			#orig CS=plt.pcolormesh(np.linspace(loni2, loni2+W*XD,num=W), np.linspace(lati2+H*YD, lati2, num=H),eofx[m_ndx][s_ndx][mode],
			#vmin=vmi,vmax=vma,
			vmin=-.1,vmax=.1,
			cmap=current_cmap,
			transform=ccrs.PlateCarree())
			#label = 'EOF loadings'
			label = 'EOF Spatial loadings'
			plt.subplots_adjust(hspace=0)
		#cax = plt.axes([0.14, 0.58, 0.20, 0.03])
		#cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
		cbar = plt.colorbar(CS, orientation='horizontal')
		cbar.set_label(label) #, rotation=270)

			#plt.setp([a.get_xticklabels() for a in fig.axes[:-1]], visible=False)
			#cbar_ax = plt.add_axes([0.85, 0.15, 0.05, 0.7])
			#plt.tight_layout()

		#plot time series
		mon=mol[0]
		if mpref=='PCR':
			f = open("../output/" +model+'_' + predictor + predictand + '_' + mpref + '_XEOFTS_' + tari + '_' + mon + '.txt','r')

			LIST = []
			for line in f:
				if re.search('[0-9]{4}', line)!= None:
					line= line.strip('\n')
					tokens = line.split('\t')
					LIST.append(tokens)
					#print(tokens[0], tokens[1])

			ax = plt.subplot(grid[mm, 1])
			#Things to plot
			time = np.array(LIST)[:,0].astype(dt)
			timeShort = [x[0].split('-')[0][:] for x in LIST]
			eofxts = np.array(LIST)[:,mode+1].astype(np.float)
			#Plotting
			ax.plot(timeShort, eofxts, label = 'X PC', color = 'red', marker = 'x', markersize = 12)
			#ax.plot(timeShort,eofyts, label = 'Y PC', color = 'green', marker = 'x', markersize = 12)

			#Ticks
			start, end = ax.get_xlim()
			ax.xaxis.set_ticks(np.arange(start-2, end, 5))

			# Labels
			ax.set_xlabel('Year')
			#ax.set_ylabel(str(model))
			#ax.set_title('EOF Temporal Scores')
			ax.set_title('Principal Component (PC)')
			ax.grid(axis = 'x', linestyle = '-.')
			ax.legend(loc='upper left')
			#boarders (aesthetics)
			ax.spines['top'].set_visible(False)
			ax.spines['right'].set_visible(False)
			ax.spines['bottom'].set_visible(False)

			f.close()

		else:	
			f = open("../output/" +model+'_' + predictor + predictand + '_' + mpref + '_XEOFTS_' + tari + '_' + mon + '.txt','r')
			f2 = open("../output/" +model+'_' + predictor + predictand + '_' + mpref + '_YEOFTS_' + tari + '_' + mon + '.txt','r')

			LIST = []
			for line in f:
				if re.search('[0-9]{4}', line)!= None:
					line= line.strip('\n')
					tokens = line.split('\t')
					LIST.append(tokens)
					#print(tokens[0], tokens[1])

			LIST2 = []
			for line in f2:
				if re.search('[0-9]{4}', line)!= None:
					#print(line)
					line2= line.strip('\n')
					tokens2 = line.split('\t')
					LIST2.append(tokens2)
					#print(tokens2[0], tokens2[1])

			#fig, ax = plt.subplots(figsize=(16,12))
			ax = plt.subplot(grid[mm, 1])
			#Things to plot
			time = np.array(LIST)[:,0].astype(dt)
			timeShort = [x[0].split('-')[0][:] for x in LIST]
			#eofxts = np.array(LIST)[:,1].astype(np.float)
			#eofyts = np.array(LIST2)[:,1].astype(np.float)
			eofxts = np.array(LIST)[:,mode+1].astype(np.float)
			eofyts = np.array(LIST2)[:,mode+1].astype(np.float)
			#Plotting
			ax.plot(timeShort, eofxts, label = 'X PC', color = 'red', marker = 'x', markersize = 12)
			ax.plot(timeShort,eofyts, label = 'Y PC', color = 'green', marker = 'x', markersize = 12)

			#Ticks
			start, end = ax.get_xlim()
			ax.xaxis.set_ticks(np.arange(start-2, end, 5))

			# Labels
			ax.set_xlabel('Year')
			#ax.set_ylabel(str(model))
			#ax.set_title('EOF Temporal Scores')
			ax.set_title('Principal Components (PCs)')
			ax.grid(axis = 'x', linestyle = '-.')
			ax.legend(loc='upper left')

			#boarders (aesthetics)
			ax.spines['top'].set_visible(False)
			ax.spines['right'].set_visible(False)
			ax.spines['bottom'].set_visible(False)

			f.close()

def plteofs(models,predictand,mode,M,loni,lone,lati,late,fprefix,mpref,tgts,mol,mons,map_color, colorbar_option, use_ocean):
	"""A simple function for ploting EOFs computed by CPT

	PARAMETERS
	----------
		models: list of models to plot
		predictand: exactly that
		mode: EOF being visualized
		M: total number of EOFs computed by CPT (max defined in PyCPT is 10)
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
		fprefix:
	"""
	#mol=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
	if mpref=='None':
		print('No EOFs are computed if MOS=None is used')
		return

	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
#				name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')

	nmods=len(models)
	current_cmap = make_cmap(map_color, continuous=colorbar_option)
	#plt.figure(figsize=(20,10))
	fig, ax = plt.subplots(figsize=(20,15),sharex=True,sharey=True)
	tari=tgts[0]
	model=models[0]
	monn=mol[0]
	nsea=len(mons)
	#Read  grid
	with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFX_'+tari+'_'+monn+'.ctl', "r") as fp:
		for line in lines_that_contain("XDEF", fp):
			W = int(line.split()[1])
			XD= float(line.split()[4])
	with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFX_'+tari+'_'+monn+'.ctl', "r") as fp:
		for line in lines_that_contain("YDEF", fp):
			H = int(line.split()[1])
			YD= float(line.split()[4])

	if mpref=='CCA':
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFY_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				Wy = int(line.split()[1])
				XDy= float(line.split()[4])
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFY_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				Hy = int(line.split()[1])
				YDy= float(line.split()[4])
		eofy=np.empty([M,Hy,Wy])  #define array for later use

	eofx=np.empty([nmods, nsea, M,H,W])  #define array for later use
	vmi = 0
	vma = 0  #for later use of vmin vmax adjustment
	k=0
	for tar in mons:
		k=k+1
		mon=mol[tgts.index(tar)]
		ax = plt.subplot(nmods+1,nsea, k, projection=ccrs.PlateCarree()) #nmods+obs

		if mpref=='CCA':  #skip if there are not predictand EOFs (e.g., PCR)
			ax.set_extent([loni,loni+Wy*XDy,lati,lati+Hy*YDy], ccrs.PlateCarree())
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFY_'+tar+'_'+mon+'.dat','rb')
			#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
			for mo in range(M):
				#Now we read the field
				recl=struct.unpack('i',f.read(4))[0]
				numval=int(recl/np.dtype('float32').itemsize) #this if for each time/EOF stamp
				A0=np.fromfile(f,dtype='float32',count=numval)
				endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
				eofy[mo,:,:]= np.transpose(A0.reshape((Wy, Hy), order='F'))

			eofy[eofy==-999.]=np.nan #nans

			CS=plt.pcolormesh(np.linspace(loni, loni+Wy*XDy,num=Wy), np.linspace(lati+Hy*YDy, lati, num=Hy), eofy[mode,:,:],
			vmin=-.1,vmax=.1,
			cmap=current_cmap,
			transform=ccrs.PlateCarree())
			label = 'EOF loadings'



		ax.add_feature(feature.LAND)
		ax.add_feature(states_provinces, edgecolor='gray')
		if str(use_ocean) == "True":
			ax.add_feature(feature.OCEAN)

		#tick_spacing=0.5
		#ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))

		pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
			  linewidth=2, color='gray', alpha=0., linestyle='--')
		pl.xlabels_top = False
		pl.xlabels_bottom = False
		pl.ylabels_left = True
		pl.ylabels_right = False
		pl.xformatter = LONGITUDE_FORMATTER
		pl.yformatter = LATITUDE_FORMATTER
		pl.xlocator = ticker.MaxNLocator(4)
		pl.ylocator = ticker.MaxNLocator(4)
		ax.set_ybound(lower=lati, upper=late)

		if k<=nsea:
			ax.set_title(tar)
		#if ax.is_first_col():
		ax.set_ylabel(model, rotation=90)
		if k==1:
			ax.text(-0.35,0.5,'Obs',rotation=90,verticalalignment='center', transform=ax.transAxes)

	nrow=0
	for model in models:
		#Read  grid
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFX_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				W = int(line.split()[1])
				XD= float(line.split()[4])
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFX_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				H = int(line.split()[1])
				YD= float(line.split()[4])
		eofx=np.empty([nmods, nsea, M,H,W])  #define array for later use
		m_ndx = models.index(model)
		nrow=nrow+1 #first model is in row=2 and nrow=1
		for tar in mons:
			s_ndx = mons.index(tar)
			k=k+1
			mon=mol[tgts.index(tar)]
			ax = plt.subplot(nmods+1,nsea, k, projection=ccrs.PlateCarree()) #nmods+obs
			if mpref=='PCR':
				ax.set_extent([loni,loni+W*XD,lati,lati+H*YD], ccrs.PlateCarree())  #EOF domains will look different between CCA and PCR if X and Y domains are different
				#print([loni,loni+W*XD,lati,lati+H*YD])
			else:
				ax.set_extent([loni,loni+Wy*XDy,lati,lati+Hy*YDy], ccrs.PlateCarree())
				#print([loni,loni+W*XD,lati,lati+H*YD])
				#print([loni,loni+Wy*XDy,lati,lati+Hy*YDy])
				#print([W,XD,H,YD])
			#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
			states_provinces = feature.NaturalEarthFeature(
				category='cultural',
#				name='admin_1_states_provinces_shp',
				name='admin_0_countries',
				scale='10m',
				facecolor='none')

			ax.add_feature(feature.LAND)
			ax.add_feature(states_provinces, edgecolor='gray')
			#ax.add_feature(feature.COASTLINE)
			if str(use_ocean) == "True":
				ax.add_feature(feature.OCEAN)
			if k == (nrow*nsea)+1:
				ax.text(-0.35,0.5,model,rotation=90,verticalalignment='center', transform=ax.transAxes)


			#tick_spacing=0.5
			#ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))

			pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				  linewidth=2, color='gray', alpha=0., linestyle='--')
			pl.xlabels_top = False
			pl.ylabels_left = True
			pl.ylabels_right = False
			pl.xlabels_bottom = False
			pl.xformatter = LONGITUDE_FORMATTER
			pl.yformatter = LATITUDE_FORMATTER
			pl.xlocator = ticker.MaxNLocator(4)
			pl.ylocator = ticker.MaxNLocator(4)
			ax.add_feature(states_provinces, edgecolor='gray')
			lon_formatter = LongitudeFormatter(number_format='.2f') #LongitudeFormatter(degree_symbol='')
			lat_formatter = LatitudeFormatter(number_format='.2f' ) #LatitudeFormatter(degree_symbol='')
			ax.xaxis.set_major_formatter(lon_formatter)
			ax.yaxis.set_major_formatter(lat_formatter)
			ax.set_ybound(lower=lati, upper=late)

			if k > (nmods+1)*nsea-nsea:
				pl.xlabels_bottom = True

			#if ax.is_first_col():
			ax.set_ylabel(model, rotation=90)

			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFX_'+tar+'_'+mon+'.dat','rb')
			#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
			for mo in range(M):
				#Now we read the field
				recl=struct.unpack('i',f.read(4))[0]
				numval=int(recl/np.dtype('float32').itemsize) #this if for each time/EOF stamp
				A0=np.fromfile(f,dtype='float32',count=numval)
				endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
				eofx[m_ndx, s_ndx, mo,:,:]= np.transpose(A0.reshape((W, H), order='F'))

			eofx[eofx==-999.]=np.nan #nans

			cmap =current_cmap
			CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), eofx[m_ndx, s_ndx,mode,:,:],
			vmin=-.1,vmax=.1,
			cmap=current_cmap,
			transform=ccrs.PlateCarree())
			label = 'EOF loadings'
			plt.subplots_adjust(hspace=0)
			#plt.setp([a.get_xticklabels() for a in fig.axes[:-1]], visible=False)
			#cbar_ax = plt.add_axes([0.85, 0.15, 0.05, 0.7])
			#plt.tight_layout()

			#plt.autoscale(enable=True)
			plt.subplots_adjust(bottom=0.15, top=0.9)
			cax = plt.axes([0.2, 0.08, 0.6, 0.04])
			cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
			cbar.set_label(label) #, rotation=270)
			f.close()


def pltccas_ccats(models,predictor,predictand,loni1,lone1,lati1,late1,loni2,lone2,lati2,late2,fprefix,mpref,tgts,mol,mons,map_color, colorbar_option, use_ocean, x_offset=0, y_offset=0):
	"""A simple function for ploting CCAs computed by CPT

	PARAMETERS
	----------
		models: list of models to plot
		predictor: exactly that
		predictand: exactly that
		mode: EOF being visualized
		M: total number of EOFs computed by CPT (max defined in PyCPT is 10)
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
		fprefix:
	"""
	#mol=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
	if mpref=='None':
		print('No EOFs are computed if MOS=None is used')
		return

	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
#                               name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')

	nmods=len(models)
	current_cmap = make_cmap(map_color, continuous=colorbar_option)
	tari=tgts[0]
	monn=mol[0]
	nsea=len(mons)
	#find number of modes with data for the models to plot CCA loadins and time series
	nmodes_plot=0
	for model in models:
		if mpref=='CCA':
			with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_YCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
				for line in lines_that_contain("TDEF", fp):
					My = int(line.split()[1])
					if My > nmodes_plot:
						nmodes_plot=My

	M=nmodes_plot
	for mode in range(nmodes_plot):
		#print("Mode "+str(mode+1))
		#plt.figure(figsize=(20,10))
		#fig, ax = plt.subplots(figsize=(20,15),sharex=True,sharey=True)
		fig, ax = plt.subplots(figsize=(20,15))
		grid = plt.GridSpec(nmods, 3, wspace=0.2,hspace=0.3)
		if mode == 0:
			fig.suptitle(tari +"  (init: "+monn+")", fontsize=18)
		vmi = 0
		vma = 0  #for later use of vmin vmax adjustment
		mm=-1
		for model in models:
			mm=mm+1
			#Read  grid
			with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
				for line in lines_that_contain("XDEF", fp):
					W = int(line.split()[1])
					XD= float(line.split()[4])
			with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
				for line in lines_that_contain("YDEF", fp):
					H = int(line.split()[1])
					YD= float(line.split()[4])

			if mpref=='CCA':
				with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_YCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
					for line in lines_that_contain("XDEF", fp):
						Wy = int(line.split()[1])
						XDy= float(line.split()[4])
				with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_YCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
					for line in lines_that_contain("YDEF", fp):
						Hy = int(line.split()[1])
						YDy= float(line.split()[4])
				with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_YCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
					for line in lines_that_contain("TDEF", fp):
						My = int(line.split()[1])

				ccay=np.empty([M,Hy,Wy])  #define array for later use
			#print(  [M,Hy,Wy],     )
			#ccax=np.empty([nmods, nsea, M,H,W])  #define array for later use
			ccax=np.empty([1, nsea, M,H,W])  #define array for later use
			if mode+1 > My:
				ax1 = plt.subplot(grid[mm, 0],projection=ccrs.PlateCarree())
				ax1.text(0.5, 0.5, 'N/A', fontsize=18, ha='center')
				ax1.text(-0.35,0.5,model,rotation=90,verticalalignment='center', transform=ax1.transAxes)
				ax1.set_title('X Spatial Loadings')
				ax2 = plt.subplot(grid[mm, 1])
				ax2.text(0.5, 0.5, 'N/A', fontsize=18, ha='center')
				ax2.set_title('Temporal Scores')
				ax3 = plt.subplot(grid[mm, 2],projection=ccrs.PlateCarree())
				ax3.text(0.5, 0.5, 'N/A', fontsize=18, ha='center')
				ax3.set_title('Y Spatial Loadings')
				#break
			else:
				#vmi = 0
				#vma = 0  #for later use of vmin vmax adjustment
				k=0
				for tar in mons:
					k=k+1
					mon=mol[tgts.index(tar)]
					#ax = plt.subplot(nmods+1,nsea, k, projection=ccrs.PlateCarree()) #nmods+obs
					#ax = plt.subplot(nsea,3, k+1, projection=ccrs.PlateCarree()) 
					ax = plt.subplot(grid[mm, 2], projection=ccrs.PlateCarree())

					if mpref=='CCA':  #skip if there are not predictand EOFs (e.g., PCR)
						ax.set_extent([loni2+x_offset,loni2+Wy*XDy+x_offset,lati2+y_offset,lati2+Hy*YDy+y_offset], ccrs.PlateCarree())
						#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
						f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_YCCAMAP_'+tar+'_'+mon+'.dat','rb')
						#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
						#Now we read the field
						CCAYDATA= []
						good = True
						while (good==True):
							try:
								recl=struct.unpack('i',f.read(4))[0] # number of bites in the record
								numval=int(recl/np.dtype('float32').itemsize) #this if for each time/EOF stamp
								A0=np.fromfile(f,dtype='float32',count=numval)
								endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
								CCAYDATA.append(np.transpose(A0.reshape((Wy, Hy), order='F')))
							except:
								good = False
						ccay = np.asarray(CCAYDATA) ## same thing for CCAX

						ccay[ccay==-999.]=np.nan #nans
						if  mm == 0:
							if -1*max(np.nanmax(ccay),np.abs(np.nanmin(ccay))) < vmi:
								vmi=-max(np.nanmax(ccay),np.abs(np.nanmin(ccay)))
								vma=-vmi
						CS=plt.pcolormesh(np.linspace(loni2+x_offset, loni2+Wy*XDy+x_offset,num=Wy), np.linspace(lati2+Hy*YDy+y_offset, lati2+y_offset, num=Hy), ccay[mode],
						#vmin=vmi,vmax=vma,
						vmin=-4,vmax=4,
						cmap=current_cmap,
						transform=ccrs.PlateCarree())
						label = 'CCA loadings'

					ax.add_feature(feature.LAND)
					ax.add_feature(states_provinces, edgecolor='gray')
					if str(use_ocean) == "True":
						ax.add_feature(feature.OCEAN)

						#tick_spacing=0.5
						#ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))

					pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
						  linewidth=2, color='gray', alpha=0., linestyle='--')
					pl.xlabels_top = False
					pl.xlabels_bottom = True
					pl.ylabels_left = True
					pl.ylabels_right = False
					pl.xformatter = LONGITUDE_FORMATTER
					pl.yformatter = LATITUDE_FORMATTER
					pl.xlocator = ticker.MaxNLocator(4)
					pl.ylocator = ticker.MaxNLocator(4)
					lon_formatter = LongitudeFormatter(number_format='.2f') #LongitudeFormatter(degree_symbol='')
					lat_formatter = LatitudeFormatter(number_format='.2f' ) #LatitudeFormatter(degree_symbol='')
					ax.xaxis.set_major_formatter(lon_formatter)
					ax.yaxis.set_major_formatter(lat_formatter)
					ax.set_ybound(lower=lati2, upper=late2)

					#if k<=nsea:
						#ax.set_title(tar)
					#if ax.is_first_col():
					#ax.set_ylabel(model, rotation=90)
					ax.set_title('Y Spatial Loadings')
					#if k==1:
					#ax.text(-0.35,0.5,'Obs',rotation=90,verticalalignment='center', transform=ax.transAxes)


					#plt.autoscale(enable=True)
				plt.subplots_adjust(bottom=0.15, top=0.9)
				#cax = plt.axes([0.69, 0.58, 0.2, 0.03])
				#cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
				cbar = plt.colorbar(CS, orientation='horizontal')
				cbar.set_label(label) #, rotation=270)

				ccax=[]
				#nrow=0
				#for model in models:
				#Read  grid
				with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
					for line in lines_that_contain("XDEF", fp):
						W = int(line.split()[1])
						XD= float(line.split()[4])
				with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
					for line in lines_that_contain("YDEF", fp):
						H = int(line.split()[1])
						YD= float(line.split()[4])
				ccax.append([])
				#m_ndx = models.index(model)
				m_ndx = 0
				#nrow=nrow+1 #first model is in row=2 and nrow=1
				for tar in mons:
					s_ndx = mons.index(tar)
					k=k+1
					mon=mol[tgts.index(tar)]
					#ax = plt.subplot(nmods+1,nsea, k, projection=ccrs.PlateCarree()) #nmods+obs
					#ax = plt.subplot(nsea,3, k, projection=ccrs.PlateCarree())
					ax = plt.subplot(grid[mm, 0], projection=ccrs.PlateCarree())
					if mpref=='PCR':
						ax.set_extent([loni1+x_offset,loni1+W*XD+x_offset,lati1+y_offset,lati1+H*YD+y_offset], ccrs.PlateCarree())  #EOF domains will look different between CCA and PCR if X and Y domains are different
					else:
						#ax.set_extent([loni,loni+Wy*XDy,lati,lati+Hy*YDy], ccrs.PlateCarree())
						ax.set_extent([loni1+x_offset,loni1+W*XD+x_offset,lati1+y_offset,lati1+H*YD+y_offset], ccrs.PlateCarree()) 

					#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
					states_provinces = feature.NaturalEarthFeature(
						category='cultural',
		#                               name='admin_1_states_provinces_shp',
						name='admin_0_countries',
						scale='10m',
						facecolor='none')

					ax.add_feature(feature.LAND)
					ax.add_feature(states_provinces, edgecolor='gray')
					#ax.add_feature(feature.COASTLINE)
					if str(use_ocean) == "True":
						ax.add_feature(feature.OCEAN)
					#if k == (nrow*nsea)+1:
					ax.text(-0.35,0.5,model,rotation=90,verticalalignment='center', transform=ax.transAxes)
					#if mode == 0:
					#       ax.text(0.3, 12.0, tar +"  (init: "+monn+")", fontsize=18, ha='center')

					#tick_spacing=0.5
					#ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))

					pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
						  linewidth=2, color='gray', alpha=0., linestyle='--')
					pl.xlabels_top = False
					pl.ylabels_left = True
					pl.ylabels_right = False
					pl.xlabels_bottom = True
					pl.xformatter = LONGITUDE_FORMATTER
					pl.yformatter = LATITUDE_FORMATTER
					pl.xlocator = ticker.MaxNLocator(4)
					pl.ylocator = ticker.MaxNLocator(4)
					ax.add_feature(states_provinces, edgecolor='gray')
					lon_formatter = LongitudeFormatter(number_format='.2f') #LongitudeFormatter(degree_symbol='')
					lat_formatter = LatitudeFormatter(number_format='.2f' ) #LatitudeFormatter(degree_symbol='')
					ax.xaxis.set_major_formatter(lon_formatter)
					ax.yaxis.set_major_formatter(lat_formatter)
					ax.set_ybound(lower=lati1, upper=late1)

					#if ax.is_first_col():
					ax.set_ylabel(model, rotation=90)
					ax.set_title('X Spatial Loadings')
					#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)

					f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCAMAP_'+tar+'_'+mon+'.dat','rb')
					#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
					CCAXDATA = []
					good = True
					ndx = 0
					while good and ndx < M:
						try:
							#Now we read the field
							recl=struct.unpack('i',f.read(4))[0]
							numval=int(recl/np.dtype('float32').itemsize) #this if for each time/EOF stamp
							A0=np.fromfile(f,dtype='float32',count=numval)
							endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
							CCAXDATA.append(np.transpose(A0.reshape((W, H), order='F')))
						except:
							good = False
						ndx += 1
					CCAXDATA = np.asarray(CCAXDATA)
					CCAXDATA[CCAXDATA==-999.]=np.nan #nans
					#if -1*max(np.nanmax(CCAXDATA),np.abs(np.nanmin(CCAXDATA))) < vmi: 
					#       vmi=-max(np.nanmax(CCAXDATA),np.abs(np.nanmin(CCAXDATA)))
					#       vma=-vmi
					ccax[m_ndx].append(CCAXDATA)
					cmap =current_cmap
					CS=plt.pcolormesh(np.linspace(loni1+x_offset, loni1+W*XD+x_offset,num=W), np.linspace(lati1+H*YD+y_offset, lati1+y_offset, num=H),ccax[m_ndx][s_ndx][mode],
					#vmin=vmi,vmax=vma,
					vmin=-4,vmax=4,
					cmap=current_cmap,
					transform=ccrs.PlateCarree())
					label = 'CCA loadings'
					plt.subplots_adjust(hspace=0)
				#cax = plt.axes([0.14, 0.58, 0.20, 0.03])
				#cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
				cbar = plt.colorbar(CS, orientation='horizontal')
				cbar.set_label(label) #, rotation=270)

					#plt.setp([a.get_xticklabels() for a in fig.axes[:-1]], visible=False)
					#cbar_ax = plt.add_axes([0.85, 0.15, 0.05, 0.7])
					#plt.tight_layout()

				#get correlations
				dss=pd.read_csv("../output/" +model+'_' + predictor + predictand + '_' + mpref + '_CCACorr_' + tari + '_' + mon + '.txt', delim_whitespace=True, header=2)
				daa=dss['correlation'].to_xarray()
				ccacorr=np.floor(daa.values[mode]*10000)/10000
				subtitle_string = "CCA Mode "+str(mode+1)+" ("+model+"): Canonical correlation = "+str(ccacorr)
				ax.text(-0.12,1.2,subtitle_string,fontsize=14,transform=ax.transAxes)
				#plot time series
				mon=mol[0]
				f = open("../output/" +model+'_' + predictor + predictand + '_' + mpref + '_XCCATS_' + tari + '_' + mon + '.txt','r')
				f2 = open("../output/" +model+'_' + predictor + predictand + '_' + mpref + '_YCCATS_' + tari + '_' + mon + '.txt','r')

				LIST = []
				for line in f:
					if re.search('[0-9]{4}', line)!= None:
						line= line.strip('\n')
						tokens = line.split('\t')
						LIST.append(tokens)
						#print(tokens[0], tokens[1])

				LIST2 = []
				for line in f2:
					if re.search('[0-9]{4}', line)!= None:
						#print(line)
						line2= line.strip('\n')
						tokens2 = line.split('\t')
						LIST2.append(tokens2)
						#print(tokens2[0], tokens2[1])

				#fig, ax = plt.subplots(figsize=(16,12))
				ax = plt.subplot(grid[mm, 1])
				#Things to plot
				time = np.array(LIST)[:,0].astype(dt)
				timeShort = [x[0].split('-')[0][:] for x in LIST]
				#ccaxts = np.array(LIST)[:,1].astype(np.float)
				#ccayts = np.array(LIST2)[:,1].astype(np.float)
				ccaxts = np.array(LIST)[:,mode+1].astype(np.float)
				ccayts = np.array(LIST2)[:,mode+1].astype(np.float)
				#Plotting
				ax.plot(timeShort, ccaxts, label = 'x scores', color = 'red', marker = 'x', markersize = 12)
				ax.plot(timeShort,ccayts, label = 'y scores', color = 'green', marker = 'x', markersize = 12)

				#Ticks
				start, end = ax.get_xlim()
				ax.xaxis.set_ticks(np.arange(start-2, end, 5))

				# Labels
				ax.set_xlabel('Year')
				#ax.set_ylabel(str(model))
				ax.set_title('Temporal Scores')
				ax.grid(axis = 'x', linestyle = '-.')
				ax.legend(loc='upper left')

				#boarders (aesthetics)
				ax.spines['top'].set_visible(False)
				ax.spines['right'].set_visible(False)
				ax.spines['bottom'].set_visible(False)

				f.close()

# swap to pltccas from eofs --- this function is still in testing
def pltccas(models,predictand,mode,M,loni,lone,lati,late,fprefix,mpref,tgts,mol,mons,map_color, colorbar_option, use_ocean):
	"""A simple function for ploting CCAs computed by CPT

	PARAMETERS
	----------
		models: list of models to plot
		predictand: exactly that
		mode: EOF being visualized
		M: total number of EOFs computed by CPT (max defined in PyCPT is 10)
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
		fprefix:
	"""
	#mol=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
	if mpref=='None':
		print('No EOFs are computed if MOS=None is used')
		return

	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
#				name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')

	nmods=len(models)
	current_cmap = make_cmap(map_color, continuous=colorbar_option)
	#plt.figure(figsize=(20,10))
	fig, ax = plt.subplots(figsize=(20,15),sharex=True,sharey=True)
	tari=tgts[0]
	model=models[0]
	monn=mol[0]
	nsea=len(mons)
	#Read  grid
	with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
		for line in lines_that_contain("XDEF", fp):
			W = int(line.split()[1])
			XD= float(line.split()[4])
	with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
		for line in lines_that_contain("YDEF", fp):
			H = int(line.split()[1])
			YD= float(line.split()[4])

	if mpref=='CCA':
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_YCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				Wy = int(line.split()[1])
				XDy= float(line.split()[4])
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_YCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				Hy = int(line.split()[1])
				YDy= float(line.split()[4])
		ccay=np.empty([M,Hy,Wy])  #define array for later use
	#print(  [M,Hy,Wy],     )
	ccax=np.empty([nmods, nsea, M,H,W])  #define array for later use
	vmi = 0
	vma = 0  #for later use of vmin vmax adjustment
	k=0
	for tar in mons:
		k=k+1
		mon=mol[tgts.index(tar)]
		ax = plt.subplot(nmods+1,nsea, k, projection=ccrs.PlateCarree()) #nmods+obs

		if mpref=='CCA':  #skip if there are not predictand EOFs (e.g., PCR)
			ax.set_extent([loni,loni+Wy*XDy,lati,lati+Hy*YDy], ccrs.PlateCarree())
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_YCCAMAP_'+tar+'_'+mon+'.dat','rb')
			#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
			#Now we read the field
			CCAYDATA= []
			good = True
			while (good==True):
				try:			
					recl=struct.unpack('i',f.read(4))[0] # number of bites in the record
					numval=int(recl/np.dtype('float32').itemsize) #this if for each time/EOF stamp
					A0=np.fromfile(f,dtype='float32',count=numval)
					endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
					CCAYDATA.append(np.transpose(A0.reshape((Wy, Hy), order='F')))
				except: 
					good = False
			ccay = np.asarray(CCAYDATA) ## same thing for CCAX

			ccay[ccay==-999.]=np.nan #nans
			if -1*max(np.nanmax(ccay),np.abs(np.nanmin(ccay))) < vmi: 
				vmi=-max(np.nanmax(ccay),np.abs(np.nanmin(ccay)))
				vma=-vmi
			CS=plt.pcolormesh(np.linspace(loni, loni+Wy*XDy,num=Wy), np.linspace(lati+Hy*YDy, lati, num=Hy), ccay[mode],
			vmin=vmi,vmax=vma,
			cmap=current_cmap,
			transform=ccrs.PlateCarree())
			label = 'CCA loadings'



		ax.add_feature(feature.LAND)
		ax.add_feature(states_provinces, edgecolor='gray')
		if str(use_ocean) == "True":
			ax.add_feature(feature.OCEAN)

		#tick_spacing=0.5
		#ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))

		pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
			  linewidth=2, color='gray', alpha=0., linestyle='--')
		pl.xlabels_top = False
		pl.xlabels_bottom = False
		pl.ylabels_left = True
		pl.ylabels_right = False
		pl.xformatter = LONGITUDE_FORMATTER
		pl.yformatter = LATITUDE_FORMATTER
		pl.xlocator = ticker.MaxNLocator(4)
		pl.ylocator = ticker.MaxNLocator(4)
		ax.set_ybound(lower=lati, upper=late)

		if k<=nsea:
			ax.set_title(tar)
		#if ax.is_first_col():
		ax.set_ylabel(model, rotation=90)
		if k==1:
			ax.text(-0.35,0.5,'Obs',rotation=90,verticalalignment='center', transform=ax.transAxes)
	ccax=[]
	nrow=0
	for model in models:
		#Read  grid
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				W = int(line.split()[1])
				XD= float(line.split()[4])
		with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCAMAP_'+tari+'_'+monn+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				H = int(line.split()[1])
				YD= float(line.split()[4])
		ccax.append([])
		m_ndx = models.index(model)
		nrow=nrow+1 #first model is in row=2 and nrow=1
		for tar in mons:
			s_ndx = mons.index(tar)
			k=k+1
			mon=mol[tgts.index(tar)]
			ax = plt.subplot(nmods+1,nsea, k, projection=ccrs.PlateCarree()) #nmods+obs
			if mpref=='PCR':
				ax.set_extent([loni,loni+W*XD,lati,lati+H*YD], ccrs.PlateCarree())  #EOF domains will look different between CCA and PCR if X and Y domains are different
			else:
				ax.set_extent([loni,loni+Wy*XDy,lati,lati+Hy*YDy], ccrs.PlateCarree())

			#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
			states_provinces = feature.NaturalEarthFeature(
				category='cultural',
#				name='admin_1_states_provinces_shp',
				name='admin_0_countries',
				scale='10m',
				facecolor='none')

			ax.add_feature(feature.LAND)
			ax.add_feature(states_provinces, edgecolor='gray')
			#ax.add_feature(feature.COASTLINE)
			if str(use_ocean) == "True":
				ax.add_feature(feature.OCEAN)
			if k == (nrow*nsea)+1:
				ax.text(-0.35,0.5,model,rotation=90,verticalalignment='center', transform=ax.transAxes)


			#tick_spacing=0.5
			#ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))

			pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				  linewidth=2, color='gray', alpha=0., linestyle='--')
			pl.xlabels_top = False
			pl.ylabels_left = True
			pl.ylabels_right = False
			pl.xlabels_bottom = False
			pl.xformatter = LONGITUDE_FORMATTER
			pl.yformatter = LATITUDE_FORMATTER
			pl.xlocator = ticker.MaxNLocator(4)
			pl.ylocator = ticker.MaxNLocator(4)
			ax.add_feature(states_provinces, edgecolor='gray')
			lon_formatter = LongitudeFormatter(number_format='.2f') #LongitudeFormatter(degree_symbol='')
			lat_formatter = LatitudeFormatter(number_format='.2f' ) #LatitudeFormatter(degree_symbol='')
			ax.xaxis.set_major_formatter(lon_formatter)
			ax.yaxis.set_major_formatter(lat_formatter)
			ax.set_ybound(lower=lati, upper=late)

			if k > (nmods+1)*nsea-nsea:  #If last row reached
				pl.xlabels_bottom = True

			#if ax.is_first_col():
			ax.set_ylabel(model, rotation=90)

			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCAMAP_'+tar+'_'+mon+'.dat','rb')
			#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
			CCAXDATA = []
			good = True
			ndx = 0
			while good and ndx < M:
				try: 
					#Now we read the field
					recl=struct.unpack('i',f.read(4))[0]
					numval=int(recl/np.dtype('float32').itemsize) #this if for each time/EOF stamp
					A0=np.fromfile(f,dtype='float32',count=numval)
					endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
					CCAXDATA.append(np.transpose(A0.reshape((W, H), order='F')))
				except: 
					good = False
				ndx += 1
			CCAXDATA = np.asarray(CCAXDATA)
			CCAXDATA[CCAXDATA==-999.]=np.nan #nans
			if -1*max(np.nanmax(CCAXDATA),np.abs(np.nanmin(CCAXDATA))) < vmi: 
				vmi=-max(np.nanmax(CCAXDATA),np.abs(np.nanmin(CCAXDATA)))
				vma=-vmi
			ccax[m_ndx].append(CCAXDATA)
			cmap =current_cmap
			CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H),ccax[m_ndx][s_ndx][mode],
			vmin=vmi,vmax=vma,
			cmap=current_cmap,
			transform=ccrs.PlateCarree())
			label = 'CCA loadings'
			plt.subplots_adjust(hspace=0)
			#plt.setp([a.get_xticklabels() for a in fig.axes[:-1]], visible=False)
			#cbar_ax = plt.add_axes([0.85, 0.15, 0.05, 0.7])
			#plt.tight_layout()

		#plt.autoscale(enable=True)
	plt.subplots_adjust(bottom=0.15, top=0.9)
	cax = plt.axes([0.2, 0.08, 0.6, 0.04])
	cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
	cbar.set_label(label) #, rotation=270)
	f.close()


def plt_ccats(models, predictor, predictand, mpref, tgts, mon):
	import re
	from datetime import datetime as dt
	tari = tgts[0]
	model = models[0]
	
	f = open("./output/" +model+'_' + predictor + predictand + '_' + mpref + '_XCCATS_' + tari + '_' + mon + '.txt','r')
	f2 = open("./output/" +model+'_' + predictor + predictand + '_' + mpref + '_YCCATS_' + tari + '_' + mon + '.txt','r')

	LIST = []
	for line in f:
		if re.search('[0-9]{4}', line)!= None:
			line= line.strip('\n')
			tokens = line.split('\t')
			LIST.append(tokens)
			#print(tokens[0], tokens[1])

	LIST2 = []
	for line in f2:
		if re.search('[0-9]{4}', line)!= None:
			#print(line)
			line2= line.strip('\n')
			tokens2 = line.split('\t')
			LIST2.append(tokens2)
			#print(tokens2[0], tokens2[1])

	fig, ax = plt.subplots(figsize=(16,12))
	#Things to plot
	time = np.array(LIST)[:,0].astype(dt)
	timeShort = [x[0].split('-')[0][:] for x in LIST]
	ccaxts = np.array(LIST)[:,1].astype(np.float)
	ccayts = np.array(LIST2)[:,1].astype(np.float)
	#Plotting
	ax.plot(timeShort, ccaxts, label = 'x scores', color = 'red', marker = 'x', markersize = 12)
	ax.plot(timeShort,ccayts, label = 'y scores', color = 'green', marker = 'x', markersize = 12)

	#Ticks
	start, end = ax.get_xlim()
	ax.xaxis.set_ticks(np.arange(start-2, end, 5))

	# Labels
	ax.set_xlabel('Year')
	ax.set_ylabel(str(model))
	ax.set_title('CCA Temporal Scores')
	ax.grid(axis = 'x', linestyle = '-.')
	ax.legend(loc='upper left')
	
	#boarders (aesthetics)
	ax.spines['top'].set_visible(False)
	ax.spines['right'].set_visible(False)
	ax.spines['bottom'].set_visible(False)

def pltmap(models,predictand,score,loni,lone,lati,late,fprefix,mpref,tgts, mo, mons, map_color, colorbar_option, use_ocean, x_offset=0, y_offset=0):
	"""A simple function for ploting the statistical scores

	PARAMETERS
	----------
		score: the score
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
	"""
	vmi=0
	nmods=len(models)
	nsea=len(mons)  #number of seasons and columns
	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
#				name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')
	#plt.figure(figsize=(20,10))
	fig, ax = plt.subplots(figsize=(20,15),sharex=True,sharey=True)
	k=0
	nrow=-1
	for model in models:
		nrow=nrow+1
		#print(model)
		kk=-1
		for tar in mons:
			kk=kk+1
			k=k+1
			#mon=mo[kk]
			mon=mo[tgts.index(tar)]
			#Read grads binary file size H, W
			with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_'+score+'_'+tar+'_'+mon+'.ctl', "r") as fp:
				for line in lines_that_contain("XDEF", fp):
					W = int(line.split()[1])
					XD= float(line.split()[4])
			with open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_'+score+'_'+tar+'_'+mon+'.ctl', "r") as fp:
				for line in lines_that_contain("YDEF", fp):
					H = int(line.split()[1])
					YD= float(line.split()[4])

#			ax = plt.subplot(nwk/2, 2, wk, projection=ccrs.PlateCarree())

			ax = plt.subplot(nmods,nsea, k, projection=ccrs.PlateCarree())
			ax.set_extent([loni + x_offset,loni+W*XD+x_offset,lati +y_offset,lati+H*YD+y_offset], ccrs.PlateCarree())
			if k == (nrow*nsea)+1:
				ax.text(-0.35,0.5,model,rotation=90,verticalalignment='center', transform=ax.transAxes)



			ax.add_feature(feature.LAND)
			ax.add_feature(states_provinces, edgecolor='gray')
			#ax.add_feature(feature.COASTLINE)
			if str(use_ocean) == 'True':
				ax.add_feature(feature.OCEAN)

			#tick_spacing=0.5
			#ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))

			pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				  linewidth=2, color='gray', alpha=0., linestyle='--')
			pl.xlabels_top = False
			pl.ylabels_left = True
			pl.ylabels_right = False
			pl.xlabels_bottom = False
			if k > (nmods)*nsea-nsea:
				pl.xlabels_bottom = True
			pl.xformatter = LONGITUDE_FORMATTER
			pl.yformatter = LATITUDE_FORMATTER
			pl.xlocator = ticker.MaxNLocator(4)
			pl.ylocator = ticker.MaxNLocator(4)
			lon_formatter = LongitudeFormatter(number_format='.2f' ) #LongitudeFormatter(degree_symbol='')
			lat_formatter = LatitudeFormatter(number_format='.2f' ) #LatitudeFormatter(degree_symbol='')
			ax.xaxis.set_major_formatter(lon_formatter)
			ax.yaxis.set_major_formatter(lat_formatter)
			ax.xaxis.set_major_locator(plt.MaxNLocator(4))
			ax.yaxis.set_major_locator(plt.MaxNLocator(4))
			ax.add_feature(states_provinces, edgecolor='gray')
			ax.set_ybound(lower=lati, upper=late)

			if k<=nsea:
				ax.set_title(tar +"  (init: "+mon+")")
				#ax.ylabel(model, fontsize=11)
			#for i, axi in enumerate(axes):  # need to enumerate to slice the data
			#	axi.set_ylabel(model, fontsize=12)

			if score == 'CCAFCST_V' or score == 'PCRFCST_V':
				#f=open('../output/'+model+'_'+fprefix+'_'+score+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.dat','rb')
				f=open('../output/'+model+'_'+fprefix+predictand+'_'+score+'_'+tar+'_'+mon+str(fday)+'.dat','rb')
				recl=struct.unpack('i',f.read(4))[0]
				numval=int(recl/np.dtype('float32').itemsize)
				#Now we read the field
				A=np.fromfile(f,dtype='float32',count=numval)
				var = np.transpose(A.reshape((W, H), order='F'))
				var[var==-999.]=np.nan #only sensible values
				current_cmap = make_cmap(map_color, N=10, continuous=colorbar_option)
				current_cmap.set_bad('white',1.0)
				current_cmap.set_under('white', 1.0)
				CS=plt.pcolormesh(np.linspace(loni+x_offset, loni+W*XD+x_offset,num=W), np.linspace(lati+H*YD+y_offset, lati+y_offset, num=H), var,
					#vmin=-max(np.max(var),np.abs(np.min(var))), #vmax=np.max(var),
					norm=MidpointNormalize(midpoint=0.),
					cmap=current_cmap,
					transform=ccrs.PlateCarree())
				ax.set_title("Deterministic forecast for Week "+str(wk))
				if fprefix == 'RFREQ':
					label ='Freq Rainy Days (days)'
				elif fprefix == 'PRCP':
					label = 'Rainfall anomaly (mm/week)'
					f.close()
				#current_cmap = plt.cm.get_cmap()
				#current_cmap.set_bad(color='white')
				#current_cmap.set_under('white', 1.0)
			else:
				vmi, vma = 0, 0
				for model2 in models: 
					#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
					f=open('../output/'+model2+'_'+fprefix+predictand+'_'+mpref+'_'+score+'_'+tar+'_'+mon+'.dat','rb')
					recl=struct.unpack('i',f.read(4))[0]
					numval=int(recl/np.dtype('float32').itemsize)
					#Now we read the field
					A=np.fromfile(f,dtype='float32',count=numval)
					var = np.transpose(A.reshape((W, H), order='F'))
					var[var==-999.]=np.nan
					if -1*max(np.nanmax(var),np.abs(np.nanmin(var))) < vmi: 
						vmi=-max(np.nanmax(var),np.abs(np.nanmin(var)))
						vma=-vmi
				#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
				f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_'+score+'_'+tar+'_'+mon+'.dat','rb')
				recl=struct.unpack('i',f.read(4))[0]
				numval=int(recl/np.dtype('float32').itemsize)
				#Now we read the field
				A=np.fromfile(f,dtype='float32',count=numval)
				var = np.transpose(A.reshape((W, H), order='F'))
				var[var==-999.]=np.nan
				#define colorbars, depending on each score	--This can be easily written as a function
				if score == '2AFC':
					CS=plt.pcolormesh(np.linspace(loni+x_offset, loni+W*XD+x_offset,num=W), np.linspace(lati+H*YD+y_offset, lati+y_offset, num=H), var,
					vmin=0,vmax=100,
					cmap=make_cmap(map_color, N=11, continuous=colorbar_option),
					transform=ccrs.PlateCarree())
					label = '2AFC (%)'

				if score == 'RocAbove' or score=='RocBelow':
					CS=plt.pcolormesh(np.linspace(loni+x_offset, loni+W*XD+x_offset,num=W), np.linspace(lati+H*YD+y_offset, lati+y_offset, num=H), var,
					vmin=0,vmax=1,
					cmap=make_cmap(map_color, N=11, continuous=colorbar_option),
					transform=ccrs.PlateCarree())
					label = 'ROC area'

				if score == 'Spearman' or score=='Pearson':
					CS=plt.pcolormesh(np.linspace(loni+x_offset, loni+W*XD+x_offset,num=W), np.linspace(lati+H*YD+y_offset, lati+y_offset, num=H), var,
					vmin=-1,vmax=1,
					cmap=make_cmap(map_color, N=11, continuous=colorbar_option),
					transform=ccrs.PlateCarree())
					label = 'Correlation'

				if score == 'RPSS':
					CS=plt.pcolormesh(np.linspace(loni+x_offset, loni+W*XD+x_offset,num=W), np.linspace(lati+H*YD+y_offset, lati+y_offset, num=H), var,
					vmin=vmi,vmax=vma,
					cmap=make_cmap(map_color, N=20, continuous=colorbar_option),
					transform=ccrs.PlateCarree())
					label = 'RPSS (all categories)'

				if score=='GROC':
					CS=plt.pcolormesh(np.linspace(loni+x_offset, loni+W*XD+x_offset,num=W), np.linspace(lati+H*YD+y_offset, lati+y_offset, num=H), var,
					vmin=0,vmax=100,
					cmap=make_cmap(map_color, N=11, continuous=colorbar_option),
					transform=ccrs.PlateCarree())
					label = 'GROC (probabilistic)'

				if score=='Ignorance':
					CS=plt.pcolormesh(np.linspace(loni+x_offset, loni+W*XD+x_offset,num=W), np.linspace(lati+H*YD+y_offset, lati+y_offset, num=H), var/1.5849,
					vmin=0.6,vmax=1.4,
					cmap=make_cmap(map_color, N=9, continuous=colorbar_option),
					transform=ccrs.PlateCarree())
					label = 'Ignorance Skill Score (all categories)'

			f.close()

		plt.subplots_adjust(hspace=0)
		#plt.setp([a.get_xticklabels() for a in fig.axes[:-1]], visible=False)
		#cbar_ax = plt.add_axes([0.85, 0.15, 0.05, 0.7])
		#plt.tight_layout()
		#plt.autoscale(enable=True)
		plt.subplots_adjust(bottom=0.15, top=0.9)
		cax = plt.axes([0.2, 0.08, 0.6, 0.04])
		cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
		cbar.set_label(label) #, rotation=270)

#Still working on it: AGM
def skilltab(models,predictand,score,wknam,lon1,lat1,lat2,lon2,loni,lone,lati,late,fprefix,mpref,training_season,mon,fday,nwk):
	"""A simple function for ploting probabilities of exceedance and PDFs (for a given threshold)

	PARAMETERS
	----------
		thrs: the threshold, in the units of the predictand
		lon: longitude
		lat: latitude
	"""

	#Read grads binary file size H, W  --it assumes all files have the same size, and that 2AFC exists
	with open('../output/'+model+'_'+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk1.ctl', "r") as fp:
		for line in lines_that_contain("XDEF", fp):
			W = int(line.split()[1])
			XD= float(line.split()[4])
	with open('../output/'+model+'_'+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk1.ctl', "r") as fp:
		for line in lines_that_contain("YDEF", fp):
			H = int(line.split()[1])
			YD= float(line.split()[4])

	#Find the gridbox:
	lonrange = np.linspace(loni, loni+W*XD,num=W)
	latrange = np.linspace(lati+H*YD, lati, num=H)  #need to reverse the latitudes because of CPT (GrADS YREV option)
	lon_grid, lat_grid = np.meshgrid(lonrange, latrange)
	#first point
	a = abs(lat_grid-lat1)+abs(lon_grid-lon1)
	i1,j1 = np.unravel_index(a.argmin(),a.shape)   #i:latitude   j:longitude
	#second point
	a = abs(lat_grid-lat2)+abs(lon_grid-lon2)
	i2,j2 = np.unravel_index(a.argmin(),a.shape)   #i:latitude   j:longitude

	df = pd.DataFrame(index=wknam[0:nwk])
	for L in range(nwk):
		wk=L+1
		for S in score:
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			f=open('../output/'+model+'_'+fprefix+'_'+mpref+'_'+str(S)+'_'+training_season+'_wk'+str(wk)+'.dat','rb')
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			#Now we read the field
			A=np.fromfile(f,dtype='float32',count=numval)
			var = np.transpose(A.reshape((W, H), order='F'))
			var[var==-999.]=np.nan #only sensible values
			df.at[wknam[L], str(S)] = round(np.nanmean(np.nanmean(var[i1:i2,j1:j2], axis=1), axis=0),2)
			df.at[wknam[L], 'max('+str(S)+')']  = round(np.nanmax(var[i1:i2,j1:j2]),2)
			df.at[wknam[L], 'min('+str(S)+')']  = round(np.nanmin(var[i1:i2,j1:j2]),2)
	return df
	f.close()

def pltmapProb(loni,lone,lati,late,fprefix,mpref,training_season, mon, fday, nwk):
	"""A simple function for ploting probabilistic forecasts

	PARAMETERS
	----------
		score: the score
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
		title: title

	Output

	EXAMPLES

	"""
	#Need this score to be defined by the calibration method!!!
	score = 'CCAFCST_P'

	plt.figure(figsize=(15,20))

	for L in range(nwk):
		wk=L+1
		#Read grads binary file size H, W  --it assumes that 2AFC file exists (template for final domain size)
		with open('../output/'+model+'_'+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				W = int(line.split()[1])
				XD= float(line.split()[4])
		with open('../output/'+model+'_'+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				H = int(line.split()[1])
				YD= float(line.split()[4])

		#Prepare to read grads binary file  [float32 for Fortran sequential binary files]
		Record = np.dtype(('float32', H*W))

		#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
		states_provinces = feature.NaturalEarthFeature(
			category='cultural',
#			name='admin_1_states_provinces_shp',
			name='admin_0_countries',
			scale='10m',
			facecolor='none')


		f=open('../output/'+model+'_'+fprefix+'_'+score+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.dat','rb')

		tit=['Below Normal','Normal','Above Normal']
		for i in range(3):
				#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
				recl=struct.unpack('i',f.read(4))[0]
				numval=int(recl/np.dtype('float32').itemsize)
				#We now read the field for that record (probabilistic files have 3 records: below, normal and above)
				B=np.fromfile(f,dtype='float32',count=numval) #astype('float')
				endrec=struct.unpack('i',f.read(4))[0]
				var = np.flip(np.transpose(B.reshape((W, H), order='F')),0)
				var[var<0]=np.nan #only positive values
				ax2=plt.subplot(nwk, 3, (L*3)+(i+1),projection=ccrs.PlateCarree())
				ax2.set_title("Week "+str(wk)+ ": "+tit[i])
				ax2.add_feature(feature.LAND)
				ax2.add_feature(feature.COASTLINE)
				#ax2.set_ybound(lower=lati, upper=late)
				pl2=ax2.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
					linewidth=2, color='gray', alpha=0.5, linestyle='--')
				pl2.xlabels_top = False
				pl2.ylabels_left = True
				pl2.ylabels_right = False
				pl.xformatter = LONGITUDE_FORMATTER
				pl.yformatter = LATITUDE_FORMATTER
				pl.xlocator = ticker.MaxNLocator(4)
				pl.ylocator = ticker.MaxNLocator(4)
				ax.add_feature(states_provinces, edgecolor='gray')
				lon_formatter = LongitudeFormatter(number_format='.2f' ) #LongitudeFormatter(degree_symbol='')
				lat_formatter = LatitudeFormatter(number_format='.2f' ) #LatitudeFormatter(degree_symbol='')
				ax.xaxis.set_major_formatter(lon_formatter)
				ax.yaxis.set_major_formatter(lat_formatter)
				ax.set_ybound(lower=lati, upper=late)
				ax2.set_extent([loni,loni+W*XD,lati,lati+H*YD], ccrs.PlateCarree())

				#ax2.set_ybound(lower=lati, upper=late)
				#ax2.set_xbound(lower=loni, upper=lone)
				#ax2.set_adjustable('box')
				#ax2.set_aspect('auto',adjustable='datalim',anchor='C')
				CS=ax2.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati,lati+H*YD, num=H), var,
				vmin=0,vmax=100,
				cmap=plt.cm.bwr,
				transform=ccrs.PlateCarree())
				#plt.show(block=False)

	plt.subplots_adjust(hspace=0)
	plt.subplots_adjust(bottom=0.15, top=0.9)
	cax = plt.axes([0.2, 0.08, 0.6, 0.04])
	cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
	cbar.set_label('Probability (%)') #, rotation=270)
	f.close()

def pltmapff(models,predictor,predictand,thrs,ispctl,ntrain,loni,lone,lati,late,fprefix,mpref,monf,fyr,mons,tgts, x_offset=0, y_offset=0):
	"""A simple function for ploting probabilistic forecasts in flexible format (for a given threshold)

	PARAMETERS
	----------
		models: models to plot (array)
		predictand: predictand used
		thrs: the threshold, in the units of the predictand
		ispctl: logical to identify if it's a percentile threshold
		ntrain: training period (number of years)
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
		fprefix: file prefix (predictor)
		mpref: MOS method
		monf: forecast month of initialization
		fyr: forecast year of initialization
		mons:
		tgts: target season(s)
	"""
	#Implement: read degrees of freedom from CPT file
	#Formally, for CCA, dof=ntrain - #CCAmodes -1 ; dof=ntrain for now
	dof=ntrain
	nmods=len(models)
	tar=tgts[mons.index(monf)]
	writeGrads('FCST_Obs', '../input/NextGen_'+predictor+'_'+tar+'_ini'+monf+'.tsv', models, predictor, predictand, mpref, tar, monf, fyr, monf)

	#Read grads binary file size H, W  --it assumes all files have the same size, and that 2AFC exists
	with open('../output/'+models[0]+'_'+fprefix+predictand+'_'+mpref+'FCST_mu_'+tar+'_'+monf+str(fyr)+'.ctl', "r") as fp:
		for line in lines_that_contain("XDEF", fp):
			W = int(line.split()[1])
			XD= float(line.split()[4])
	with open('../output/'+models[0]+'_'+fprefix+predictand+'_'+mpref+'FCST_mu_'+tar+'_'+monf+str(fyr)+'.ctl', "r") as fp:
		for line in lines_that_contain("YDEF", fp):
			H = int(line.split()[1])
			YD= float(line.split()[4])
	with open('../output/'+models[0]+'_'+fprefix+predictand+'_'+mpref+'FCST_Obs_'+tar+'_'+monf+str(fyr)+'.ctl', "r") as fp:
		for line in lines_that_contain("TDEF", fp):
			T = int(line.split()[1])
			TD= 1  #not used

	#plt.figure(figsize=(15,20))
	if ispctl:
		thrso=thrs
		thrst = thrs * 100

	fig, ax = plt.subplots(figsize=(16,nmods*10),sharex=True,sharey=True)
	k=0
	for model in models:
		k=k+1
		#Read mean
		#Read grads binary file size H, W  --it assumes all files have the same size, and that 2AFC exists
		f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_mu_'+tar+'_'+monf+str(fyr)+'.dat','rb')
		recl=struct.unpack('i',f.read(4))[0]
		numval=int(recl/np.dtype('float32').itemsize)
		#Now we read the field
		A=np.fromfile(f,dtype='float32',count=numval)
		muf = np.transpose(A.reshape((W, H), order='F'))
		muf[muf==-999.]=np.nan #only sensible values

		#Read variance
		f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_var_'+tar+'_'+monf+str(fyr)+'.dat','rb')
		recl=struct.unpack('i',f.read(4))[0]
		numval=int(recl/np.dtype('float32').itemsize)
		#Now we read the field
		A=np.fromfile(f,dtype='float32',count=numval)
		vari = np.transpose(A.reshape((W, H), order='F'))
		vari[vari<0.]=np.nan #only positive values

		#Obs file--------
		#Compute obs mean and variance.
		#
		muc0=np.empty([T,H,W])  #define array for later use
		#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
		f=open('../output/'+models[0]+'_'+fprefix+predictand+'_'+mpref+'FCST_Obs_'+tar+'_'+monf+str(fyr)+'.dat','rb')
		#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
		for it in range(T):
			#Now we read the field
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize) #this if for each time stamp
			A0=np.fromfile(f,dtype='float32',count=numval)
			endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
			muc0[it,:,:]= np.transpose(A0.reshape((W, H), order='F'))
			
## AWR 12/04/21: Because the above 'FCST_Obs_' file erroneously contains the NG deterministic hindcasts, we overwrite muc0 here with the contents of obs Y file (eg obs_T2M_Oct-Dec.tsv)
		lats_obs, longs_obs, all_vals, years_obs = read_obs_CPTv10(filename='../input/obs'+'_'+predictand+'_'+tar+'.tsv')
		muc0 = all_vals	
		muc0[muc0==-999.]=np.nan #identify NaNs
		muc=np.nanmean(muc0, axis=0)  #axis 0 is T
		#Compute obs variance
		varc=np.nanvar(muc0, axis=0)  #axis 0 is T
		
		# make sure that muf.shape == muc.shape and varc.shape == varf.shape

		if muc.shape[0]*muc.shape[1] != muf.shape[0]*muf.shape[1]:	
			# take care of shape in longitude dimension:
			if muc.shape[1] - muf.shape[1] == 2: # ingrid has rounded up on both sides 
				muc = muc[:, 1:muc.shape[1]-1] # remove one from both sides
				varc = varc[:, 1:varc.shape[1]-1] # same for varc 
			elif muc.shape[1] - muf.shape[1] == 1: # ingrid only rounded up on one side  
				if longs_obs[0] < loni: # then ingrid has rounded to larger domain to the west, remove from left 
					muc = muc[:, 1:] # rmove one from the western edge
					varc = varc[:, 1:] # same for varc 
				elif longs_obs[-1] > lone: # then ingrid has rounded to larger domain to the east
					muc = muc[:, :muc.shape[1]-1]
					varc = varc[:, :varc.shape[1]-1]
				else: 
					print( 'PROBLEM:\n  OBS - W:{}, E:{}, SIZE: {}\n FCST - W: {}, E:{}, SIZE:{}'.format( longs_obs[0], longs_obs[-1], muc.shape[1], loni, lone, muf.shape[1]) ) 
			else: 
				print('PROBLEM: OBS SHAPE {} BUT FCST SHAPE {}'.format(muc.shape, muf.shape))
			
			# take care of shape in latitude direction
			if muc.shape[0] - muf.shape[0] == 2: # ingrid has rounded up on both sides
				muc = muc[1:muc.shape[0]-1, :] # remove one from both sides
				varc = varc[1:varc.shape[0]-1, :] # same for varc
			elif muc.shape[1] - muf.shape[1] == 1: # ingrid only rounded up on one side
				if min(lats_obs) < lati: # then ingrid has rounded to larger domain to the south
					muc = muc[1:,:] # rmove one from the western edge
					varc = varc[1:,:] # same for varc
				elif max(lats_obs) > late: # the ingrid rounded to larger domain to the north 
					muc = muc[:muc.shape[0]-1, :]
					varc = varc[:varc.shape[0]-1, :] 
				else:
					print( 'PROBLEM:\n  OBS - S:{}, N:{}, SIZE: {}\n FCST - S: {}, N:{}, SIZE:{}'.format( lats_obs[0], lats_obs[-1], muc.shape[0], lati, late, muf.shape[0]) )
			else:
				print('PROBLEM: OBS SHAPE {} BUT FCST SHAPE {}'.format(muc.shape, muf.shape))


		#Compute scale parameter for the t-Student distribution
		# scalef=np.sqrt(dof*vari) 		 #due to transformation from Gamma
		scalef=np.sqrt((dof-2)/dof*vari) # Changed by AWR 10/28/21 to match scalec 
		scalec=np.sqrt((dof-2)/dof*varc)

		if ispctl:
			thrs=t.ppf(thrso, dof, loc=muc, scale=scalec)  #If using percentiles, compute value using climo

		fprob = exceedprob(thrs,dof,muf,scalef)

		ax = plt.subplot(nmods, 1, k, projection=ccrs.PlateCarree())
		ax.set_extent([loni+x_offset,loni+W*XD+x_offset,lati+y_offset,lati+H*YD+y_offset], ccrs.PlateCarree())

		#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
		states_provinces = feature.NaturalEarthFeature(
			category='cultural',
#			name='admin_1_states_provinces_shp',
			name='admin_0_countries',
			scale='10m',
			facecolor='none')

		ax.add_feature(feature.LAND)
		ax.add_feature(feature.COASTLINE)

		if k==1:
			if ispctl:
				ax.set_title('Probability (%) of exceeding the '+str(int(thrst))+'th percentile')
			else:
				ax.set_title('Probability (%) of exceeding '+str(thrs)+" mm/month")

		pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
			linewidth=2, color='gray', alpha=0.5, linestyle='--')
		pl.xlabels_top = False
		pl.ylabels_left = True
		pl.ylabels_right = False
		pl.xformatter = LONGITUDE_FORMATTER
		pl.yformatter = LATITUDE_FORMATTER
		pl.xlocator = ticker.MaxNLocator(4)
		pl.ylocator = ticker.MaxNLocator(4)
		ax.add_feature(states_provinces, edgecolor='gray')
		lon_formatter = LongitudeFormatter(number_format='.2f' ) #LongitudeFormatter(degree_symbol='')
		lat_formatter = LatitudeFormatter(number_format='.2f' ) #LatitudeFormatter(degree_symbol='')
		ax.xaxis.set_major_formatter(lon_formatter)
		ax.yaxis.set_major_formatter(lat_formatter)
		ax.set_ybound(lower=lati, upper=late)
		CS=plt.pcolormesh(np.linspace(loni+x_offset, loni+W*XD+x_offset,num=W), np.linspace(lati+H*YD+y_offset, lati+y_offset, num=H), fprob,
			vmin=0,vmax=100,
			cmap=plt.cm.bwr,
			transform=ccrs.PlateCarree())
		label = 'Probability (%) of Exceedance'
		ax.text(-0.2,0.5,model,rotation=90,verticalalignment='center', transform=ax.transAxes)


		#plt.autoscale(enable=True)

		plt.tight_layout()
		plt.subplots_adjust(hspace=0)
		plt.subplots_adjust(bottom=0.15, top=0.9)
		cax = plt.axes([0.2, 0.08, 0.6, 0.04])
		cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')

		# for i, row in enumerate(ax):
		# 	for j, cell in enumerate(row):
		# 		if i == len(ax) - 1:
		# 			cell.set_xlabel("noise column: {0:d}".format(j + 1))
		# 		if j == 0:
		# 			cell.set_ylabel("noise row: {0:d}".format(i + 1))

		ax.set_ylabel(model, rotation=90)
		cbar.set_label(label) #, rotation=270)
		f.close()

def pltprobff(models,predictor,predictand,thrs,ntrain,lon,lat,loni,lone,lati,late,fprefix,mpref,monf,fyr,mons,tgts):
	"""A simple function for ploting probabilities of exceedance and PDFs (for a given threshold)

	PARAMETERS
	----------
		thrs: the threshold, in the units of the predictand
		lon: longitude
		lat: latitude
	"""
	#Implement: read degrees of freedom from CPT file
	#Formally, for CCA, dof=ntrain - #CCAmodes -1 ; since ntrain is huge after concat, dof~=ntrain for now
	dof=ntrain
	tar=tgts[mons.index(monf)]
	#Read grads binary file size H, W  --it assumes all files have the same size, and that 2AFC exists
	with open('../output/'+models[0]+'_'+fprefix+predictand+'_'+mpref+'FCST_mu_'+tar+'_'+monf+str(fyr)+'.ctl', "r") as fp:
		for line in lines_that_contain("XDEF", fp):
			W = int(line.split()[1])
			XD= float(line.split()[4])
	with open('../output/'+models[0]+'_'+fprefix+predictand+'_'+mpref+'FCST_mu_'+tar+'_'+monf+str(fyr)+'.ctl', "r") as fp:
		for line in lines_that_contain("YDEF", fp):
			H = int(line.split()[1])
			YD= float(line.split()[4])
	with open('../output/'+models[0]+'_'+fprefix+predictand+'_'+mpref+'FCST_Obs_'+tar+'_'+monf+str(fyr)+'.ctl', "r") as fp:
		for line in lines_that_contain("TDEF", fp):
			T = int(line.split()[1])
			TD= 1  #not used

	#Find the gridbox:
	lonrange = np.linspace(loni, loni+(W-1)*XD,num=W)
	latrange = np.linspace(lati+(H-1)*YD, lati, num=H)  #need to reverse the latitudes because of CPT (GrADS YREV option)
	if lon > np.max(lonrange) or lon < np.min(lonrange) or lat < np.min(latrange) or lat > np.max(latrange):
		print('LAT ({}) and LON({}) MUST BE WITHIN GIVEN RANGES: LAT({}, {}), LON({},{})'.format(lat, lon, np.min(latrange) , np.max(latrange), np.min(lonrange), np.max(lonrange)))
		return 
	lon_grid, lat_grid = np.meshgrid(lonrange, latrange)
	a = abs(lat_grid-lat)+abs(lon_grid-lon)
	i,j = np.unravel_index(a.argmin(),a.shape)   #i:latitude   j:longitude

	#Now compute stuff and plot
	plt.figure(figsize=(15,15))

	k=0
	for model in models:
		k=k+1
		#Forecast files--------
		#Read mean
		#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
		f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_mu_'+tar+'_'+monf+str(fyr)+'.dat','rb')
		recl=struct.unpack('i',f.read(4))[0]
		numval=int(recl/np.dtype('float32').itemsize)
		#Now we read the field
		A=np.fromfile(f,dtype='float32',count=numval)
		muf = np.transpose(A.reshape((W, H), order='F'))
		muf[muf==-999.]=np.nan #only sensible values

		#Read variance
		f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_var_'+tar+'_'+monf+str(fyr)+'.dat','rb')
		recl=struct.unpack('i',f.read(4))[0]
		numval=int(recl/np.dtype('float32').itemsize)
		#Now we read the field
		A=np.fromfile(f,dtype='float32',count=numval)
		varf = np.transpose(A.reshape((W, H), order='F'))
		varf[varf==-999.]=np.nan #only sensible values

		#Obs file--------
		#Compute obs mean and variance.
		#
		muc0=np.empty([T,H,W])  #define array for later use
		#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
		f=open('../output/'+models[0]+'_'+fprefix+predictand+'_'+mpref+'FCST_Obs_'+tar+'_'+monf+str(fyr)+'.dat','rb')
		#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
		for it in range(T):
			#Now we read the field
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize) #this if for each time stamp
			A0=np.fromfile(f,dtype='float32',count=numval)
			endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
			muc0[it,:,:]= np.transpose(A0.reshape((W, H), order='F'))
			
## AWR 12/04/21: Because the above 'FCST_Obs_' file erroneously contains the NG deterministic hindcasts, we overwrite muc0 here with the contents of obs Y file (eg obs_T2M_Oct-Dec.tsv)
		lats_obs, longs_obs, all_vals, years_obs = read_obs_CPTv10(filename='../input/obs'+'_'+predictand+'_'+tar+'.tsv')
		muc0 = all_vals	

		muc0[muc0==-999.]=np.nan #identify NaNs
		muc=np.nanmean(muc0, axis=0)  #axis 0 is T
		#Compute obs variance
		varc=np.nanvar(muc0, axis=0)  #axis 0 is T

		# make sure that muf.shape == muc.shape and varc.shape == varf.shape

		if muc.shape[0]*muc.shape[1] != muf.shape[0]*muf.shape[1]:
			# take care of shape in longitude dimension:
			if muc.shape[1] - muf.shape[1] == 2: # ingrid has rounded up on both sides
				muc = muc[:, 1:muc.shape[1]-1] # remove one from both sides
				varc = varc[:, 1:varc.shape[1]-1] # same for varc
			elif muc.shape[1] - muf.shape[1] == 1: # ingrid only rounded up on one side
				if longs_obs[0] < loni: # then ingrid has rounded to larger domain to the west, remove from left
					muc = muc[:, 1:] # rmove one from the western edge
					varc = varc[:, 1:] # same for varc
				elif longs_obs[-1] > lone: # then ingrid has rounded to larger domain to the east
					muc = muc[:, :muc.shape[1]-1]
					varc = varc[:, :varc.shape[1]-1]
				else:
					print( 'PROBLEM:\n  OBS - W:{}, E:{}, SIZE: {}\n FCST - W: {}, E:{}, SIZE:{}'.format( longs_obs[0], longs_obs[-1], muc.shape[1], loni, lone, muf.shape[1]) )
			else:
				print('PROBLEM: OBS SHAPE {} BUT FCST SHAPE {}'.format(muc.shape, muf.shape))

		# take care of shape in latitude direction
		if muc.shape[0] - muf.shape[0] == 2: # ingrid has rounded up on both sides
			muc = muc[1:muc.shape[0]-1, :] # remove one from both sides
			varc = varc[1:varc.shape[0]-1, :] # same for varc
		elif muc.shape[1] - muf.shape[1] == 1: # ingrid only rounded up on one side
			if min(lats_obs) < lati: # then ingrid has rounded to larger domain to the south
				muc = muc[1:,:] # rmove one from the western edge
				varc = varc[1:,:] # same for varc
			elif max(lats_obs) > late: # the ingrid rounded to larger domain to the north
				muc = muc[:muc.shape[0]-1, :]
				varc = varc[:varc.shape[0]-1, :]
			else:
				print( 'PROBLEM:\n  OBS - S:{}, N:{}, SIZE: {}\n FCST - S: {}, N:{}, SIZE:{}'.format( lats_obs[0], lats_obs[-1], muc.shape[0], lati, late, muf.shape[0]) )
		else:
			print('PROBLEM: OBS SHAPE {} BUT FCST SHAPE {}'.format(muc.shape, muf.shape))
		
		#Select gridbox values
		muc=muc[i,j]
		muf=muf[i,j]

		#print(muc)   #Test it's actually zero
		varc=varc[i,j]
		varf=varf[i,j]

		#Compute scale parameter for the t-Student distribution
		# scalef=np.sqrt(dof*varf)   #due to transformation from Gamma
		scalef=np.sqrt((dof-2)/dof*varf) # Changed by AWR 10/28/21 to match scalec
		scalec=np.sqrt((dof-2)/dof*varc)

		x = np.linspace(min(t.ppf(0.00001, dof, loc=muf, scale=scalef),t.ppf(0.00001, dof, loc=muc, scale=scalec)),max(t.ppf(0.9999, dof, loc=muf, scale=scalef),t.ppf(0.9999, dof, loc=muc, scale=scalec)), 100)

		style = dict(size=10, color='black')

		#cprob = special.erfc((x-muc)/scalec)
		cprob = exceedprob(thrs,dof,muc,scalec)
		fprob = exceedprob(thrs,dof,muf,scalef)
		cprobth = round(t.sf(thrs, dof, loc=muc, scale=scalec)*100,2)
		fprobth = round(t.sf(thrs, dof, loc=muf, scale=scalef)*100,2)
		cpdf=t.pdf(x, dof, loc=muc, scale=scalec)*100
		fpdf=t.pdf(x, dof, loc=muf, scale=scalef)*100
		oddsrc =(fprobth/cprobth)

		fig, ax = plt.subplots(1, 2,figsize=(12,4))
		#font = {'family' : 'Palatino',
		#        'size'   : 16}
		#plt.rc('font', **font)
		#plt.rc('text', usetex=True)
		#plt.rc('font', family='serif')

		plt.subplot(1, 2, 1)
		plt.plot(x, t.sf(x, dof, loc=muc, scale=scalec)*100,'b-', lw=5, alpha=0.6, label='clim')
		plt.plot(x, t.sf(x, dof, loc=muf, scale=scalef)*100,'r-', lw=5, alpha=0.6, label='fcst')
		plt.axvline(x=thrs, color='k', linestyle='--')
		plt.plot(thrs, fprobth,'ok')
		plt.plot(thrs, cprobth,'ok')
		plt.text(thrs+0.05, cprobth, str(cprobth)+'%', **style)
		plt.text(thrs+0.05, fprobth, str(fprobth)+'%', **style)
		#plt.text(0.1, 10, r'$\frac{P(fcst)}{P(clim)}=$'+str(round(oddsrc,1)), **style)
		plt.text(min(t.ppf(0.0001, dof, loc=muf, scale=scalef),t.ppf(0.0001, dof, loc=muc, scale=scalec)), -20, 'P(fcst)/P(clim)='+str(round(oddsrc,1)), **style)
		plt.legend(loc='best', frameon=False)
		# Add title and axis names
		plt.title('Probabilities of Exceedance')
		plt.xlabel('Rainfall')
		plt.ylabel('Probability (%)')
		# Limits for the Y axis
		plt.xlim(min(t.ppf(0.00001, dof, loc=muf, scale=scalef),t.ppf(0.00001, dof, loc=muc, scale=scalec)),max(t.ppf(0.9999, dof, loc=muf, scale=scalef),t.ppf(0.9999, dof, loc=muc, scale=scalec)))

		plt.subplot(1, 2, 2)
		plt.plot(x, cpdf,'b-', lw=5, alpha=0.6, label='clim')
		plt.plot(x, fpdf,'r-', lw=5, alpha=0.6, label='fcst')
		plt.axvline(x=thrs, color='k', linestyle='--')
		plt.legend(loc='best', frameon=False)
		# Add title and axis names
		plt.title('Probability Density Functions')
		plt.xlabel('Rainfall')
		plt.ylabel('')
		# Limits for the Y axis
		plt.xlim(min(t.ppf(0.00001, dof, loc=muf, scale=scalef),t.ppf(0.00001, dof, loc=muc, scale=scalec)),max(t.ppf(0.9999, dof, loc=muf, scale=scalef),t.ppf(0.9999, dof, loc=muc, scale=scalec)))


	plt.subplots_adjust(hspace=0)
	plt.subplots_adjust(bottom=0.15, top=0.9)
	#cax = plt.axes([0.2, 0.08, 0.6, 0.04])
	#cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
	#cbar.set_label(label) #, rotation=270)
	f.close()

def read_forecast_bin( fcst_type, model, predictor, predictand, mpref, mons, mon, fyr):
	if fcst_type == 'deterministic':
		f = open("./output/" + model + '_' + predictor + predictand +'_' + mpref + 'FCST_mu_' +mons + '_' +mon+str(fyr)+'.dat', 'rb')
	elif fcst_type == 'probabilistic':
		f = open("./output/" + model + '_' + predictor +predictand + '_' + mpref + 'FCST_P_' +mons + '_' +mon+str(fyr)+'.dat', 'rb')
	else:
		print('invalid fcst_type')
		return

	if fcst_type == 'deterministic':
		with  open("./output/" + model + '_' + predictor +predictand + '_' + mpref + 'FCST_mu_' +mons + '_' +mon+str(fyr)+'.ctl', 'r') as fp:
			for line in lines_that_contain("XDEF", fp): #lons
				W = int(line.split()[1])
				XD= float(line.split()[4])
				Wi = float(line.split()[3])
		with  open("./output/" + model + '_' + predictor + predictand + '_' + mpref + 'FCST_mu_' +mons + '_' +mon+str(fyr)+'.ctl', 'r') as fp:
			for line in lines_that_contain("YDEF", fp):  #lats
				H = int(line.split()[1])
				YD= float(line.split()[4])
				Hi = float(line.split()[3])


		garb = struct.unpack('s', f.read(1))[0]
		recl = struct.unpack('i', f.read(4))[0]
		numval=int(recl/np.dtype('float32').itemsize)
		A0 = np.fromfile(f, dtype='float32', count=numval)
		var = np.transpose(A0.reshape((W, H), order='F'))
		var[var==-999.]=np.nan #only sensible values
		recl = struct.unpack('i', f.read(4))[0]
		garb = struct.unpack('s', f.read(1))[0]
		lats, lons = np.linspace(Hi+H*YD, Hi, num=H+1), np.linspace(Wi, Wi+W*XD, num=W+1)
		return lats, lons, np.asarray([var])

	if fcst_type == 'probabilistic':
		with  open("./output/" + model + '_' + predictor + predictand +'_' + mpref + 'FCST_P_' +mons + '_' +mon+str(fyr)+'.ctl', 'r') as fp:
			for line in lines_that_contain("XDEF", fp): #lons
				W = int(line.split()[1])
				XD= float(line.split()[4])
				Wi = float(line.split()[3])
		with  open("./output/" + model + '_' + predictor +predictand + '_' + mpref + 'FCST_P_' +mons + '_' +mon+str(fyr)+'.ctl', 'r') as fp:
			for line in lines_that_contain("YDEF", fp):  #lats
				H = int(line.split()[1])
				YD= float(line.split()[4])
				Hi = float(line.split()[3])

		vars = []
		for ii in range(3):
			garb = struct.unpack('s', f.read(1))[0]
			recl = struct.unpack('i', f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			A0 = np.fromfile(f, dtype='float32', count=numval)
			var = np.transpose(A0.reshape((W, H), order='F'))
			var[var==-1.]=np.nan #only sensible values
			recl = struct.unpack('i', f.read(4))[0]
			garb = struct.unpack('s', f.read(1))[0]
			vars.append(var)
		lats, lons = np.linspace(Hi+H*YD, Hi, num=H+1), np.linspace(Wi, Wi+W*XD, num=W+1)
		return lats, lons, np.asarray(vars)


def read_forecast( fcst_type, model, predictor, predictand, mpref, mons, mon, fyr, filename='None', converting_tsv=False):
	if filename == 'None':
		if fcst_type == 'deterministic':
			try:
				f = open("./output/" + model + '_' + predictor + predictand + '_' + mpref + 'FCST_mu_' +mons + '_' +mon+str(fyr)+'.txt', 'r')
			except:
				f = open("./output/" + model + '_' + predictor + predictand + '_' + mpref + 'FCST_mu_' +mons + '_' +mon+str(fyr)+'.tsv', 'r')
		elif fcst_type == 'probabilistic':
			f = open("./output/" + model + '_' + predictor + predictand + '_' + mpref + 'FCST_P_' +mons + '_' +mon+str(fyr)+'.txt', 'r')
		else:
			print('invalid fcst_type')
			return
	else:
		f = open(filename,'r')

	lats, all_vals, vals, years = [], [], [], []
	past_header, flag = False, 0
	for line in f:
		if line[0:4] == 'cpt:':
			if converting_tsv:
				if not past_header:
					past_header=True
				else:
					years.append(int(line.split(',')[3][7:11]))
					if flag == 2:
						vals = np.asarray(vals, dtype=float)
						if fcst_type == 'deterministic':
							vals[vals == -999.0] = np.nan
						if fcst_type == 'probabilistic':
							vals[vals == -1.0] = np.nan
						all_vals.append(vals)
						lats = []
						vals = []
					flag = 1
			else:
				if flag == 2:
					vals = np.asarray(vals, dtype=float)
					if fcst_type == 'deterministic':
						vals[vals == -999.0] = np.nan
					if fcst_type == 'probabilistic':
						vals[vals == -1.0] = np.nan
					all_vals.append(vals)
					lats = []
					vals = []
				flag = 1
		elif flag == 1 and line[0:4] != 'cpt:':
			longs = line.strip().split('\t')
			longs = [float(i) for i in longs]
			flag = 2
		elif flag == 2:
			latvals = line.strip().split('\t')
			lats.append(float(latvals.pop(0)))
			vals.append(latvals)
	vals = np.asarray(vals, dtype=float)
	if fcst_type == 'deterministic':
		vals[vals == -999.0] = np.nan
	if fcst_type == 'probabilistic':
		vals[vals == -1.0] = np.nan
	all_vals.append(vals)
	all_vals = np.asarray(all_vals)
	return lats, longs, all_vals, years
	
		
def read_obs_CPTv10(filename='None'):
# Read_obs_CPTv10 by AWR 12/04/21, based on read_forecast	
# Read the obs Y file, like obs_T2M_Oct-Dec.tsv
#
# Obs format looks like this:
# xmlns:cpt=http://iri.columbia.edu/CPT/v10/
# xmlns:cf=http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/
# cpt:nfields=1
# cpt:T	1993-10/12	1994-10/12	1995-10/12	1996-10/12	1997-10/12	1998-10/12	1999-10/12	2000-10/12	2001-10/12	2002-10/12	2003-10/12	2004-10/12	2005-10/12	2006-10/12	2007-10/12	2008-10/12	2009-10/12	2010-10/12	2011-10/12	2012-10/12	2013-10/12	2014-10/12
# cpt:field=asum, cpt:T=1993-10/12, cpt:nrow=40, cpt:ncol=40, cpt:row=Y, cpt:col=X, cpt:units=Celsius_scale, cf:standard_name=air_temperature, cpt:missing=-999
# 	28.25	28.75	29.25	29.75	30.25	30.75	31.25	31.75	32.25	32.75	33.25	33.75	34.25	34.75	35.25	35.75	36.25	36.75	37.25	37.75	38.25	38.75	39.25	39.75	40.25	40.75	41.25	41.75	42.25	42.75	43.25	43.75	44.25	44.75	45.25	45.75	46.25	46.75	47.25	47.75
# 14.75	  26.3905 

# NG forecast file format is like this:
# xmlns:cpt=http://iri.columbia.edu/CPT/v10/
# cpt:nfields=1
# cpt:field=prec, cpt:L=2.5 months, cpt:S=1993-09-01T00:00, cpt:T=1993-10/12, cpt:nrow=40, cpt:ncol=40, cpt:row=Y, cpt:col=X, cpt:units=mm, cpt:missing=-999.
# 28.250000	28.750000	29.250000	29.750000	30.250000	30.750000	31.250000	31.750000	32.250000	32.750000	33.250000	33.750000	34.250000	34.750000	35.250000	35.750000	36.250000	36.750000	37.250000	37.750000	38.250000	38.750000	39.250000	39.750000	40.250000	40.750000	41.250000	41.750000	42.250000	42.750000	43.250000	43.750000	44.250000	44.750000	45.250000	45.750000	46.250000	46.750000	47.250000	47.750000	
# 14.750000	24.556513	24.775240	24.988209	25.483667	25.947063	26.459925	27.074654	27.609411	27.888069	27.804754	27.567066	27.507309	27.790388	27.981215	28.254971	28.652113	28.499275	27.331442	25.533611	24.334986	21.690851	18.429398	17.113058	21.132170	27.055462	27.127050	27.507532	-999.000000	-999.000000	29.114103	28.346760	21.213522	16.154606	16.918201	18.694923	20.737459	21.932928	21.952417	20.170586	20.903542	
# 14.250000
#
	if filename == 'None':
		print('invalid filename in read_obs_CPTv10')
		return
	else:
		f = open(filename,'r')

	lats, all_vals, vals, years = [], [], [], []
	past_header, flag = False, 0
	for line in f:
#		if line[0:6] == 'xmlns:':   # these lines get skipped because flag=0
		if line[0:4] == 'cpt:':
				if not past_header:  # Skip "cpt:nfields=1" line  ; we need to skip the "cpt:T" line too
					past_header=True
				elif line[0:5] == 'cpt:T':
					print('Skipping cpt:T line')
				else:
#					years.append(int(line.split(',')[3][7:11])) # previous version of this line: this reads cpt:T=1993-10/12
					years.append(int(line.split(',')[1][7:11]))
					if flag == 2:
						vals = np.asarray(vals, dtype=float)
						all_vals.append(vals)
						lats = []
						vals = []
					flag = 1

		elif flag == 1 and line[0:4] != 'cpt:':
			longs = line.strip().split('\t')
			longs = [float(i) for i in longs]
			flag = 2
		elif flag == 2:
			latvals = line.strip().split('\t')
			lats.append(float(latvals.pop(0)))
			vals.append(latvals)
	vals = np.asarray(vals, dtype=float)
	vals[vals == -999.0] = np.nan	# AWR

	all_vals.append(vals)
	all_vals = np.asarray(all_vals)
	return lats, longs, all_vals, years
	

# def readNetCDF_predictand(infile,outfile, predictand, wlo2, elo2, sla2, nla2, tar):
# 	"""Function to read the user's predictand NetCDF file and write to CPT format.
#
# 	PARAMETERS
#         ----------
# 		predictand: a DataArray with dimensions T,Y,X
# 	"""
#
# 	ds=xr.open_dataset(infile,decode_times=False)
# 	da=list(ds.coords)
#
# 	for i in range(len(da)):
# 		if da[i]=='X' or da[i]=='lon' or da[i]=='longitude':
# 			ds = ds.rename({da[i]:'X'})
# 		if da[i]=='Y' or da[i]=='lat' or da[i]=='latitude':
# 			ds = ds.rename({da[i]:'Y'})
# 		if da[i]=='T' or da[i]=='time':
# 			deltastyr=int(ds[da[i]][0]/12)
# 			ds = ds.rename({da[i]:'time'})
# 			nmon=ds.time.shape[0]
# 			nyr=int(nmon/12)
# 			if 'months since' in ds.time.units:
# 				line=ds.time.units
# 				stdate=str(int(line.split()[2][:4])+deltastyr)+line.split()[2][-6:]
# 				ds['time'] = pd.date_range(stdate, periods=ds.time.shape[0], freq='M')
#
# #	ds1=ds.sel(X=slice(wlo2,elo2),Y=slice(sla2,nla2))
# 	ds1_tmp=ds.sel(X=slice(wlo2,elo2),Y=slice(sla2,nla2))
# 	ds1=ds1_tmp.reindex(Y=ds1_tmp.Y[::-1]) #Y from N to S
# 	Xarr=ds1.X.values
# 	Yarr=ds1.Y.values
# 	W=ds1.X.shape[0]
# 	H=ds1.Y.shape[0]
# 	var1=ds1[predictand]
# 	units=ds[predictand].units
# 	Ti=int(ds.time.dt.year[0])
# 	vari = predictand
# 	varname = vari
# 	if 'True' in np.isnan(var):
# 	        var[np.isnan(var)]=-999. #use CPT missing value
#
# 	monthdic = {'Jan':'01','Feb':'02','Mar':'03','Apr':'04','May':'05','Jun':'06','Jul':'07','Aug':'08','Sep':'09','Oct':'10','Nov':'11','Dec':'12'}
# 	mi=monthdic[tar.split("-")[0]]
# 	mf=monthdic[tar.split("-")[1]]
#
# 	if mi==str(11):
# 		var1_N=var1[(var1.time.dt.month==11)]
# 		var1_N1=var1_N.groupby(var1_N.time.dt.year).mean('time').sel(year=slice(Ti,Ti+nyr-2))
# 		var1_D=var1[(var1.time.dt.month==12)]
# 		var1_D1=var1_D.groupby(var1_D.time.dt.year).mean('time').sel(year=slice(Ti,Ti+nyr-2))
# 		var1_J=var1[(var1.time.dt.month==1)]
# 		var1_J1=var1_J.groupby(var1_J.time.dt.year).mean('time').sel(year=slice(Ti+1,Ti+nyr-1))
# 		var=np.zeros(var1_D1.shape)
# 		for i in range(len(var1_D1.year)):
# 			var[i,:,:]=(var1_N1[i,:,:]+var1_D1[i,:,:]+var1_J1[i,:,:])/3.
# 	elif mi==str(12):
# 		var1_D=var1[(var1.time.dt.month==12)]
# 		var1_D1=var1_D.groupby(var1_D.time.dt.year).mean('time').sel(year=slice(Ti,Ti+nyr-2))
# 		var1_J=var1[(var1.time.dt.month==1)]
# 		var1_J1=var1_J.groupby(var1_J.time.dt.year).mean('time').sel(year=slice(Ti+1,Ti+nyr-1))
# 		var1_F = var1[(var1.time.dt.month==2)]
# 		var1_F1=var1_F.groupby(var1_F.time.dt.year).mean('time').sel(year=slice(Ti+1,Ti+nyr-1))
# 		var=np.zeros(var1_D1.shape)
# 		for i in range(len(var1_D1.year)):
# 			var[i,:,:]=(var1_D1[i,:,:]+var1_J1[i,:,:]+var1_F1[i,:,:])/3.
# 	else:
# 		var1_season = var1[(var1.time.dt.month>=mi)&(var1.time.dt.month<=mf)]
# 		var=var1_season.groupby(var1_season.time.dt.year).mean(dim=('time')).sel(year=slice(Ti+1,Ti+nyr-1))
# 	if tar=='Dec-Feb' or tar=='Nov-Jan':  #double check years are sync
# 		Ti=Ti
# 		xyear=True  #flag a cross-year season
# 	else:
# 		Ti=Ti+1
# 		xyear=False
#
# 	T=nyr-1
# 	Tarr = np.arange(Ti, Ti+T)
#
#     #Now write the CPT file
# 	outfile="usr_"+predictand+"_"+tar+".tsv"
# 	f = open(outfile, 'w')
# 	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
# 	f.write("cpt:nfields=1\n")
# 	for it in range(T):
# 		if xyear==True:
# 			f.write("cpt:field="+vari+", cpt:T="+str(Tarr[it])+"-"+mi+"/"+str(Tarr[it]+1)+"-"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
# 		else:
# 			f.write("cpt:field="+vari+", cpt:T="+str(Tarr[it])+"-"+mi+"/"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
# 		np.savetxt(f, Xarr[0:-1], fmt="%.6f",newline='\t')
# 		f.write("\n") #next line
# 		for iy in range(H):
# 			np.savetxt(f,np.r_[Yarr[iy],var[it,iy,0:]],fmt="%.6f", newline='\t')  #excise extra line
# 			f.write("\n") #next line
# 	f.close()
#
# def readNetCDF_Hindcasts(infile, outfile, wlo1, elo1, sla1, nla1, tgti, tgtf, mon, tar):
# 	"""Function to read the user's Hindcasts NetCDF file and write to CPT format.
#
# 	PARAMETERS
# 	----------
# 		Hindcats: a DataArray with dimensions S,M,L,Y,X
# 	"""
# 	ds=xr.open_dataset(infile,decode_times=False)
# 	da=list(ds.coords)
#
# 	for i in range(len(da)):
# 		if da[i]=='X' or da[i]=='lon' or da[i]=='longitude':
# 			ds = ds.rename({da[i]:'X'})
# 		if da[i]=='Y' or da[i]=='lat' or da[i]=='latitude':
# 			ds = ds.rename({da[i]:'Y'})
# 		if da[i]=='S':
# 			deltastyr=int(ds[da[i]][0]/12)
# 			nmon=ds.S.shape[0]
# 			nyr=int(nmon/12)
# 		if 'months since' in ds.S.units:
# 				line=ds.S.units
# 				stdate=str(int(line.split()[2][:4])+deltastyr)+line.split()[2][-6:]
# 				ds['S'] = pd.date_range(stdate, periods=ds.S.shape[0], freq='M')
#
# 	ds1=ds.sel(X=slice(wlo1,elo1),Y=slice(sla1,nla1),L=slice(float(tgti),float(tgtf))).mean(dim='L',skipna=True)
# 	ds2=ds1.mean(dim='M',skipna=True)
# 	Xarr=ds2.X.values
# 	Yarr=ds2.Y.values
# 	W=ds2.X.shape[0]
# 	H=ds2.Y.shape[0]
# 	a=list(ds)
#
# 	var1=ds2[a[0]]
# 	units=ds[a[0]].units
# 	Ti=1982
#
# 	vari = a[0]
# 	varname = vari
# 	L=0.5*(float(tgtf)+float(tgti))
#
# 	monthdic = {'Jan':'01','Feb':'02','Mar':'03','Apr':'04','May':'05','Jun':'06','Jul':'07','Aug':'08','Sep':'09','Oct':'10','Nov':'11','Dec':'12'}
# 	S1=monthdic[mon]
# 	mi=monthdic[tar.split("-")[0]]
# 	mf=monthdic[tar.split("-")[1]]
#
# 	var1_stmon=var1[(var1.S.dt.month==int(monthdic[mon]))]
# 	var=var1_stmon.groupby(var1_stmon.S.dt.year).mean(dim=('S')).sel(year=slice(1982,2009))
# 	var_N2S=var.reindex(Y=var.Y[::-1]) #Y from N to S
# 	Yarr=var_N2S.Y.values
# 	if tar=='Dec-Feb' or tar=='Nov-Jan':  #double check years are sync
# 		xyear=True  #flag a cross-year season
# 	else:
# 		xyear=False
# 	T=2009-1982+1
# 	Tarr = np.arange(Ti, Ti+T)
#
# 	if 'True' in np.isnan(var):
# 		var[np.isnan(var)]=-999. #use CPT missing value
#         #Now write the CPT file
# 	outfile="usr_"+a[0]+"_"+tar+"_ini"+mon+".tsv"
# 	f = open(outfile, 'w')
# 	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
# 	f.write("cpt:nfields=1\n")
#
# 	for it in range(T):
# 		if xyear==True:
# 			f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Tarr[it])+"-"+S1+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+str(Tarr[it]+1)+"-"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
# 		else:
# 			f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Tarr[it])+"-"+S1+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
# 	np.savetxt(f, Xarr, fmt="%.6f",newline='\t')
# 	f.write("\n") #next line
# 	for iy in range(H):
# 		np.savetxt(f,np.r_[Yarr[iy],var_N2S[it,iy,0:]],fmt="%.6f", newline='\t')  #excise extra line
# 		f.write("\n") #next line
# 	f.close()
#
# def readNetCDF_Forecast(infile, outfile, monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1):
#         """Function to read the user's forecast NetCDF file and write to CPT format.
#
#         PARAMETERS
#         ----------
#                 Forecat: a DataArray with dimensions S,M,L,Y,X
#         """
#         ds=xr.open_dataset(infile,decode_times=False)
#         da=list(ds.coords)
#
#         for i in range(len(da)):
#                 if da[i]=='X' or da[i]=='lon' or da[i]=='longitude':
#                         ds = ds.rename({da[i]:'X'})
#                 if da[i]=='Y' or da[i]=='lat' or da[i]=='latitude':
#                         ds = ds.rename({da[i]:'Y'})
#                 if da[i]=='S':
#                         deltastyr=int(ds[da[i]][0]/12)
#                         nmon=ds.S.shape[0]
#                         nyr=int(nmon/12)
#                         if 'months since' in ds.S.units:
#                                 line=ds.S.units
#                                 stdate=str(int(line.split()[2][:4])+deltastyr)+line.split()[2][-6:]
#                                 ds['S'] = pd.date_range(stdate, periods=ds.S.shape[0], freq='M')
#
#         ds1=ds.sel(X=slice(wlo1,elo1),Y=slice(sla1,nla1),L=slice(float(tgti),float(tgtf))).mean(dim='L',skipna=True)
#         ds2=ds1.mean(dim='M',skipna=True)
#         Xarr=ds2.X.values
#         Yarr=ds2.Y.values
#         W=ds2.X.shape[0]
#         H=ds2.Y.shape[0]
#         a=list(ds)
#
#         var1=ds2[a[0]]
#         units=ds[a[0]].units
#         Ti=fyr
#
#         vari = a[0]
#         varname = vari
#         L=0.5*(float(tgtf)+float(tgti))
#
#         monthdic = {'Jan':'01','Feb':'02','Mar':'03','Apr':'04','May':'05','Jun':'06','Jul':'07','Aug':'08','Sep':'09','Oct':'10','Nov':'11','Dec':'12'}
#         S1=monthdic[monf]
#         mi=monthdic[tar.split("-")[0]]
#         mf=monthdic[tar.split("-")[1]]
#
#         var1_stmon=var1[(var1.S.dt.month==int(monthdic[monf]))]
#         var=var1_stmon.groupby(var1_stmon.S.dt.year).mean(dim=('S')).sel(year=fyr)
#         var_N2S=var.reindex(Y=var.Y[::-1])
#         Yarr=var_N2S.Y.values
#         if tar=='Dec-Feb' or tar=='Nov-Jan':  #double check years are sync
#                 xyear=True  #flag a cross-year season
#         else:
#                 xyear=False
#         T=1
#         Tarr = np.arange(Ti, Ti+T)
#
#         if 'True' in np.isnan(var):
#                 var[np.isnan(var)]=-999. #use CPT missing value
#         #Now write the CPT file
#         outfile="usr_fcst_"+a[0]+"_"+tar+"_ini"+monf+str(fyr)+".tsv"
#         f = open(outfile, 'w')
#         f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
#         f.write("cpt:nfields=1\n")
#
#         for it in range(T):
#                 if xyear==True:
#                         f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Tarr[it])+"-"+S1+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+str(Tarr[it]+1)+"-"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
#                 else:
#                         f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Tarr[it])+"-"+S1+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
#         np.savetxt(f, Xarr, fmt="%.6f",newline='\t')
#         f.write("\n") #next line
#         for iy in range(H):
#                 np.savetxt(f,np.r_[Yarr[iy],var_N2S[iy,0:]],fmt="%.6f", newline='\t')  #excise extra line
#                 f.write("\n") #next line
#         f.close()

def GetHindcasts(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"_PRCP_"+tar+"_ini"+mon+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {	'CanSIPSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanSIPSv2/.HINDCAST/.MONTHLY/.prec/SOURCES/.Models/.NMME/.CanSIPSv2/.FORECAST/.MONTHLY/.prec/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC1-CanCM3': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC1-CanCM3/.HINDCAST/.MONTHLY/.prec/SOURCES/.Models/.NMME/.CMC1-CanCM3/.FORECAST/.MONTHLY/.prec/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC2-CanCM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC2-CanCM4/.HINDCAST/.MONTHLY/.prec/SOURCES/.Models/.NMME/.CMC2-CanCM4/.FORECAST/.MONTHLY/.prec/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'COLA-RSMAS-CCSM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY/.prec/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'GFDL-CM2p5-FLOR-A06': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-A06/.MONTHLY/.prec/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'GFDL-CM2p5-FLOR-B01': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-B01/.MONTHLY/.prec/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'NASA-GEOSS2S': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GEOSS2S/.HINDCAST/.MONTHLY/.prec/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'NCEP-CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NCEP-CFSv2/.HINDCAST/.PENTAD_SAMPLES/.MONTHLY/.prec/SOURCES/.Models/.NMME/.NCEP-CFSv2/.FORECAST/.PENTAD_SAMPLES/.MONTHLY/.prec/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/M/%281%29%2824%29RANGE/%5BM%5D/average/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#			}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_hcst_PRCP'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"_PRCP_"+tar+"_ini"+mon+".tsv")

def GetHindcasts_elr(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea_elr):
	if not force_download:
		try:
			ff=open(model+"_PRCP_"+tar+"_ini"+mon+".nc", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea_elr[model+'_hcst_PRCP'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"_PRCP_"+tar+"_ini"+mon+".nc")


def GetHindcasts_T2M(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"_T2M_"+tar+"_ini"+mon+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {	'CanSIPSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanSIPSv2/.HINDCAST/.MONTHLY/.tref/SOURCES/.Models/.NMME/.CanSIPSv2/.FORECAST/.MONTHLY/.tref/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC1-CanCM3': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC1-CanCM3/.HINDCAST/.MONTHLY/.tref/SOURCES/.Models/.NMME/.CMC1-CanCM3/.FORECAST/.MONTHLY/.tref/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC2-CanCM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC2-CanCM4/.HINDCAST/.MONTHLY/.tref/SOURCES/.Models/.NMME/.CMC2-CanCM4/.FORECAST/.MONTHLY/.tref/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#			'COLA-RSMAS-CCSM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY/.tref/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'GFDL-CM2p5-FLOR-A06': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-A06/.MONTHLY/.tref/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'GFDL-CM2p5-FLOR-B01': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-B01/.MONTHLY/.tref/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'NASA-GEOSS2S': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GEOSS2S/.HINDCAST/.MONTHLY/.tref/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'NCEP-CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NCEP-CFSv2/.HINDCAST/.PENTAD_SAMPLES/.MONTHLY/.tref/SOURCES/.Models/.NMME/.NCEP-CFSv2/.FORECAST/.PENTAD_SAMPLES/.MONTHLY/.tref/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/M/%281%29%2824%29RANGE/%5BM%5D/average/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#			}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_hcst_T2M'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"_T2M_"+tar+"_ini"+mon+".tsv")

def GetHindcasts_TMAX(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"_TMAX_"+tar+"_ini"+mon+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
#		dic = { 'CanSIPSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanSIPSv2/.HINDCAST/.MONTHLY/.tmax/SOURCES/.Models/.NMME/.CanSIPSv2/.FORECAST/.MONTHLY/.tmax/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'CMC1-CanCM3': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC1-CanCM3/.HINDCAST/.MONTHLY/.tmax/SOURCES/.Models/.NMME/.CMC1-CanCM3/.FORECAST/.MONTHLY/.tmax/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'CMC2-CanCM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC2-CanCM4/.HINDCAST/.MONTHLY/.tmax/SOURCES/.Models/.NMME/.CMC2-CanCM4/.FORECAST/.MONTHLY/.tmax/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'COLA-RSMAS-CCSM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY/.tmax/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'GFDL-CM2p5-FLOR-A06': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-A06/.MONTHLY/.tmax/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'GFDL-CM2p5-FLOR-B01': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-B01/.MONTHLY/.tmax/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'NASA-GEOSS2S': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GEOSS2S/.HINDCAST/.MONTHLY/.tmax/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'NCEP-CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NCEP-CFSv2/.HINDCAST/.PENTAD_SAMPLES/.MONTHLY/.tmax/SOURCES/.Models/.NMME/.NCEP-CFSv2/.FORECAST/.PENTAD_SAMPLES/.MONTHLY/.tmax/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/M/%281%29%2824%29RANGE/%5BM%5D/average/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                }
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_hcst_TMAX'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"_TMAX_"+tar+"_ini"+mon+".tsv")

def GetHindcasts_TMIN(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"_TMIN_"+tar+"_ini"+mon+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
#		dic = { 'CanSIPSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanSIPSv2/.HINDCAST/.MONTHLY/.tmin/SOURCES/.Models/.NMME/.CanSIPSv2/.FORECAST/.MONTHLY/.tmin/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC1-CanCM3': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC1-CanCM3/.HINDCAST/.MONTHLY/.tmin/SOURCES/.Models/.NMME/.CMC1-CanCM3/.FORECAST/.MONTHLY/.tmin/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'CMC2-CanCM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC2-CanCM4/.HINDCAST/.MONTHLY/.tmin/SOURCES/.Models/.NMME/.CMC2-CanCM4/.FORECAST/.MONTHLY/.tmin/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'COLA-RSMAS-CCSM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY/.tmin/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'GFDL-CM2p5-FLOR-A06': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-A06/.MONTHLY/.tmin/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'GFDL-CM2p5-FLOR-B01': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-B01/.MONTHLY/.tmin/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'NASA-GEOSS2S': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GEOSS2S/.HINDCAST/.MONTHLY/.tmin/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'NCEP-CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NCEP-CFSv2/.HINDCAST/.PENTAD_SAMPLES/.MONTHLY/.tmin/SOURCES/.Models/.NMME/.NCEP-CFSv2/.FORECAST/.PENTAD_SAMPLES/.MONTHLY/.tmin/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/M/%281%29%2824%29RANGE/%5BM%5D/average/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                        }
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_hcst_TMIN'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"_TMIN_"+tar+"_ini"+mon+".tsv")

def GetHindcasts_RFREQ(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, wetday_threshold, tar, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"_RFREQ_"+tar+"_ini"+mon+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {	'CanSIPSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanSIPSv2/.HINDCAST/.MONTHLY/.prec/SOURCES/.Models/.NMME/.CanSIPSv2/.FORECAST/.MONTHLY/.prec/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC1-CanCM3': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC1-CanCM3/.HINDCAST/.MONTHLY/.prec/SOURCES/.Models/.NMME/.CMC1-CanCM3/.FORECAST/.MONTHLY/.prec/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC2-CanCM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC2-CanCM4/.HINDCAST/.MONTHLY/.prec/SOURCES/.Models/.NMME/.CMC2-CanCM4/.FORECAST/.MONTHLY/.prec/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'COLA-RSMAS-CCSM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY/.prec/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'GFDL-CM2p5-FLOR-A06': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-A06/.MONTHLY/.hght/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'GFDL-CM2p5-FLOR-B01': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-B01/.MONTHLY/.prec/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'NASA-GEOSS2S': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GEOSS2S/.HINDCAST/.MONTHLY/.prec/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'NCEP-CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NCEP-CFSv2/.HINDCAST/.PENTAD_SAMPLES/.MONTHLY/.prec/SOURCES/.Models/.NMME/.NCEP-CFSv2/.FORECAST/.PENTAD_SAMPLES/.MONTHLY/.prec/appendstream/S/%280000%201%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/M/%281%29%2824%29RANGE/%5BM%5D/average/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_hcst_RFREQ'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"_RFREQ_"+tar+"_ini"+mon+".tsv")

def GetHindcasts_UQ(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea, pressure):
	if not force_download:
		try:
			ff=open(model+"_UQ_"+tar+"_ini"+mon+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {'NCEP-CFSv2': 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.PGBF/.pressure_level/.VGRD/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.PGBF/.pressure_level/.SPFH/mul/P/850/VALUE/S/%2812%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_hcst_UQ'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"_UQ_"+tar+"_ini"+mon+".tsv")


def GetHindcasts_VQ(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea, pressure):
	if not force_download:
		try:
			ff=open(model+"_VQ_"+tar+"_ini"+mon+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {'NCEP-CFSv2': 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.PGBF/.pressure_level/.VGRD/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.PGBF/.pressure_level/.SPFH/mul/P/850/VALUE/S/%281%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
		# url=dic[model]
		url=eval(dic_sea[model+'_hcst_VQ'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"_VQ_"+tar+"_ini"+mon+".tsv")

def GetHindcasts_UA(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea, pressure):
	if not force_download:
		try:
			ff=open(model+"_UA_"+tar+"_ini"+mon+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
#dictionary:
#		dic = {'NCEP-CFSv2': 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.PGBF/.pressure_level/.VGRD/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.PGBF/.pressure_level/.SPFH/mul/P/850/VALUE/S/%2812%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_hcst_UA'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"_UA_"+tar+"_ini"+mon+".tsv")


def GetHindcasts_VA(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea, pressure):
	if not force_download:
		try:
			ff=open(model+"_VA_"+tar+"_ini"+mon+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {'NCEP-CFSv2': 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.PGBF/.pressure_level/.VGRD/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.PGBF/.pressure_level/.SPFH/mul/P/850/VALUE/S/%281%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_hcst_VA'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"_VA_"+tar+"_ini"+mon+".tsv")

def GetHindcasts_SST(tini,tend,wlo1, elo1, sla1, nla1, tgti, tgtf, mon, os, tar, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"_SST_"+tar+"_ini"+mon+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {'NCEP-CFSv2': 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.PGBF/.pressure_level/.VGRD/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.PGBF/.pressure_level/.SPFH/mul/P/850/VALUE/S/%281%20'+mon+'%20'+str(tini)+'-'+str(tend)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_hcst_SST'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"_SST_"+tar+"_ini"+mon+".tsv")

def GetObs(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea):
	if not force_download:
		try:
			ff=open("obs_"+predictand+"_"+tar+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs precip file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		if obs_source=='home/.xchourio/.ACToday/.CHL/.prcp':
#			url='http://iridl.ldeo.columbia.edu/'+obs_source+'/T/%28'+tar+'%29/seasonalAverage/-999/setmissing_value/%5B%5D%5BT%5Dcptv10.tsv'
			url=eval(dic_sea[obs+'_obs_'+predictand])
		elif obs_source=='ANACAFE':
			url='http://iridl.ldeo.columbia.edu/IRIONLY/home/.xchourio/.ACToday/.COFFEE/.GUATEMALA/.ANUAL/.1989_2015/.Index_C/T/%28'+tar+'%29/seasonalAverage/-999/setmissing_value/%5B%5D%5BT%5Dcptv10.tsv'
		else:
#			url='https://iridl.ldeo.columbia.edu/'+obs_source+'/T/(1%20Jan%20'+str(tini)+')/(31%20Dec%20'+str(tend)+')/RANGE/T/%28'+tar+'%20'+str(tini)+'-'+str(tend)+'%29/seasonalAverage/Y/%28'+str(sla2)+'%29/%28'+str(nla2)+'%29/RANGEEDGES/X/%28'+str(wlo2)+'%29/%28'+str(elo2)+'%29/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv'
			url=eval(dic_sea[obs+'_obs_'+predictand])

		print("\n Obs (Rainfall) data URL: \n\n "+url)
		get_ipython().system("curl -k '"+url+"' > obs_"+predictand+"_"+tar+".tsv")
		if station==True:   #weirdly enough, Ingrid sends the file with nfields=0. This is my solution for now. AGM
			replaceAll("obs_"+predictand+"_"+tar+".tsv","cpt:nfields=0","cpt:nfields=1")

def GetObs_hc_elr(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea_elr):
	if not force_download:
		try:
			ff=open("obs_"+predictand+"_"+tar+"_hc.nc", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs precip file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		url=eval(dic_sea_elr[obs+'_obs_hc_'+predictand])
		print("\n Obs (Rainfall) data URL: \n\n "+url)
		get_ipython().system("curl -k '"+url+"' > obs_"+predictand+"_"+tar+"_hc.nc")


def GetObs_T2M(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea):
	if not force_download:
		try:
			ff=open("obs_T2M"+"_"+tar+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs precip file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
#		obs_source = 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.temperature/.daily/.tmin'
#		url='https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.temperature/.daily/tmax/tmin/add/2/div/T/(1%20Jan%20'+str(tini)+')/(31%20Dec%20'+str(tend)+')/RANGE/T/%28'+tar+'%20'+str(tini)+'-'+str(tend)+'%29/seasonalAverage/Y/%28'+str(sla2)+'%29/%28'+str(nla2)+'%29/RANGEEDGES/X/%28'+str(wlo2)+'%29/%28'+str(elo2)+'%29/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv'
		url=eval(dic_sea[obs+'obs_T2M'])
		print("\n Obs (Tmin) data URL: \n\n "+url)
		get_ipython().system("curl -k '"+url+"' > obs_T2M"+"_"+tar+".tsv")

def GetObs_TMAX(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea):
	if not force_download:
		try:
			ff=open("obs_TMAX"+"_"+tar+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs precip file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
#		obs_source = 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.temperature/.daily/.tmax'
#		url='https://iridl.ldeo.columbia.edu/'+obs_source+'/T/(1%20Jan%20'+str(tini)+')/(31%20Dec%20'+str(tend)+')/RANGE/T/%28'+tar+'%20'+str(tini)+'-'+str(tend)+'%29/seasonalAverage/Y/%28'+str(sla2)+'%29/%28'+str(nla2)+'%29/RANGEEDGES/X/%28'+str(wlo2)+'%29/%28'+str(elo2)+'%29/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv'
		url=eval(dic_sea[obs+'obs_TMAX'])
		print("\n Obs (Tmax) data URL: \n\n "+url)
		get_ipython().system("curl -k '"+url+"' > obs_TMAX"+"_"+tar+".tsv")

def GetObs_TMIN(predictand, tini,tend,wlo2, elo2, sla2, nla2, tar, obs, obs_source, hdate_last, force_download,station, dic_sea):
	if not force_download:
		try:
			ff=open("obs_TMIN"+"_"+tar+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs precip file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
#		obs_source = 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.temperature/.daily/.tmin'
#		url='https://iridl.ldeo.columbia.edu/'+obs_source+'/T/(1%20Jan%20'+str(tini)+')/(31%20Dec%20'+str(tend)+')/RANGE/T/%28'+tar+'%20'+str(tini)+'-'+str(tend)+'%29/seasonalAverage/Y/%28'+str(sla2)+'%29/%28'+str(nla2)+'%29/RANGEEDGES/X/%28'+str(wlo2)+'%29/%28'+str(elo2)+'%29/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv'
		url=eval(dic_sea[obs+'obs_TMIN'])
		print("\n Obs (Tmin) data URL: \n\n "+url)
		get_ipython().system("curl -k '"+url+"' > obs_TMIN"+"_"+tar+".tsv")

def GetObs_RFREQ(predictand, tini,tend,wlo2, elo2, sla2, nla2, wetday_threshold, threshold_pctle, tar, obs_source, hdate_last, force_download,station, dic_sea):
	if not force_download:
		try:
			ff=open("obs_"+predictand+"_"+tar+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs freq-rainfall file doesn't exist --SOLVING: downloading file")
			force_download = True
	if force_download:
		#Need to work on it
		if threshold_pctle:
				url='https://iridl.ldeo.columbia.edu/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/(1%20Jan%20'+str(tini)+')/(31%20Dec%20'+str(end)+')/RANGEEDGES/%5BT%5Dpercentileover/'+str(wetday_threshold)+'/flagle/T/'+str(nday)+'/runningAverage/'+str(nday)+'/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz'
		else:
			print("Obs freq-rainfall file doesn't exist --SOLVING: downloading file")
			force_download = True
	if force_download:
		#Need to work on it
		if threshold_pctle:
				url='https://iridl.ldeo.columbia.edu/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/(1%20Jan%20'+str(tini)+')/(31%20Dec%20'+str(end)+')/RANGEEDGES/%5BT%5Dpercentileover/'+str(wetday_threshold)+'/flagle/T/'+str(nday)+'/runningAverage/'+str(nday)+'/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz'

		else:
				#url='https://iridl.ldeo.columbia.edu/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/dup/pentadmean/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/I/3/-1/roll/.T/replaceGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz'
#				url='http://datoteca.ole2.org/SOURCES/.UEA/.CRU/.TS4p0/.monthly/.wet/lon/%28X%29/renameGRID/lat/%28Y%29/renameGRID/time/%28T%29/renameGRID/T/(1%20Jan%20'+str(tini)+')/(31%20Dec%20'+str(end)+')/RANGE/T/%28'+tar+'%29/seasonalAverage/Y/'+str(sla2)+'/'+str(nla2)+'/RANGEEDGES/X/'+str(wlo2)+'/'+str(elo2)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv'
				url=eval(dic_sea['CRU_obs_'+predictand])

		print("\n Obs (Freq) data URL: \n\n "+url)
		get_ipython().system("curl -k '"+url+"' > obs_"+predictand+"_"+tar+".tsv")

def GetForecast(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"fcst_PRCP_"+tar+"_ini"+monf+str(fyr)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {	'CanSIPSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanSIPSv2/.FORECAST/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC1-CanCM3': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC1-CanCM3/.FORECAST/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#			    'CMC2-CanCM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC2-CanCM4/.FORECAST/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'COLA-RSMAS-CCSM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'GFDL-CM2p5-FLOR-A06': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-A06/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'GFDL-CM2p5-FLOR-B01': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-B01/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'NASA-GEOSS2S': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GEOSS2S/.FORECAST/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'NCEP-CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NCEP-CFSv2/.FORECAST/.PENTAD_SAMPLES/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
#		url=dic[model]
		url=eval(dic_sea[model+'_fcst_PRCP'])
		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"fcst_PRCP_"+tar+"_ini"+monf+str(fyr)+".tsv")
def GetForecast_T2M(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"fcst_T2M_"+tar+"_ini"+monf+str(fyr)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'CanSIPSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanSIPSv2/.FORECAST/.MONTHLY/.tref/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC1-CanCM3': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC1-CanCM3/.FORECAST/.MONTHLY/.tref/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'CMC2-CanCM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC2-CanCM4/.FORECAST/.MONTHLY/.tref/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'COLA-RSMAS-CCSM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY/.tref/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'GFDL-CM2p5-FLOR-A06': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-A06/.MONTHLY/.tref/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'GFDL-CM2p5-FLOR-B01': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-B01/.MONTHLY/.tref/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'NASA-GEOSS2S': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GEOSS2S/.FORECAST/.MONTHLY/.tref/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'NCEP-CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NCEP-CFSv2/.FORECAST/.PENTAD_SAMPLES/.MONTHLY/.tref/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                }
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_fcst_T2M'])
		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"fcst_T2M_"+tar+"_ini"+monf+str(fyr)+".tsv")
def GetForecast_TMAX(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"fcst_TMAX_"+tar+"_ini"+monf+str(fyr)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'CanSIPSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanSIPSv2/.FORECAST/.MONTHLY/.tmax/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'CMC1-CanCM3': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC1-CanCM3/.FORECAST/.MONTHLY/.tmax/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'CMC2-CanCM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC2-CanCM4/.FORECAST/.MONTHLY/.tmax/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'COLA-RSMAS-CCSM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY/.tmax/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'GFDL-CM2p5-FLOR-A06': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-A06/.MONTHLY/.tmax/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'GFDL-CM2p5-FLOR-B01': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-B01/.MONTHLY/.tmax/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'NASA-GEOSS2S': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GEOSS2S/.FORECAST/.MONTHLY/.tmax/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'NCEP-CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NCEP-CFSv2/.FORECAST/.PENTAD_SAMPLES/.MONTHLY/.tmax/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                }
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_fcst_TMAX'])
		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"fcst_TMAX_"+tar+"_ini"+monf+str(fyr)+".tsv")

def GetForecast_TMIN(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"fcst_TMIN_"+tar+"_ini"+monf+str(fyr)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'CanSIPSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanSIPSv2/.FORECAST/.MONTHLY/.tmin/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC1-CanCM3': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC1-CanCM3/.FORECAST/.MONTHLY/.tmin/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC2-CanCM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC2-CanCM4/.FORECAST/.MONTHLY/.tmin/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'COLA-RSMAS-CCSM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY/.tmin/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'GFDL-CM2p5-FLOR-A06': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-A06/.MONTHLY/.tmin/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'GFDL-CM2p5-FLOR-B01': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-B01/.MONTHLY/.tmin/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'NASA-GEOSS2S': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GEOSS2S/.FORECAST/.MONTHLY/.tmin/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                                'NCEP-CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NCEP-CFSv2/.FORECAST/.PENTAD_SAMPLES/.MONTHLY/.tmin/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#                }
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_sea[model+'_fcst_TMIN'])
		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"fcst_TMIN_"+tar+"_ini"+monf+str(fyr)+".tsv")

def GetForecast_UQ(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea, pressure):
	if not force_download:
		try:
			ff=open(model+"fcst_UQ_"+tar+"_ini"+monf+str(fyr)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {'NCEP-CFSv2': 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.PGBF/.pressure_level/.VGRD/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.PGBF/.pressure_level/.SPFH/mul/P/850/VALUE/S/%281%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
#		url=dic[model]
		url=eval(dic_sea[model+'_fcst_UQ'])
		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"fcst_UQ_"+tar+"_ini"+monf+str(fyr)+".tsv")

def GetForecast_VQ(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea, pressure):
	if not force_download:
		try:
			ff=open(model+"fcst_VQ_"+tar+"_ini"+monf+str(fyr)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {'NCEP-CFSv2': 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.PGBF/.pressure_level/.VGRD/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.PGBF/.pressure_level/.SPFH/mul/P/850/VALUE/S/%281%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
#		url=dic[model]
		url=eval(dic_sea[model+'_fcst_VQ'])

		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"fcst_VQ_"+tar+"_ini"+monf+str(fyr)+".tsv")

def GetForecast_UA(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea, pressure):
	if not force_download:
		try:
			ff=open(model+"fcst_UA_"+tar+"_ini"+monf+str(fyr)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {'NCEP-CFSv2': 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.PGBF/.pressure_level/.VGRD/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.PGBF/.pressure_level/.SPFH/mul/P/850/VALUE/S/%281%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
#		url=dic[model]
		url=eval(dic_sea[model+'_fcst_UA'])
		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"fcst_UA_"+tar+"_ini"+monf+str(fyr)+".tsv")

def GetForecast_VA(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea, pressure):
	if not force_download:
		try:
			ff=open(model+"fcst_VA_"+tar+"_ini"+monf+str(fyr)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {'NCEP-CFSv2': 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.PGBF/.pressure_level/.VGRD/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.PGBF/.pressure_level/.SPFH/mul/P/850/VALUE/S/%281%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
#		url=dic[model]
		url=eval(dic_sea[model+'_fcst_VA'])

		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"fcst_VA_"+tar+"_ini"+monf+str(fyr)+".tsv")

def GetForecast_SST(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"fcst_SST_"+tar+"_ini"+monf+str(fyr)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {'NCEP-CFSv2': 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.PGBF/.pressure_level/.VGRD/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.PGBF/.pressure_level/.SPFH/mul/P/850/VALUE/S/%281%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
#		url=dic[model]
		url=eval(dic_sea[model+'_fcst_SST'])

		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"fcst_SST_"+tar+"_ini"+monf+str(fyr)+".tsv")

def GetForecast_RFREQ(monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1, wetday_threshold, model, force_download, dic_sea):
	if not force_download:
		try:
			ff=open(model+"fcst_RFREQ_"+tar+"_ini"+monf+str(fyr)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:  #CFSv2 needs to be transformed to RFREQ!
#		dic = {	'CanSIPSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanSIPSv2/.FORECAST/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'CMC1-CanCM3': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC1-CanCM3/.FORECAST/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#			    'CMC2-CanCM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CMC2-CanCM4/.FORECAST/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'COLA-RSMAS-CCSM4': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'GFDL-CM2p5-FLOR-A06': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-A06/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'GFDL-CM2p5-FLOR-B01': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-CM2p5-FLOR-B01/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'NASA-GEOSS2S': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GEOSS2S/.FORECAST/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#				'NCEP-CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NCEP-CFSv2/.FORECAST/.PENTAD_SAMPLES/.MONTHLY/.prec/S/%280000%201%20'+monf+'%20'+str(fyr)+'%29/VALUES/L/'+tgti+'/'+tgtf+'/RANGEEDGES/%5BL%5D//keepgrids/average/%5BM%5D/average/Y/'+str(sla1)+'/'+str(nla1)+'/RANGEEDGES/X/'+str(wlo1)+'/'+str(elo1)+'/RANGEEDGES/30/mul/-999/setmissing_value/%5BX/Y%5D%5BL/S/add%5D/cptv10.tsv',
#		}
		# calls curl to download data
#		url=dic[model]
		url=eval(dic_sea[model+'_fcst_RFREQ'])
		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -k "+url+" > "+model+"fcst_RFREQ_"+tar+"_ini"+monf+str(fyr)+".tsv")

def CPTscript(model,predictand, mon,monf,fyr,tini,tend,nla1,sla1,wlo1,elo1,nla2,sla2,wlo2,elo2,fprefix,mpref,tar,ntrain,MOS,station, xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min, ccamodes_max, forecast_anomaly, forecast_spi):
		"""Function to write CPT namelist file

		"""
		# Set up CPT parameter file
		f=open("params","w")
		if MOS=='CCA':
			# Opens CCA
			f.write("611\n")
		elif MOS=='PCR':
			# Opens PCR
			f.write("612\n")
		elif MOS=='ELR':
			# Opens GCM (no calibration performed in CPT)
			f.write("614\n")
		elif MOS=='None':
			# Opens GCM (no calibration performed in CPT)
			f.write("614\n")
		else:
			print ("MOS option is invalid")

		# First, ask CPT to stop if error is encountered
		f.write("571\n")
		f.write("3\n")

		# Opens X input file
		f.write("1\n")
		file='../input/'+model+'_'+fprefix+'_'+tar+'_ini'+mon+'.tsv\n'
		f.write(file)
		# Nothernmost latitude
		f.write(str(nla1)+'\n')
		# Southernmost latitude
		f.write(str(sla1)+'\n')
		# Westernmost longitude
		f.write(str(wlo1)+'\n')
		# Easternmost longitude
		f.write(str(elo1)+'\n')
		if MOS=='CCA' or MOS=='PCR':
			# Minimum number of X modes
			f.write("{}\n".format(xmodes_min))
			# Maximum number of X modes
			f.write("{}\n".format(xmodes_max))

			# Opens forecast (X) file
			f.write("3\n")
			file='../input/'+model+'fcst_'+fprefix+'_'+tar+'_ini'+monf+str(fyr)+'.tsv\n'
			f.write(file)
			#Start forecast:
			f.write("223\n")
			if monf=="Dec":  #for multi-seasons, we need to add a better approach here AGMS
				f.write(str(fyr+1)+"\n")
			else:
				f.write(str(fyr)+"\n")

		# Opens Y input file
		f.write("2\n")
		file='../input/obs_'+predictand+'_'+tar+'.tsv\n'
		f.write(file)
		if station==False:
			# Nothernmost latitude
			f.write(str(nla2)+'\n')
			# Southernmost latitude
			f.write(str(sla2)+'\n')
			# Westernmost longitude
			f.write(str(wlo2)+'\n')
			# Easternmost longitude
			f.write(str(elo2)+'\n')
		if MOS=='CCA':
			# Minimum number of Y modes
			f.write("{}\n".format(ymodes_min))
			# Maximum number of Y modes
			f.write("{}\n".format(ymodes_max))

			# Minimum number of CCA modes
			f.write("{}\n".format(ccamodes_min))
			# Maximum number of CCAmodes
			f.write("{}\n".format(ccamodes_max))

		# X training period
		f.write("4\n")
		# First year of X training period
		if monf=="Dec":
			f.write(str(tini+1)+'\n')
		else:
			f.write(str(tini)+'\n')
		# Y training period
		f.write("5\n")
		# First year of Y training period
		if monf=="Dec":
			f.write(str(tini+1)+'\n')
		else:
			f.write(str(tini)+'\n')

		# Goodness index
		f.write("531\n")
		# Kendall's tau
		f.write("3\n")

		# Option: Length of training period
		f.write("7\n")
		# Length of training period
		f.write(str(ntrain)+'\n')
		# Option: Length of cross-validation window
		f.write("8\n")
		# Enter length
		f.write("5\n")

#		if MOS!="None": # Commented out by AWR 10/28/21 to fix flex forecasts
			# Turn ON transform predictand data
#			f.write("541\n")
		if fprefix=='RFREQ':
			# Turn ON zero bound for Y data	 (automatically on by CPT if variable is precip)
			f.write("542\n")
		# Turn ON synchronous predictors
		f.write("545\n")
		# Turn ON p-values for masking maps
		#f.write("561\n")

		### Missing value options
		f.write("544\n")
		# Missing value X flag:
		blurb='-999\n'
		f.write(blurb)
		# Maximum % of missing values
		f.write("10\n")
		# Maximum % of missing gridpoints
		f.write("10\n")
		# Number of near-neighbors
		f.write("1\n")
		# Missing value replacement : best-near-neighbors
		f.write("4\n")
		# Y missing value flag
		blurb='-999\n'
		f.write(blurb)
		# Maximum % of missing values
		f.write("10\n")
		# Maximum % of missing stations
		f.write("10\n")
		# Number of near-neighbors
		f.write("1\n")
		# Best near neighbor
		f.write("4\n")

		# Transformation settings
		#f.write("554\n")
		# Empirical distribution
		#f.write("1\n")

		#######BUILD MODEL AND VALIDATE IT	!!!!!


		# save goodness index
		f.write("112\n")
		file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_Kendallstau_'+tar+'_'+mon+'\n'
		f.write(file)

		# NB: Default output format is GrADS format
		# select output format
		f.write("131\n")
		# Not GrADS, but txt format
		f.write("2\n")

		#set sig figs to 6 
		f.write('132\n')
		f.write('6\n')



		# Build cross-validated model
		f.write("311\n")   #In the seasonal case, training periods are usually too short to do retroactive analysis
		#Retroactive for s2s, due to the large sample size - deactivated for CFSv2
#		f.write("312\n")
#		#Length of initial training period: (Just quits w/o error msg if 80>ntrain)
#		f.write(str(lit)+'\n')
#		#Update interval:
#		f.write(str(liti)+'\n')   #--old comment from AGM: 80 for speeding up tests, change to 20 later (~same results so far with 20 or 80)


		# save EOFs
		if MOS=='CCA':

		
			#save CCA time series maps data
			f.write('111\n')
			f.write('412\n')
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCATS_'+tar+'_'+mon+'\n'
			f.write(file)
			f.write('0\n')

			f.write('111\n')
			f.write('422\n')
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_YCCATS_'+tar+'_'+mon+'\n'
			f.write(file)
			f.write('0\n')

			#save Canonical Correlations
			f.write('111\n')
			f.write('401\n')
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_CCACorr_'+tar+'_'+mon+'\n'
			f.write(file)
			f.write('0\n')

                        #save Y EOF time series 
			f.write('111\n')
			f.write('313\n')
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_YEOFTS_'+tar+'_'+mon+'\n'
			f.write(file)
			f.write('0\n')

		if MOS=='CCA' or MOS=='PCR':
			#save X EOF time series 
			f.write('111\n')
			f.write('303\n')
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XEOFTS_'+tar+'_'+mon+'\n'
			f.write(file)
			f.write('0\n')

		# NB: Default output format is GrADS format
		# select output format
		f.write("131\n")
		# GrADS format
		f.write("3\n")



		# Build cross-validated model
		f.write("311\n")   #In the seasonal case, training periods are usually too short to do retroactive analysis
		#Retroactive for s2s, due to the large sample size - deactivated for CFSv2
#		f.write("312\n")
#		#Length of initial training period: (Just quits w/o error msg if 80>ntrain)
#		f.write(str(lit)+'\n')
#		#Update interval:
#		f.write(str(liti)+'\n')   #--old comment from AGM: 80 for speeding up tests, change to 20 later (~same results so far with 20 or 80)


		# save EOFs
		if MOS=='CCA' or MOS=='PCR':
			f.write("111\n")
			#X EOF
			f.write("302\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFX_'+tar+'_'+mon+'\n'
			f.write(file)
			#Exit submenu
			f.write("0\n")
		if MOS=='CCA':
			f.write("111\n")
			#Y EOF
			f.write("312\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_EOFY_'+tar+'_'+mon+'\n'
			f.write(file)
			#Exit submenu
			f.write("0\n")
			
			#save X and Y CCA Loadings Maps
			f.write('111\n')
			f.write('421\n')
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_YCCAMAP_'+tar+'_'+mon+'\n'
			f.write(file)
			f.write('0\n')

			f.write('111\n')
			f.write('411\n')
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_XCCAMAP_'+tar+'_'+mon+'\n'
			f.write(file)
			f.write('0\n')

			#save CCA time series maps data



		# cross-validated skill maps
		f.write("413\n")
		# save Pearson's Correlation
		f.write("1\n")
		file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_Pearson_'+tar+'_'+mon+'\n'
		f.write(file)

		# cross-validated skill maps
		f.write("413\n")
		# save Spearman's Correlation
		f.write("2\n")
		file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_Spearman_'+tar+'_'+mon+'\n'
		f.write(file)

		# cross-validated skill maps
		f.write("413\n")
		# save 2AFC score
		f.write("3\n")
		file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_2AFC_'+tar+'_'+mon+'\n'
		f.write(file)

		# cross-validated skill maps
		f.write("413\n")
		# save RocBelow score
		f.write("15\n")
		file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_RocBelow_'+tar+'_'+mon+'\n'
		f.write(file)

		# cross-validated skill maps
		f.write("413\n")
		# save RocAbove score
		f.write("16\n")
		file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_RocAbove_'+tar+'_'+mon+'\n'
		f.write(file)

		if MOS=='CCA' or MOS=='PCR':   #DO NOT USE CPT to compute probabilities if MOS='None' --use IRIDL for direct counting
			if forecast_anomaly:
				# Anomalies
				#f.write("0\n")
				f.write("533\n")
				f.write("2\n")
				f.write("1\n")
				f.write(".33\n")
				f.write(".33\n")
			elif forecast_spi:
				# Standardized Index (SPI)
				#f.write("0\n")
				f.write("533\n")
				f.write("4\n")
				f.write("1\n")
				f.write(".33\n")
				f.write(".33\n")
				f.write("Y\n")
				f.write("311\n")
			else:
				# Full field
				print('Full field')

			#######FORECAST(S)	!!!!!
			# Probabilistic (3 categories) maps
			f.write("455\n")
			# Output results
			f.write("111\n")
			# Forecast probabilities
			f.write("501\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_P_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			#502 # Forecast odds
			#Exit submenu
			f.write("0\n")

			# Compute deterministc values and prediction limits
			f.write("454\n")
			# Output results
			f.write("111\n")
			# Forecast values
			f.write("511\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_V_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			#502 # Forecast odds


			#######Following files are used to plot the flexible format
			# Save cross-validated predictions
			f.write("201\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_xvPr_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# Save deterministic forecasts [mu for Gaussian fcst pdf]
			f.write("511\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_mu_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# Save prediction error variance [sigma^2 for Gaussian fcst pdf]
			f.write("514\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_var_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# Save z
			f.write("532\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_z_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# Save predictand [to build predictand pdf]
			f.write("102\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_Obs_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)

			#Exit submenu
			f.write("0\n")

			# Change to ASCII format to send files to DL
			f.write("131\n")
			# ASCII format
			f.write("2\n")

			#set sig figs to 6 
			f.write('132\n')
			f.write('6\n')
			
			# Output results
			f.write("111\n")
			# Save cross-validated predictions
			f.write("201\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_xvPr_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# Save deterministic forecasts [mu for Gaussian fcst pdf]
			f.write("511\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_mu_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# Forecast probabilities
			f.write("501\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_P_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# Save prediction error variance [sigma^2 for Gaussian fcst pdf]
			f.write("514\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_var_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# Save z
			f.write("532\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_z_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# Save predictand [to build predictand pdf]
			f.write("102\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'FCST_Obs_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# cross-validated skill maps
			if MOS=="PCR" or MOS=="CCA":
				f.write("0\n")
			f.write("413\n")
			# save 2AFC score  #special request from Chile
			f.write("3\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_2AFC_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# Stop saving  (not needed in newest version of CPT)

		###########PFV --Added by AGM in version 1.5
		#Compute and write retrospective forecasts for prob skill assessment.
		#Re-define forecas file if PCR or CCA
		if MOS=="PCR" or MOS=="CCA":
			f.write("3\n")
			file='../input/'+model+'_'+fprefix+'_'+tar+'_ini'+mon+'.tsv\n'  #here a conditional should choose if rainfall freq is being used
			f.write(file)
		#Forecast period settings
		f.write("6\n")
		# First year to forecast. Save ALL forecasts (for "retroactive" we should only assess second half)
		if monf=="Oct" or monf=="Nov" or monf=="Dec":
			f.write(str(tini+1)+'\n')
		else:
			f.write(str(tini)+'\n')
		#Number of forecasts option
		f.write("9\n")
		# Number of reforecasts to produce
		if monf=="Oct" or monf=="Nov" or monf=="Dec":
			f.write(str(ntrain-1)+'\n')
		else:
			f.write(str(ntrain)+'\n')
		# Change to ASCII format
		f.write("131\n")
		# ASCII format
		f.write("2\n")
		# Probabilistic (3 categories) maps
		f.write("455\n")
		# Output results
		f.write("111\n")
		# Forecast probabilities --Note change in name for reforecasts:
		f.write("501\n")
		file='../output/'+model+'_RFCST_'+fprefix+'_'+tar+'_ini'+monf+str(fyr)+'\n'
		f.write(file)
		#502 # Forecast odds
		#Exit submenu
		f.write("0\n")

		# Close X file so we can access the PFV option
		f.write("121\n")
		f.write("Y\n")  #Yes to cleaning current results:# WARNING:
		#Select Probabilistic Forecast Verification (PFV)
		f.write("621\n")
		# Opens X input file
		f.write("1\n")
		file='../output/'+model+'_RFCST_'+fprefix+'_'+tar+'_ini'+monf+str(fyr)+'.txt\n'
		f.write(file)
		# Nothernmost latitude
		f.write(str(nla2)+'\n')
		# Southernmost latitude
		f.write(str(sla2)+'\n')
		# Westernmost longitude
		f.write(str(wlo2)+'\n')
		# Easternmost longitude
		f.write(str(elo2)+'\n')

		f.write("5\n")
		# First year of the PFV
		# for "retroactive" only first half of the entire training period is typically used --be wise, as sample is short)
		if monf=="Oct" or monf=="Nov" or monf=="Dec":
			f.write(str(tini+1)+'\n')
		else:
			f.write(str(tini)+'\n')

		#If these prob forecasts come from a cross-validated prediction (as it's coded right now)
		#we don't want to cross-validate those again (it'll change, for example, the xv error variances)
		#Forecast Settings menu
		f.write("552\n")
		#Conf level at 50% to have even, dychotomous intervals for reliability assessment (as per Simon suggestion)
		f.write("50\n")
		#Fitted error variance option  --this is the key option: 3 is 0-leave-out cross-validation, so no cross-validation!
		f.write("3\n")
		#-----Next options are required but not really used here:
		#Ensemble size
		f.write("10\n")
		#Odds relative to climo?
		f.write("N\n")
		#Exceedance probabilities: show as non-exceedance?
		f.write("N\n")
		#Precision options:
		#Number of decimal places (Max 8):
		f.write("3\n")
		#Forecast probability rounding:
		f.write("1\n")
		#End of required but not really used options ----

		#Verify
		f.write("313\n")

		#Reliability diagram
		f.write("431\n")
		f.write("Y\n") #yes, save results to a file
		file='../output/'+model+'_RFCST_reliabdiag_'+fprefix+'_'+tar+'_ini'+monf+str(fyr)+'.tsv\n'
		f.write(file)

		# select output format -- GrADS, so we can plot it in Python
		f.write("131\n")
		# GrADS format
		f.write("3\n")

		# Probabilistic skill maps
		f.write("437\n")
		# save Ignorance (all cats)
		f.write("101\n")
		file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_Ignorance_'+tar+'_'+mon+'\n'
		f.write(file)

		# Probabilistic skill maps
		f.write("437\n")
		# save Ranked Probability Skill Score (all cats)
		f.write("122\n")
		file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_RPSS_'+tar+'_'+mon+'\n'
		f.write(file)

		# Probabilistic skill maps
		f.write("437\n")
		# save GROC (all cats)
		f.write("131\n")
		file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_GROC_'+tar+'_'+mon+'\n'
		f.write(file)

		# Exit
		f.write("0\n")
		f.write("0\n")
		f.close()
		get_ipython().system("cp params "+model+"_"+fprefix+predictand+"_"+mpref+"_"+tar+"_"+mon+".cpt")

def ensemblefiles(models,work):
	"""A simple function for preparing the NextGen ensemble files for the DL

	PARAMETERS
	----------
		models: array with selected models
	"""
	get_ipython().system("mkdir ../output/NextGen/")
	#Go to folder and delate old TXT and TGZ files in folder
	get_ipython().system("cd ../output/NextGen/; rm -Rf *_NextGen.tgz *.txt")
	for i in range(len(models)):
		get_ipython().system("cp ../*"+models[i]+"*.txt .")

	get_ipython().system("tar cvzf NextGen/"+work+"_NextGen.tgz *.txt")
	get_ipython().system("pwd")
	print("Compressed file "+work+"_NextGen.tgz created in output/NextGen/")
	print("Now send that file to your contact at the IRI")

def NGensemble(models,fprefix,predictor,predictand,mpref,id,tar,mon,tgti,tgtf,monf,fyr):
	"""A simple function for computing the NextGen ensemble

	PARAMETERS
	----------
		models: array with selected models
	"""
	nmods=len(models)

	W, Wi, XD, H, Hi, YD, T, Ti, TD = readGrADSctl(models,fprefix,predictand,mpref,id,tar,monf,fyr)

	ens  =np.empty([nmods,T,H,W])  #define array for later use

	k=-1
	for model in models:
		k=k+1 #model
		memb0=np.empty([T,H,W])  #define array for later use

		#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
		f=open('../output/'+model+'_'+fprefix+predictand+'_'+mpref+id+'_'+tar+'_'+monf+str(fyr)+'.dat','rb')
		#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
		for it in range(T):
			#Now we read the field
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize) #this if for each time stamp
			A0=np.fromfile(f,dtype='float32',count=numval)
			endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
			memb0[it,:,:]= np.transpose(A0.reshape((W, H), order='F'))

		memb0[memb0==-999.]=np.nan #identify NaNs

		ens[k,:,:,:]=memb0

	# NextGen ensemble mean (perhaps try median too?)
	NG=np.nanmean(ens, axis=0)  #axis 0 is ensemble member


	#Now write output:
	#writeCPT(NG,'../output/NextGen_'+fprefix+'_'+tar+'_ini'+mon+'.tsv',models,fprefix,predictand,mpref,id,tar,mon,tgti,tgtf,monf,fyr)
	if id=='FCST_xvPr':
		writeCPT(NG,'../input/NextGen_'+predictor+'_'+tar+'_ini'+mon+'.tsv',models,predictor,predictand,mpref,id,tar,mon,tgti,tgtf,monf,fyr)
		print('Cross-validated prediction files successfully produced')
	if id=='FCST_mu':
		writeCPT(NG,'../output/NextGen_'+predictor+predictand+'_'+mpref+'FCST_mu_'+tar+'_'+monf+str(fyr)+'.tsv',models,predictor,predictand,mpref,id,tar,mon,tgti,tgtf,monf,fyr)
		writeGrads(id, '../output/NextGen_'+predictor+predictand+'_'+mpref+'FCST_mu_'+tar+'_'+monf+str(fyr)+'.tsv', models, predictor, predictand, mpref, tar, mon, fyr, monf)
		print('Forecast files successfully produced')
	if id=='FCST_var':
		writeCPT(NG,'../output/NextGen_'+predictor+predictand+'_'+mpref+'FCST_var_'+tar+'_'+monf+str(fyr)+'.tsv',models,predictor,predictand,mpref,id,tar,mon,tgti,tgtf,monf,fyr)
		writeGrads(id, '../output/NextGen_'+predictor+predictand+'_'+mpref+'FCST_var_'+tar+'_'+monf+str(fyr)+'.tsv', models, predictor, predictand, mpref, tar, mon, fyr, monf)
		print('Forecast error files successfully produced')

def writeCPT(var,outfile,models,fprefix,predictand,mpref,id,tar,mon,tgti,tgtf,monf,fyr):
	"""Function to write seasonal output in CPT format,
	using information contained in a GrADS ctl file.

	PARAMETERS
	----------
		var: a Dataframe with dimensions T,Y,X
	"""
	vari = 'prec'
	varname = vari
	units = 'mm'
	var[np.isnan(var)]=-999. #use CPT missing value

	L=0.5*(float(tgtf)+float(tgti))
	monthdic = {'Jan':'01','Feb':'02','Mar':'03','Apr':'04','May':'05','Jun':'06','Jul':'07','Aug':'08','Sep':'09','Oct':'10','Nov':'11','Dec':'12'}
	S=monthdic[mon]
	mi=monthdic[tar.split("-")[0]]
	mf=monthdic[tar.split("-")[1]]

	#Read grads file to get needed coordinate arrays
	W, Wi, XD, H, Hi, YD, T, Ti, TD = readGrADSctl(models,fprefix,predictand,mpref,id,tar,monf,fyr)
	if tar=='Dec-Feb' or tar=='Nov-Jan':  #double check years are sync
		xyear=True  #flag a cross-year season
	else:
		#Ti=Ti+1
		xyear=False

	Tarr = np.arange(Ti, Ti+T)
	Xarr = np.linspace(Wi, Wi+W*XD,num=W+1)
	Yarr = np.linspace(Hi+H*YD, Hi,num=H+1)

	#Now write the CPT file
	f = open(outfile, 'w')
	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
	#f.write("xmlns:cf=http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/\n")   #not really needed
	f.write("cpt:nfields=1\n")
	#f.write("cpt:T	" + str(Tarr)+"\n")  #not really needed
	for it in range(T):
		if xyear==True:
			f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Ti)+"-"+S+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+str(Tarr[it]+1)+"-"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		else:
			f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Ti)+"-"+S+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		#f.write("\t")
		np.savetxt(f, Xarr[0:-1], fmt="%.6f",newline='\t') #f.write(str(Xarr)[1:-1])
		f.write("\n") #next line
		for iy in range(H):
			#f.write(str(Yarr[iy]) + "\t" + str(var[it,iy,0:-1])[1:-1]) + "\n")
			np.savetxt(f,np.r_[Yarr[iy+1],var[it,iy,0:]],fmt="%.6f", newline='\t')  #excise extra line
			f.write("\n") #next line
	f.close()

def save_notebook_with_naming_convention(workdir,destination,mons,tgtii,month_abbr,nla1,sla1,nla2,sla2,wlo1,elo1,wlo2,elo2,PREDICTAND,obs,PREDICTOR,models,fyr):
    
    
    i = -1    # index
    fs=''     # forecast season
    fi = 0    # forecast index month
    fm = str(mons)[2:5]

    for month in month_abbr:
        i+=1
        if (month == fm):
            fi = int(i+float(tgtii[0]))
            try:
                fs = str(month_abbr[fi][0]+month_abbr[fi+1][0]+month_abbr[fi+2][0])  #forecasting season
            except:
                fs = str(month_abbr[fi-12][0]+month_abbr[fi-11][0]+month_abbr[fi-10][0])

    # turning negative/positive geographical bounds into N, S, E and W. 
    if(nla1>0): nla1n=str(nla1)+"N"
    elif(nla1<0): nla1n=str(abs(nla1))+"S"
    if(sla1>0): sla1n=str(sla1)+"N"
    elif(sla1<0): sla1n=str(abs(sla1))+"S"
    if(nla2>0): nla2n=str(nla2)+"N"
    elif(nla2<0): nla2n=str(abs(nla2))+"S"
    if(sla2>0): sla2n=str(sla2)+"N"
    elif(sla2<0): sla2n=str(abs(sla2))+"S"

    if(wlo1>0): wlo1n=str(wlo1)+"E"
    elif(wlo1<0): wlo1n=str(abs(wlo1))+"W"
    if(elo1>0): elo1n=str(elo1)+"E"
    elif(elo1<0): elo1n=str(abs(elo1))+"W"
    if(wlo2>0): wlo2n=str(wlo2)+"E"
    elif(wlo2<0): wlo2n=str(abs(wlo2))+"W"
    if(elo2>0): elo2n=str(elo2)+"E"
    elif(elo2<0): elo2n=str(abs(elo2))+"W"

    name = 'PyCPT_seav1.9_' + PREDICTAND +'_'+str(nla1n)+'_'+str(sla1n)+'_'+str(wlo1n)+'_'+str(elo1n)+'_'+obs+'_'+fs+'_'+ PREDICTOR +'_'+str(nla2n)+'_'+str(sla2n)+'_'+str(wlo2n)+'_'+str(elo2n)+'_'+'_'.join(models)+'_'+fm+str(fyr)+'.ipynb'
    src = workdir + '/PyCPT_seav1.9.ipynb'
    dest = destination + name
    os.rename(src,dest)


def ELR_CPTscript(model,predictand,mon,monf,fyr,tini,tend,lit,nla1,sla1,wlo1,elo1,nla2,sla2,wlo2,elo2,fprefix,mpref,tar,ntrain,MOS,station,flag):
		"""Function to write ELR CPT namelist file

		"""
		# Set up CPT parameter file
		f=open("params","w")
		if flag==1: # Training period, run CPT with GCM option to compute deterministic scores
			f.write("614\n")
		else: # Verification period, run CPT with PFV to calculate probabilistic forecast scores 
			f.write("621\n")

		# First, ask CPT to stop if error is encountered
		f.write("571\n")
		f.write("3\n")

		# Opens X input file
		f.write("1\n")
		if flag==1:
			file='../input/'+model+'_'+fprefix+'_'+tar+'_ini'+mon+'_elr_training.tsv\n'
		else:
			file='../input/'+model+'_'+fprefix+'_'+tar+'_ini'+mon+'_elr_verification.txt\n'
		f.write(file)
		# Nothernmost latitude
		f.write(str(nla1)+'\n')
		# Southernmost latitude
		f.write(str(sla1)+'\n')
		# Westernmost longitude
		f.write(str(wlo1)+'\n')
		# Easternmost longitude
		f.write(str(elo1)+'\n')

		# Opens forecast (X) file
		#f.write("3\n")
		#file='../input/'+model+'fcst_'+fprefix+'_'+tar+'_ini'+monf+str(fyr)+'.tsv\n'
		#f.write(file)
		#Start forecast:
		#f.write("223\n")
		#if monf=="Dec":  #for multi-seasons, we need to add a better approach here AGMS
		#       f.write(str(fyr+1)+"\n")
		#else:
		#       f.write(str(fyr)+"\n")

		# Opens Y input file
		f.write("2\n")
		if flag==1:
			file='../input/obs_'+predictand+'_'+tar+'_training.tsv\n'
		else:
			file='../input/obs_'+predictand+'_'+tar+'_verification.tsv\n'
		f.write(file)
		# Nothernmost latitude
		f.write(str(nla2)+'\n')
		# Southernmost latitude
		f.write(str(sla2)+'\n')
		# Westernmost longitude
		f.write(str(wlo2)+'\n')
		# Easternmost longitude
		f.write(str(elo2)+'\n')

		# X training period
		#f.write("4\n")
		# First year of X training period
		#if monf=="Dec":
		#       f.write(str(tini+1)+'\n')
		#else:
		#       f.write(str(tini)+'\n')
		# Y training period
		#f.write("5\n")
		# First year of Y training period
		#if monf=="Dec":
		#       f.write(str(tini+1)+'\n')
		#else:
		#       f.write(str(tini)+'\n')

		# Goodness index
		f.write("531\n")
		# Kendall's tau
		f.write("3\n")

		# Option: Length of training period
		#f.write("7\n")
		# Length of training period
		#f.write(str(ntrain)+'\n')
		#f.write(str(lit)+'\n')
		# Option: Length of cross-validation window
		#f.write("8\n")
		# Enter length
		#f.write("5\n")

		# Turn ON Transform predictand data
		f.write("541\n")
		if fprefix=='RFREQ':
			# Turn ON zero bound for Y data  (automatically on by CPT if variable is precip)
			f.write("542\n")
		# Turn ON synchronous predictors
		f.write("545\n")

		## Missing value options
		#f.write("544\n")
		## Missing value X flag:
		#if flag==1:
		#       blurb='-999\n'
		#       f.write(blurb)
		#else:
		#       blurb='-1\n'
		#       f.write(blurb)
		## Maximum % of missing values
		#f.write("10\n")
		## Maximum % of missing gridpoints
		#f.write("10\n")
		## Number of near-neighbors
		#f.write("1\n")
		## Missing value replacement : best-near-neighbors
		#f.write("4\n")
		## Y missing value flag
		#blurb='-999\n'
		#f.write(blurb)
		## Maximum % of missing values
		#f.write("10\n")
		## Maximum % of missing stations
		#f.write("10\n")
		## Number of near-neighbors
		#f.write("1\n")
		## Best near neighbor
		#f.write("4\n") 

		#######BUILD MODEL AND VALIDATE IT      !!!!!


		if flag==1:
			# save goodness index
			f.write("112\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_Kendallstau_'+tar+'_'+mon+'\n'
			f.write(file)

			# NB: Default output format is GrADS format
			# select output format
			f.write("131\n")
			# GrADS format
			f.write("3\n")

			# Build cross-validated model
			f.write("311\n")   #In the seasonal case, training periods are usually too short to do retroactive analysis
			# Retroactive for s2s, due to the large sample size
			#f.write("312\n")
			#Length of initial training period:
			#f.write(str(lit)+'\n')
			#Update interval:
			#f.write(str(liti)+'\n')
			# for some weird reason for None we need to run it twice for it to work
			#f.write("312\n")
			#Length of initial training period:
			#f.write(str(lit)+'\n')
			#Update interval:
			#f.write(str(liti)+'\n')

			# cross-validated skill maps
			f.write("413\n")
			# save Pearson's Correlation
			f.write("1\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_Pearson_'+tar+'_'+mon+'\n'
			f.write(file)

			# cross-validated skill maps
			f.write("413\n")
			# save Spearman's Correlation
			f.write("2\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_Spearman_'+tar+'_'+mon+'\n'
			f.write(file)

			# cross-validated skill maps
			f.write("413\n")
			# save 2AFC score
			f.write("3\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_2AFC_'+tar+'_'+mon+'\n'
			f.write(file)

			# cross-validated skill maps
			f.write("413\n")
			# save RocBelow score
			f.write("15\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_RocBelow_'+tar+'_'+mon+'\n'
			f.write(file)

			# cross-validated skill maps
			f.write("413\n")
			# save RocAbove score
			f.write("16\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_RocAbove_'+tar+'_'+mon+'\n'
			f.write(file)

			#######FORECAST(S)      !!!!!
			# Opens forecast (X) file
			f.write("3\n")
			file='../input/'+model+'fcst_'+fprefix+'_'+tar+'_ini'+monf+str(fyr)+'.tsv\n'
			f.write(file)
			#Start forecast:
			f.write("223\n")
			if monf=="Dec":  #for multi-seasons, we need to add a better approach here AGMS
			       f.write(str(fyr+1)+"\n")
			else:
			       f.write(str(fyr)+"\n")

			# cross-validated skill maps
			f.write("413\n")
			# save 2AFC score  #special request from Chile
			f.write("3\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_2AFC_'+tar+'_'+monf+str(fyr)+'\n'
			f.write(file)
			# Stop saving  (not needed in newest version of CPT)
		else:

			f.write("5\n")
			# First year of the PFV
			if monf=="Oct" or monf=="Nov" or monf=="Dec":
				fypfv=tini+1+lit
			else:
				fypfv=tini+lit

			f.write(str(fypfv)+'\n')

			#Forecast Settings menu
			f.write("552\n")
			#Conf level at 50% to have even, dychotomous intervals for reliability assessment (as per Simon suggestion)
			f.write("50\n")
			#Fitted error variance option  --this is the key option: 3 is 0-leave-out cross-validation, so no cross-validation!
			f.write("3\n")
			#-----Next options are required but not really used here:
			#Ensemble size
			f.write("10\n")
			#Odds relative to climo?
			f.write("N\n")
			#Exceedance probabilities: show as non-exceedance?
			f.write("N\n")
			#Precision options:
			#Number of decimal places (Max 8):
			f.write("3\n")
			#Forecast probability rounding:
			f.write("1\n")
			#End of required but not really used options ----

			#Verify
			f.write("313\n")

			#Reliability diagram
			f.write("431\n")
			f.write("Y\n") #yes, save results to a file
			file='../output/'+model+'_RFCST_reliabdiag_'+fprefix+'_'+tar+'_ini'+monf+str(fyr)+'.tsv\n'
			f.write(file)

			# select output format -- GrADS, so we can plot it in Python
			f.write("131\n")
			# GrADS format
			f.write("3\n")

			# Probabilistic skill maps (all cats)
			f.write("437\n")
			# save Ignorance (all cats)
			f.write("101\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_Ignorance_'+tar+'_'+mon+'\n'
			f.write(file)

			# Probabilistic skill maps (all cats)
			f.write("437\n")
			# save Ranked Probability Skill Score (all cats)
			f.write("122\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_RPSS_'+tar+'_'+mon+'\n'
			f.write(file)

			# Probabilistic skill maps (all cats)
			f.write("437\n")
			# save Ranked Probability Skill Score (all cats)
			f.write("131\n")
			file='../output/'+model+'_'+fprefix+predictand+'_'+mpref+'_GROC_'+tar+'_'+mon+'\n'
			f.write(file)

		# Exit
		f.write("0\n")
		f.write("0\n")
		f.close()
		if flag ==1:
			get_ipython().system("cp params "+model+"_"+fprefix+predictand+"_"+mpref+"_"+tar+"_"+mon+"_training.cpt")
		else:
			get_ipython().system("cp params "+model+"_"+fprefix+predictand+"_"+mpref+"_"+tar+"_"+mon+"_verification.cpt")
		#get_ipython().system('cp params '+fprefix+'_'+mpref+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.cpt')


def elr_fit(x, y):
	"""
	Fits an Extended Logistic Regression to a set of tercile-category hindcasts at a single point.
	Uses sklearn.linear_model.LogisticRegression

	Parameters
	----------
	x : np array containing the set of GCM ensemble-mean hindcasts (rows) at a single gridpoint (1 column)
	y: np array containing the corresponding validating observations 

	Returns  
	A tuple containing: 
	elrmodel: class object sklearn.linear_model.LogisticRegression.fit()
	(p33, p67): tuple of the tercile breaks
	"""
# NB: the training in univariate; we have a different model at each gridpoint in the current implementation;
# once could imagine a bayesian spatial hierchical model, but that's not what's done here.
# Thus, the function will be called within a loop over gripoints.
# There could also be an XV loop.

	# Tercile breaks
	p33 = np.percentile(y,100/3); p67 = np.percentile(y,100*2/3)
	# stack y_bn & y_bn_nn vecors below each other (reshaping first into col vectors)
	yc = np.vstack((np.where(y <= p33, 1, 0).reshape(-1, 1),np.where(y <= p67, 1, 0).reshape(-1, 1)))

	# Construct 2D predictor matrix, with x in col 1 and q in col 2
	ndat = np.size(x)
	qq = np.vstack((np.full(ndat, p33).reshape(-1, 1), np.full(ndat, p67).reshape(-1, 1)))
	xmat = np.hstack((np.vstack((x.reshape(-1, 1), x.reshape(-1, 1))), qq))

# Train the regression
	elr_model = LogisticRegression(fit_intercept=True).fit(xmat, np.ravel(yc))
	terciles = (p33, p67)
	return elr_model, terciles

def elr_tercilesPredict(x, terciles, elr_model):
	"""
	Make a set of tercile predictions from a predictor series and an extended logistic regression model.
	The tercile breaks from the training period are required.
	Uses sklearn.linear_model.LogisticRegression.predict_proba()
    
	Parameters
	---------
	x : np array containing the set of predictors at a single gridpoint
	terciles: tuple containing the tercile breaks from the training period (p33, p67)
	elr_model: class object sklearn.linear_model.LogisticRegression.fit()
    
	Returns
	-------
	fterciles: np array containing the tercile-category forecasts (3 cols: BN, NN, AN)
	"""
	ndat = np.size(x)
	f33 = elr_model.predict_proba(np.hstack((x.reshape(-1, 1), np.full(ndat, terciles[0]).reshape(-1, 1))))
	f67 = elr_model.predict_proba(np.hstack((x.reshape(-1, 1), np.full(ndat, terciles[1]).reshape(-1, 1))))
	fterciles = np.vstack( ( f33[:,1], f67[:,1]-f33[:,1], np.ones((ndat,))-f67[:,1] ) ).T
	return fterciles

def elr_quantilePredict(x, elr_model):
	"""
	Make a set of quantile(median) predictions from a predictor series.
	Uses sklearn.linear_model.LogisticRegression
    
	Parameters
	---------
	x : np array containing the set of predictors at a single gridpoint
	elr_model: class object sklearn.linear_model.LogisticRegression.fit()
    
	Returns
	-------
	fquantile: np array containing the quantile forecast
	"""
	# Forecast model parameters
	c = elr_model.intercept_
	b = elr_model.coef_
	# Forecast of median
	p=0.5
	fquantile = (1/b[0,1]) * np.log( p / ((1-p) * np.exp(c + b[0,0]*x)))
	#fquantile=np.quantile(x, q)
	return fquantile

def elr_poe(x, elr_model_forecast, elr_model_climo):
	"""
	Probability of Exceedance Curve for Extended Logistic Regression forecast at a single gridpoint.
	Calculates PoE (poe, ordinate; 0 to 1) vs precip amounts (qclim & qfcst, abscissa).
	Uses sklearn.linear_model.LogisticRegression
	[***Under Construction***]
    
	Parameters
	----------
	x : float containing the GCM ensemble-mean forecast value at a single gridpoint (1 column)
	elr_model_forecast: class object sklearn.linear_model.LogisticRegression containing the ELR forecast model
	elr_model_climo: As elr_model_forecast, but for a model trained with x=0 (ie the climatological forecast)
    
	Returns
	-------
	poe: array of exceedance probabilities on the domain ]0,1[
	q_fcst: array of forecast quantile values for each poe value
	q_clim: array of climatological quantile values for each poe value
	"""

	# Forecast model parameters
	c = elr_model_forecast.intercept_
	b = elr_model_forecast.coef_
	# Climo model parameters
	C = elr_model_climo.intercept_
	B = elr_model_climo.coef_

	# Create a vector of npoints p in (0, 1) 
	npoints = 500
	p = np.linspace(0.001, 1-0.001, npoints)
	poe = 1 - p

	# Forecast and climo curves
	q_clim = (1/B[0,1]) * np.log( p / ((1-p) * np.exp(C)))
	q_fcst = (1/b[0,1]) * np.log( p / ((1-p) * np.exp(c + b[0,0]*x)))
	return poe, q_fcst, q_clim

def ELRscript(model,predictand,mon,monf,fyr,tini,tend,tgti,tgtf,lit,nla1,sla1,wlo1,elo1,nla2,sla2,wlo2,elo2,fprefix,mpref,tar,ntrain,MOS,station):
	"""Function to perform ELR forecast

	"""

#%% model Hindcasts 
	fh_xh = Dataset('../input/'+model+'_'+fprefix+'_'+tar+'_ini'+mon+'.nc', mode='r')
	fh_yh = Dataset('../input/obs_'+predictand+'_'+tar+'_hc.nc', mode='r')

	lons = fh_xh.variables['X'][:]
	lats = fh_xh.variables['Y'][:]

	x = fh_xh.variables['prec'][:]; x = np.squeeze(x)
	y = fh_yh.variables['prcp'][:]
	ndat1, nlat, nlon = np.shape(x)
	x1=x[:,1,1]
	I = np.where(x1>10000)
	bad_value_num=len(x1[I])
	ndat=ndat1-bad_value_num

#%% ELR: Train the models
# Make a dictionary to contain the 'LogisticRegression' objects and terciles
	elr_dict = {}   # create an empty dictionary
	elr_climo_dict = {}   # create an empty dictionary for the climo forecast

	ym = np.mean(y,axis=0)
	msk = ma.getmask(ym)
	index_land = np.empty((nlat,nlon),dtype=int)
	xm0 = x
	#xm = xm0[0:int(ndat/2),:,:]
	xm = xm0[0:lit,:,:]

	x0 = np.zeros(np.shape(xm))     # array of zeros to construct the climo forecast
	ijland = -1
	for j in range(nlat):
	#    print("in j loop, j=", j)
		for i in range(nlon):
			if msk[j,i] == False:    # fit model just for landpoints
				ijland = ijland + 1
				index_land[j,i] = ijland     # index of land points
				#elr_dict[ijland] = elr_fit(xm[:,j,i], y[0:int(ndat/2),j,i])
				#elr_climo_dict[ijland] = elr_fit(x0[:,j,i], y[0:int(ndat/2),j,i])
				elr_dict[ijland] = elr_fit(xm[:,j,i], y[0:lit,j,i])
				elr_climo_dict[ijland] = elr_fit(x0[:,j,i], y[0:lit,j,i])
			# ijland is the dictionary key that can be used to assess the entries, like this
			# mymodel, mytercs = mydict[0]
			# mymodel.coef_
	nland = ijland+1
	#print('ELR training done with total landpoints = ',nland)

	#%% Make set of ELR in-sample hindcasts (no XV)
	#elr_hc = np.empty((int(ndat/2),nlat,nlon)); elr_hc.fill(np.nan)
	elr_hc = np.empty((lit,nlat,nlon)); elr_hc.fill(np.nan)
	ijland = -1
	for j in range(nlat):
		for i in range(nlon):
			if msk[j,i] == False:    # fit model just for landpoints
				ijland = ijland + 1
				elrmodel, terciles = elr_dict[ijland]
				elr_hc[:,j,i] = elr_quantilePredict(xm[:,j,i], elrmodel)

	#print('Set of ELR hindcasts made on a map of xy gridpoints')
#---------------------------------------------
	#Now write the CPT file
	outfile=model+'_'+fprefix+'_'+tar+'_ini'+mon+'_elr_training.tsv'
	f = open(outfile, 'w')
	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
	f.write("cpt:nfields=1\n")
	W=nlon
	H=nlat
	#T=int(ndat/2)
	T=lit
	Xarr=lons
	Yarr=lats[::-1]
	vari='prec'
	var=np.flip(elr_hc, axis=1)
	var[np.isnan(var)]=-999. #use CPT missing value
	dss=xr.open_dataset('../input/'+model+'_'+fprefix+'_'+tar+'_ini'+mon+'.nc',decode_times=False)
	a=list(dss)
	units=dss[a[0]].units
	L=0.5*(float(tgtf)+float(tgti))
	monthdic = {'Jan':'01','Feb':'02','Mar':'03','Apr':'04','May':'05','Jun':'06','Jul':'07','Aug':'08','Sep':'09','Oct':'10','Nov':'11','Dec':'12'}
	S=monthdic[mon]
	mi=monthdic[tar.split("-")[0]]
	mf=monthdic[tar.split("-")[1]]
	Ti=tini
	if tar=='Dec-Feb' or tar=='Nov-Jan':  #double check years are sync
		xyear=True  #flag a cross-year season
	else:
		xyear=False

	Tarr=np.empty(ndat,dtype=int)
	for it in range(ndat):
		Tarr[it]=tini+it

	for it in range(T):
		if xyear==True:
			f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Ti)+"-"+S+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+str(Tarr[it]+1)+"-"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		else:
			f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Ti)+"-"+S+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		#f.write("\t")
		np.savetxt(f, Xarr, fmt="%.6f",newline='\t') #f.write(str(Xarr)[1:-1])
		f.write("\n") #next line
		for iy in range(H):
			np.savetxt(f,np.r_[Yarr[iy],var[it,iy,0:]],fmt="%.4f", newline='\t')  #excise extra line
			f.write("\n") #next line

	f.close()

	#write CPT for observation
	outfile='obs_'+predictand+'_'+tar+'_training.tsv'
	f = open(outfile, 'w')
	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
	f.write("cpt:nfields=1\n")
	W=nlon
	H=nlat
	Xarr=lons
	Yarr=lats[::-1]
	vari='prec'
	#var=np.flip(y[0:int(ndat/2),:,:], axis=1)
	var=np.flip(y[0:lit,:,:], axis=1)
	var[np.isnan(var)]=-999. #use CPT missing value
	dss=xr.open_dataset('../input/obs_'+predictand+'_'+tar+'_hc.nc',decode_times=False)
	a=list(dss)
	units=dss[a[0]].units
	#T1=int(ndat/2)
	T1=lit
	for it in range(T1):
		f.write("cpt:field="+vari+", cpt:T="+str(Tarr[it])+"-"+mi+"/"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		f.write("\t")
		np.savetxt(f, Xarr, fmt="%.1f",newline='\t')
		f.write("\n") #next line
		for iy in range(H):
			np.savetxt(f,np.r_[Yarr[iy],var[it,iy,0:]],fmt="%.4f", newline='\t')  #excise extra line
			f.write("\n") #next line
	f.close()

#---------------------------------------------------------
	ndat_fc = ndat-lit
	xf = x[lit:ndat,:,:]
	yf = y[lit:ndat,:,:]

#%%  Verification period
########################################

	elr_fc = np.empty((ndat_fc,nlat,nlon,3)); elr_fc.fill(np.nan)
	rpss_ELR_fc = np.ma.array(np.empty((nlat,nlon)), mask=msk, fill_value=np.nan)

	ijland = -1
	for j in range(nlat):
		for i in range(nlon):
			if msk[j,i] == False:    # fit model just for landpoints
				ijland = ijland + 1
				elrmodel, terciles = elr_dict[ijland]
				elr_fc[:,j,i,:] = elr_tercilesPredict(xf[:,j,i], terciles, elrmodel)
	#print('Set of ELR forcasts made on a map of xy gridpoints')

#----------------------------------------------------------
	#Now write the CPT file
	outfile=model+'_'+fprefix+'_'+tar+'_ini'+mon+'_elr_verification.txt'
	f = open(outfile, 'w')
	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
	f.write("cpt:nfields=1\n")
	f.write("cpt:ncats=3\n")
	W=nlon
	H=nlat
	ds=xr.open_dataset('../input/'+model+'_'+fprefix+'_'+tar+'_ini'+mon+'.nc',decode_times=False)
	T=ndat-lit
	#Tarr1=Tarr[int(ndat/2):]
	Tarr1=Tarr[lit:]
	Xarr=lons
	Yarr1=lats
	Yarr=Yarr1[::-1] #Y should from N to S
	vari='prec'
	var=np.flip(elr_fc, axis=1)*100
	var[np.isnan(var)]=-1.0 #use CPT missing value

	for it in range(T):
		f.write("cpt:field="+vari+", cpt:C=1, cpt:clim_prob=0.33333333333300003, cpt:T="+str(Tarr1[it])+"-"+mi+"/"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units=probability (%), cpt:missing=-1.0000000000000000\n")
		f.write("\t")
		np.savetxt(f, Xarr, fmt="%.1f",newline='\t')

		f.write("\n") #next line
		for iy in range(H):
			np.savetxt(f,np.r_[Yarr[iy],var[it,iy,0:,0]],fmt="%.1f", newline='\t')  #excise extra line
			f.write("\n") #next line
		f.write("cpt:C=2, cpt:clim_prob=0.33333333333400000\n")
		f.write("\t")
		np.savetxt(f, Xarr, fmt="%.1f",newline='\t')
		f.write("\n") #next line
		for iy in range(H):
			np.savetxt(f,np.r_[Yarr[iy],var[it,iy,0:,1]],fmt="%.1f", newline='\t')  #excise extra line
			f.write("\n") #next line
		f.write("cpt:C=3, cpt:clim_prob=0.33333333333299997\n")
		f.write("\t")
		np.savetxt(f, Xarr, fmt="%.1f",newline='\t')
		f.write("\n") #next line
		for iy in range(H):
			np.savetxt(f,np.r_[Yarr[iy],var[it,iy,0:,2]],fmt="%.1f", newline='\t')  #excise extra line
			f.write("\n") #next line
	f.close()

	#write CPT for observation
	outfile='obs_'+predictand+'_'+tar+'_verification.tsv'
	f = open(outfile, 'w')
	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
	f.write("cpt:nfields=1\n")
	W=nlon
	H=nlat
	Xarr=lons
	Yarr=lats[::-1]
	vari='prec'
	#var=np.flip(y[int(ndat/2):,:,:], axis=1)
	var=np.flip(y[lit:,:,:], axis=1)
	var[np.isnan(var)]=-999. #use CPT missing value
	dss=xr.open_dataset('../input/obs_'+predictand+'_'+tar+'_hc.nc',decode_times=False)
	a=list(dss)
	units=dss[a[0]].units
	#T1=int(ndat/2)
	T1=ndat-lit
	for it in range(T1):
		f.write("cpt:field="+vari+", cpt:T="+str(Tarr1[it])+"-"+mi+"/"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		f.write("\t")
		np.savetxt(f, Xarr, fmt="%.1f",newline='\t')
		f.write("\n") #next line
		for iy in range(H):
			np.savetxt(f,np.r_[Yarr[iy],var[it,iy,0:]],fmt="%.4f", newline='\t')  #excise extra line
			f.write("\n") #next line
	f.close()






