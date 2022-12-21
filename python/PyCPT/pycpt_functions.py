#This is PyCPT_functions.py (version1.9) -- 18 Nov 2020
#Authors: ÁG Muñoz (agmunoz@iri.columbia.edu), AW Robertson (awr@iri.columbia.edu), Cuihua Li (OCP)
#Collaborators: T Turkington (NEA), Bohar Singh, SJ Mason, Liseth Campos (DIMAR), Ana Lucía Caicedo (DIMAR)
#Notes: be sure it matches version of PyCPT
#Log: see version.log in GitHub

import os
import warnings
import sys
import platform
import struct
import xarray as xr
import numpy as np
import pandas as pd
from copy import copy
from scipy.stats import t
from scipy.stats import invgamma
import cartopy.crs as ccrs
from cartopy import feature
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.ticker as ticker
from matplotlib.ticker import Formatter, MaxNLocator
from matplotlib.colors import LinearSegmentedColormap
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from cartopy.mpl.geoaxes import GeoAxes
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
from cartopy.mpl.ticker import LatitudeFormatter, LongitudeFormatter
from netCDF4 import Dataset
import fileinput
import numpy.ma as ma
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

class MidpointNormalize(colors.Normalize):
    def __init__(self, vmin=None, vmax=None, midpoint=None, clip=False):
        self.midpoint = midpoint
        colors.Normalize.__init__(self, vmin, vmax, clip)

    def __call__(self, value, clip=None):
        # I'm ignoring masked values and all kinds of edge cases to make a
        # simple example...
        x, y = [self.vmin, self.midpoint, self.vmax], [0, 0.5, 1]
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

def ncdump(nc_fid, verb=True):
    '''
    ncdump outputs dimensions, variables and their attribute information.
    The information is similar to that of NCAR's ncdump utility.
    ncdump requires a valid instance of Dataset.
	Note: Modified by Ángel G. Muñoz from original version by Chris Slocum - CSU.

    Parameters
    ----------
    nc_fid : netCDF4.Dataset
        A netCDF4 dateset object
    verb : Boolean
        whether or not nc_attrs, nc_dims, and nc_vars are printed

    Returns
    -------
    nc_attrs : list
        A Python list of the NetCDF file global attributes
    nc_dims : list
        A Python list of the NetCDF file dimensions
    nc_vars : list
        A Python list of the NetCDF file variables
    '''
    def print_ncattr(key):
        """
        Prints the NetCDF file attributes for a given key

        Parameters
        ----------
        key : unicode
            a valid netCDF4.Dataset.variables key
        """
        try:
            print ("\t\ttype:", repr(nc_fid.variables[key].dtype))
            for ncattr in nc_fid.variables[key].ncattrs():
                print ('\t\t%s:' % ncattr,\
                      repr(nc_fid.variables[key].getncattr(ncattr)))
        except KeyError:
            print ("\t\tWARNING: %s does not contain variable attributes" % key)

    # NetCDF global attributes
    nc_attrs = nc_fid.ncattrs()
    if verb:
        print ("NetCDF Global Attributes:")
        for nc_attr in nc_attrs:
            print ('\t%s:' % nc_attr, repr(nc_fid.getncattr(nc_attr)))
    nc_dims = [dim for dim in nc_fid.dimensions]  # list of nc dimensions
    # Dimension shape information.
    if verb:
        print ("NetCDF dimension information:")
        for dim in nc_dims:
            print ("\tName:", dim)
            print ("\t\tsize:", len(nc_fid.dimensions[dim]))
            print_ncattr(dim)
    # Variable information.
    nc_vars = [var for var in nc_fid.variables]  # list of nc variables
    if verb:
        print ("NetCDF variable information:")
        for var in nc_vars:
            if var not in nc_dims:
                print ('\tName:', var)
                print ("\t\tdimensions:", nc_fid.variables[var].dimensions)
                print ("\t\tsize:", nc_fid.variables[var].size)
                print_ncattr(var)
    return nc_attrs, nc_dims, nc_vars

#def PrepFiles(rainfall_frequency, threshold_pctle, wlo1, wlo2,elo1, elo2,sla1, sla2,nla1, nla2, day1, day2, fday, nday, fyr, mon, os, authkey, wk, wetday_threshold, nlag, training_season, hstep, model, obs_source, obsclimo_source,obsclimo2_source, hdate_last, GEPShdate1, force_download,mpref, dic_s2s):
def PrepFiles(rainfall_frequency, threshold_pctle, wlo1, wlo2,elo1, elo2,sla1, sla2,nla1, nla2, day1, day2, fday, nday, fyr, mon, os, authkey, wk, wetday_threshold, nlag, training_season, hstep, model, obs_source, obsclimo_source, obsclimo2_source, hdate_last, GEPShdate1, force_download, mpref, MOS, dic_s2s, dic_s2s_elr):

	"""Function to download (or not) the needed files"""
	if obs_source=='userdef':
		GetHindcastsUser(wlo1, elo1, sla1, nla1, day1, day2, fyr, mon, os, authkey, wk, nlag, nday, training_season, hstep, model, hdate_last, force_download, dic_s2s)
		print('Hindcasts file ready to go')
		print('----------------------------------------------')
		GetObsUser(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, authkey, wk, nlag, training_season, hstep, model, obs_source, obsclimo_source, hdate_last, force_download, dic_s2s)
		print('Obs:precip file ready to go')
		print('----------------------------------------------')
		GetForecastUser(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, wlo2, elo2, sla2, nla2, obs_source, authkey, wk, nlag, model, hdate_last, threshold_pctle,training_season,wetday_threshold,force_download,mpref, dic_s2s)
		print('Forecasts file ready to go')
		print('----------------------------------------------')
	else:
		if rainfall_frequency:
			GetObs_RFREQ(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, authkey, wk, wetday_threshold, threshold_pctle, nlag, training_season, hstep, model, obs_source, force_download, dic_s2s)
			print('Obs:rfreq file ready to go')
			print('----------------------------------------------')
#			nday added after nlag for GEFS & CFSv2
			GetHindcasts(wlo1, elo1, sla1, nla1, day1, day2, fyr, mon, os, authkey, wk, nlag, nday, training_season, hstep, model, hdate_last, force_download, dic_s2s)
			#GetHindcasts_RFREQ(wlo1, elo1, sla1, nla1, day1, day2, nday, fyr, mon, os, authkey, wk, wetday_threshold, nlag, training_season, hstep, model, force_download)
			print('Hindcasts file ready to go')
			print('----------------------------------------------')
			#GetForecast_RFREQ(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, authkey, wk, wetday_threshold, nlag, model, force_download)
			GetForecast(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, wlo2, elo2, sla2, nla2, obs_source, authkey, wk, nlag, model, hdate_last,threshold_pctle,training_season,wetday_threshold,force_download,mpref, dic_s2s)
			print('Forecasts file ready to go')
			print('----------------------------------------------')
		else:
			# if temp:
			# 	GetHindcasts_T2M(wlo1, elo1, sla1, nla1, day1, day2, fyr, mon, os, key, week, nlag, nday, training_season, hstep, model, hdate_last, force_download)
			# 	print('Hindcasts file ready to go')
			# 	print('----------------------------------------------')
			# 	GetObs_T2M(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, key, week, nlag, training_season, hstep, model, obs_source, hdate_last, force_download)
			# 	print('Obs:temp file ready to go')
			# 	print('----------------------------------------------')
			# 	GetForecast_T2M(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, wlo2, elo2, sla2, nla2, obs_source, key, week, nlag, model, hdate_last, threshold_pctle,training_season,wetday_threshold,force_download)
			# 	print('Forecasts file ready to go')
			# 	print('----------------------------------------------')
			# else:
			#GetHindcasts(wlo1, elo1, sla1, nla1, day1, day2, fyr, mon, os, authkey, wk, nlag, training_season, hstep, model, force_download)
			#nday added after nlag for GEFS & CFSv2
			if MOS == 'ELR':
				GetHindcasts_elr(wlo1, elo1, sla1, nla1, day1, day2, fyr, mon, os, authkey, wk, nlag, nday, training_season, hstep, model, hdate_last, GEPShdate1, force_download, dic_s2s_elr)
				print('Hindcasts netcdf file ready to go')
				print('----------------------------------------------')
				GetObs_hc_elr(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, authkey, wk, nlag, training_season, hstep, model, obs_source, obsclimo_source, obsclimo2_source, hdate_last, GEPShdate1, force_download, dic_s2s_elr)
				print('Obs hc:precip netcdf file ready to go')
				print('----------------------------------------------')
				GetForecast(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, wlo2, elo2, sla2, nla2, obs_source,obsclimo_source, authkey, wk, nlag, model, hdate_last, threshold_pctle,training_season,wetday_threshold,force_download,mpref, dic_s2s)
				print('Forecasts tsv file ready to go')
				print('----------------------------------------------')
			else:
				GetHindcasts(wlo1, elo1, sla1, nla1, day1, day2, fyr, mon, os, authkey, wk, nlag, nday, training_season, hstep, model, hdate_last, GEPShdate1, force_download, dic_s2s)
				print('Hindcasts file ready to go')
				print('----------------------------------------------')
				GetObs(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, authkey, wk, nlag, training_season, hstep, model, obs_source, obsclimo_source, obsclimo2_source, hdate_last, GEPShdate1,force_download, dic_s2s)
				print('Obs:precip file ready to go')
				print('----------------------------------------------')
				GetForecast(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, wlo2, elo2, sla2, nla2, obs_source,obsclimo_source, authkey, wk, nlag, model, hdate_last, threshold_pctle,training_season,wetday_threshold,force_download,mpref, dic_s2s)
				print('Forecasts file ready to go')
				print('----------------------------------------------')


#			GetHindcasts(wlo1, elo1, sla1, nla1, day1, day2, fyr, mon, os, authkey, wk, nlag, nday, training_season, hstep, model, hdate_last, GEPShdate1, force_download, dic_s2s)
#			print('Hindcasts file ready to go')
#			print('----------------------------------------------')
#			GetObs(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, authkey, wk, nlag, training_season, hstep, model, obs_source, obsclimo_source,obsclimo2_source, hdate_last, GEPShdate1, force_download, dic_s2s)
#			print('Obs:precip file ready to go')
#			print('----------------------------------------------')
#			GetForecast(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, wlo2, elo2, sla2, nla2, obs_source,obsclimo_source, authkey, wk, nlag, model, hdate_last, threshold_pctle,training_season,wetday_threshold,force_download,mpref, dic_s2s)
#			print('Forecasts file ready to go')
#			print('----------------------------------------------')

def PrepFiles_usrNetcdf(fprefix, predictand, wlo1, wlo2,elo1, elo2, sla1, sla2, nla1, nla2, tgti, tgtf, mon, monf, fyr, tar, infile_predictand, infile_hindcast, infile_forecast):
		"""Function to user-provided NetCDF files"""

		readNetCDF_predictand(infile_predictand,outfile, predictand, wlo2, elo2, sla2, nla2, tar)
		print('Obs:precip file ready to go')
		print('----------------------------------------------')

		readNetCDF_Hindcasts(infile_hindcast, outfile, wlo1, elo1, sla1, nla1, tgti, tgtf, mon, tar)
		print('Hindcasts file ready to go')
		print('----------------------------------------------')

		readNetCDF_Forecast(infile_forecast, outfile, monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1)
		print('Forecasts file ready to go')
		print('----------------------------------------------')

def pltdomain(loni1,lone1,lati1,late1,loni2,lone2,lati2,late2):
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
		name='admin_1_states_provinces_shp',
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
		ax.stock_img()

		ax.add_feature(feature.LAND)
		ax.add_feature(feature.COASTLINE)
		ax.set_title(title[i]+" domain")
		pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				  linewidth=2, color='gray', alpha=0.5, linestyle='--')
		pl.xlabels_top = False
		pl.ylabels_left = False
		pl.xformatter = LONGITUDE_FORMATTER
		pl.yformatter = LATITUDE_FORMATTER
		ax.add_feature(states_provinces, edgecolor='gray')
	plt.show()

def pltmap(model,score,loni,lone,lati,late,fprefix,mpref,training_season, mon, fday, nwk, wki):
# wki is the week identifier, eg 1,2,34

	"""A simple function for ploting the statistical score

	PARAMETERS
	----------
		score: the score
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
		title: title
	"""

	plt.figure(figsize=(20,5))

	for L in range(nwk):
		wk=L+1
		#Read grads binary file size H, W  --it assumes all files have the same size, and that 2AFC exists
		with open('../output/'+model+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk'+str(wki[wk-1])+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				W = int(line.split()[1])
				XD= float(line.split()[4])
		with open('../output/'+model+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk'+str(wki[wk-1])+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				H = int(line.split()[1])
				YD= float(line.split()[4])

#		ax = plt.subplot(nwk/2, 2, wk, projection=ccrs.PlateCarree())
		ax = plt.subplot(1,nwk, wk, projection=ccrs.PlateCarree())
		ax.set_extent([loni,loni+W*XD,lati,lati+H*YD], ccrs.PlateCarree())

		#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
		states_provinces = feature.NaturalEarthFeature(
			category='cultural',
#			name='admin_1_states_provinces_shp',
			name='admin_0_countries',
			scale='10m',
			facecolor='none')

		ax.add_feature(feature.LAND)
		ax.add_feature(feature.COASTLINE)
		ax.set_title(score+' for Week '+str(wki[wk-1]))
		pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				  linewidth=2, color='gray', alpha=0., linestyle='--')
		pl.xlabels_top = False
		pl.ylabels_left = True
		pl.ylabels_right = False
		pl.xformatter = LONGITUDE_FORMATTER
		pl.yformatter = LATITUDE_FORMATTER
		ax.add_feature(states_provinces, edgecolor='gray')
		lon_formatter = LongitudeFormatter(degree_symbol='')
		lat_formatter = LatitudeFormatter(degree_symbol='')
		ax.xaxis.set_major_formatter(lon_formatter)
		ax.yaxis.set_major_formatter(lat_formatter)
		ax.set_ybound(lower=lati, upper=late)

		if score == 'CCAFCST_V' or score == 'PCRFCST_V' or score == 'noMOSFCST_V' or score == 'ELRhoFCST_V':
			f=open('../output/'+model+fprefix+'_'+score+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki[wk-1])+'.dat','rb')
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			#Now we read the field
			A=np.fromfile(f,dtype='float32',count=numval)
			var = np.transpose(A.reshape((W, H), order='F'))
			var[var==-999.]=np.nan #only sensible values
			current_cmap = plt.cm.BrBG
			current_cmap.set_bad('white',1.0)
			current_cmap.set_under('white', 1.0)
			if fprefix == 'RFREQ':
				label ='Freq Rainy Days (days)'
				var=var/100 #weird 100 factor coming from CPT for frq rainy days!! ??
			elif fprefix == 'PRCP':
				label = 'Rainfall anomaly (mm/week)'
			CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				#vmin=-max(np.max(var),np.abs(np.min(var))), #vmax=np.max(var),
                vmin=-max(np.nanmax(var),np.abs(np.nanmin(var))),
				vmax= max(np.nanmax(var),np.abs(np.nanmin(var))),
				#norm=MidpointNormalize(midpoint=0.),
				cmap=current_cmap,
				transform=ccrs.PlateCarree())
			ax.set_title("Deterministic forecast for Week "+str(wki[wk-1]))
			f.close()
			#current_cmap = plt.cm.get_cmap()
			#current_cmap.set_bad(color='white')
			#current_cmap.set_under('white', 1.0)
		else:
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			f=open('../output/'+model+fprefix+'_'+mpref+'_'+score+'_'+training_season+'_wk'+str(wki[wk-1])+'.dat','rb')
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			#Now we read the field
			A=np.fromfile(f,dtype='float32',count=numval)
			var = np.transpose(A.reshape((W, H), order='F'))
			#define colorbars, depending on each score	--This can be easily written as a function
			if score == '2AFC':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=0,vmax=100,
				cmap=discrete_cmap(11, 'bwr'),
				transform=ccrs.PlateCarree())
				label = '2AFC (%)'

			if score == 'RocAbove' or score=='RocBelow':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=0,vmax=1,
				cmap=discrete_cmap(11, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'ROC area'

			if score == 'Spearman' or score=='Pearson':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=-1,vmax=1,
				cmap=discrete_cmap(11, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'Correlation'

			if score == 'RPSS':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=-20,vmax=20,
				cmap=discrete_cmap(11, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'RPSS (all categories)'

			if score=='GROC':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=0,vmax=100,
				cmap=discrete_cmap(11, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'GROC (probabilistic)'

			if score=='Ignorance':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=0.8,vmax=2.,
				cmap=discrete_cmap(20, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'Ignorance (all categories)'

			# if score=='Ignorance':  #This is really the Ignorance Skill Score (Ignorance normalized by log2(3))
			# 	var[var==-999.]=np.nan #only sensible values
			# 	CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var/1.5849,
			# 	vmin=.8,vmax=1.2,
			# 	cmap=discrete_cmap(11, 'bwr'),
			# 	transform=ccrs.PlateCarree())
			# 	label = 'Ignorance Skill Score (all categories)'

			if score=='Ignorance_AN':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				#vmin=1.,vmax=2.,
				cmap=discrete_cmap(20, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'Ignorance (above normal)'

			if score=='RelIgn_AN':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				#vmin=1.,vmax=2.,
				cmap=discrete_cmap(20, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'Reliability (Ignorance, above normal)'

			if score=='ResIgn_AN':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				#vmin=1.,vmax=2.,
				cmap=discrete_cmap(20, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'Resolution (Ignorance, above normal)'

			if score=='Ignorance_BN':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				#vmin=1.,vmax=2.,
				cmap=discrete_cmap(20, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'Ignorance (below normal)'

			if score=='RelIgn_BN':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				#vmin=1.,vmax=2.,
				cmap=discrete_cmap(20, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'Reliability (Ignorance, below normal)'

			if score=='ResIgn_BN':
				var[var==-999.]=np.nan #only sensible values
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				#vmin=1.,vmax=2.,
				cmap=discrete_cmap(20, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'Resolution (Ignorance, below normal)'

		f.close()
	plt.subplots_adjust(hspace=0)
	plt.subplots_adjust(bottom=0.15, top=0.9)
	cax  = plt.axes([0.2, 0.08, 0.6, 0.04])
	cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
	cbar.set_label(label) #, rotation=270)

def plteofs_eofts(models,mode,M,loni1,lone1,lati1,late1,loni2,lone2,lati2,late2,fprefix,mpref,training_season,nwk, wk, wknam, map_color, colorbar_option, use_ocean):
	"""A simple function for ploting EOFs computed by CPT

	PARAMETERS
	----------
		models: list of models to plot
		mode: EOF being visualized
		M: total number of EOFs computed by CPT (max defined in PyCPT is 10)
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
		fprefix:
	"""
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
	fig, ax = plt.subplots(figsize=(20,15))
	grid = plt.GridSpec(nmods, 3, wspace=0.2,hspace=0.3)
	vmi = 0
	vma = 0  #for later use of vmin vmax adjustment
	mm=-1
	for model in models:
		mm=mm+1
		#Read  grid
		with open('../output/'+model+fprefix+'_'+mpref+'_EOFX_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				W = int(line.split()[1])
				XD = float(line.split()[4])

		with open('../output/'+model+fprefix+'_'+mpref+'_EOFX_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				H = int(line.split()[1])
				YD= float(line.split()[4])

		if mpref=='CCA':

			with open('../output/'+model+fprefix+'_'+mpref+'_EOFY_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
				for line in lines_that_contain("XDEF", fp):
					Wy = int(line.split()[1])
					XDy= float(line.split()[4])
			with open('../output/'+model+fprefix+'_'+mpref+'_EOFY_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
				for line in lines_that_contain("YDEF", fp):
					Hy = int(line.split()[1])
					YDy= float(line.split()[4])

			eofy=np.empty([M,Hy,Wy])  #define array for later use
		#print(  [M,Hy,Wy],     )
		eofx=np.empty([M,H,W])  #define array for later use
		#ax = plt.subplot(grid[mm, 2], projection=ccrs.PlateCarree())

		if mpref=='CCA':  #skip if there are not predictand EOFs (e.g., PCR)
			ax = plt.subplot(grid[mm, 2], projection=ccrs.PlateCarree())
			ax.set_extent([loni2,loni2+Wy*XDy,lati2,lati2+Hy*YDy], ccrs.PlateCarree())
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			f=open('../output/'+model+fprefix+'_'+mpref+'_EOFY_'+training_season+'_wk'+str(wk)+'.dat','rb')
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
			CS=plt.pcolormesh(np.linspace(loni2, loni2+Wy*XDy,num=Wy), np.linspace(lati2+Hy*YDy, lati2, num=Hy), eofy[mode],
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

			ax.set_title('Y EOF Spatial Loadings')

			plt.subplots_adjust(bottom=0.15, top=0.9)
			#cax = plt.axes([0.69, 0.58, 0.2, 0.03])
			#cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
			cbar = plt.colorbar(CS, orientation='horizontal')
			cbar.set_label(label) #, rotation=270)

		eofx=[]
		eofx.append([])
		#m_ndx = models.index(model)
		m_ndx = 0
		s_ndx = 0
		ax = plt.subplot(grid[mm, 0], projection=ccrs.PlateCarree())
		if mpref=='PCR':
			ax.set_extent([loni1,loni1+W*XD,lati1,lati1+H*YD], ccrs.PlateCarree())  #EOF domains will look different between CCA and PCR if X and Y domains are different
		else:
			#ax.set_extent([loni,loni+Wy*XDy,lati,lati+Hy*YDy], ccrs.PlateCarree())
			ax.set_extent([loni1,loni1+W*XD,lati1,lati1+H*YD], ccrs.PlateCarree())
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
			ax.text(-0.35,1.3,"EOF "+str(mode+1)+"      "+wknam,fontsize=18,fontweight='bold',transform=ax.transAxes)
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

		#ax.set_ylabel(model, rotation=90)
		#ax.set_title('X Spatial Loadings')
		ax.set_title('X EOF Spatial Loadings')
		#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
		f=open('../output/'+model+fprefix+'_'+mpref+'_EOFX_'+training_season+'_wk'+str(wk)+'.dat','rb')
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
		CS=plt.pcolormesh(np.linspace(loni1, loni1+W*XD,num=W), np.linspace(lati1+H*YD, lati1, num=H),eofx[m_ndx][s_ndx][mode],
		#orig CS=plt.pcolormesh(np.linspace(loni2, loni2+W*XD,num=W), np.linspace(lati2+H*YD, lati2, num=H),eofx[m_ndx][s_ndx][mode],
		#vmin=vmi,vmax=vma,
		vmin=-.1,vmax=.1,
		cmap=current_cmap,
		transform=ccrs.PlateCarree())
		#label = 'EOF loadings'
		label = 'EOF Spatial loadings'
		plt.subplots_adjust(hspace=0)
		cbar = plt.colorbar(CS, orientation='horizontal')
		cbar.set_label(label) #, rotation=270)

		#plot time series
		if mpref=='PCR':
			f = open("../output/" +model+fprefix+'_' + mpref + '_XEOFTS_' +training_season+'_wk'+str(wk)+ '.txt','r')

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

			#Ticks
			start, end = ax.get_xlim()
			ax.xaxis.set_ticks(np.arange(start-2, end, 10))

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
			f = open("../output/" +model+fprefix+'_' + mpref + '_XEOFTS_' +training_season+'_wk'+str(wk)+ '.txt','r')
			f2 = open("../output/" +model+fprefix+'_' + mpref + '_YEOFTS_' +training_season+'_wk'+str(wk)+ '.txt','r')

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
			ax.xaxis.set_ticks(np.arange(start-2, end, 10))

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


def plteofs(model,mode,M,loni,lone,lati,late,fprefix,mpref,training_season,nwk, wk):
	"""A simple function for ploting EOFs computed by CPT"""

	if mpref=='None':
		print('No EOFs are computed if MOS=None is used')
		return

	plt.figure(figsize=(20,5))
	fig, ax = plt.subplots(figsize=(20,5),sharex=True,sharey=True)

	with open('../output/'+model+fprefix+'_'+mpref+'_EOFX_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
		for line in lines_that_contain("XDEF", fp):
			W = int(line.split()[1])
			XD = float(line.split()[4])

	with open('../output/'+model+fprefix+'_'+mpref+'_EOFX_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
		for line in lines_that_contain("YDEF", fp):
			H = int(line.split()[1])
			YD= float(line.split()[4])

	if mpref=='CCA':

		with open('../output/'+model+fprefix+'_'+mpref+'_EOFY_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				Wy = int(line.split()[1])
				XDy= float(line.split()[4])
		with open('../output/'+model+fprefix+'_'+mpref+'_EOFY_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				Hy = int(line.split()[1])
				YDy= float(line.split()[4])

		eofy=np.empty([M,Hy,Wy])  #define array for later use

	eofx=np.empty([M,H,W])  #define array for later use


	ax = plt.subplot(2,1,1,projection=ccrs.PlateCarree()) #nmods+obs

	if mpref=='CCA':  #skip if there are not predictand EOFs (e.g., PCR)
		ax.set_extent([loni,loni+Wy*XDy,lati,lati+Hy*YDy], ccrs.PlateCarree())
		#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
		f=open('../output/'+model+fprefix+'_'+mpref+'_EOFY_'+training_season+'_wk'+str(wk)+'.dat','rb')
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
		cmap=plt.cm.bwr,
		transform=ccrs.PlateCarree())
		label = 'EOF loadings'

		#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
		states_provinces = feature.NaturalEarthFeature(
			category='cultural',
	#			name='admin_1_states_provinces_shp',
			name='admin_0_countries',
			scale='10m',
			facecolor='none')

		ax.add_feature(feature.LAND)
		ax.add_feature(feature.COASTLINE)

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
		ax.add_feature(states_provinces, edgecolor='gray')
		ax.set_ybound(lower=lati, upper=late)


		#if ax.is_first_col():
		ax.set_ylabel(model, rotation=90)

		ax.text(-0.35,0.5,'Obs',rotation=90,verticalalignment='center', transform=ax.transAxes)


		ax = plt.subplot(2,1, 2, projection=ccrs.PlateCarree()) #nmods+obs
		if mpref=='PCR':
			ax.set_extent([loni,loni+W*XD,lati,lati+H*YD], ccrs.PlateCarree())  #EOF domains will look different between CCA and PCR if X and Y domains are different
		else:
			ax.set_extent([loni,loni+Wy*XDy,lati,lati+Hy*YDy], ccrs.PlateCarree())

		#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
		states_provinces = feature.NaturalEarthFeature(
			category='cultural',
	#		name='admin_1_states_provinces_shp',
			name='admin_0_countries',
			scale='10m',
			facecolor='none')

		ax.add_feature(feature.LAND)
		ax.add_feature(feature.COASTLINE)
		#if k == 2:
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



		#if ax.is_first_col():
		ax.set_ylabel(model, rotation=90)

		#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
		f=open('../output/'+model+fprefix+'_'+mpref+'_EOFX_'+training_season+'_wk'+str(wk)+'.dat','rb')
		#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
		for mo in range(M):
			#Now we read the field
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize) #this if for each time/EOF stamp
			A0=np.fromfile(f,dtype='float32',count=numval)
			endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
			eofx[mo,:,:]= np.transpose(A0.reshape((W, H), order='F'))

		eofx[eofx==-999.]=np.nan #nans

		CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), eofx[mode,:,:],
		vmin=-.1,vmax=.1,
		cmap=plt.cm.bwr,
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

def pltccas_ccats(models,mode,M,loni1,lone1,lati1,late1,loni2,lone2,lati2,late2,fprefix,mpref,training_season,nwk, wk, map_color, colorbar_option, use_ocean):
	"""A simple function for ploting CCAs computed by CPT

	PARAMETERS
	----------
		models: list of models to plot
		mode: CCA loadings being visualized
		M: total number of CCAs computed by CPT 
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
	"""
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
	fig, ax = plt.subplots(figsize=(20,15))
	grid = plt.GridSpec(nmods, 3, wspace=0.2,hspace=0.3)
	vmi = 0
	vma = 0  #for later use of vmin vmax adjustment
	mm=-1
	for model in models:
		mm=mm+1
		#Read  grid
		with open('../output/'+model+fprefix+'_'+mpref+'_XCCAMAP_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				W = int(line.split()[1])
				XD= float(line.split()[4])
		with open('../output/'+model+fprefix+'_'+mpref+'_XCCAMAP_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				H = int(line.split()[1])
				YD= float(line.split()[4])

		if mpref=='CCA':
			with open('../output/'+model+fprefix+'_'+mpref+'_YCCAMAP_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
				for line in lines_that_contain("XDEF", fp):
					Wy = int(line.split()[1])
					XDy= float(line.split()[4])
			with open('../output/'+model+fprefix+'_'+mpref+'_YCCAMAP_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
				for line in lines_that_contain("YDEF", fp):
					Hy = int(line.split()[1])
					YDy= float(line.split()[4])

			with open('../output/'+model+fprefix+'_'+mpref+'_YCCAMAP_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
				for line in lines_that_contain("TDEF", fp):
					My = int(line.split()[1])

			ccay=np.empty([M,Hy,Wy])  #define array for later use
		#print(  [M,Hy,Wy],     )
		ccax=np.empty([M,H,W])  #define array for later use
		if mode+1 > My:
			ax1 = plt.subplot(grid[mm, 0],projection=ccrs.PlateCarree())
			ax1.set_extent([loni1,loni1+W*XD,lati1,lati1+H*YD], ccrs.PlateCarree())
			subtitle_string = "CCA Mode "+str(mode+1)+" Week "+str(wk)+" ("+model+"): Canonical correlation = N/A"
			ax1.text(-0.35,1.2,subtitle_string,fontsize=14,transform=ax1.transAxes)
			label = 'CCA loadings'
			cbar = plt.colorbar(CS, orientation='horizontal')
			cbar.set_label(label)
			ax1.text(loni1+(W*XD)/2., lati1+(H*YD)/2., 'N/A', fontsize=18, ha='center')
			ax1.text(-0.35,0.5,model,rotation=90,verticalalignment='center', transform=ax1.transAxes)
			ax1.set_title('X Spatial Loadings')
			ax2 = plt.subplot(grid[mm, 1])
			ax2.text(0.5, 0.5, 'N/A', fontsize=18, ha='center')
			ax2.set_title('Temporal Scores')
			ax3 = plt.subplot(grid[mm, 2],projection=ccrs.PlateCarree())
			ax3.set_extent([loni2,loni2+Wy*XDy,lati2,lati2+Hy*YDy], ccrs.PlateCarree())
			plt.subplots_adjust(hspace=0)
			plt.subplots_adjust(bottom=0.15, top=0.9)
			cbar = plt.colorbar(CS, orientation='horizontal')
			cbar.set_label(label)
			ax3.text(loni2+(Wy*XDy)/2., lati2+(Hy*YDy)/2., 'N/A', fontsize=18, ha='center')
			ax3.set_title('Y Spatial Loadings')
		else:
			ax = plt.subplot(grid[mm, 2], projection=ccrs.PlateCarree())

			if mpref=='CCA':  #skip if there are not predictand EOFs (e.g., PCR)
				ax.set_extent([loni2,loni2+Wy*XDy,lati2,lati2+Hy*YDy], ccrs.PlateCarree())
				#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
				f=open('../output/'+model+fprefix+'_'+mpref+'_YCCAMAP_'+training_season+'_wk'+str(wk)+'.dat','rb')

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
				CS=plt.pcolormesh(np.linspace(loni2, loni2+Wy*XDy,num=Wy), np.linspace(lati2+Hy*YDy, lati2, num=Hy), ccay[mode],
				#vmin=vmi,vmax=vma,
				vmin=-4,vmax=4,
				cmap=current_cmap,
				transform=ccrs.PlateCarree())
				label = 'CCA loadings'

			ax.add_feature(feature.LAND)
			ax.add_feature(states_provinces, edgecolor='gray')
			if str(use_ocean) == "True":
				ax.add_feature(feature.OCEAN)

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

			ax.set_title('Y Spatial Loadings')
			plt.subplots_adjust(bottom=0.15, top=0.9)
			cbar = plt.colorbar(CS, orientation='horizontal')
			cbar.set_label(label) #, rotation=270)

			ccax=[]
			#nrow=0
			#for model in models:
			#Read  grid
			with open('../output/'+model+fprefix+'_'+mpref+'_XCCAMAP_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
				for line in lines_that_contain("XDEF", fp):
					W = int(line.split()[1])
					XD= float(line.split()[4])
			with open('../output/'+model+fprefix+'_'+mpref+'_XCCAMAP_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
				for line in lines_that_contain("YDEF", fp):
					H = int(line.split()[1])
					YD= float(line.split()[4])
			ccax.append([])
			#m_ndx = models.index(model)
			m_ndx = 0
			s_ndx = 0
			ax = plt.subplot(grid[mm, 0], projection=ccrs.PlateCarree())
			if mpref=='PCR':
				ax.set_extent([loni1,loni1+W*XD,lati1,lati1+H*YD], ccrs.PlateCarree())  #EOF domains will look different between CCA and PCR if X and Y domains are different
			else:
				#ax.set_extent([loni,loni+Wy*XDy,lati,lati+Hy*YDy], ccrs.PlateCarree())
				ax.set_extent([loni1,loni1+W*XD,lati1,lati1+H*YD], ccrs.PlateCarree())

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
			ax.text(-0.35,0.5,model,rotation=90,verticalalignment='center', transform=ax.transAxes)

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

			ax.set_title('X Spatial Loadings')
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)

			f=open('../output/'+model+fprefix+'_'+mpref+'_XCCAMAP_'+training_season+'_wk'+str(wk)+'.dat','rb')
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
			CS=plt.pcolormesh(np.linspace(loni1, loni1+W*XD,num=W), np.linspace(lati1+H*YD, lati1, num=H),ccax[m_ndx][s_ndx][mode],
			#vmin=vmi,vmax=vma,
			vmin=-4,vmax=4,
			cmap=current_cmap,
			transform=ccrs.PlateCarree())
			label = 'CCA loadings'
			plt.subplots_adjust(hspace=0)
			cbar = plt.colorbar(CS, orientation='horizontal')
			cbar.set_label(label) #, rotation=270)

			#get correlations
			dss=pd.read_csv("../output/" +model+fprefix+ '_' + mpref + '_CCACorr_' +training_season+'_wk'+str(wk) + '.txt', delim_whitespace=True, header=2)
			daa=dss['correlation'].to_xarray()
			ccacorr=np.floor(daa.values[mode]*10000)/10000
			subtitle_string = "CCA Mode "+str(mode+1)+" Week "+str(wk)+" ("+model+"): Canonical correlation = "+str(ccacorr)
			ax.text(-0.35,1.2,subtitle_string,fontsize=14,transform=ax.transAxes)
			#plot time series
			f = open("../output/" +model+fprefix+ '_' + mpref + '_XCCATS_' + training_season+'_wk'+str(wk) + '.txt','r')
			f2 = open("../output/" +model+fprefix+ '_' + mpref + '_YCCATS_' + training_season+'_wk'+str(wk) + '.txt','r')

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
			ax.xaxis.set_ticks(np.arange(start-2, end, 10))

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



def pltmapdiff(score,loni,lone,lati,late,fprefix,mpref1,mpref2,training_season, mon, fday, nwk):
	"""A simple function for ploting differences of the skill scores

	PARAMETERS
	----------
		score: the score
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
		title: title
	"""

	plt.figure(figsize=(20,5))

	for L in range(nwk):
		wk=L+1
		#Read grads binary file size H, W  --it assumes all files have the same size, and that 2AFC exists
		with open('../output/'+fprefix+'_'+mpref1+'_2AFC_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				W = int(line.split()[1])
				XD= float(line.split()[4])
		with open('../output/'+fprefix+'_'+mpref1+'_2AFC_'+training_season+'_wk'+str(wk)+'.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				H = int(line.split()[1])
				YD= float(line.split()[4])

#		ax = plt.subplot(nwk/2, 2, wk, projection=ccrs.PlateCarree())
		ax = plt.subplot(1,nwk, wk, projection=ccrs.PlateCarree())
		ax.set_extent([loni,loni+W*XD,lati,lati+H*YD], ccrs.PlateCarree())

		#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
		states_provinces = feature.NaturalEarthFeature(
			category='cultural',
#			name='admin_1_states_provinces_shp',
			name='admin_0_countries',
			scale='10m',
			facecolor='none')

		ax.add_feature(feature.LAND)
		ax.add_feature(feature.COASTLINE)
		ax.set_title('Week '+str(wk))
		pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				  linewidth=2, color='gray', alpha=0., linestyle='--')
		pl.xlabels_top = False
		pl.ylabels_left = True
		pl.ylabels_right = False
		pl.xformatter = LONGITUDE_FORMATTER
		pl.yformatter = LATITUDE_FORMATTER
		ax.add_feature(states_provinces, edgecolor='gray')
		ax.set_ybound(lower=lati, upper=late)

		if score == 'CCAFCST_V' or score == 'PCRFCST_V' or score == 'noMOSFCST_V':
			f=open('../output/'+fprefix+'_'+score+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.dat','rb')
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			#Now we read the field
			A=np.fromfile(f,dtype='float32',count=numval)
			var = np.transpose(A.reshape((W, H), order='F'))
			var[var==-999.]=np.nan #only sensible values
			current_cmap = plt.cm.BrBG
			current_cmap.set_bad('white',1.0)
			current_cmap.set_under('white', 1.0)
			if fprefix == 'RFREQ':
				label ='Freq Rainy Days (days)'
				var=var/100 #weird 100 factor coming from CPT for frq rainy days!! ??
			elif fprefix == 'PRCP':
				label = 'Rainfall anomaly (mm/week)'
			CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				#vmin=-max(np.max(var),np.abs(np.min(var))), #vmax=np.max(var),
				norm=MidpointNormalize(midpoint=0.),
				cmap=current_cmap,
				transform=ccrs.PlateCarree())
			ax.set_title("Deterministic forecast for Week "+str(wk))
			f.close()
			#current_cmap = plt.cm.get_cmap()
			#current_cmap.set_bad(color='white')
			#current_cmap.set_under('white', 1.0)
		else:
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			f=open('../output/'+fprefix+'_'+mpref1+'_'+score+'_'+training_season+'_wk'+str(wk)+'.dat','rb')
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			#Now we read the field
			A=np.fromfile(f,dtype='float32',count=numval)
			var1 = np.transpose(A.reshape((W, H), order='F'))
			var1[var1==-999]=np.nan
			f=open('../output/'+fprefix+'_'+mpref2+'_'+score+'_'+training_season+'_wk'+str(wk)+'.dat','rb')
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			#Now we read the field
			A=np.fromfile(f,dtype='float32',count=numval)
			var2 = np.transpose(A.reshape((W, H), order='F'))
			var2[var2==-999]=np.nan
			var=var2-var1
			vmi=-max(np.nanmax(var),np.abs(np.nanmin(var)))
			vma=-vmi
			#define colorbars, depending on each score	--This can be easily written as a function
			if score == '2AFC':
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=vmi, vmax=vma,
				cmap=discrete_cmap(11, 'bwr'),
				transform=ccrs.PlateCarree())
				label = '2AFC (%)'
			if score == 'RocAbove' or score=='RocBelow':
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=vmi, vmax=vma,
				cmap=discrete_cmap(11, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'ROC area'
			if score == 'Spearman' or score=='Pearson':
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=vmi, vmax=vma,
				cmap=discrete_cmap(11, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'Correlation'
			if score == 'RPSS':
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=vmi, vmax=vma,
				cmap=discrete_cmap(20, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'RPSS (all categories)'
			if score=='GROC':
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=vmi, vmax=vma,
				cmap=discrete_cmap(11, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'GROC (probabilistic)'
			if score=='Ignorance':
				CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), var,
				vmin=vmi, vmax=vma,
				cmap=discrete_cmap(20, 'bwr'),
				transform=ccrs.PlateCarree())
				label = 'Ignorance (all categories)'

		plt.subplots_adjust(hspace=0)
		#plt.setp([a.get_xticklabels() for a in fig.axes[:-1]], visible=False)
		#cbar_ax = plt.add_axes([0.85, 0.15, 0.05, 0.7])
		#plt.tight_layout()
		plt.subplots_adjust(bottom=0.15, top=0.9)
		cax = plt.axes([0.2, 0.08, 0.6, 0.04])
		cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
		cbar.set_label(label) #, rotation=270)
		f.close()


def skilltab(model,score,wknam,lon1,lat1,lat2,lon2,loni,lone,lati,late,fprefix,mpref,training_season,mon,fday,nwk,wki):
	"""Creates a table with min, max and average values of skills computed over a certain domain

	PARAMETERS
	----------
		thrs: the threshold, in the units of the predictand
		lon: longitude
		lat: latitude
	"""

	#Read grads binary file size H, W  --it assumes all files have the same size, and that 2AFC exists
	with open('../output/'+model+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk1.ctl', "r") as fp:
		for line in lines_that_contain("XDEF", fp):
			W = int(line.split()[1])
			XD= float(line.split()[4])
	with open('../output/'+model+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk1.ctl', "r") as fp:
		for line in lines_that_contain("YDEF", fp):
			H = int(line.split()[1])
			YD= float(line.split()[4])

	#Find the gridbox:
	lonrange = np.linspace(loni, loni+(W-1)*XD,num=W)
	latrange = np.linspace(lati+(H-1)*YD, lati, num=H)  #need to reverse the latitudes because of CPT (GrADS YREV option)
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
			f=open('../output/'+model+fprefix+'_'+mpref+'_'+str(S)+'_'+training_season+'_wk'+str(wki[wk-1])+'.dat','rb')
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

def pltmapProbNC(loni,lone,lati,late,fprefix,mpref,training_season, mon, fday,nwk,wki):
	"""A simple function for ploting probabilistic forecasts from netcdf files
	[FOR NOW IT ONLY WORKS FOR ECMWF]
	"""
	plt.figure(figsize=(15,15))
	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
	#	name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')

	for L in range(nwk):
		wk=L+1

		#Read each tercile probabilities using the 3 different files downloaded from IRIDL
		nc_abo = Dataset('../input/noMOS/modelfcst_above_'+fprefix+'_'+mon+'_wk'+str(wki[wk-1])+'.nc', 'r')
		nc_bel = Dataset('../input/noMOS/modelfcst_below_'+fprefix+'_'+mon+'_wk'+str(wki[wk-1])+'.nc', 'r')
		nc_attrs, nc_dims, nc_vars = ncdump(nc_abo,verb=False)
		# Extract data from NetCDF file
		lats = nc_abo.variables['Y'][:]
		H = nc_abo.variables['Y'].size
		YD = 1.5 #ECMWF; in the future, read it from the Y:pointwidth attribute in the NC file
		lons = nc_abo.variables['X'][:]
		W = nc_abo.variables['X'].size
		XD = 1.5 #ECMWF; in the future, read it from the X:pointwidth attribute in the NC file
		probab = nc_abo.variables['flag'][:]
		probbe = nc_bel.variables['flag'][:]
		probno = [(x * 0.) + 100 for x in probab] - probab - probbe #we just compute the normal cat as the residual, to simplify things


		var=[probbe,probno,probab]

		tit=['Below Normal','Normal','Above Normal']
		for i in range(3):
			ax2=plt.subplot(nwk, 3, (L*3)+(i+1),projection=ccrs.PlateCarree())
			ax2.set_title("Week "+str(wki[wk-1])+ ": "+tit[i])
			ax2.add_feature(feature.LAND)
			ax2.add_feature(feature.COASTLINE)
			#ax2.set_ybound(lower=lati, upper=late)
			pl2=ax2.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
				linewidth=2, color='gray', alpha=0.5, linestyle='--')
			pl2.xlabels_top = False
			pl2.ylabels_left = True
			pl2.ylabels_right = False
			pl2.xformatter = LONGITUDE_FORMATTER
			pl2.yformatter = LATITUDE_FORMATTER
			ax2.add_feature(states_provinces, edgecolor='black')
			ax2.set_extent([loni,loni+W*XD,lati,lati+H*YD], ccrs.PlateCarree())

			ax2.set_ybound(lower=lati, upper=late)
			ax2.set_xbound(lower=loni, upper=lone)
			#ax2.set_adjustable('box')
			#ax2.set_aspect('auto',adjustable='datalim',anchor='C')
			CS=ax2.pcolormesh(np.linspace(lons[0], lons[-1],num=W), np.linspace(lats[0], lats[-1], num=H), np.squeeze(var[i]),
			vmin=0,vmax=100,
			cmap=plt.cm.bwr,
			transform=ccrs.PlateCarree())
			#plt.show(block=False)

	plt.subplots_adjust(hspace=0)
	plt.subplots_adjust(bottom=0.15, top=0.9)
	cax = plt.axes([0.2, 0.08, 0.6, 0.04])
	cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
	cbar.set_label('Probability (%)') #, rotation=270)


def pltmapProb( model,loni,lone,lati,late,fprefix,mpref,training_season, mon, fday, nwk, wki):
	"""A simple function for ploting probabilistic forecasts

	PARAMETERS
	----------
		score: the score
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
		title: title
	"""
	if mpref=='noMOS' and fprefix=='PRCP':
		pltmapProbNC(loni,lone,lati,late,fprefix,mpref,training_season, mon, fday, nwk,wki)
	else:
		#Need this score to be defined by the calibration method!!!
		score = mpref+'FCST_P'

		plt.figure(figsize=(15,15))
		#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
		states_provinces = feature.NaturalEarthFeature(
			category='cultural',
	#		name='admin_1_states_provinces_shp',
			name='admin_0_countries',
			scale='10m',
			facecolor='none')

		for L in range(nwk):
			wk=L+1
			#Read grads binary file size H, W  --it assumes that 2AFC file exists (template for final domain size)
			with open('../output/'+model+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk'+str(wki[wk-1])+'.ctl', "r") as fp:
				for line in lines_that_contain("XDEF", fp):
					W = int(line.split()[1])
					XD= float(line.split()[4])
			with open('../output/'+model+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk'+str(wki[wk-1])+'.ctl', "r") as fp:
				for line in lines_that_contain("YDEF", fp):
					H = int(line.split()[1])
					YD= float(line.split()[4])

			#Prepare to read grads binary file  [float32 for Fortran sequential binary files]
			Record = np.dtype(('float32', H*W))

			#B = np.fromfile('../output/'+fprefix+'_'+score+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.dat',dtype=Record, count=-1).astype('float')
			f=open('../output/'+model+fprefix+'_'+score+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki[wk-1])+'.dat','rb')

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
					ax2.set_title("Week "+str(wki[wk-1])+ ": "+tit[i])
					ax2.add_feature(feature.LAND)
					ax2.add_feature(feature.COASTLINE)
					#ax2.set_ybound(lower=lati, upper=late)
					pl2=ax2.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
						linewidth=2, color='gray', alpha=0.5, linestyle='--')
					pl2.xlabels_top = False
					pl2.ylabels_left = True
					pl2.ylabels_right = False
					pl2.xformatter = LONGITUDE_FORMATTER
					pl2.yformatter = LATITUDE_FORMATTER
					ax2.add_feature(states_provinces, edgecolor='gray')
					ax2.set_extent([loni,loni+W*XD,lati,lati+H*YD], ccrs.PlateCarree())

					#ax2.set_ybound(lower=lati, upper=late)
					#ax2.set_xbound(lower=loni, upper=lone)
					#ax2.set_adjustable('box')
					#ax2.set_aspect('auto',adjustable='datalim',anchor='C')
					CS=ax2.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati,lati+H*YD, num=H), var,
					vmin=0,vmax=100,
					cmap=discrete_cmap(20, 'YlGnBu'),
					transform=ccrs.PlateCarree())
					#plt.show(block=False)

		plt.subplots_adjust(hspace=0)
		plt.subplots_adjust(bottom=0.15, top=0.9)
		cax = plt.axes([0.2, 0.08, 0.6, 0.04])
		cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
		cbar.set_label('Probability (%)') #, rotation=270)
		f.close()

def pltmapffNC(thrs,ispctl,ntrain,loni,lone,lati,late,fprefix,mpref,training_season,mon,fday,nwk,wki):
	"""A simple function for ploting probabilistic forecasts in flexible format (for a given threshold)
	using netcdf files
	[FOR NOW, IT ONLY WORKS FOR ECMWF]

	PARAMETERS
	----------
		thrs: the threshold, in the units of the predictand
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
	"""
	#Implement: read degrees of freedom from CPT file
	#Formally, for CCA, dof=ntrain - #CCAmodes -1 ; since ntrain is huge after concat, dof~=ntrain for now
	dof=20

	plt.figure(figsize=(15,15))

	if ispctl:
		thrso=thrs
		thrst = [x * 100 for x in thrs]

	#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
	states_provinces = feature.NaturalEarthFeature(
		category='cultural',
	#	name='admin_1_states_provinces_shp',
		name='admin_0_countries',
		scale='10m',
		facecolor='none')

	for L in range(nwk):
		wk=L+1

		#Read mu and sigma (average and std) directly from the NC files
		nc_fmu  = Dataset('../input/noMOS/modelfcst_mu_'+fprefix+'_'+mon+'_wk'+str(wki[wk-1])+'.nc', 'r')
		nc_fstd = Dataset('../input/noMOS/modelfcst_std_'+fprefix+'_'+mon+'_wk'+str(wki[wk-1])+'.nc', 'r')
		nc_omu  = Dataset('../input/noMOS/obs_mu_'+fprefix+'_'+mon+'_wk'+str(wki[wk-1])+'.nc', 'r')
		nc_ostd = Dataset('../input/noMOS/obs_std_'+fprefix+'_'+mon+'_wk'+str(wki[wk-1])+'.nc', 'r')
		nc_attrs, nc_dims, nc_vars = ncdump(nc_fmu,verb=False)
		# Extract data from NetCDF file
		lats = nc_fmu.variables['Y'][:]
		H = nc_fmu.variables['Y'].size
		YD = 1.5 #ECMWF; in the future, read it from the Y:pointwidth attribute in the NC file
		lons = nc_fmu.variables['X'][:]
		W = nc_fmu.variables['X'].size
		XD = 1.5 #ECMWF; in the future, read it from the X:pointwidth attribute in the NC file
		muf = np.squeeze(nc_fmu.variables['ratio'][:])
		vari = (np.squeeze(nc_fstd.variables['ratio'][:]))**2
		muc = np.squeeze(nc_omu.variables['tp'][:])
		varc = (np.squeeze(nc_ostd.variables['tp'][:]))**2

		#Compute scale parameter for the t-Student distribution
		scalef=np.sqrt(dof*vari)   #due to transformation from Gamma
		scalec=np.sqrt((dof-2)/dof*varc)

		if ispctl:
			thrs[wk-1]=t.ppf(thrso[wk-1], dof, loc=muc, scale=scalec)  #If using percentiles, compute value using climo

		fprob = exceedprob(thrs[wk-1],dof,muf,scalef)

		if (nwk % 2) == 0:  #is nwk even or odd?
			ax = plt.subplot(nwk/2, 2, wk, projection=ccrs.PlateCarree())
		else:
			ax = plt.subplot(1, nwk, wk, projection=ccrs.PlateCarree())  #odd nwk case
		ax.set_extent([loni,loni+W*XD,lati,lati+H*YD], ccrs.PlateCarree())

		ax.add_feature(feature.LAND)
		ax.add_feature(feature.COASTLINE)
		if ispctl:
			ax.set_title('Probability (%) of exceeding the '+str(int(thrst[wk-1]))+'th percentile for Week '+str(wki[wk-1]))
		else:
			ax.set_title('Probability (%) of exceeding '+str(thrs[wk-1])+" mm/week"+' for Week '+str(wki[wk-1]))

		pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
		 	linewidth=2, color='gray', alpha=0.5, linestyle='--')
		pl.xlabels_top = False
		pl.ylabels_left = True
		pl.ylabels_right = False
		pl.xformatter = LONGITUDE_FORMATTER
		pl.yformatter = LATITUDE_FORMATTER
		ax.add_feature(states_provinces, edgecolor='gray')
		ax.set_ybound(lower=lati, upper=late)
		CS=plt.pcolormesh(np.linspace(lons[0], lons[-1],num=W), np.linspace(lats[0], lats[-1], num=H), np.squeeze(fprob),
    		vmin=0,vmax=100,
    		cmap=plt.cm.bwr,
    		transform=ccrs.PlateCarree())
		label = 'Probability (%) of Exceedance'

		plt.subplots_adjust(hspace=0)
		plt.subplots_adjust(bottom=0.15, top=0.9)
		cax = plt.axes([0.2, 0.08, 0.6, 0.04])
		cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
		cbar.set_label(label) #, rotation=270)

def pltmapff(model,thrs,ispctl,ntrain,loni,lone,lati,late,fprefix,mpref,training_season,mon,fday,nwk,wki):
	"""A simple function for ploting probabilistic forecasts in flexible format (for a given threshold)

	PARAMETERS
	----------
		thrs: the threshold, in the units of the predictand
		loni: western longitude
		lone: eastern longitude
		lati: southern latitude
		late: northern latitude
	"""
	if mpref=='noMOS' and fprefix=='PRCP':
		pltmapffNC(thrs,ispctl,ntrain,loni,lone,lati,late,fprefix,mpref,training_season,mon,fday,nwk,wki)
	else:
		#Implement: read degrees of freedom from CPT file
		#Formally, for CCA, dof=ntrain - #CCAmodes -1 ; since ntrain is huge after concat, dof~=ntrain for now
		dof=ntrain

		#Read grads binary file size H, W  --it assumes all files have the same size
		with open('../output/'+model+fprefix+'_'+mpref+'FCST_mu_'+training_season+'_'+str(mon)+str(fday)+'_wk1.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				W = int(line.split()[1])
				XD= float(line.split()[4])
		with open('../output/'+model+fprefix+'_'+mpref+'FCST_mu_'+training_season+'_'+str(mon)+str(fday)+'_wk1.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				H = int(line.split()[1])
				YD= float(line.split()[4])
		with open('../output/'+model+fprefix+'_'+mpref+'FCST_Obs_'+training_season+'_'+str(mon)+str(fday)+'_wk1.ctl', "r") as fp:
			for line in lines_that_contain("TDEF", fp):
				T = int(line.split()[1])
				TD= 1  #not used

		plt.figure(figsize=(15,15))

		if ispctl:
			thrso=thrs
			thrst = [x * 100 for x in thrs]

		for L in range(nwk):
			wk=L+1
			#Read mean
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			#print(model+fprefix+'_'+mpref+'FCST_mu_'+training_season+'_'+str(mon)+str(fday)+'_wk'+str(wki[wk-1])+'.dat')
			f=open('../output/'+model+fprefix+'_'+mpref+'FCST_mu_'+training_season+'_'+str(mon)+str(fday)+'_wk'+str(wki[wk-1])+'.dat','rb')
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			#Now we read the field
			A=np.fromfile(f,dtype='float32',count=numval)
			muf = np.transpose(A.reshape((W, H), order='F'))
			muf[muf==-999.]=np.nan #only sensible values
			# if fprefix=='RFREQ':
			# 	muf=muf/100

			#Read variance
			f=open('../output/'+model+fprefix+'_'+mpref+'FCST_var_'+training_season+'_'+str(mon)+str(fday)+'_wk'+str(wki[wk-1])+'.dat','rb')
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			#Now we read the field
			A=np.fromfile(f,dtype='float32',count=numval)
			vari = np.transpose(A.reshape((W, H), order='F'))
			vari[vari==-999.]=np.nan #only sensible values
			# if fprefix=='RFREQ':
			# 	vari=vari/100

			#Obs file--------
			#Compute obs mean and variance.
			#
			muc0=np.empty([T,H,W])  #define array for later use
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			f=open('../output/'+model+fprefix+'_'+mpref+'FCST_Obs_'+training_season+'_'+str(mon)+str(fday)+'_wk'+str(wki[wk-1])+'.dat','rb')
			print(model+fprefix+'_'+mpref+'FCST_Obs_'+training_season+'_'+str(mon)+str(fday)+'_wk'+str(wki[wk-1])+'.dat')
			#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
			for it in range(T):
				#Now we read the field
				
				recl=struct.unpack('i',f.read(4))[0]
				numval=int(recl/np.dtype('float32').itemsize) #this if for each time stamp
				A0=np.fromfile(f,dtype='float32',count=numval)
				endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
				muc0[it,:,:]= np.transpose(A0.reshape((W, H), order='F'))

			muc0[muc0==-999.]=np.nan #identify NaNs
			muc=np.nanmean(muc0, axis=0)  #axis 0 is T
			#Compute obs variance
			varc=np.nanvar(muc0, axis=0)  #axis 0 is T

			#Compute scale parameter for the t-Student distribution
			scalef=np.sqrt(dof*vari)   #due to transformation from Gamma
			scalec=np.sqrt((dof-2)/dof*varc)

			if ispctl:
				thrs[wk-1]=t.ppf(thrso[wk-1], dof, loc=muc, scale=scalec)  #If using percentiles, compute value using climo

			fprob = exceedprob(thrs[wk-1],dof,muf,scalef)

			if (nwk % 2) == 0:  #is nwk even or odd?
				ax = plt.subplot(nwk/2, 2, wk, projection=ccrs.PlateCarree())
			else:
				ax = plt.subplot(nwk, 1, wk, projection=ccrs.PlateCarree())  #odd nwk case

			ax.set_extent([loni,loni+W*XD,lati,lati+H*YD], ccrs.PlateCarree())

			#Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
			states_provinces = feature.NaturalEarthFeature(
				category='cultural',
	#			name='admin_1_states_provinces_shp',
				name='admin_0_countries',
				scale='10m',
				facecolor='none')

			ax.add_feature(feature.LAND)
			ax.add_feature(feature.COASTLINE)
			if ispctl:
				ax.set_title('Probability (%) of exceeding the '+str(int(thrst[wk-1]))+'th percentile for Week '+str(wki[wk-1]))
			else:
				ax.set_title('Probability (%) of exceeding '+str(thrs[wk-1])+" mm/week"+' for Week '+str(wki[wk-1]))

			pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
			 	linewidth=2, color='gray', alpha=0.5, linestyle='--')
			pl.xlabels_top = False
			pl.ylabels_left = True
			pl.ylabels_right = False
			pl.xformatter = LONGITUDE_FORMATTER
			pl.yformatter = LATITUDE_FORMATTER
			ax.add_feature(states_provinces, edgecolor='gray')
			ax.set_ybound(lower=lati, upper=late)
			CS=plt.pcolormesh(np.linspace(loni, loni+W*XD,num=W), np.linspace(lati+H*YD, lati, num=H), fprob,
	    		vmin=0,vmax=100,
	    		cmap=plt.cm.bwr,
	    		transform=ccrs.PlateCarree())
			label = 'Probability (%) of Exceedance'

			plt.subplots_adjust(hspace=0)
			plt.subplots_adjust(bottom=0.15, top=0.9)
			cax = plt.axes([0.2, 0.08, 0.6, 0.04])
			cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
			cbar.set_label(label) #, rotation=270)
			f.close()

def pltprobffNC(thrsn,ispctl,ntrain,lon,lat,loni,lone,lati,late,fprefix,mpref,training_season,mon,fday,nwk,wki):
	"""A simple function for ploting probabilities of exceedance and PDFs (for a given threshold)

	PARAMETERS
	----------
		thrs: the threshold, in the units of the predictand
		lon: longitude
		lat: latitude
	"""
	#Implement: read degrees of freedom from CPT file
	#Formally, for CCA, dof=ntrain - #CCAmodes -1 ; since ntrain is huge after concat, dof~=ntrain for now
	dof=20
	thrs=thrsn

	nc_fmu  = Dataset('../input/noMOS/modelfcst_mu_'+fprefix+'_'+mon+'_wk1.nc', 'r')
	#nc_attrs, nc_dims, nc_vars = ncdump(nc_fmu,verb=False)
	# Extract data from NetCDF file
	lats = nc_fmu.variables['Y'][:]
	H = nc_fmu.variables['Y'].size
	YD = 1.5 #ECMWF; in the future, read it from the Y:pointwidth attribute in the NC file
	lons = nc_fmu.variables['X'][:]
	W = nc_fmu.variables['X'].size
	XD = 1.5 #ECMWF; in the future, read it from the X:pointwidth attribute in the NC file

	#Find the gridbox:
	lonrange = np.linspace(lons[0], lons[-1],num=W)
	latrange = np.linspace(lats[0], lats[-1], num=H)  #need to reverse the latitudes because of CPT (GrADS YREV option)
	lon_grid, lat_grid = np.meshgrid(lonrange, latrange)
	a = abs(lat_grid-lat)+abs(lon_grid-lon)
	i,j = np.unravel_index(a.argmin(),a.shape)   #i:latitude   j:longitude

	#Now compute stuff and plot
	plt.figure(figsize=(15,15))

	thrso=thrs

	#Fix x-axis to that of week 4, expected to have higher spread
		#Read mu and sigma (average and std) directly from the NC files
	nc_fmu  = Dataset('../input/noMOS/modelfcst_mu_'+fprefix+'_'+mon+'_wk'+str(wki[3])+'.nc', 'r')
	nc_fstd = Dataset('../input/noMOS/modelfcst_std_'+fprefix+'_'+mon+'_wk'+str(wki[3])+'.nc', 'r')
	nc_omu  = Dataset('../input/noMOS/obs_mu_'+fprefix+'_'+mon+'_wk'+str(wki[3])+'.nc', 'r')
	nc_ostd = Dataset('../input/noMOS/obs_std_'+fprefix+'_'+mon+'_wk'+str(wki[3])+'.nc', 'r')
	nc_attrs, nc_dims, nc_vars = ncdump(nc_fmu,verb=False)
		# Extract data from NetCDF file
	muf4 = np.squeeze(nc_fmu.variables['ratio'][:])
	muf4 = muf4[i,j]
	varf4 = (np.squeeze(nc_fstd.variables['ratio'][:]))**2
	varf4 = varf4[i,j]
	muc4 = np.squeeze(nc_omu.variables['tp'][:])
	muc4 = muc4[i,j]
	varc4 = (np.squeeze(nc_ostd.variables['tp'][:]))**2
	varc4 = varc4[i,j]
		#Compute scale parameter for the t-Student distribution
	scalef4=np.sqrt(dof*varf4)   #due to transformation from Gamma
	scalec4=np.sqrt((dof-2)/dof*varc4)
	x = np.linspace(min(t.ppf(0.00001, dof, loc=muf4, scale=scalef4),t.ppf(0.00001, dof, loc=muc4, scale=scalec4)),max(t.ppf(0.9999, dof, loc=muf4, scale=scalef4),t.ppf(0.9999, dof, loc=muc4, scale=scalec4)), 100)


	for L in range(nwk):
		wk=L+1
		#Read mu and sigma (average and std) directly from the NC files
		nc_fmu  = Dataset('../input/noMOS/modelfcst_mu_'+fprefix+'_'+mon+'_wk'+str(wki[wk-1])+'.nc', 'r')
		nc_fstd = Dataset('../input/noMOS/modelfcst_std_'+fprefix+'_'+mon+'_wk'+str(wki[wk-1])+'.nc', 'r')
		nc_omu  = Dataset('../input/noMOS/obs_mu_'+fprefix+'_'+mon+'_wk'+str(wki[wk-1])+'.nc', 'r')
		nc_ostd = Dataset('../input/noMOS/obs_std_'+fprefix+'_'+mon+'_wk'+str(wki[wk-1])+'.nc', 'r')
		nc_attrs, nc_dims, nc_vars = ncdump(nc_fmu,verb=False)
		# Extract data from NetCDF file
		muf = np.squeeze(nc_fmu.variables['ratio'][:])
		muf=muf[i,j]
		varf = (np.squeeze(nc_fstd.variables['ratio'][:]))**2
		varf=varf[i,j]
		muc = np.squeeze(nc_omu.variables['tp'][:])
		muc=muc[i,j]
		varc = (np.squeeze(nc_ostd.variables['tp'][:]))**2
		varc=varc[i,j]

		#Compute scale parameter for the t-Student distribution
		scalef=np.sqrt(dof*varf)   #due to transformation from Gamma
		scalec=np.sqrt((dof-2)/dof*varc)

		if ispctl:
			thrs[wk-1]=t.ppf(thrso[wk-1], dof, loc=muc, scale=scalec)  #If using percentiles, compute value using climo
			#print('Week '+str(wk)+': percentile '+str(int(thrso[wk-1]))+' is '+str(np.round(thrs[wk-1]))+' mm')

		#Original case: dynamic x-axis
		#x = np.linspace(min(t.ppf(0.00001, dof, loc=muf, scale=scalef),t.ppf(0.00001, dof, loc=muc, scale=scalec)),max(t.ppf(0.9999, dof, loc=muf, scale=scalef),t.ppf(0.9999, dof, loc=muc, scale=scalec)), 100)

		style = dict(size=10, color='black')

		#cprob = special.erfc((x-muc)/scalec)
		cprob = exceedprob(thrs[wk-1],dof,muc,scalec)
		fprob = exceedprob(thrs[wk-1],dof,muf,scalef)
		cprobth = np.round(t.sf(thrs[wk-1], dof, loc=muc, scale=scalec)*100,2)
		fprobth = np.round(t.sf(thrs[wk-1], dof, loc=muf, scale=scalef)*100,2)
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
		plt.axvline(x=thrs[wk-1], color='k', linestyle='--')
		plt.plot(thrs[wk-1], fprobth,'ok')
		plt.plot(thrs[wk-1], cprobth,'ok')
		plt.text(thrs[wk-1]+0.05, cprobth, str(cprobth)+'%', **style)
		plt.text(thrs[wk-1]+0.05, fprobth, str(fprobth)+'%', **style)
		#plt.text(0.1, 10, r'$\frac{P(fcst)}{P(clim)}=$'+str(round(oddsrc,1)), **style)
		plt.text(min(t.ppf(0.0001, dof, loc=muf4, scale=scalef4),t.ppf(0.0001, dof, loc=muc4, scale=scalec4)), -20, 'P(fcst)/P(clim)='+str(round(oddsrc,1)), **style)
		plt.legend(loc='best', frameon=False)
		# Add title and axis names
		plt.title('Probabilities of Exceedance for Week '+str(wk))
		plt.xlabel('Rainfall')
		plt.ylabel('Probability (%)')
		# Limits for the X axis
		plt.xlim(min(t.ppf(0.00001, dof, loc=muf4, scale=scalef4),t.ppf(0.00001, dof, loc=muc4, scale=scalec4)),max(t.ppf(0.9999, dof, loc=muf4, scale=scalef4),t.ppf(0.9999, dof, loc=muc4, scale=scalec4)))

		plt.subplot(1, 2, 2)
		plt.plot(x, cpdf,'b-', lw=5, alpha=0.6, label='clim')
		plt.plot(x, fpdf,'r-', lw=5, alpha=0.6, label='fcst')
		plt.axvline(x=thrs[wk-1], color='k', linestyle='--')
		#fill area under the curve --not done
		#section = np.arange(min(t.ppf(0.00001, dof, loc=muf, scale=scalef),t.ppf(0.00001, dof, loc=muc, scale=scalec)), thrs, 1/20.)
		#plt.fill_between(section,f(section))
		plt.legend(loc='best', frameon=False)
		# Add title and axis names
		plt.title('Probability Density Functions for Week '+str(wk))
		plt.xlabel('Rainfall')
		plt.ylabel('Density')
		# Limits for the X axis
		plt.xlim(min(t.ppf(0.00001, dof, loc=muf4, scale=scalef4),t.ppf(0.00001, dof, loc=muc4, scale=scalec4)),max(t.ppf(0.9999, dof, loc=muf4, scale=scalef4),t.ppf(0.9999, dof, loc=muc4, scale=scalec4)))

	plt.subplots_adjust(hspace=0)
	plt.subplots_adjust(bottom=0.15, top=0.9)
	#cax = plt.axes([0.2, 0.08, 0.6, 0.04])
	#cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
	#cbar.set_label(label) #, rotation=270)

def pltprobff(model,thrsn,ispctl,ntrain,lon,lat,loni,lone,lati,late,fprefix,mpref,training_season,mon,fday,nwk,wki):
	"""A simple function for ploting probabilities of exceedance and PDFs (for a given threshold)

	PARAMETERS
	----------
		thrs: the threshold, in the units of the predictand
		lon: longitude
		lat: latitude
	"""
	if mpref=='noMOS' and fprefix=='PRCP':
		pltprobffNC(thrsn,ispctl,ntrain,lon,lat,loni,lone,lati,late,fprefix,mpref,training_season,mon,fday,nwk,wki)
	else:
		#Implement: read degrees of freedom from CPT file
		#Formally, for CCA, dof=ntrain - #CCAmodes -1 ; since ntrain is huge after concat, dof~=ntrain for now
		dof=ntrain
		thrs=thrsn


		#Read grads binary file size H, W  --it assumes all files have the same size, and that 2AFC exists
		with open('../output/'+model+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk1.ctl', "r") as fp:
			for line in lines_that_contain("XDEF", fp):
				W = int(line.split()[1])
				XD= float(line.split()[4])
		with open('../output/'+model+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk1.ctl', "r") as fp:
			for line in lines_that_contain("YDEF", fp):
				H = int(line.split()[1])
				YD= float(line.split()[4])
		with open('../output/'+model+fprefix+'_'+mpref+'FCST_Obs_'+training_season+'_'+str(mon)+str(fday)+'_wk1.ctl', "r") as fp:
			for line in lines_that_contain("TDEF", fp):
				T = int(line.split()[1])
				TD= 1  #not used

		#Find the gridbox:
		lonrange = np.linspace(loni, loni+W*XD,num=W)
		latrange = np.linspace(lati+H*YD, lati, num=H)  #need to reverse the latitudes because of CPT (GrADS YREV option)
		lon_grid, lat_grid = np.meshgrid(lonrange, latrange)
		a = abs(lat_grid-lat)+abs(lon_grid-lon)
		i,j = np.unravel_index(a.argmin(),a.shape)   #i:latitude   j:longitude

		#Now compute stuff and plot
		plt.figure(figsize=(15,15))

		thrso=thrs

		for L in range(nwk):
			wk=L+1
			#Forecast files--------
			#Read mean
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			f=open('../output/'+model+fprefix+'_'+mpref+'FCST_mu_'+training_season+'_'+str(mon)+str(fday)+'_wk'+str(wki[wk-1])+'.dat','rb')
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			#Now we read the field
			A=np.fromfile(f,dtype='float32',count=numval)
			muf = np.transpose(A.reshape((W, H), order='F'))
			muf[muf==-999.]=np.nan #identify NaNs
			muf=muf[i,j]
			if fprefix=='RFREQ':
				muf=muf/100

			#Read variance
			f=open('../output/'+model+fprefix+'_'+mpref+'FCST_var_'+training_season+'_'+str(mon)+str(fday)+'_wk'+str(wki[wk-1])+'.dat','rb')
			recl=struct.unpack('i',f.read(4))[0]
			numval=int(recl/np.dtype('float32').itemsize)
			#Now we read the field
			A=np.fromfile(f,dtype='float32',count=numval)
			varf = np.transpose(A.reshape((W, H), order='F'))
			varf[varf==-999.]=np.nan #identify NaNs
			varf=varf[i,j]
			if fprefix=='RFREQ':
				varf=varf/10000

			#Obs file--------
			#Compute obs mean and variance.
			#
			muc0=np.empty([T,H,W])  #define array for later use
			#Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
			f=open('../output/'+model+fprefix+'_'+mpref+'FCST_Obs_'+training_season+'_'+str(mon)+str(fday)+'_wk'+str(wki[wk-1])+'.dat','rb')
			#cycle for all time steps  (same approach to read GrADS files as before, but now read T times)
			for it in range(T):
				#Now we read the field
				recl=struct.unpack('i',f.read(4))[0]
				numval=int(recl/np.dtype('float32').itemsize) #this if for each time stamp
				A0=np.fromfile(f,dtype='float32',count=numval)
				endrec=struct.unpack('i',f.read(4))[0]  #needed as Fortran sequential repeats the header at the end of the record!!!
				muc0[it,:,:]= np.transpose(A0.reshape((W, H), order='F'))

			muc0[muc0==-999.]=np.nan #identify NaNs
			muc=np.nanmean(muc0, axis=0)  #axis 0 is T
			#Compute obs variance
			varc=np.nanvar(muc0, axis=0)  #axis 0 is T
			#Select gridbox values
			muc=muc[i,j]
			#print(muc)   #Test it's actually zero
			varc=varc[i,j]

			#Compute scale parameter for the t-Student distribution
			scalef=np.sqrt(dof*varf)   #due to transformation from Gamma
			scalec=np.sqrt((dof-2)/dof*varc)

			if ispctl:
				thrs[wk-1]=t.ppf(thrso[wk-1], dof, loc=muc, scale=scalec)  #If using percentiles, compute value using climo
				#print('Week '+str(wk)+': percentile '+str(int(thrso[wk-1]))+' is '+str(np.round(thrs[wk-1]))+' mm')

			x = np.linspace(min(t.ppf(0.00001, dof, loc=muf, scale=scalef),t.ppf(0.00001, dof, loc=muc, scale=scalec)),max(t.ppf(0.9999, dof, loc=muf, scale=scalef),t.ppf(0.9999, dof, loc=muc, scale=scalec)), 100)

			style = dict(size=10, color='black')

			#cprob = special.erfc((x-muc)/scalec)
			cprob = exceedprob(thrs[wk-1],dof,muc,scalec)
			fprob = exceedprob(thrs[wk-1],dof,muf,scalef)
			cprobth = np.round(t.sf(thrs[wk-1], dof, loc=muc, scale=scalec)*100,2)
			fprobth = np.round(t.sf(thrs[wk-1], dof, loc=muf, scale=scalef)*100,2)
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
			plt.axvline(x=thrs[wk-1], color='k', linestyle='--')
			plt.plot(thrs[wk-1], fprobth,'ok')
			plt.plot(thrs[wk-1], cprobth,'ok')
			plt.text(thrs[wk-1]+0.05, cprobth, str(cprobth)+'%', **style)
			plt.text(thrs[wk-1]+0.05, fprobth, str(fprobth)+'%', **style)
			#plt.text(0.1, 10, r'$\frac{P(fcst)}{P(clim)}=$'+str(round(oddsrc,1)), **style)
			plt.text(min(t.ppf(0.0001, dof, loc=muf, scale=scalef),t.ppf(0.0001, dof, loc=muc, scale=scalec)), -20, 'P(fcst)/P(clim)='+str(round(oddsrc,1)), **style)
			plt.legend(loc='best', frameon=False)
			# Add title and axis names
			plt.title('Probabilities of Exceedance for Week '+str(wki[wk-1]))
			if fprefix=='PRCP':
				plt.xlabel('Rainfall')
			elif fprefix=='RFREQ':
				plt.xlabel('Rainfall freq.')
			plt.ylabel('Probability (%)')
			# Limits for the X axis
			plt.xlim(min(t.ppf(0.00001, dof, loc=muf, scale=scalef),t.ppf(0.00001, dof, loc=muc, scale=scalec)),max(t.ppf(0.9999, dof, loc=muf, scale=scalef),t.ppf(0.9999, dof, loc=muc, scale=scalec)))

			plt.subplot(1, 2, 2)
			plt.plot(x, cpdf,'b-', lw=5, alpha=0.6, label='clim')
			plt.plot(x, fpdf,'r-', lw=5, alpha=0.6, label='fcst')
			plt.axvline(x=thrs[wk-1], color='k', linestyle='--')
			#fill area under the curve --not done
			#section = np.arange(min(t.ppf(0.00001, dof, loc=muf, scale=scalef),t.ppf(0.00001, dof, loc=muc, scale=scalec)), thrs, 1/20.)
			#plt.fill_between(section,f(section))
			plt.legend(loc='best', frameon=False)
			# Add title and axis names
			plt.title('Probability Density Functions for Week '+str(wk))
			if fprefix=='PRCP':
				plt.xlabel('Rainfall')
			elif fprefix=='RFREQ':
				plt.xlabel('Rainfall freq.')
			plt.ylabel('Density')
			# Limits for the X axis
			plt.xlim(min(t.ppf(0.00001, dof, loc=muf, scale=scalef),t.ppf(0.00001, dof, loc=muc, scale=scalec)),max(t.ppf(0.9999, dof, loc=muf, scale=scalef),t.ppf(0.9999, dof, loc=muc, scale=scalec)))

		plt.subplots_adjust(hspace=0)
		plt.subplots_adjust(bottom=0.15, top=0.9)
		#cax = plt.axes([0.2, 0.08, 0.6, 0.04])
		#cbar = plt.colorbar(CS,cax=cax, orientation='horizontal')
		#cbar.set_label(label) #, rotation=270)
		f.close()

def readNetCDF_predictand(infile,outfile, predictand, wlo2, elo2, sla2, nla2, tar):
	"""Function to read the user's predictand NetCDF file and write to CPT format.

	PARAMETERS
	----------
	predictand: a DataArray with dimensions T,Y,X
	"""

	ds=xr.open_dataset(infile,decode_times=False)
	da=list(ds.coords)

	for i in range(len(da)):
		if da[i]=='X' or da[i]=='lon' or da[i]=='longitude':
			ds = ds.rename({da[i]:'X'})
		if da[i]=='Y' or da[i]=='lat' or da[i]=='latitude':
			ds = ds.rename({da[i]:'Y'})
		if da[i]=='T' or da[i]=='time':
			deltastyr=int(ds[da[i]][0]/12)
			ds = ds.rename({da[i]:'time'})
			nmon=ds.time.shape[0]
			nyr=int(nmon/12)
			if 'months since' in ds.time.units:
				line=ds.time.units
				stdate=str(int(line.split()[2][:4])+deltastyr)+line.split()[2][-6:]
				ds['time'] = pd.date_range(stdate, periods=ds.time.shape[0], freq='M')

#	ds1=ds.sel(X=slice(wlo2,elo2),Y=slice(sla2,nla2))
	ds1_tmp=ds.sel(X=slice(wlo2,elo2),Y=slice(sla2,nla2))
	ds1=ds1_tmp.reindex(Y=ds1_tmp.Y[::-1]) #Y from N to S
	Xarr=ds1.X.values
	Yarr=ds1.Y.values
	W=ds1.X.shape[0]
	H=ds1.Y.shape[0]
	var1=ds1[predictand]
	units=ds[predictand].units
	Ti=int(ds.time.dt.year[0])
	vari = predictand
	varname = vari
	if 'True' in np.isnan(var):
	        var[np.isnan(var)]=-999. #use CPT missing value

	monthdic = {'Jan':'01','Feb':'02','Mar':'03','Apr':'04','May':'05','Jun':'06','Jul':'07','Aug':'08','Sep':'09','Oct':'10','Nov':'11','Dec':'12'}
	mi=monthdic[tar.split("-")[0]]
	mf=monthdic[tar.split("-")[1]]

	if mi==str(11):
		var1_N=var1[(var1.time.dt.month==11)]
		var1_N1=var1_N.groupby(var1_N.time.dt.year).mean('time').sel(year=slice(Ti,Ti+nyr-2))
		var1_D=var1[(var1.time.dt.month==12)]
		var1_D1=var1_D.groupby(var1_D.time.dt.year).mean('time').sel(year=slice(Ti,Ti+nyr-2))
		var1_J=var1[(var1.time.dt.month==1)]
		var1_J1=var1_J.groupby(var1_J.time.dt.year).mean('time').sel(year=slice(Ti+1,Ti+nyr-1))
		var=np.zeros(var1_D1.shape)
		for i in range(len(var1_D1.year)):
			var[i,:,:]=(var1_N1[i,:,:]+var1_D1[i,:,:]+var1_J1[i,:,:])/3.
	elif mi==str(12):
		var1_D=var1[(var1.time.dt.month==12)]
		var1_D1=var1_D.groupby(var1_D.time.dt.year).mean('time').sel(year=slice(Ti,Ti+nyr-2))
		var1_J=var1[(var1.time.dt.month==1)]
		var1_J1=var1_J.groupby(var1_J.time.dt.year).mean('time').sel(year=slice(Ti+1,Ti+nyr-1))
		var1_F = var1[(var1.time.dt.month==2)]
		var1_F1=var1_F.groupby(var1_F.time.dt.year).mean('time').sel(year=slice(Ti+1,Ti+nyr-1))
		var=np.zeros(var1_D1.shape)
		for i in range(len(var1_D1.year)):
			var[i,:,:]=(var1_D1[i,:,:]+var1_J1[i,:,:]+var1_F1[i,:,:])/3.
	else:
		var1_season = var1[(var1.time.dt.month>=mi)&(var1.time.dt.month<=mf)]
		var=var1_season.groupby(var1_season.time.dt.year).mean(dim=('time')).sel(year=slice(Ti+1,Ti+nyr-1))
	if tar=='Dec-Feb' or tar=='Nov-Jan':  #double check years are sync
		Ti=Ti
		xyear=True  #flag a cross-year season
	else:
		Ti=Ti+1
		xyear=False

	T=nyr-1
	Tarr = np.arange(Ti, Ti+T)

        #Now write the CPT file
	outfile="usr_"+predictand+"_"+tar+".tsv"
	f = open(outfile, 'w')
	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
	f.write("cpt:nfields=1\n")
	for it in range(T):
		if xyear==True:
			f.write("cpt:field="+vari+", cpt:T="+str(Tarr[it])+"-"+mi+"/"+str(Tarr[it]+1)+"-"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		else:
			f.write("cpt:field="+vari+", cpt:T="+str(Tarr[it])+"-"+mi+"/"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		np.savetxt(f, Xarr[0:-1], fmt="%.6f",newline='\t')
		f.write("\n") #next line
		for iy in range(H):
			np.savetxt(f,np.r_[Yarr[iy],var[it,iy,0:]],fmt="%.6f", newline='\t')  #excise extra line
			f.write("\n") #next line
	f.close()

def readNetCDF_Hindcasts(infile, outfile, wlo1, elo1, sla1, nla1, tgti, tgtf, mon, tar):
	"""Function to read the user's Hindcasts NetCDF file and write to CPT format.

	PARAMETERS
	----------
		Hindcats: a DataArray with dimensions S,M,L,Y,X
	"""
	ds=xr.open_dataset(infile,decode_times=False)
	da=list(ds.coords)

	for i in range(len(da)):
		if da[i]=='X' or da[i]=='lon' or da[i]=='longitude':
			ds = ds.rename({da[i]:'X'})
		if da[i]=='Y' or da[i]=='lat' or da[i]=='latitude':
			ds = ds.rename({da[i]:'Y'})
		if da[i]=='S':
			deltastyr=int(ds[da[i]][0]/12)
			nmon=ds.S.shape[0]
			nyr=int(nmon/12)
		if 'months since' in ds.S.units:
				line=ds.S.units
				stdate=str(int(line.split()[2][:4])+deltastyr)+line.split()[2][-6:]
				ds['S'] = pd.date_range(stdate, periods=ds.S.shape[0], freq='M')

	ds1=ds.sel(X=slice(wlo1,elo1),Y=slice(sla1,nla1),L=slice(float(tgti),float(tgtf))).mean(dim='L',skipna=True)
	ds2=ds1.mean(dim='M',skipna=True)
	Xarr=ds2.X.values
	Yarr=ds2.Y.values
	W=ds2.X.shape[0]
	H=ds2.Y.shape[0]
	a=list(ds)

	var1=ds2[a[0]]
	units=ds[a[0]].units
	Ti=1982

	vari = a[0]
	varname = vari
	L=0.5*(float(tgtf)+float(tgti))

	monthdic = {'Jan':'01','Feb':'02','Mar':'03','Apr':'04','May':'05','Jun':'06','Jul':'07','Aug':'08','Sep':'09','Oct':'10','Nov':'11','Dec':'12'}
	S1=monthdic[mon]
	mi=monthdic[tar.split("-")[0]]
	mf=monthdic[tar.split("-")[1]]

	var1_stmon=var1[(var1.S.dt.month==int(monthdic[mon]))]
	var=var1_stmon.groupby(var1_stmon.S.dt.year).mean(dim=('S')).sel(year=slice(1982,2009))
	var_N2S=var.reindex(Y=var.Y[::-1]) #Y from N to S
	Yarr=var_N2S.Y.values
	if tar=='Dec-Feb' or tar=='Nov-Jan':  #double check years are sync
		xyear=True  #flag a cross-year season
	else:
		xyear=False
	T=2009-1982+1
	Tarr = np.arange(Ti, Ti+T)

	if 'True' in np.isnan(var):
		var[np.isnan(var)]=-999. #use CPT missing value
        #Now write the CPT file
	outfile="usr_"+a[0]+"_"+tar+"_ini"+mon+".tsv"
	f = open(outfile, 'w')
	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
	f.write("cpt:nfields=1\n")

	for it in range(T):
		if xyear==True:
			f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Tarr[it])+"-"+S1+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+str(Tarr[it]+1)+"-"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		else:
			f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Tarr[it])+"-"+S1+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
	np.savetxt(f, Xarr, fmt="%.6f",newline='\t')
	f.write("\n") #next line
	for iy in range(H):
		np.savetxt(f,np.r_[Yarr[iy],var_N2S[it,iy,0:]],fmt="%.6f", newline='\t')  #excise extra line
		f.write("\n") #next line
	f.close()

def readNetCDF_Forecast(infile, outfile, monf, fyr, tgti, tgtf, tar, wlo1, elo1, sla1, nla1):
        """Function to read the user's forecast NetCDF file and write to CPT format.

        PARAMETERS
        ----------
                Forecat: a DataArray with dimensions S,M,L,Y,X
        """
        ds=xr.open_dataset(infile,decode_times=False)
        da=list(ds.coords)

        for i in range(len(da)):
                if da[i]=='X' or da[i]=='lon' or da[i]=='longitude':
                        ds = ds.rename({da[i]:'X'})
                if da[i]=='Y' or da[i]=='lat' or da[i]=='latitude':
                        ds = ds.rename({da[i]:'Y'})
                if da[i]=='S':
                        deltastyr=int(ds[da[i]][0]/12)
                        nmon=ds.S.shape[0]
                        nyr=int(nmon/12)
                        if 'months since' in ds.S.units:
                                line=ds.S.units
                                stdate=str(int(line.split()[2][:4])+deltastyr)+line.split()[2][-6:]
                                ds['S'] = pd.date_range(stdate, periods=ds.S.shape[0], freq='M')

        ds1=ds.sel(X=slice(wlo1,elo1),Y=slice(sla1,nla1),L=slice(float(tgti),float(tgtf))).mean(dim='L',skipna=True)
        ds2=ds1.mean(dim='M',skipna=True)
        Xarr=ds2.X.values
        Yarr=ds2.Y.values
        W=ds2.X.shape[0]
        H=ds2.Y.shape[0]
        a=list(ds)

        var1=ds2[a[0]]
        units=ds[a[0]].units
        Ti=fyr

        vari = a[0]
        varname = vari
        L=0.5*(float(tgtf)+float(tgti))

        monthdic = {'Jan':'01','Feb':'02','Mar':'03','Apr':'04','May':'05','Jun':'06','Jul':'07','Aug':'08','Sep':'09','Oct':'10','Nov':'11','Dec':'12'}
        S1=monthdic[monf]
        mi=monthdic[tar.split("-")[0]]
        mf=monthdic[tar.split("-")[1]]

        var1_stmon=var1[(var1.S.dt.month==int(monthdic[monf]))]
        var=var1_stmon.groupby(var1_stmon.S.dt.year).mean(dim=('S')).sel(year=fyr)
        var_N2S=var.reindex(Y=var.Y[::-1])
        Yarr=var_N2S.Y.values
        if tar=='Dec-Feb' or tar=='Nov-Jan':  #double check years are sync
                xyear=True  #flag a cross-year season
        else:
                xyear=False
        T=1
        Tarr = np.arange(Ti, Ti+T)

        if 'True' in np.isnan(var):
                var[np.isnan(var)]=-999. #use CPT missing value
        #Now write the CPT file
        outfile="usr_fcst_"+a[0]+"_"+tar+"_ini"+monf+str(fyr)+".tsv"
        f = open(outfile, 'w')
        f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
        f.write("cpt:nfields=1\n")

        for it in range(T):
                if xyear==True:
                        f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Tarr[it])+"-"+S1+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+str(Tarr[it]+1)+"-"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
                else:
                        f.write("cpt:field="+vari+", cpt:L="+str(L)+" months, cpt:S="+str(Tarr[it])+"-"+S1+"-01T00:00, cpt:T="+str(Tarr[it])+"-"+mi+"/"+mf+", cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
        np.savetxt(f, Xarr, fmt="%.6f",newline='\t')
        f.write("\n") #next line
        for iy in range(H):
                np.savetxt(f,np.r_[Yarr[iy],var_N2S[iy,0:]],fmt="%.6f", newline='\t')  #excise extra line
                f.write("\n") #next line
        f.close()

def GetHindcasts(wlo1, elo1, sla1, nla1, day1, day2, fyr, mon, os, key, week, nlag, nday, training_season, hstep, model, hdate_last, GEPShdate1, force_download, dic_s2s):
	nwi=4  #number of weeks to use for real-time ECMWF training period (2 initializations per week) --Send to namelist in the future
	if not force_download:
		try:
			ff=open(model+"_precip_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
##		dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/3./mul/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/add/4./div/S/('+training_season+')/VALUES/S/'+str(hstep)+'/STEP/dup/S/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.S/replaceGRID/L1/S/add/0/RECHUNK//name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/c://name//water_density/def/998/%28kg/m3%29/:c/div//mm/unitconvert//name/(tp)/def/grid://name/%28T%29/def//units/%28months%20since%201960-01-01%29/def//standard_name/%28time%29/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/:grid/use_as_grid//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/3./mul/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/add/4./div/S/('+mon+')/VALUES/S/'+str(hstep)+'/STEP/dup/S/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.S/replaceGRID/L1/S/add/0/RECHUNK//name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/c://name//water_density/def/998/%28kg/m3%29/:c/div//mm/unitconvert//name/(tp)/def/grid://name/%28T%29/def//units/%28months%20since%201960-01-01%29/def//standard_name/%28time%29/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/:grid/use_as_grid//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%201%20'+mon+'%20'+str(fyr)+')%20(2300%2028%20'+mon+'%20'+str(fyr)+')/RANGE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2060/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last%206%20'+str(nwi)+'%20mul%20sub)%20(last)/RANGE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2060/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',

#				'GEFS': 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFS/.hindcast/.pr/S/(0000%206%20Jan%201999)/(0000%2028%20Dec%202015)/RANGEEDGES/S/(days%20since%201999-01-01)/streamgridunitconvert/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/RANGEEDGES/%5BM%5Daverage/L/'+str(nday)+'/runningAverage/SOURCES/.Models/.SubX/.EMC/.GEFS/.hindcast/.dc9915/.pr/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/RANGEEDGES/L/'+str(nday)+'/runningAverage/S/(T)/renameGRID/pentadmean/T/(S)/renameGRID/%5BS%5DregridLinear/sub/S/('+training_season+')/VALUES/L/removeGRID/S/(T)/renameGRID/c%3A/0.001/(m3%20kg-1)/%3Ac/mul/c%3A/1000/(mm%20m-1)/%3Ac/mul/c%3A/86400/(s%20day-1)/%3Ac/mul/c%3A/7.0//units//days/def/%3Ac/mul/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2301/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_hcst_PRCP'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > '"+model+"_precip_"+mon+"_wk"+str(week)+".tsv.gz'")
		get_ipython().system("gunzip -f "+model+"_precip_"+mon+"_wk"+str(week)+".tsv.gz")
		#! curl -g -k -b '__dlauth_id='$key'' ''$url'' > model_precip_${mo}.tsv

def GetHindcasts_elr(wlo1, elo1, sla1, nla1, day1, day2, fyr, mon, os, key, week, nlag, nday, training_season, hstep, model, hdate_last, GEPShdate1, force_download, dic_s2s_elr):
	nwi=4  #number of weeks to use for real-time ECMWF training period (2 initializations per week) --Send to namelist in the future
	if not force_download:
		try:
			ff=open(model+"_precip_"+mon+"_wk"+str(week)+".nc", 'r')
			#s = ff.variables['Y'][:]
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s_elr[model+'_hcst_PRCP'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > "+model+"_precip_"+mon+"_wk"+str(week)+".nc")

def GetHindcasts_T2M(wlo1, elo1, sla1, nla1, day1, day2, fyr, mon, os, key, week, nlag, nday, training_season, hstep, model, hdate_last, force_download, dic_s2s):
	if not force_download:
		try:
			ff=open("model_T2M_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#               dic = { 'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.2m_above_ground/.2t/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/LA/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%201%20'+mon+'%20'+str(fyr)+')%20(2300%2028%20'+mon+'%20'+str(fyr)+')/RANGE/%5BL%5Daverage//Celsius/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2060/ensotime/%3Agrid/replaceGRID//name/(temp)/def//units/(Celsius)/def//long_name/(2-m temperature)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_hcst_T2M'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > "+model+"_T2M_"+mon+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f model_T2M_"+mon+"_wk"+str(week)+".tsv.gz")
		#! curl -g -k -b '__dlauth_id='$key'' ''$url'' > model_precip_${mo}.tsv

def GetHindcasts_RFREQ(wlo1, elo1, sla1, nla1, day1, day2, nday, fyr, mon, os, key, week, wetday_threshold, nlag, training_season, hstep,model, force_download, dic_s2s):
	nwi=4  #number of weeks to use for real-time ECMWF training period (2 initializations per week) --Send to namelist in the future
	if not force_download:
		try:
			ff=open("model_RFREQ_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/3./mul/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/add/4./div/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/('+training_season+')/VALUES/S/'+str(hstep)+'/STEP/dup/S/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.S/replaceGRID/L1/S/add/0/RECHUNK//name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/c://name//water_density/def/998/%28kg/m3%29/:c/div//mm/unitconvert//name/(tp)/def/grid://name/%28T%29/def//units/%28months%20since%201960-01-01%29/def//standard_name/%28time%29/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/:grid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%201%20'+mon+'%20'+str(fyr)+')%20(2300%2028%20'+mon+'%20'+str(fyr)+')/RANGE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/'+str(wetday_threshold)+'/flagge/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2060/ensotime/%3Agrid/replaceGRID//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last%206%20'+str(nwi)+'%20mul%20sub)%20(last)/RANGE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/'+str(wetday_threshold)+'/flagge/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2060/ensotime/%3Agrid/replaceGRID//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_hcst_RFREQ'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > model_RFREQ_"+mon+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f model_RFREQ_"+mon+"_wk"+str(week)+".tsv.gz")
		#! curl -g -k -b '__dlauth_id='$key'' ''$url'' > model_precip_${mo}.tsv

def GetHindcastsUser(wlo1, elo1, sla1, nla1, day1, day2, fyr, mon, os, key, week, nlag, nday, training_season, hstep, model, hdate_last, force_download, dic_s2s):
	if not force_download:
		try:
			ff=open("model_precip_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Hindcasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/3./mul/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/add/4./div/S/('+training_season+')/VALUES/S/'+str(hstep)+'/STEP/dup/S/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.S/replaceGRID/L1/S/add/0/RECHUNK//name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/c://name//water_density/def/998/%28kg/m3%29/:c/div//mm/unitconvert//name/(tp)/def/grid://name/%28T%29/def//units/%28months%20since%201960-01-01%29/def//standard_name/%28time%29/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/:grid/use_as_grid//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		dic = { 'NextGenR0': 'http://iridl.ldeo.columbia.edu/home/.xchourio/.IRAP2/.S2S/.R0HIND/.Caminadeetal/home/.xchourio/.IRAP2/.S2S/.R0HIND/.Mordecaietal/add/home/.xchourio/.IRAP2/.S2S/.R0HIND/.Wesolowskietal/add/home/.xchourio/.IRAP2/.S2S/.R0HIND/.LiuHelmerssonetal/add/4/div/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/RANGE/%5BL%5D//keepgrids/average/%5BM%5Daverage/S/%28days%20since%201960-01-01%29/streamgridunitconvert/%5BX/Y%5DREORDER/2/RECHUNK/S//pointwidth/0/def/30/shiftGRID/S//units//days/def/L/add/0/RECHUNK//name//T/def//long_name/%28Target%20date%29/def/2/%7Bexch%5BS/L%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/T/grid://name/%28T%29/def//units/%28months%20since%201960-01-01%29/def//standard_name/%28time%29/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/:grid/replaceGRID//name/%28R0%29/def//units/%28unitless%29/def//long_name/%28R0%29/def/-999/replaceNaN/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_hcst_PRCP'])
		print("\n Hindcasts URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > model_precip_"+mon+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f model_precip_"+mon+"_wk"+str(week)+".tsv.gz")
		#! curl -g -k -b '__dlauth_id='$key'' ''$url'' > model_precip_${mo}.tsv

def GetObs(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, key, week, nlag, training_season, hstep, model, obs_source, obsclimo_source,obsclimo2_source, hdate_last, GEPShdate1, force_download, dic_s2s):
	nwi=4  #number of weeks to use for real-time ECMWF training period (2 initializations per week) --Send to namelist in the future
	if not force_download:
		try:
			ff=open("obs_precip_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs precip file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
##		dic = {'CFSv2':                'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/('+training_season+')/VALUES/S/'+str(hstep)+'/STEP/L1/S/add/0/RECHUNK/name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/3/flagge/dup/pentadmean/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/I/3/-1/roll/.T/replaceGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		dic = {'CFSv2':                'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/('+mon+')/VALUES/S/'+str(hstep)+'/STEP/L1/S/add/0/RECHUNK/name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/(T)/use_as_grid/'+obs_source+'/T/(days%20since%201960-01-01)/streamgridunitconvert/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/'+obsclimo_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#			   'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%201%20'+mon+'%20'+str(fyr)+')%20(2300%2028%20'+mon+'%20'+str(fyr)+')/RANGE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/dup%5BT%5Daverage/sub/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2060/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#			   'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last%206%20'+str(nwi)+'%20mul%20sub)%20(last)/RANGE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/dup%5BT%5Daverage/sub/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2060/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#			   'GEFS':       'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFS/.hindcast/.pr/S/(0000%206%20Jan%201999)/(0000%2028%20Dec%202015)/RANGEEDGES/L/('+str(day1)+')/('+str(day2)+')/RANGEEDGES/L/'+str(nday)+'/runningAverage/S/('+training_season+')/VALUES/L/S/add/0/RECHUNK//name//T/def/2/%7Bexch%5BL/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/dup/pentadmean/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/I/3/-1/roll/.T/replaceGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2301/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz'
#			  }
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_obs_PRCP'])
		print("\n Obs (Rainfall) data URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > obs_precip_"+mon+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f obs_precip_"+mon+"_wk"+str(week)+".tsv.gz")
		#curl -g -k -b '__dlauth_id='$key'' ''$url'' > obs_precip_${mo}.tsv

def GetObs_hc_elr(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, key, week, nlag, training_season, hstep, model, obs_source, obsclimo_source, obsclimo2_source,  hdate_last, GEPShdate1, force_download, dic_s2s_elr):
	nwi=4  #number of weeks to use for real-time ECMWF training period (2 initializations per week) --Send to namelist in the future
	if not force_download:
		try:
			ff=open("obs_precip_"+mon+"_wk"+str(week)+"_hc.nc", 'r')
			#s = ff.variables['Y'][:]
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs precip file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s_elr[model+'_obs_hc_PRCP'])
		print("\n Obs (Rainfall) data URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > obs_precip_"+mon+"_wk"+str(week)+"_hc.nc")

def GetObs_T2M(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, key, week, nlag, training_season, hstep, model, obs_source, hdate_last, force_download, dic_s2s):
	if not force_download:
		try:
			ff=open("obs_T2M_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs temp file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#               dic = {'ECMWF':  'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.2m_above_ground/.2t/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/LA/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%201%20'+mon+'%20'+str(fyr)+')%20(2300%2028%20'+mon+'%20'+str(fyr)+')/RANGE/%5BLA%5D//keepgrids/average//Celsius/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/LA/hdate/add/add/0/RECHUNK/LA/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/SOURCES/.NOAA/.NCEP/.CPC/.temperature/.daily/a%3A/.tmax/%3Aa%3A/.tmin/%3Aa/add/2/div/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/T/2/index/.T/SAMPLE/dup/%5BT%5Daverage/sub/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12.0/16/Jan/2060/ensotime/%3Agrid/replaceGRID//name/(2t)/def//units/(Celsius)/def//long_name/(2-m%20temperature)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#			  }
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_obs_T2M'])
		print("\n Obs (2m Temp) data URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > obs_T2M_"+mon+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f obs_T2M_"+mon+"_wk"+str(week)+".tsv.gz")
		#curl -g -k -b '__dlauth_id='$key'' ''$url'' > obs_precip_${mo}.tsv

def GetObs_TMAX(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, key, week, nlag, training_season, hstep, model, obs_source, hdate_last, force_download, dic_s2s):
	if not force_download:
		try:
			ff=open("obs_TMAX_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs temp file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
                #dictionary:
#               dic = {'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.2m_above_ground/.2t/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/LA/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%201%20'+mon+'%20'+str(fyr)+')%20(2300%2028%20'+mon+'%20'+str(fyr)+')/RANGE/%5BLA%5D//keepgrids/average//Celsius/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/LA/hdate/add/add/0/RECHUNK/LA/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/SOURCES/.NOAA/.NCEP/.CPC/.temperature/.daily/.tmax/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/T/2/index/.T/SAMPLE/dup/%5BT%5Daverage/sub/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12.0/16/Jan/2060/ensotime/%3Agrid/replaceGRID//name/(tmax)/def//units/(Celsius)/def//long_name/(maxmum%20temperature)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#                         }
                # calls curl to download data
                #url=dic[model]
		url=eval(dic_s2s[model+'_obs_TMAX'])
		print("\n Obs (Temp max) data URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > obs_TMAX_"+mon+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f obs_TMAX_"+mon+"_wk"+str(week)+".tsv.gz")
                #curl -g -k -b '__dlauth_id='$key'' ''$url'' > obs_precip_${mo}.tsv

def GetObs_TMIN(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, key, week, nlag, training_season, hstep, model, obs_source, hdate_last, force_download, dic_s2s):
	if not force_download:
		try:
			ff=open("obs_TMIN_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs temp file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
                #dictionary:
#               dic = {'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.2m_above_ground/.2t/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/LA/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%201%20'+mon+'%20'+str(fyr)+')%20(2300%2028%20'+mon+'%20'+str(fyr)+')/RANGE/%5BLA%5D//keepgrids/average//Celsius/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/LA/hdate/add/add/0/RECHUNK/LA/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/SOURCES/.NOAA/.NCEP/.CPC/.temperature/.daily/.tmin/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/T/2/index/.T/SAMPLE/dup/%5BT%5Daverage/sub/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12.0/16/Jan/2060/ensotime/%3Agrid/replaceGRID//name/(tmin)/def//units/(Celsius)/def//long_name/(minimum%20temperature)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#                         }
                # calls curl to download data
                #url=dic[model]
		url=eval(dic_s2s[model+'_obs_TMIN'])
		print("\n Obs (Temp min) data URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > obs_TMIN_"+mon+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f obs_TMIN_"+mon+"_wk"+str(week)+".tsv.gz")
                #curl -g -k -b '__dlauth_id='$key'' ''$url'' > obs_precip_${mo}.tsv

def GetObs_RFREQ(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, key, week, wetday_threshold, threshold_pctle, nlag, training_season, hstep, model, obs_source, force_download, dic_s2s):
	nwi=4  #number of weeks to use for real-time ECMWF training period (2 initializations per week) --Send to namelist in the future
	if not force_download:
		try:
			ff=open("obs_RFREQ_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs freq-rainfall file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionaries:
		if threshold_pctle:
#				dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/('+training_season+')/VALUES/S/'+str(hstep)+'/STEP/L1/S/add/0/RECHUNK//name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/(1%20Jan%201999)/(31%20Dec%202011)/RANGEEDGES/%5BT%5Dpercentileover/'+str(wetday_threshold)+'/flagle/T/'+str(nday)+'/runningAverage/'+str(nday)+'/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#						'ECMWF':'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%201%20'+mon+'%20'+str(fyr)+')%20(2300%2028%20'+mon+'%20'+str(fyr)+')/RANGE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/%5BT%5Dpercentileover/'+str(wetday_threshold)+'/flagle/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2060/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#						'ECMWFrt':'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last%206%20'+str(nwi)+'%20mul%20sub)%20(last)/RANGE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/%5BT%5Dpercentileover/'+str(wetday_threshold)+'/flagle/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2060/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#							}
				url=eval(dic_s2s[model+'_obs_RFREQ_threshold_pctle'])
		else:
#				dic = { 'CFSv2':                     'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/('+training_season+')/VALUES/S/'+str(hstep)+'/STEP/L1/S/add/0/RECHUNK/name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/dup/pentadmean/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/I/3/-1/roll/.T/replaceGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#						'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%201%20'+mon+'%20'+str(fyr)+')%20(2300%2028%20'+mon+'%20'+str(fyr)+')/RANGE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2060/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#						'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last%206%20'+str(nwi)+'%20mul%20sub)%20(last)/RANGE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2060/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#						'GEFS':       'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFS/.hindcast/.pr/S/(0000%206%20Jan%201999)/(0000%2028%20Dec%202015)/RANGEEDGES/L/('+str(day1)+')/('+str(day2)+')/RANGEEDGES/L/'+str(nday)+'/runningAverage/S/('+training_season+')/VALUES/L/S/add/0/RECHUNK//name//T/def/2/%7Bexch%5BL/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/SOURCES/.NASA/.GES-DAAC/.TRMM_L3/.TRMM_3B42/.v7/.daily/.precipitation/X/0./1.5/360./GRID/Y/-50/1.5/50/GRID/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/dup/pentadmean/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/I/3/-1/roll/.T/replaceGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2301/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz'
#						}
				url=eval(dic_s2s[model+'_obs_RFREQ'])
		# calls curl to download data
		#url=dic[model]
		print("\n Obs (Freq) data URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > obs_RFREQ_"+mon+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f obs_RFREQ_"+mon+"_wk"+str(week)+".tsv.gz")
		#curl -g -k -b '__dlauth_id='$key'' ''$url'' > obs_precip_${mo}.tsv

def GetObsUser(day1, day2, mon, fyr, wlo2, elo2, sla2, nla2, nday, key, week, nlag, training_season, hstep, model, obs_source, obsclimo_source, hdate_last, force_download, dic_s2s):
	if not force_download:
		try:
			ff=open("obs_precip_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs precip file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
##		dic = {'CFSv2':                'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/('+training_season+')/VALUES/S/'+str(hstep)+'/STEP/L1/S/add/0/RECHUNK/name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/3/flagge/dup/pentadmean/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/I/3/-1/roll/.T/replaceGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		dic = {'NextGenR0': 'http://iridl.ldeo.columbia.edu/home/.xchourio/.IRAP2/.S2S/.R0HIND/.Caminadeetal/home/.xchourio/.IRAP2/.S2S/.R0HIND/.Mordecaietal/add/home/.xchourio/.IRAP2/.S2S/.R0HIND/.Wesolowskietal/add/home/.xchourio/.IRAP2/.S2S/.R0HIND/.LiuHelmerssonetal/add/4/div/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/RANGE/%5BL%5D//keepgrids/average/%5BM%5Daverage/S/(days%20since%201960-01-01)/streamgridunitconvert/%5BX/Y%5DREORDER/2/RECHUNK/S//pointwidth/0/def/30/shiftGRID/S//units//days/def/L/add/0/RECHUNK//name//T/def//long_name/(Target%20date)/def/2/%7Bexch%5BS/L%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/home/.xchourio/.IRAP2/.S2S/.R0OBS/.Caminadeetal/home/.xchourio/.IRAP2/.S2S/.R0OBS/.Mordecaietal/add/home/.xchourio/.IRAP2/.S2S/.R0OBS/.Wesolowskietal/add/home/.xchourio/.IRAP2/.S2S/.R0OBS/.LiuHelmerssonetal/add/4/div/T/2/index/.T/SAMPLE/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(R0%20obs)/def//units/(unitless)/def//long_name/(R0)/def/-999/replaceNaN/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_obs_PRCP'])
		print("\n R0 'obs' data URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > obs_precip_"+mon+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f obs_precip_"+mon+"_wk"+str(week)+".tsv.gz")
		#curl -g -k -b '__dlauth_id='$key'' ''$url'' > obs_precip_${mo}.tsv

def GetForecastUser(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, wlo2, elo2, sla2, nla2, obs_source, key, week, nlag, model, hdate_last, threshold_pctle,training_season,wetday_threshold,force_download,mpref, dic_s2s):
	if not force_download:
		try:
			ff=open("modelfcst_precip_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'NextGenR0': 'http://iridl.ldeo.columbia.edu/home/.xchourio/.IRAP2/.S2S/.R0HIND/.Caminadeetal/home/.xchourio/.IRAP2/.S2S/.R0HIND/.Mordecaietal/add/home/.xchourio/.IRAP2/.S2S/.R0HIND/.Wesolowskietal/add/home/.xchourio/.IRAP2/.S2S/.R0HIND/.LiuHelmerssonetal/add/4/div/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/RANGE/%5BL%5D//keepgrids/average/%5BM%5Daverage/S/%28days%20since%201960-01-01%29/streamgridunitconvert/%5BX/Y%5DREORDER/2/RECHUNK/S//pointwidth/0/def/30/shiftGRID/S//units//days/def/L/add/0/RECHUNK//name//T/def//long_name/%28Target%20date%29/def/2/%7Bexch%5BS/L%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/T/grid://name/%28T%29/def//units/%28months%20since%201960-01-01%29/def//standard_name/%28time%29/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/:grid/replaceGRID//name/%28R0%29/def//units/%28unitless%29/def//long_name/%28R0%29/def/-999/replaceNaN/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_fcst_PRCP'])
		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > modelfcst_precip_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f modelfcst_precip_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv.gz")
		#curl -g -k -b '__dlauth_id='$key'' ''$url'' > modelfcst_precip_fday${fday}.tsv

def GetForecast(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, wlo2, elo2, sla2, nla2, obs_source,obsclimo_source, key, week, nlag, model, hdate_last, threshold_pctle,training_season,wetday_threshold,force_download,mpref, dic_s2s):
	if not force_download:
		try:
			ff=open(model+"fcst_precip_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecasts file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {	'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/3.0/mul/SOURCES/.ECMWF/.S2S/.NCEP/.forecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/add/4.0/div/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')VALUE/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/3.0/mul/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/add/4.0/div/S/(0000%20'+str(fday)+'%20'+mon+')VALUES/%5BS%5Daverage/sub/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/1/Jan/3001/ensotime/12.0/1/Jan/3001/ensotime/%3Agrid/addGRID/T//pointwidth/0/def/pop//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/%5Bhdate%5Daverage/sub/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/1/Jan/3001/ensotime/12.0/1/Jan/3001/ensotime/%3Agrid/addGRID/T//pointwidth/0/def/pop//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'ECMWFrt': 'http://iridl.ldeo.columbia.edu/home/.jingyuan/.ECMWF/.realtime_S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/%5Bhdate%5Daverage/sub/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/1/Jan/3001/ensotime/12.0/1/Jan/3001/ensotime/%3Agrid/addGRID/T//pointwidth/0/def/pop//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'GEFS':  'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFS/.forecast/.pr/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUES/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/RANGEEDGES/%5BM%5Daverage/L/'+str(nday)+'/runningAverage/SOURCES/.Models/.SubX/.EMC/.GEFS/.hindcast/.dc9915/.pr/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/RANGEEDGES/L/'+str(nday)+'/runningAverage/S/(T)/renameGRID/pentadmean/T/(S)/renameGRID/%5BS%5DregridLinear/S/1/setgridtype/pop/S/2/index/.S/SAMPLE/sub/c%3A/0.001/(m3%20kg-1)/%3Ac/mul/c%3A/1000/(mm%20m-1)/%3Ac/mul/c%3A/86400/(s%20day-1)/%3Ac/mul/c%3A/7.0//units//days/def/%3Ac/mul/S/(T)/renameGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/3001/ensotime/12.0/16/Jan/3001/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz'
#			  }
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_fcst_PRCP'])
		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > "+model+"fcst_precip_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f "+model+"fcst_precip_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv.gz")
		#curl -g -k -b '__dlauth_id='$key'' ''$url'' > modelfcst_precip_fday${fday}.tsv

	#False force_download
	if mpref=='noMOS':
		force_download = False

		#The next two if-blocks are used for noMOS forecasts ##Added by AGM
		#Short hindcast to correctly compute climatological period of the actual forecast
		if not force_download:
			try:
				ff=open("noMOS/modelshort_precip_"+mon+"_wk"+str(week)+".tsv", 'r')
				s = ff.readline()
			except OSError as err:
				print("\033[1mWarning:\033[0;0m {0}".format(err))
				print("Short hindcast file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
				force_download = True
		if force_download:
			#dictionary:
#			dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/home/.xchourio/.S2SDB/.NCEP/.reforecasts/.perturbed/.anomalies/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/3./mul/home/.xchourio/.S2SDB/.NCEP/.reforecasts/.control/.anomalies/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/add/4./div/S/(0000%20'+str(fday)+'%20'+mon+')/VALUES/L1/S/add/0/RECHUNK//name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/c://name//water_density/def/998/%28kg/m3%29/:c/div//mm/unitconvert//name/(tp)/def/grid://name/%28T%29/def//units/%28months%20since%201960-01-01%29/def//standard_name/%28time%29/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/:grid/use_as_grid//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#					'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#					'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#			}
			# calls curl to download data
			#url=dic[model]
			url=eval(dic_s2s[model+'_fcst_noMOS/modelshort_PRCP'])
			print("\n Short hindcast file\n") #URL: \n\n "+url)
			get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/modelshort_precip_"+mon+"_wk"+str(week)+".tsv.gz")
			get_ipython().system("gunzip -f noMOS/modelshort_precip_"+mon+"_wk"+str(week)+".tsv.gz")
		#Short obs period corresponding to the short hindcast period

		#False force_download
		force_download = False

		if not force_download:
			try:
				ff=open("noMOS/obsshort_precip_"+mon+"_wk"+str(week)+".tsv", 'r')
				s = ff.readline()
			except OSError as err:
				print("\033[1mWarning:\033[0;0m {0}".format(err))
				print("Short obs precip file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
				force_download = True
		if force_download:
			#dictionary:
#			dic = {'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/(0000%20'+str(fday)+'%20'+mon+')/VALUES/L1/S/add/0/RECHUNK/name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/(T)/use_as_grid/'+obs_source+'/T/(days%20since%201960-01-01)/streamgridunitconvert/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/'+obsclimo_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				   'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/dup%5BT%5Daverage/sub/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				   'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/dup%5BT%5Daverage/sub/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				   }
			# calls curl to download data
			#url=dic[model]
			url=eval(dic_s2s[model+'_fcst_noMOS/obsshort_PRCP'])
			print("\n Short obs (Rainfall) data URL: \n\n "+url)
			get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/obsshort_precip_"+mon+"_wk"+str(week)+".tsv.gz")
			get_ipython().system("gunzip -f noMOS/obsshort_precip_"+mon+"_wk"+str(week)+".tsv.gz")

		#False force_download
		force_download = False

		#The next block is used for noMOS probabilistic forecasts ##Added by AGM
		#Above normal:
		if not force_download:
			try:
				ff=Dataset('noMOS/modelfcst_above_PRCP_'+mon+'_wk'+str(week)+'.nc', 'r')
				s = ff.variables['Y'][:]
			except OSError as err:
				print("\033[1mWarning:\033[0;0m {0}".format(err))
				print("Above normal probability forecast file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
				force_download = True
		if force_download:
			#dictionary:
#			dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/('+str(day1)+')/('+str(day2)+')/VALUES%5BL1%5Ddifferences/SOURCES/.ECMWF/.S2S/.NCEP/.forecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/('+str(day1)+')/('+str(day2)+')/VALUES%5BL1%5Ddifferences//M//ids/ordered%5B16%5DNewGRID/addGRID/appendstream%5BX/Y/M/L1/S%5DREORDER/2/RECHUNK/S/-2/1/0/shiftdatashort/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE%5BX/Y/L1/S/M/S_lag%5DREORDER/4/RECHUNK%5BM/S_lag%5D//M1/nchunk/NewIntegerGRID/replaceGRIDstream/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/('+str(day1)+')/('+str(day2)+')/VALUES%5BL1%5Ddifferences/S/-2/1/0/shiftdatashort/S/(0000%20'+str(fday)+'%20'+mon+')/VALUES%5BX/Y/L1/M/S_lag/S%5DREORDER/3/RECHUNK%5BM/S_lag/S%5D//M2/nchunk/NewIntegerGRID/replaceGRIDstream%5BM2%5D0.33/0.66/0/replacebypercentile/percentile/0.67/VALUE/flaggt%5BM1%5Daverage/100/mul//long_name/%28Probability%20of%20Above%20Normal%20Tercile%29def//units/%28%25%29def/data.nc',
#				    'ECMWF': 'http://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE%5BL%5Ddifferences%5BM%5Daverage%5Bhdate%5D0.33/0.66/0/replacebypercentile/percentile/0.66/VALUE/flaggt%5BM%5Daverage/100/mul//long_name/%28Probability%20of%20Above%20Normal%20Tercile%29def//units/%28%25%29def/data.nc',
#				    'ECMWFrt': 'http://iridl.ldeo.columbia.edu/home/.jingyuan/.ECMWF/.realtime_S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE%5BL%5Ddifferences%5BM%5Daverage%5Bhdate%5D0.33/0.66/0/replacebypercentile/percentile/0.66/VALUE/flaggt%5BM%5Daverage/100/mul//long_name/%28Probability%20of%20Above%20Normal%20Tercile%29def//units/%28%25%29def/data.nc',
#			}
			# calls curl to download data
			#url=dic[model]
			url=eval(dic_s2s[model+'_fcst_noMOS/modelfcst_above_PRCP'])
			print("\n Short hindcast URL: \n\n "+url)
			get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/modelfcst_above_PRCP_"+mon+"_wk"+str(week)+".nc")

		#False force_download
		force_download = False

		#Below normal:
		if not force_download:
			try:
				ff=Dataset("noMOS/modelfcst_below_PRCP_"+mon+"_wk"+str(week)+".nc", 'r')
				s = ff.variables['Y'][:]
			except OSError as err:
				print("\033[1mWarning:\033[0;0m {0}".format(err))
				print("Below normal probability forecast file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
				force_download = True
		if force_download:
			#dictionary:
#			dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/('+str(day1)+')/('+str(day2)+')/VALUES%5BL1%5Ddifferences/SOURCES/.ECMWF/.S2S/.NCEP/.forecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/('+str(day1)+')/('+str(day2)+')/VALUES%5BL1%5Ddifferences//M//ids/ordered%5B16%5DNewGRID/addGRID/appendstream%5BX/Y/M/L1/S%5DREORDER/2/RECHUNK/S/-2/1/0/shiftdatashort/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE%5BX/Y/L1/S/M/S_lag%5DREORDER/4/RECHUNK%5BM/S_lag%5D//M1/nchunk/NewIntegerGRID/replaceGRIDstream/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/('+str(day1)+')/('+str(day2)+')/VALUES%5BL1%5Ddifferences/S/-2/1/0/shiftdatashort/S/(0000%20'+str(fday)+'%20'+mon+')/VALUES%5BX/Y/L1/M/S_lag/S%5DREORDER/3/RECHUNK%5BM/S_lag/S%5D//M2/nchunk/NewIntegerGRID/replaceGRIDstream%5BM2%5D0.33/0.66/0/replacebypercentile/percentile/0.33/VALUE/flaglt%5BM1%5Daverage/100/mul//long_name/%28Probability%20of%20Below%20Normal%20Tercile%29def//units/%28%25%29def/data.nc',
#				'ECMWF': 'http://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE%5BL%5Ddifferences%5BM%5Daverage%5Bhdate%5D0.33/0.66/0/replacebypercentile/percentile/0.33/VALUE/flaglt%5BM%5Daverage/100/mul//long_name/%28Probability%20of%20Below%20Normal%20Tercile%29def//units/%28%25%29def/data.nc',
#				'ECMWFrt': 'http://iridl.ldeo.columbia.edu/home/.jingyuan/.ECMWF/.realtime_S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE%5BL%5Ddifferences%5BM%5Daverage%5Bhdate%5D0.33/0.66/0/replacebypercentile/percentile/0.33/VALUE/flaglt%5BM%5Daverage/100/mul//long_name/%28Probability%20of%20Below%20Normal%20Tercile%29def//units/%28%25%29def/data.nc',
#			}
			# calls curl to download data
			#url=dic[model]
			url=eval(dic_s2s[model+'_fcst_noMOS/modelfcst_below_PRCP'])
			print("\n Short hindcast URL: \n\n "+url)
			get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/modelfcst_below_PRCP_"+mon+"_wk"+str(week)+".nc")

		#False force_download
		force_download = False

		#The next block is used for noMOS flexible probabilistic forecasts ##Added by AGM
		#Ensemble mean:
		if not force_download:
			try:
				ff=Dataset('noMOS/modelfcst_mu_PRCP_'+mon+'_wk'+str(week)+'.nc', 'r')
				s = ff.variables['Y'][:]
			except OSError as err:
				print("\033[1mWarning:\033[0;0m {0}".format(err))
				print("Ensemble mean file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
				force_download = True
		if force_download:
			#dictionary:
#			dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/3.0/mul/SOURCES/.ECMWF/.S2S/.NCEP/.forecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/add/4.0/div/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')VALUE/home/.xchourio/.S2SDB/.NCEP/.reforecasts/.climatologies/.sfc_precip/.tpSmooth/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%20'+str(fday)+'%20'+mon+')VALUES/sub/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/data.nc',
#			        'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/%5Bhdate%5Daverage/sub/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/data.nc',
#			        'ECMWFrt': 'http://iridl.ldeo.columbia.edu/home/.jingyuan/.ECMWF/.realtime_S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/%5Bhdate%5Daverage/sub/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/data.nc',
#			}
			# calls curl to download data
			#url=dic[model]
			url=eval(dic_s2s[model+'_fcst_noMOS/modelfcst_mu_PRCP'])
			print("\n Ensemble mean URL: \n\n "+url)
			get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/modelfcst_mu_PRCP_"+mon+"_wk"+str(week)+".nc")

		#False force_download
		force_download = False

		#Ensemble standard deviation:
		if not force_download:
			try:
				ff=Dataset("noMOS/modelfcst_std_PRCP_"+mon+"_wk"+str(week)+".nc", 'r')
				s = ff.variables['Y'][:]
			except OSError as err:
				print("\033[1mWarning:\033[0;0m {0}".format(err))
				print("Ensemble standard deviation file file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
				force_download = True
		if force_download:
			#dictionary:
#			dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/('+str(day1)+')/('+str(day2)+')/VALUES%5BL1%5Ddifferences/SOURCES/.ECMWF/.S2S/.NCEP/.forecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L1/('+str(day1)+')/('+str(day2)+')/VALUES%5BL1%5Ddifferences//M//ids/ordered%5B16%5DNewGRID/addGRID/appendstream%5BX/Y/M/L1/S%5DREORDER/2/RECHUNK/S/-2/1/0/shiftdatashort/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE%5BX/Y/L1/S/M/S_lag%5DREORDER/4/RECHUNK%5BM/S_lag%5D//M1/nchunk/NewIntegerGRID/replaceGRIDstream/%5BM1%5Drmsover/c://name//water_density/def/998/%28kg/m3%29/:c/div//mm/unitconvert/data.nc',
#			    'ECMWF': 'http://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Drmsover/c://name//water_density/def/998/%28kg/m3%29/:c/div//mm/unitconvert/data.nc',
#				'ECMWFrt': 'http://iridl.ldeo.columbia.edu/home/.jingyuan/.ECMWF/.realtime_S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/%5BM%5Drmsover/c://name//water_density/def/998/%28kg/m3%29/:c/div//mm/unitconvert/data.nc',
#			}
			# calls curl to download data
			#url=dic[model]
			url=eval(dic_s2s[model+'_fcst_noMOS/modelfcst_std_PRCP'])
			#print("\n Ensemble std URL: \n\n "+url)
			get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/modelfcst_std_PRCP_"+mon+"_wk"+str(week)+".nc")

		#Obs mean:
		if not force_download:
			try:
				ff=Dataset('noMOS/obs_mu_PRCP_'+mon+'_wk'+str(week)+'.nc', 'r')
				s = ff.variables['Y'][:]
			except OSError as err:
				print("\033[1mWarning:\033[0;0m {0}".format(err))
				print("Obs mean file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
				force_download = True
		if force_download:
			#dictionary:
#			dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/(0000%20'+str(fday)+'%20'+mon+')/VALUES/L1/S/add/0/RECHUNK/name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/(T)/use_as_grid/'+obs_source+'/T/(days%20since%201960-01-01)/streamgridunitconvert/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/'+obsclimo_source+'/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/%5BT%5Daverage//name/(tp)/def/data.nc',
#			        'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/dup%5BT%5Daverage/sub/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/%5BT%5Daverage/data.nc',
#			        'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/dup%5BT%5Daverage/sub/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/%5BT%5Daverage/data.nc',
#			}
			# calls curl to download data
			#url=dic[model]
			url=eval(dic_s2s[model+'_fcst_noMOS/obs_mu_PRCP'])
			#print("\n Obs mean URL: \n\n "+url)
			get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/obs_mu_PRCP_"+mon+"_wk"+str(week)+".nc")

		#False force_download
		force_download = False

		#Obs std:
		if not force_download:
			try:
				ff=Dataset('noMOS/obs_std_PRCP_'+mon+'_wk'+str(week)+'.nc', 'r')
				s = ff.variables['Y'][:]
			except OSError as err:
				print("\033[1mWarning:\033[0;0m {0}".format(err))
				print("Obs std file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
				force_download = True
		if force_download:
			#dictionary:
#			dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/(0000%20'+str(fday)+'%20'+mon+')/VALUES/L1/S/add/0/RECHUNK/name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/(T)/use_as_grid/'+obs_source+'/T/(days%20since%201960-01-01)/streamgridunitconvert/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/'+obsclimo_source+'/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/%5BT%5Drmsover//name/(tp)/def/data.nc',
#			      'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/%5BT%5Drmsover/data.nc',
#			      'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/%5BT%5Drmsover/data.nc',
#			}
			# calls curl to download data
			#url=dic[model]
			url=eval(dic_s2s[model+'_fcst_noMOS/obs_std_PRCP'])
			#print("\n Obs std URL: \n\n "+url)
			get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/obs_std_PRCP_"+mon+"_wk"+str(week)+".nc")

		#False force_download
		force_download = False

		if not force_download:
			try:
				ff=open("noMOS/obsshort_RFREQ_"+mon+"_wk"+str(week)+".tsv", 'r')
				s = ff.readline()
			except OSError as err:
				print("\033[1mWarning:\033[0;0m {0}".format(err))
				print("Short obs precip file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
				force_download = True
		#dictionaries:
		if threshold_pctle:
#			dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/(0000%20'+str(fday)+'%20'+mon+')/VALUES/L1/S/add/0/RECHUNK/name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/(T)/use_as_grid/'+obs_source+'/T/(days%20since%201960-01-01)/streamgridunitconvert/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/'+obsclimo_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#					'ECMWF':'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/%5BT%5Dpercentileover/'+str(wetday_threshold)+'/flagle/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#					'ECMWFrt':'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/%5BT%5Dpercentileover/'+str(wetday_threshold)+'/flagle/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#					}
			url=eval(dic_s2s[model+'_fcst_noMOS/obsshort_RFREQ_threshold_pctle'])
		else:
#			dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/(0000%20'+str(fday)+'%20'+mon+')/VALUES/L1/S/add/0/RECHUNK/name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/(T)/use_as_grid/'+obs_source+'/T/(days%20since%201960-01-01)/streamgridunitconvert/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/'+obsclimo_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#					'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#					'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#					#'GEFS':       'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFS/.hindcast/.pr/S/(0000%206%20Jan%201999)/(0000%2028%20Dec%202015)/RANGEEDGES/L/('+str(day1)+')/('+str(day2)+')/RANGEEDGES/L/'+str(nday)+'/runningAverage/S/('+training_season+')/VALUES/L/S/add/0/RECHUNK//name//T/def/2/%7Bexch%5BL/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/SOURCES/.NASA/.GES-DAAC/.TRMM_L3/.TRMM_3B42/.v7/.daily/.precipitation/X/0./1.5/360./GRID/Y/-50/1.5/50/GRID/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/dup/pentadmean/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/I/3/-1/roll/.T/replaceGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2301/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz'
#					}
			url=eval(dic_s2s[model+'_fcst_noMOS/obsshort_RFREQ'])
 		# calls curl to download data
		#url=dic[model]
		print("\n Short hindcast file\n") #URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/obsshort_RFREQ_"+mon+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f noMOS/obsshort_RFREQ_"+mon+"_wk"+str(week)+".tsv.gz")
	else:
		print("Data download of forecast individual ensemble members skipped for MOS case")

def GetForecast_RFREQ(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, wlo2, elo2, sla2, nla2, obs_source, key, week, wetday_threshold, nlag, model, hdate_last,force_download, dic_s2s):
	# if not force_download:
	# 	try:
	# 		ff=open("modelfcst_RFREQ_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv", 'r')
	# 		s = ff.readline()
	# 	except OSError as err:
	# 		#print("OS error: {0}".format(err))
	# 		print("Forecasts file doesn't exist --SOLVING: downloading file")
	# 		force_download = True
	# if force_download:
	# 	#dictionary:  #CFSv2 needs to be transformed to RFREQ!
	# 	dic = {	'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/3./mul/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/add/4./div/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/L1/removeGRID/S/(0000%20'+str(fday)+'%20'+mon+')/VALUES/%5BS%5Daverage/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.6_hourly_rotating/.FLXF/.surface/.PRATE/%5BL%5D1/0.0/boxAverage/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')VALUE/%5BX/Y%5DregridLinear/L/'+str(day1)+'/'+str(day2)+'/RANGEEDGES/%5BL%5Daverage/%5BS%5Daverage/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div/(mm/day)/unitconvert/'+str(nday)+'/mul//units/(mm)/def/exch/sub/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/1/Jan/3001/ensotime/12.0/1/Jan/3001/ensotime/%3Agrid/addGRID/T//pointwidth/0/def/pop//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
	# 			'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/'+str(wetday_threshold)+'/flagge/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1-1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/'+str(wetday_threshold)+'/flagge/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/%5Bhdate%5Daverage/sub/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/1/Jan/3001/ensotime/12.0/1/Jan/3001/ensotime/%3Agrid/addGRID/T//pointwidth/0/def/pop//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz'
	# 		  }
	# 	# calls curl to download data
	# 	url=dic[model]
	# 	print("\n Forecast URL: \n\n "+url)
	# 	get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > modelfcst_RFREQ_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv.gz")
	# 	get_ipython().system("gunzip -f modelfcst_RFREQ_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv.gz")
	# 	#curl -g -k -b '__dlauth_id='$key'' ''$url'' > modelfcst_precip_fday${fday}.tsv
	#
	# #False force_download
	# force_download = False

	#We're using model's rainfall as predictor.
	if not force_download:
		try:
			ff=open("modelfcst_precip_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecast file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = {	'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/3./mul/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/add/4./div/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/L1/removeGRID/S/(0000%20'+str(fday)+'%20'+mon+')/VALUES/%5BS%5Daverage/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.6_hourly_rotating/.FLXF/.surface/.PRATE/%5BL%5D1/0.0/boxAverage/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag/M%5Daverage/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')VALUE/%5BX/Y%5DregridLinear/L/'+str(day1)+'/'+str(day2)+'/RANGEEDGES/%5BL%5Daverage/%5BS%5Daverage/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div/(mm/day)/unitconvert/'+str(nday)+'/mul//units/(mm)/def/exch/sub/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/1/Jan/3001/ensotime/12.0/1/Jan/3001/ensotime/%3Agrid/addGRID/T//pointwidth/0/def/pop//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/%5Bhdate%5Daverage/sub/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/1/Jan/3001/ensotime/12.0/1/Jan/3001/ensotime/%3Agrid/addGRID/T//pointwidth/0/def/pop//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'ECMWFrt': 'http://iridl.ldeo.columbia.edu/home/.jingyuan/.ECMWF/.realtime_S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/%5Bhdate%5Daverage/sub/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/1/Jan/3001/ensotime/12.0/1/Jan/3001/ensotime/%3Agrid/addGRID/T//pointwidth/0/def/pop//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'GEFS':           'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFS/.forecast/.pr/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUES/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/RANGEEDGES/%5BM%5Daverage/L/'+str(nday)+'/runningAverage/SOURCES/.Models/.SubX/.EMC/.GEFS/.hindcast/.dc9915/.pr/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/RANGEEDGES/L/'+str(nday)+'/runningAverage/S/(T)/renameGRID/pentadmean/T/(S)/renameGRID/%5BS%5DregridLinear/S/1/setgridtype/pop/S/2/index/.S/SAMPLE/sub/c%3A/0.001/(m3%20kg-1)/%3Ac/mul/c%3A/1000/(mm%20m-1)/%3Ac/mul/c%3A/86400/(s%20day-1)/%3Ac/mul/c%3A/7.0//units//days/def/%3Ac/mul/S/(T)/renameGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/3001/ensotime/12.0/16/Jan/3001/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz'
#			  }
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_fcst_PRCP'])
		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > modelfcst_precip_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f modelfcst_precip_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv.gz")
		#curl -g -k -b '__dlauth_id='$key'' ''$url'' > modelfcst_precip_fday${fday}.tsv

	#False force_download
	force_download = False

	#The next two if-blocks are used for noMOS forecasts ##Added by AGM
	#Short hindcast to correctly compute climatological period of the forecast
	if not force_download:
		try:
			ff=open("noMOS/modelshort_precip_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Short hindcast file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_fcst_noMOS/modelshort_PRCP'])
		print("\n Short hindcast file\n") #URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/modelshort_precip_"+mon+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f noMOS/modelshort_precip_"+mon+"_wk"+str(week)+".tsv.gz")

	#Short obs period corresponding to the short hindcast period
	#False force_download
	force_download = False

	if not force_download:
		try:
			ff=open("noMOS/obsshort_RFREQ_"+mon+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Short obs precip file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	#dictionaries:
	if threshold_pctle:
#		dic = { 'CFSv2': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/('+training_season+')/VALUES/S/'+str(hstep)+'/STEP/L1/S/add/0/RECHUNK//name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/(1%20Jan%201999)/(31%20Dec%202011)/RANGEEDGES/%5BT%5Dpercentileover/'+str(wetday_threshold)+'/flagle/T/'+str(nday)+'/runningAverage/'+str(nday)+'/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'ECMWF':'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/%5BT%5Dpercentileover/'+str(wetday_threshold)+'/flagle/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#				'ECMWFrt':'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/%5BT%5Dpercentileover/'+str(wetday_threshold)+'/flagle/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz'
#				}
		url=eval(dic_s2s[model+'_fcst_noMOS/obsshort_RFREQ_threshold_pctle'])
	else:
#		dic = { 'CFSv2':                     'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.NCEP/.reforecast/.control/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/S/-'+str(nlag-1)+'/1/0/shiftdatashort/%5BS_lag%5Daverage/S/(0000%201%20Jan%201999)/(0000%2031%20Dec%202010)/RANGEEDGES/L1/'+str(day1)+'/'+str(day2)+'/VALUES/%5BL1%5Ddifferences/S/('+training_season+')/VALUES/S/'+str(hstep)+'/STEP/L1/S/add/0/RECHUNK/name//T/def/2/%7Bexch%5BL1/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/dup/pentadmean/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/I/3/-1/roll/.T/replaceGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/3001/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
	#			'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
	#			'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(fyr-1)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/dup/I/5/splitstreamgrid/%5BI2%5Daverage/sub/I/3/-1/roll/.T/replaceGRID/-999/setmissing_value/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/use_as_grid//name/(fp)/def//units/(unitless)/def//long_name/(rainfall_freq)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
	#			'GEFS':       'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFS/.hindcast/.pr/S/(0000%206%20Jan%201999)/(0000%2028%20Dec%202015)/RANGEEDGES/L/('+str(day1)+')/('+str(day2)+')/RANGEEDGES/L/'+str(nday)+'/runningAverage/S/('+training_season+')/VALUES/L/S/add/0/RECHUNK//name//T/def/2/%7Bexch%5BL/S%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/SOURCES/.NASA/.GES-DAAC/.TRMM_L3/.TRMM_3B42/.v7/.daily/.precipitation/X/0./1.5/360./GRID/Y/-50/1.5/50/GRID/Y/'+str(sla2)+'/'+str(nla2)+'/RANGE/X/'+str(wlo2)+'/'+str(elo2)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/'+str(wetday_threshold)+'/flagge/dup/pentadmean/%5BT%5D/regridLinear/sub/T/'+str(nday)+'/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/I/3/-1/roll/.T/replaceGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/2301/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz'
#				}
		url=eval(dic_s2s[model+'_fcst_noMOS/obsshort_RFREQ'])
	# calls curl to download data
	#url=dic[model]
	print("\n Short hindcast file\n") #URL: \n\n "+url)
	get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/obsshort_precip_"+mon+"_wk"+str(week)+".tsv.gz")
	get_ipython().system("gunzip -f noMOS/obsshort_RFREQ_"+mon+"_wk"+str(week)+".tsv.gz")

	#False force_download
	force_download = False

	#The next block is used for noMOS probabilistic forecasts ##Added by AGM
	#Above normal:
	if not force_download:
		try:
			ff=Dataset('noMOS/modelfcst_above_RFREQ_'+mon+'_wk'+str(week)+'.nc', 'r')
			s = ff.variables['Y'][:]
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Above normal probability forecast file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'ECMWF': 'http://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE%5BL%5Ddifferences%5BM%5Daverage%5Bhdate%5D0.33/0.66/0/replacebypercentile/percentile/0.66/VALUE/flaggt%5BM%5Daverage/100/mul//long_name/%28Probability%20of%20Above%20Normal%20Tercile%29def//units/%28%25%29def/data.nc',
#		'ECMWFrt': 'http://iridl.ldeo.columbia.edu/home/.jingyuan/.ECMWF/.realtime_S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE%5BL%5Ddifferences%5BM%5Daverage%5Bhdate%5D0.33/0.66/0/replacebypercentile/percentile/0.66/VALUE/flaggt%5BM%5Daverage/100/mul//long_name/%28Probability%20of%20Above%20Normal%20Tercile%29def//units/%28%25%29def/data.nc',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_fcst_noMOS/modelfcst_above_RFREQ'])
		#print("\n Short hindcast URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/modelfcst_above_RFREQ_"+mon+"_wk"+str(week)+".nc")

	#False force_download
	force_download = False

	#Below normal:
	if not force_download:
		try:
			ff=Dataset("noMOS/modelfcst_below_RFREQ_"+mon+"_wk"+str(week)+".nc", 'r')
			s = ff.variables['Y'][:]
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Below normal probability forecast file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'ECMWF': 'http://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE%5BL%5Ddifferences%5BM%5Daverage%5Bhdate%5D0.33/0.66/0/replacebypercentile/percentile/0.33/VALUE/flaglt%5BM%5Daverage/100/mul//long_name/%28Probability%20of%20Below%20Normal%20Tercile%29def//units/%28%25%29def/data.nc',
#		'ECMWFrt': 'http://iridl.ldeo.columbia.edu/home/.jingyuan/.ECMWF/.realtime_S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE%5BL%5Ddifferences%5BM%5Daverage%5Bhdate%5D0.33/0.66/0/replacebypercentile/percentile/0.33/VALUE/flaglt%5BM%5Daverage/100/mul//long_name/%28Probability%20of%20Below%20Normal%20Tercile%29def//units/%28%25%29def/data.nc',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_fcst_noMOS/modelfcst_below_RFREQ'])
		#print("\n Short hindcast URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/modelfcst_below_RFREQ_"+mon+"_wk"+str(week)+".nc")

	#False force_download
	force_download = False

	#The next block is used for noMOS flexible probabilistic forecasts ##Added by AGM
	#Ensemble mean:
	if not force_download:
		try:
			ff=Dataset('noMOS/modelfcst_mu_PRCP_'+mon+'_wk'+str(week)+'.nc', 'r')
			s = ff.variables['Y'][:]
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Ensemble mean file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/%5Bhdate%5Daverage/sub/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/data.nc',
#		'ECMWFrt': 'http://iridl.ldeo.columbia.edu/home/.jingyuan/.ECMWF/.realtime_S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Daverage/%5Bhdate%5Daverage/sub/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/data.nc',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_fcst_noMOS/modelfcst_mu_PRCP'])
		print("\n Ensemble mean URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/modelfcst_mu_PRCP_"+mon+"_wk"+str(week)+".nc")

	#False force_download
	force_download = False

	#Ensemble standard deviation:
	if not force_download:
		try:
			ff=Dataset("noMOS/modelfcst_std_PRCP_"+mon+"_wk"+str(week)+".nc", 'r')
			s = ff.variables['Y'][:]
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Ensemble standard deviation file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'ECMWF': 'http://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/%5BM%5Drmsover/c://name//water_density/def/998/%28kg/m3%29/:c/div//mm/unitconvert/data.nc',
#				'ECMWFrt': 'http://iridl.ldeo.columbia.edu/home/.jingyuan/.ECMWF/.realtime_S2S/.ECMF/.forecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/%5BM%5Drmsover/c://name//water_density/def/998/%28kg/m3%29/:c/div//mm/unitconvert/data.nc',
#				}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_fcst_noMOS/modelfcst_std_PRCP'])
		#print("\n Ensemble std URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/modelfcst_std_PRCP_"+mon+"_wk"+str(week)+".nc")

	#Obs mean:
	if not force_download:
		try:
			ff=Dataset('noMOS/obs_mu_PRCP_'+mon+'_wk'+str(week)+'.nc', 'r')
			s = ff.variables['Y'][:]
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs mean file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#		dic = { 'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/dup%5BT%5Daverage/sub/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/%5BT%5Daverage/data.nc',
#		'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/dup%5BT%5Daverage/sub/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/%5BT%5Daverage/data.nc',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_fcst_noMOS/obs_mu_PRCP'])
		#print("\n Obs mean URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/obs_mu_PRCP_"+mon+"_wk"+str(week)+".nc")

	#False force_download
	force_download = False

	#Obs std:
	if not force_download:
		try:
			ff=Dataset('noMOS/obs_std_PRCP_'+mon+'_wk'+str(week)+'.nc', 'r')
			s = ff.variables['Y'][:]
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Obs std file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary: http://iridl.ldeo.columbia.edu/home/.jingyuan/.ECMWF/.realtime_S2S/.ECMF/
#		dic = { 'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/%5BT%5Drmsover/data.nc',
#		'ECMWFrt': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.reforecast/.perturbed/.sfc_precip/.tp/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/L/('+str(day1)+')/('+str(day2)+')/VALUES/S/(last)/VALUE/%5BL%5Ddifferences/c%3A//name//water_density/def/998/(kg/m3)/%3Ac/div//mm/unitconvert/-999/setmissing_value/hdate/('+str(fyr-20)+')/('+str(hdate_last)+')/RANGE/dup/%5Bhdate%5Daverage/sub/%5BM%5Daverage/hdate//pointwidth/0/def/-6/shiftGRID/hdate/(days%20since%201960-01-01)/streamgridunitconvert/S/(days%20since%20'+str(fyr)+'-01-01)/streamgridunitconvert/S//units//days/def/L/hdate/add/add/0/RECHUNK/L/removeGRID//name//T/def/2/%7Bexch%5BS/hdate%5D//I/nchunk/NewIntegerGRID/replaceGRIDstream%7Drepeat/use_as_grid/'+obs_source+'/%5BX/Y%5D/regridAverage/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/T/(days%20since%201960-01-01)/streamgridunitconvert/T/'+str(nday)+'/runningAverage/'+str(nday)+'.0/mul/T/2/index/.T/SAMPLE/-999/setmissing_value/nip/T/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1901/ensotime/12./16/Jan/1920/ensotime/%3Agrid/replaceGRID//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/%5BT%5Drmsover/data.nc',
#		}
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_fcst_noMOS/obs_std_PRCP'])
		#print("\n Obs std URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > noMOS/obs_std_PRCP_"+mon+"_wk"+str(week)+".nc")

def GetForecast_T2M(day1, day2, fday, mon, fyr, nday, wlo1, elo1, sla1, nla1, wlo2, elo2, sla2, nla2, obs_source, key, week, nlag, model, hdate_last, threshold_pctle,training_season,wetday_threshold,force_download, dic_s2s):
	if not force_download:
		try:
			ff=open("modelfcst_T2M_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv", 'r')
			s = ff.readline()
		except OSError as err:
			print("\033[1mWarning:\033[0;0m {0}".format(err))
			print("Forecast file doesn't exist --\033[1mSOLVING: downloading file\033[0;0m")
			force_download = True
	if force_download:
		#dictionary:
#               dic = { 'ECMWF': 'https://iridl.ldeo.columbia.edu/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.2m_above_ground/.2t/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/LA/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Daverage/%5BM%5Daverage/SOURCES/.ECMWF/.S2S/.ECMF/.forecast/.perturbed/.sfc_temperature/.skt/Y/'+str(sla1)+'/'+str(nla1)+'/RANGE/X/'+str(wlo1)+'/'+str(elo1)+'/RANGE/LA/('+str(day1)+')/('+str(day2)+')/VALUES/S/(0000%20'+str(fday)+'%20'+mon+'%20'+str(fyr)+')/VALUE/%5BL%5Daverage/%5BM%5Daverage/%5Bhdate%5Daverage/sub//Celsius/unitconvert/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/1/Jan/3001/ensotime/12.0/1/Jan/3001/ensotime/%3Agrid/addGRID/T//pointwidth/0/def/pop//name/(temp)/def//units/(Celsius)/def//long_name/(2-m temperature)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv.gz',
#			  }
		# calls curl to download data
		#url=dic[model]
		url=eval(dic_s2s[model+'_fcst_T2M'])
		print("\n Forecast URL: \n\n "+url)
		get_ipython().system("curl -g -k -b '__dlauth_id="+key+"' '"+url+"' > modelfcst_T2M_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv.gz")
		get_ipython().system("gunzip -f modelfcst_T2M_"+mon+"_fday"+str(fday)+"_wk"+str(week)+".tsv.gz")
		#curl -g -k -b '__dlauth_id='$key'' ''$url'' > modelfcst_precip_fday${fday}.tsv

def CPTscript(model,mon,fday,lit,liti,wk,nla1,sla1,wlo1,elo1,nla2,sla2,wlo2,elo2,fprefix,mpref,training_season,ntrain,rainfall_frequency,MOS, xmodes_min, xmodes_max, ymodes_min, ymodes_max, ccamodes_min, ccamodes_max):
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
			# Opens GCM; because the calibration takes place via sklearn.linear_model (in the Jupyter notebook)
			f.write("614\n")
		elif MOS=='None':
			# Opens GCM (no calibration performed in CPT)
			f.write("614\n")
		else:
			print ("MOS option is invalid")

		# First, ask CPT to stop if error is encountered
		f.write("571\n")
		f.write("3\n")
		# Second, ask CPT to not show those menus again....  (deactivate this if debugging!)
		f.write("572\n")

		# Opens X input file
		f.write("1\n")
		if rainfall_frequency:
			file='../input/'+model+'_precip_'+mon+'_wk'+str(wk)+'.tsv\n'  #in the future: use model freq
		else:
			file='../input/'+model+'_precip_'+mon+'_wk'+str(wk)+'.tsv\n'
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
			#f.write("1\n")
			# Maximum number of X modes
			#f.write("10\n")
			# Minimum number of X modes
			f.write("{}\n".format(xmodes_min))
			# Maximum number of X modes
			f.write("{}\n".format(xmodes_max))

			# Opens forecast (X) file
			f.write("3\n")
			if rainfall_frequency:
				file='../input/'+model+'fcst_precip_'+mon+'_fday'+str(fday)+'_wk'+str(wk)+'.tsv\n'
			else:
				file='../input/'+model+'fcst_precip_'+mon+'_fday'+str(fday)+'_wk'+str(wk)+'.tsv\n'
			f.write(file)

		# Opens Y input file
		f.write("2\n")
		if rainfall_frequency:
			file='../input/obs_RFREQ_'+mon+'_wk'+str(wk)+'.tsv\n'
		else:
			file='../input/obs_precip_'+mon+'_wk'+str(wk)+'.tsv\n'
		f.write(file)
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
			#f.write("1\n")
			# Maximum number of Y modes
			#f.write("10\n")

			# Minimum number of CCA modes
			#f.write("1\n")
			# Maximum number of CCAmodes
			#f.write("5\n")
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
		f.write("1901\n")
		# Y training period
		f.write("5\n")
		# First year of Y training period
		f.write("1901\n")

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

		# Turn ON Transform predictand data
		f.write("541\n")
		# Turn ON zero bound for Y data	 (automatically on by CPT if variable is precip)
		#f.write("542\n")
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

		# NB: Default output format is GrADS format
		# select output format
		f.write("131\n")
		# GrADS format
		f.write("3\n")

		# # save goodness index
		# f.write("112\n")
		# file='../output/'+fprefix+'_'+mpref+'_Kendallstau_'+training_season+'_wk'+str(wk)+'\n'
		# f.write(file)

		## Cross-validation
		#f.write("311\n")   #--deactivated

		#Retroactive for s2s, due to the large sample size - deactivated for CFSv2
		f.write("312\n")
		#Length of initial training period: (Just quits w/o error msg if 80>ntrain)
		f.write(str(lit)+'\n')
		#Update interval:
		f.write(str(liti)+'\n')   #--old comment from AGM: 80 for speeding up tests, change to 20 later (~same results so far with 20 or 80)

		if MOS=='None': #for some weird reason for None we need to run it twice for it to work (at least in v16.2*)
			# Retroactive for s2s, due to the large sample size
			f.write("312\n")
			#Length of initial training period:
			f.write(str(lit)+'\n')
			#Update interval:
			f.write(str(lit)+'\n')   #--80 for speeding up tests, change to 20 later (~same results so far with 20 or 80)

		# NB: Default output format is GrADS format
		# select output format
		f.write("131\n")
		# Not GrADS, but txt format
		f.write("2\n")
		
		#set sig figs to 6 
		f.write('132\n')
		f.write('6\n')
		
		# save EOFs
		if MOS=='CCA':
			#save CCA time series maps data
			f.write('111\n')
			f.write('412\n')
			file='../output/'+model+fprefix+'_'+mpref+'_XCCATS_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)
			f.write('0\n')

			f.write('111\n')
			f.write('422\n')
			file='../output/'+model+fprefix+'_'+mpref+'_YCCATS_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)
			f.write('0\n')

			#save Canonical Correlations
			f.write('111\n')
			f.write('401\n')
			file='../output/'+model+fprefix+'_'+mpref+'_CCACorr_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)
			f.write('0\n')

			#save Y EOF time series 
			f.write('111\n')
			f.write('313\n')
			file='../output/'+model+fprefix+'_'+mpref+'_YEOFTS_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)
			f.write('0\n')

		if MOS=='CCA' or MOS=='PCR':
			#save X EOF time series 
			f.write('111\n')
			f.write('303\n')
			file='../output/'+model+fprefix+'_'+mpref+'_XEOFTS_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)
			f.write('0\n')

		# NB: Default output format is GrADS format
		# select output format
		f.write("131\n")
		# GrADS format
		f.write("3\n")


		# save EOFs
		if MOS=='CCA' or MOS=='PCR':
			f.write("111\n")
			#X EOF
			f.write("302\n")
			file='../output/'+model+fprefix+'_'+mpref+'_EOFX_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)
			#Exit submenu
			f.write("0\n")
		if MOS=='CCA':
			f.write("111\n")
			#Y EOF
			f.write("312\n")
			file='../output/'+model+fprefix+'_'+mpref+'_EOFY_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)
			#Exit submenu
			f.write("0\n")

			#save X and Y CCA Loadings Maps
			f.write('111\n')
			f.write('421\n')
			file='../output/'+model+fprefix+'_'+mpref+'_YCCAMAP_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)
			f.write('0\n')

			f.write('111\n')
			f.write('411\n')
			file='../output/'+model+fprefix+'_'+mpref+'_XCCAMAP_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)
			f.write('0\n')


		# cross-validated skill maps (413)
		# Retroactive skill maps (423)
		f.write("423\n")
		# save Pearson's Correlation
		f.write("1\n")
		file='../output/'+model+fprefix+'_'+mpref+'_Pearson_'+training_season+'_wk'+str(wk)+'\n'
		f.write(file)

		# cross-validated skill maps
		# Retroactive skill maps (423)
		f.write("423\n")
		# save Spearmans Correlation
		f.write("2\n")
		file='../output/'+model+fprefix+'_'+mpref+'_Spearman_'+training_season+'_wk'+str(wk)+'\n'
		f.write(file)

		# cross-validated skill maps
		# Retroactive skill maps (423)
		f.write("423\n")
		# save 2AFC score
		f.write("3\n")
		file='../output/'+model+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk'+str(wk)+'\n'
		f.write(file)

		# cross-validated skill maps
		# Retroactive skill maps (423)
		f.write("423\n")
		# save RocBelow score
		f.write("15\n")
		file='../output/'+model+fprefix+'_'+mpref+'_RocBelow_'+training_season+'_wk'+str(wk)+'\n'
		f.write(file)

		# cross-validated skill maps
		# Retroactive skill maps (423)
		f.write("423\n")
		# save RocAbove score
		f.write("16\n")
		file='../output/'+model+fprefix+'_'+mpref+'_RocAbove_'+training_season+'_wk'+str(wk)+'\n'
		f.write(file)

		#Now implementing forecasts for also noMOS case. Perhaps the best is to compute everything in the DL.
		#if MOS=='CCA' or MOS=='PCR':   #DO NOT USE CPT to compute probabilities if MOS='None' --use IRIDL for direct counting

		if MOS=='None':
		#######Probabilistic Forecasts Verification for NoMOS (PFV) --already computed if using retroactive option
			#Reliability diagram
			f.write("431\n")
			f.write("Y\n") #yes, save results to a file
			file='../output/'+model+fprefix+'_'+mpref+'RFCST_reliabdiag_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.txt\n'
			f.write(file)

			# select output format -- GrADS, so we can plot it in Python
			f.write("131\n")
			# GrADS format
			f.write("3\n")

			# Probabilistic skill maps
			f.write("437\n")
			# save Ignorance (all cats)
			f.write("101\n")
			file='../output/'+model+fprefix+'_'+mpref+'_Ignorance_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps
			f.write("437\n")
			# save Ranked Probability Skill Score (all cats)
			f.write("122\n")
			file='../output/'+model+fprefix+'_'+mpref+'_RPSS_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps
			f.write("437\n")
			# save Ranked Probability Skill Score (all cats)
			f.write("131\n")
			file='../output/'+model+fprefix+'_'+mpref+'_GROC_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

		#######FORECAST(S)	!!!!!
			# Re-opens X input file and use the short hindcasts so climo is consistent with forecast file
			f.write("1\n")
			f.write("Y\n")  #Yes to cleaning current results
			if rainfall_frequency:
				file='../input/noMOS/'+model+'short_precip_'+mon+'_wk'+str(wk)+'.tsv\n'  #in the future: use model freq
			else:
				file='../input/noMOS/'+model+'short_precip_'+mon+'_wk'+str(wk)+'.tsv\n'
			f.write(file)
			# Nothernmost latitude
			f.write(str(nla1)+'\n')
			# Southernmost latitude
			f.write(str(sla1)+'\n')
			# Westernmost longitude
			f.write(str(wlo1)+'\n')
			# Easternmost longitude
			f.write(str(elo1)+'\n')

			# Just in case CPT is confused: Open forecast (X) file
			f.write("3\n")
			if rainfall_frequency:
				file='../input/'+model+'fcst_precip_'+mon+'_fday'+str(fday)+'_wk'+str(wk)+'.tsv\n'
			else:
				file='../input/'+model+'fcst_precip_'+mon+'_fday'+str(fday)+'_wk'+str(wk)+'.tsv\n'
			f.write(file)

			# Re-opens Y input file, and use short version to be consistent with hindcasts above
			f.write("2\n")
			if rainfall_frequency:
				file='../input/noMOS/obsshort_RFREQ_'+mon+'_wk'+str(wk)+'.tsv\n'
			else:
				file='../input/noMOS/obsshort_precip_'+mon+'_wk'+str(wk)+'.tsv\n'
			f.write(file)
			# Nothernmost latitude
			f.write(str(nla2)+'\n')
			# Southernmost latitude
			f.write(str(sla2)+'\n')
			# Westernmost longitude
			f.write(str(wlo2)+'\n')
			# Easternmost longitude
			f.write(str(elo2)+'\n')

			# Need to shorten the length of training period
			f.write("7\n")
			# Length of training period
			f.write("20\n")

			# Cross-validation due to the shorter sample (only 20 steps) --just for forecast purposes. Skill computed with retroactive
			f.write("311\n")

		# Probabilistic (3 categories) maps
		f.write("455\n")
		# Output results
		f.write("111\n")
		# Forecast probabilities
		f.write("501\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_P_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)
		#502 # Forecast odds
		#Exit submenu
		f.write("0\n")

		# Compute deterministic values and prediction limits
		f.write("454\n")
		# Output results
		f.write("111\n")
		# Forecast values
		f.write("511\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_V_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)
		#502 # Forecast odds


		#######Following files are used to plot the flexible format
		# Save cross-validated predictions
		f.write("201\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_xvPr_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)
		# Save deterministic forecasts [mu for Gaussian fcst pdf]
		f.write("511\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_mu_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)
		# Save prediction error variance [sigma^2 for Gaussian fcst pdf]
		f.write("514\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_var_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)
		# Save z
		#f.write("532\n")
		#file='../output/'+fprefix+'_'+mpref+'FCST_z_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		#f.write(file)
		# Save predictand [to build predictand pdf]
		f.write("102\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_Obs_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)

		#Exit submenu
		f.write("0\n")
        
#========================================================================================================
# This piece of the scripts is copied from PyCPT Seasonal for the plotting requirement in txt format 
#                                    Bohar Singh (06/11/2021)
#========================================================================================================
			
		# Change to ASCII format to send files to DL
		f.write("131\n")
			# ASCII format
		f.write("2\n")
			# Output results
		f.write("111\n")
			# Save cross-validated predictions
		f.write("201\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_xvPr_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)
			# Save deterministic forecasts [mu for Gaussian fcst pdf]
		f.write("511\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_mu_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)
			# Forecast probabilities
		f.write("501\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_P_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)
			# Save prediction error variance [sigma^2 for Gaussian fcst pdf]
		f.write("514\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_var_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)
			# Save z
		f.write("532\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_z_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)
			# Save predictand [to build predictand pdf]
		f.write("102\n")
		file='../output/'+model+fprefix+'_'+mpref+'FCST_obs_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
		f.write(file)
		#Exit submenu
		f.write("0\n")
#========================================================================================================
        
		if MOS=='CCA' or MOS=='PCR':
			
			# ###########PFV --Added by AGM in version 1.5
			# #Compute and write retrospective forecasts for prob skill assessment.
			# #Re-define forecas file
			# f.write("3\n")
			# if rainfall_frequency:
			# 	file='../input/model_precip_'+mon+'_wk'+str(wk)+'.tsv\n'  #in the future: use model freq
			# else:
			# 	file='../input/model_precip_'+mon+'_wk'+str(wk)+'.tsv\n'
			# f.write(file)
			# #Forecast period settings
			# f.write("6\n")
			# # First year to forecast. Save ALL forecasts (for "retroactive" we should only assess second half)
			# f.write("1901\n")
			# #Number of forecasts option
			# f.write("9\n")
			# # Number of reforecasts to produce
			# f.write("160\n")
			# # Change to ASCII format to re0use in CPT
			# f.write("131\n")
			# # ASCII format
			# f.write("2\n")
			# # Probabilistic (3 categories) maps
			# f.write("455\n")
			# # Output results
			# f.write("111\n")
			# # Forecast probabilities --Note change in name for reforecasts:
			# f.write("501\n")
			# file='../output/'+fprefix+'_'+mpref+'RFCST_P_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'\n'
			# f.write(file)
			# #502 # Forecast odds
			# #Exit submenu
			# f.write("0\n")
			#
			# # Close X file so we can access the PFV option
			# f.write("121\n")
			# f.write("Y\n")  #Yes to cleaning current results:# WARNING:
			# #Select Probabilistic Forecast Verification (PFV)
			# f.write("621\n")
			# # Opens X input file
			# f.write("1\n")
			# file='../output/'+fprefix+'_'+mpref+'RFCST_P_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.txt\n'
			# f.write(file)
			# # Nothernmost latitude
			# f.write(str(nla2)+'\n')
			# # Southernmost latitude
			# f.write(str(sla2)+'\n')
			# # Westernmost longitude
			# f.write(str(wlo2)+'\n')
			# # Easternmost longitude
			# f.write(str(elo2)+'\n')
			#
			# f.write("5\n")
			# # First year of the PFV
			# # for "retroactive" only second half of the entire period should be used --this value is for ECMWF only)
			# fypfv=1901+lit
			# f.write(str(fypfv)+'\n')
			# #f.write("1901\n")
			#
			# #Verify
			# f.write("313\n")
			#
			# #Reliability diagram
			# f.write("431\n")
			# f.write("Y\n") #yes, save results to a file
			# file='../output/'+fprefix+'_'+mpref+'RFCST_reliabdiag_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.txt\n'
			# f.write(file)
			#
			# # select output format -- GrADS, so we can plot it in Python
			f.write("131\n")
			 # GrADS format
			f.write("3\n")

			# Probabilistic skill maps (all cats)
			f.write("437\n")
			# save Ignorance (all cats)
			f.write("101\n")
			file='../output/'+model+fprefix+'_'+mpref+'_Ignorance_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (all cats)
			f.write("437\n")
			# save Ranked Probability Skill Score (all cats)
			f.write("122\n")
			file='../output/'+model+fprefix+'_'+mpref+'_RPSS_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (all cats)
			f.write("437\n")
			# save Ranked Probability Skill Score (all cats)
			f.write("131\n")
			file='../output/'+model+fprefix+'_'+mpref+'_GROC_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (Above normal)
			f.write("437\n")
			# save Ignorance (Above normal)
			f.write("201\n")
			f.write("3\n") #3 is above normal
			file='../output/'+model+fprefix+'_'+mpref+'_Ignorance_AN_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (Above normal)
			f.write("437\n")
			# save Ignorance Reliability (Above normal)
			f.write("202\n")
			f.write("3\n") #3 is above normal
			file='../output/'+model+fprefix+'_'+mpref+'_RelIgn_AN_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (Above normal)
			f.write("437\n")
			# save Ignorance Resolution (Above normal)
			f.write("203\n")
			f.write("3\n") #3 is above normal
			file='../output/'+model+fprefix+'_'+mpref+'_ResIgn_AN_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (Below normal)
			f.write("437\n")
			# save Ignorance (Below normal)
			f.write("201\n")
			f.write("1\n") #1 is below normal
			file='../output/'+model+fprefix+'_'+mpref+'_Ignorance_BN_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (Below normal)
			f.write("437\n")
			# save Ignorance Reliability (Below normal)
			f.write("202\n")
			f.write("1\n") #1 is below normal
			file='../output/'+model+fprefix+'_'+mpref+'_RelIgn_BN_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (Below normal)
			f.write("437\n")
			# save Ignorance Resolution (Below normal)
			f.write("203\n")
			f.write("1\n") #3 is below normal
			file='../output/'+model+fprefix+'_'+mpref+'_ResIgn_BN_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

    		#Exit submenu
			f.write("0\n")


		# Exit
		f.write("0\n")

		f.close()
		get_ipython().system('cp params '+model+fprefix+'_'+mpref+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.cpt')


def read_forecast( fcst_type, model, fprefix,mpref,training_season,wki,fday,mon,fyr, filename='None', converting_tsv=False):
    if filename == 'None':
        if fcst_type == 'deterministic':
            try:
                f = open("./output/"+model+fprefix+'_'+mpref+ 'FCST_mu_' +training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.txt', 'r')
            except:
                f = open("./output/"+model+fprefix+'_'+mpref+ 'FCST_mu_' +training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.tsv', 'r')
        elif fcst_type == 'probabilistic':
            f = open("./output/"+model+fprefix+'_'+mpref+ 'FCST_P_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.txt', 'r')
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
                    
                    years.append(int(line.split(',')[1][7:11]))
                    #print(int(line.split(',')[1][7:11]))
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

def writeGrads(fcsttype, filename, models, fprefix,mpref,id,training_season,wk,fday,mon,fyr):
    if fcsttype == 'FCST_Obs':
        lats, longs, data, years = read_forecast('deterministic', models[0], fprefix,mpref,training_season,wk,fday,mon,fyr, filename='../input/MME_precip_'+mon+'_wk'+str(wk)+'.tsv', converting_tsv=True)
    else:
        lats, longs, data, years = read_forecast('deterministic', models[0], fprefix,mpref,training_season,wk,fday,mon,fyr, filename='../output/MME'+fprefix+'_'+mpref+fcsttype+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.tsv', converting_tsv=True)
    W, XD = len(longs), longs[1] - longs[0]
    H, YD = len(lats), lats[0] - lats[1]
    T = len(years)
    f=open('../output/MME'+fprefix+'_'+mpref+fcsttype+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.ctl','w')
    f.write('DSET {}\n'.format('../output/MME'+fprefix+'_'+mpref+fcsttype+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.dat'))
    f.write('TITLE {}\n'.format('NextGen_{}'.format(fcsttype)))
    f.write('UNDEF -999.000000\n')
    f.write('OPTIONS yrev sequential little_endian\n')
    f.write('XDEF {} LINEAR {} {}\n'.format(W, longs[0], XD))
    f.write('YDEF {} LINEAR {} {}\n'.format(H, lats[-1], YD))
    f.write('TDEF {} LINEAR 1{}{} 1yr\n'.format(T, 'Jan', years[0]))
    f.write('ZDEF 1 LINEAR 1 1\n')
    f.write('VARS 1\n')
    f.write('a 0 99 {} unitless\n'.format(fprefix))
    f.write('ENDVARS\n')
    f.close()
    print('Wrote {}'.format('../output/MME'+fprefix+'_'+mpref+fcsttype+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.ctl'))

    f=open('../output/MME'+fprefix+'_'+mpref+fcsttype+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.dat','wb')
    for t in range(T):
        data[t][np.isnan(data[t])] = -999.000
        f.write(struct.pack('i', int(W*H*np.dtype('float32').itemsize)))
        for i in range(H):
            for j in range(W):
                f.write(struct.pack('f', float(data[t][i][j])))
        f.write(struct.pack('i', int(W*H*np.dtype('float32').itemsize)))
    f.close()
    print('Wrote {}'.format('../output/MME'+fprefix+'_'+mpref+fcsttype+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.dat'))



def readGrADSctl(models,fprefix,mpref,id,training_season,wki,fday,mon,fyr):
	model =models[0]
	#Read grads binary file size H, W, T
	with open('../output/'+model+fprefix+'_'+mpref+id+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.ctl', "r") as fp:
		for line in lines_that_contain("XDEF", fp):
			W = int(line.split()[1])
			Wi= float(line.split()[3])
			XD= float(line.split()[4])
	with open('../output/'+model+fprefix+'_'+mpref+id+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.ctl', "r") as fp:
		for line in lines_that_contain("YDEF", fp):
			H = int(line.split()[1])
			Hi= float(line.split()[3])
			YD= float(line.split()[4])
	with open('../output/'+model+fprefix+'_'+mpref+id+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.ctl', "r") as fp:
		for line in lines_that_contain("TDEF", fp):
			T = int(line.split()[1])
			Ti= int((line.split()[3])[-4:])
			TD= 1  #not used
	return (W, Wi, XD, H, Hi, YD, T, Ti, TD)


def NGensemble(models,fprefix,mpref,id,training_season,wki,fday,mon,fyr):
    """A simple function for computing the NextGen ensemble

    PARAMETERS
    ----------
    models: array with selected models
    """
    nmods=len(models)
    #print(wki,id)
    W, Wi, XD, H, Hi, YD, T, Ti, TD = readGrADSctl(models,fprefix,mpref,id,training_season,wki,fday,mon,fyr)
    
    ens  =np.empty([nmods,T,H,W])  #define array for later use
    #print(ens.shape)
    #print(T)
    k=-1
    for model in models:
        k=k+1 #model
        memb0=np.empty([T,H,W])  #define array for later us

        #Since CPT writes grads files in sequential format, we need to excise the 4 bytes between records (recl)
        f=open('../output/'+model+fprefix+'_'+mpref+id+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.dat','rb')
        #print(filepath+model+fprefix+'_'+mpref+id+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.dat')
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
        #print(ens)
    # NextGen ensemble mean (perhaps try median too?)
    NG=np.nanmean(ens, axis=0)  #axis 0 is ensemble member
    #Now write output:
    #writeCPT(NG,'../output/NextGen_'+fprefix+'_'+tar+'_ini'+mon+'.tsv',models,fprefix,predictand,mpref,id,tar,mon,tgti,tgtf,monf,fyr)
    
    if id=='FCST_xvPr':
        writeCPT(NG,'../input/MME_precip_'+mon+'_wk'+str(wki)+'.tsv',models,fprefix,mpref,id,training_season,wki,fday,mon,fyr)
        print('Cross-validated prediction files successfully produced')
    if id=='FCST_mu':
        writeCPT(NG,'../output/MME'+fprefix+'_'+mpref+'FCST_mu_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.tsv',models,fprefix,mpref,id,training_season,wki,fday,mon,fyr)
        writeGrads(id, '../output/MME'+fprefix+'_'+mpref+'FCST_mu_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.tsv', models, fprefix,mpref,id,training_season,wki,fday,mon,fyr)
        print('Forecast files successfully produced')
    if id=='FCST_var':
        writeCPT(NG,'../output/MME'+fprefix+'_'+mpref+'FCST_var_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.tsv',models,fprefix,mpref,id,training_season,wki,fday,mon,fyr)
        writeGrads(id, '../output/MME'+fprefix+'_'+mpref+'FCST_var_'+training_season+'_'+mon+str(fday)+'_wk'+str(wki)+'.tsv', models, fprefix,mpref,id,training_season,wki,fday,mon,fyr)
        print('Forecast error files successfully produced')
        #Added by Bohar to write FCST_Obs grads file for MME in output
        writeGrads('FCST_Obs', '../input/MME_precip_'+mon+'_wk'+str(wki)+'.tsv', models, fprefix,mpref,'FCST_Obs',training_season,wki,fday,mon,fyr)

def writeCPT(var,outfile,models,fprefix,mpref,id,training_season,wki,fday,mon,fyr):
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

    #Read grads file to get needed coordinate arrays
    W, Wi, XD, H, Hi, YD, T, Ti, TD = readGrADSctl(models,fprefix,mpref,id,training_season,wki,fday,mon,fyr)
    
    Tarr = np.arange(Ti, Ti+T)
    Xarr = np.linspace(Wi, Wi+W*XD,num=W+1)
    Yarr = np.linspace(Hi+H*YD, Hi,num=H+1)

    #Now write the CPT file
    f = open(outfile, 'w')
    f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
    f.write("xmlns:cf=http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/\n")   #not really needed
    f.write("cpt:nfields=1\n")
    #f.write("cpt:T	" + str(Tarr)+"\n")  #not really needed
    for it in range(T):
        
        f.write("cpt:field="+vari+", cpt:T="+str(Tarr[it])+"-"+str(TD)+"-01T00:00,cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+",cf:standard_name=precipitation_flux,cpt:missing=-999.\n")
        f.write("\t")
        np.savetxt(f, Xarr[0:-1], fmt="%.6f",newline='\t') #f.write(str(Xarr)[1:-1])
        f.write("\n") #next line
        for iy in range(H):
            #f.write(str(Yarr[iy]) + "\t" + str(var[it,iy,0:-1])[1:-1]) + "\n")
            np.savetxt(f,np.r_[Yarr[iy+1],var[it,iy,0:]],fmt="%.6f", newline='\t')  #excise extra line
            f.write("\n") #next line
    f.close()



def plt_ng_probabilistic(models,loni,lone,lati,late,fprefix,mpref,training_season,wk,nwk,fday,mon,fyr, use_ocean):
	cbar_loc, fancy = 'bottom', True
	nmods=len(models)
	#nwk=len(wk)
	xdim=1
	#
	list_probabilistic_by_season = [[[], [], []] for i in range(nwk)]
	list_det_by_season = [[] for i in range(nwk)]
	for i in range(nmods):
		for j in range(nwk):
			if platform.system() == "Windows":
				plats, plongs, av, years = read_forecast_bin('probabilistic', models[i], fprefix,mpref,training_season,wk[j],fday,mon,fyr )
			else:
				plats, plongs, av, years = read_forecast('probabilistic', models[i], fprefix,mpref,training_season,wk[j],fday,mon,fyr)
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
	for j in range(nwk):
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

	fig, ax = plt.subplots(nrows=xdim, ncols=nwk, figsize=(nwk*13, xdim*10), sharex=False,sharey=False, subplot_kw={'projection': ccrs.PlateCarree()})

	if nwk == 1:
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
		for j in range(nwk):
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
			ax[i][j].set_title("Probabilistic forecast for Week "+str(wk[j]),fontsize=30)
			
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


def plt_ng_deterministic(models,loni,lone,lati,late,fprefix,mpref,training_season,wk,nwk,fday,mon,fyr, use_ocean):
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
	#nwk=len(wk)

	xdim = 1
	list_probabilistic_by_season = [[[], [], []] for i in range(nwk)]
	list_det_by_season = [[] for i in range(nwk)]
	for i in range(nmods):
		for j in range(nwk):
			#plats, plongs, av = read_forecast('probabilistic', models[i], predictand, mpref, mons[j], mon, fyr )
			#list_probabilistic_by_season[j][0].append(av[0])
			#list_probabilistic_by_season[j][1].append(av[1])
			#list_probabilistic_by_season[j][2].append(av[2])
			wki =wk[j]
			#print(wki, wk)
			dlats, dlongs, av, years = read_forecast('deterministic', models[i],fprefix,mpref,training_season,wki,fday,mon,fyr,filename='None', converting_tsv=False)
			list_det_by_season[j].append(av[0])

	ng_probfcst_by_season = []
	ng_detfcst_by_season = []
	for j in range(nwk):
		d_array = np.asarray(list_det_by_season[j])
		d_nanmean = np.nanmean(d_array, axis=0)
		ng_detfcst_by_season.append(d_nanmean)

	fig, ax = plt.subplots(nrows=xdim, ncols=nwk, figsize=(nwk*13, xdim*10), sharex=True,sharey=True, subplot_kw={'projection': ccrs.PlateCarree()})
	if nwk == 1:
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
		for j in range(nwk):
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
			ax[i][j].set_title("Deterministic forecast for Week "+str(wk[j]),fontsize=30)

			#fancy deterministic
			var = ng_detfcst_by_season[j]

		#	bounds = [int(xx) for xx in np.linspace(0, np.nanmax(var), 11)]
			CS_det = ax[i][j].pcolormesh(np.linspace(longs[0], longs[-1],num=len(longs)), np.linspace(lats[0], lats[-1], num=len(lats)), var,
				#norm=MidpointNormalize(midpoint=0.)
				vmin =-20,vmax=20,
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
	

def ELR_CPTscript(model,mon,fday,lit,liti,wk,nla1,sla1,wlo1,elo1,nla2,sla2,wlo2,elo2,fprefix,mpref,training_season,ntrain,rainfall_frequency,MOS,flag):
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
		# Second, ask CPT to not show those menus again....  (deactivate this if debugging!)
		#f.write("572\n")

		# Opens X input file
		f.write("1\n")
		if flag==1:
			file='../input/'+model+'_precip_'+mon+'_wk'+str(wk)+'_elr_training.tsv\n'
		else:
			file='../input/'+model+'_precip_'+mon+'_wk'+str(wk)+'_elr_verification.txt\n'
		f.write(file)
		# Nothernmost latitude
		f.write(str(nla1)+'\n')
		# Southernmost latitude
		f.write(str(sla1)+'\n')
		# Westernmost longitude
		f.write(str(wlo1)+'\n')
		# Easternmost longitude
		f.write(str(elo1)+'\n')

		# Opens Y input file
		f.write("2\n")
		if flag==1:
			file='../input/obs_precip_'+mon+'_wk'+str(wk)+'_training.tsv\n'
		else:
			file='../input/obs_precip_'+mon+'_wk'+str(wk)+'_verification.tsv\n'
		f.write(file)
		# Nothernmost latitude
		f.write(str(nla2)+'\n')
		# Southernmost latitude
		f.write(str(sla2)+'\n')
		# Westernmost longitude
		f.write(str(wlo2)+'\n')
		# Easternmost longitude
		f.write(str(elo2)+'\n')

		# Turn ON Transform predictand data
		f.write("541\n")
		# Turn ON zero bound for Y data  (automatically on by CPT if variable is precip)
		#f.write("542\n")
		# Turn ON synchronous predictors
		f.write("545\n")

		### Missing value options
		#f.write("544\n")
		# Missing value X flag:
		#if flag==1:
		#       blurb='-999\n'
		#       f.write(blurb)
		#else:
		#       blurb='-1\n'
		#       f.write(blurb)
		# Maximum % of missing values
		#f.write("10\n")
		# Maximum % of missing gridpoints
		#f.write("10\n")
		# Number of near-neighbors
		#f.write("1\n")
		# Missing value replacement : best-near-neighbors
		#f.write("4\n")
		# Y missing value flag
		#blurb='-999\n'
		#f.write(blurb)
		# Maximum % of missing values
		#f.write("10\n")
		# Maximum % of missing stations
		#f.write("10\n")
		# Number of near-neighbors
		#f.write("1\n")
		# Best near neighbor
		#f.write("4\n") 

		#Open forecast (X) file
		#f.write("3\n")
		#if rainfall_frequency:
		#	file='../input/'+model+'fcst_precip_'+mon+'_fday'+str(fday)+'_wk'+str(wk)+'.tsv\n'
		#else:
		#	file='../input/'+model+'fcst_precip_'+mon+'_fday'+str(fday)+'_wk'+str(wk)+'.tsv\n'
		#f.write(file)

		if flag==1:
			# Retroactive for s2s, due to the large sample size
			f.write("312\n")
			#Length of initial training period:
			f.write(str(lit)+'\n')
			#Update interval:
			f.write(str(liti)+'\n')
			# for some weird reason we need to run it twice for it to work
			f.write("312\n")
			#Length of initial training period:
			f.write(str(lit)+'\n')
			#Update interval:
			f.write(str(liti)+'\n')

			# select output format -- GrADS, so we can plot it in Python
			f.write("131\n")
			# GrADS format
			f.write("3\n")

			# Just in case CPT is confused: Open forecast (X) file
			f.write("3\n")
			if rainfall_frequency:
				file='../input/'+model+'fcst_precip_'+mon+'_fday'+str(fday)+'_wk'+str(wk)+'.tsv\n'
			else:
				file='../input/'+model+'fcst_precip_'+mon+'_fday'+str(fday)+'_wk'+str(wk)+'.tsv\n'
			f.write(file)

			#502 # Forecast odds
			#Exit submenu
			#f.write("0\n")

			# cross-validated skill maps(413)
			# Retroactive skill maps (423)
			f.write("423\n")
			# save Pearson's Correlation
			f.write("1\n")
			file='../output/'+model+fprefix+'_'+mpref+'_Pearson_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# cross-validated skill maps(413)
			# Retroactive skill maps (423)
			f.write("423\n")
			# save Spearmans Correlation
			f.write("2\n")
			file='../output/'+model+fprefix+'_'+mpref+'_Spearman_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# cross-validated skill maps
			# Retroactive skill maps (423)
			f.write("423\n")
			# save 2AFC score
			f.write("3\n")
			file='../output/'+model+fprefix+'_'+mpref+'_2AFC_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# cross-validated skill maps
			# Retroactive skill maps (423)
			f.write("423\n")
			# save RocBelow score
			f.write("15\n")
			file='../output/'+model+fprefix+'_'+mpref+'_RocBelow_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# cross-validated skill maps
			# Retroactive skill maps (423)
			f.write("423\n")
			# save RocAbove score
			f.write("16\n")
			file='../output/'+model+fprefix+'_'+mpref+'_RocAbove_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			#Exit submenu
			f.write("0\n")

		else:

			#f.write("5\n")
			# First year of the PFV
			fypfv=1901+lit
			f.write(str(fypfv)+'\n')

			#Verify
			f.write("313\n")

			#Reliability diagram
			f.write("431\n")
			f.write("Y\n") #yes, save results to a file
			file='../output/'+model+fprefix+'_'+mpref+'RFCST_reliabdiag_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'.txt\n'
			f.write(file)

			# select output format -- GrADS, so we can plot it in Python
			f.write("131\n")
			# GrADS format
			f.write("3\n")
			# Probabilistic skill maps (all cats)
			f.write("437\n")
			# save Ignorance (all cats)
			f.write("101\n")
			file='../output/'+model+fprefix+'_'+mpref+'_Ignorance_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (all cats)
			f.write("437\n")
			# save Ranked Probability Skill Score (all cats)
			f.write("122\n")
			file='../output/'+model+fprefix+'_'+mpref+'_RPSS_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (all cats)
			f.write("437\n")
			# save Ranked Probability Skill Score (all cats)
			f.write("131\n")
			file='../output/'+model+fprefix+'_'+mpref+'_GROC_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (Above normal)
			f.write("437\n")
			# save Ignorance (Above normal)
			f.write("201\n")
			f.write("3\n") #3 is above normal
			file='../output/'+model+fprefix+'_'+mpref+'_Ignorance_AN_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (Above normal)
			f.write("437\n")
			# save Ignorance Reliability (Above normal)
			f.write("202\n")
			f.write("3\n") #3 is above normal
			file='../output/'+model+fprefix+'_'+mpref+'_RelIgn_AN_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (Above normal)
			f.write("437\n")
			# save Ignorance Resolution (Above normal)
			f.write("203\n")
			f.write("3\n") #3 is above normal
			file='../output/'+model+fprefix+'_'+mpref+'_ResIgn_AN_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (Below normal)
			f.write("437\n")
			# save Ignorance (Below normal)
			f.write("201\n")
			f.write("1\n") #1 is below normal
			file='../output/'+model+fprefix+'_'+mpref+'_Ignorance_BN_'+training_season+'_wk'+str(wk)+'\n'

			f.write(file)

			# Probabilistic skill maps (Below normal)
			f.write("437\n")
			# save Ignorance Reliability (Below normal)
			f.write("202\n")
			f.write("1\n") #1 is below normal
			file='../output/'+model+fprefix+'_'+mpref+'_RelIgn_BN_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

			# Probabilistic skill maps (Below normal)
			f.write("437\n")
			# save Ignorance Resolution (Below normal)
			f.write("203\n")
			f.write("1\n") #3 is below normal
			file='../output/'+model+fprefix+'_'+mpref+'_ResIgn_BN_'+training_season+'_wk'+str(wk)+'\n'
			f.write(file)

		# Exit
		f.write("0\n")

		f.close()
		if flag ==1:
			get_ipython().system('cp params '+fprefix+'_'+mpref+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'_training.cpt')
		else:
			get_ipython().system('cp params '+fprefix+'_'+mpref+'_'+training_season+'_'+mon+str(fday)+'_wk'+str(wk)+'_verification.cpt')
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


def ELRscript(model,mon,fday,fyr,day1,day2,nday,hdate_last,lit,liti,wk,nla1,sla1,wlo1,elo1,nla2,sla2,wlo2,elo2,fprefix,mpref,training_season,ntrain,rainfall_frequency,MOS):

	"""Function to perform ELR forecast

	"""

#%% model Hindcasts 
	fh_xh = Dataset('../input/'+model+'_precip_'+mon+'_wk'+str(wk)+'.nc', mode='r')
	fh_yh = Dataset('../input/obs_precip_'+mon+'_wk'+str(wk)+'_hc.nc', mode='r')

	lons = fh_xh.variables['X'][:]
	lats = fh_xh.variables['Y'][:]

	x = fh_xh.variables['tp'][:]; x = np.squeeze(x)
	y = fh_yh.variables['tp'][:]
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
	#elr_hc = np.empty((ndat,nlat,nlon,3)); elr_hc.fill(np.nan)
	#elr_hc = np.empty((int(ndat/2),nlat,nlon)); elr_hc.fill(np.nan)
	elr_hc = np.empty((lit,nlat,nlon)); elr_hc.fill(np.nan)
	ijland = -1
	for j in range(nlat):
		for i in range(nlon):
			if msk[j,i] == False:    # fit model just for landpoints
				ijland = ijland + 1
				elrmodel, terciles = elr_dict[ijland]
				#elr_hc[:,j,i,:] = elr_tercilesPredict(xm[:,j,i], terciles, elrmodel)
				elr_hc[:,j,i] = elr_quantilePredict(xm[:,j,i], elrmodel)

#       ijland = index_land[lat1, lon1]
#       elrmodel, terciles = elr_dict[ijland]
#       elrmodel_climo, terciles = elr_climo_dict[ijland]
#       poe, q_fcst, q_clim, = elr_poe( xm[idat,lat1,lon1], elrmodel, elrmodel_climo )
#       plt.figure()

	#print('Set of ELR hindcasts made on a map of xy gridpoints')
#---------------------------------------------
	#Now write the CPT file
	outfile=model+'_precip_'+mon+'_wk'+str(wk)+'_elr_training.tsv'
	f = open(outfile, 'w')
	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
	f.write("cpt:nfields=1\n")
	W=nlon
	H=nlat
	#T=int(ndat/2)
	T=lit
	Xarr=lons
	Yarr=lats[::-1]
	vari='tp'
	var=np.flip(elr_hc, axis=1)
	var[np.isnan(var)]=-999. #use CPT missing value
	dss=xr.open_dataset('../input/'+model+'_precip_'+mon+'_wk'+str(wk)+'.nc',decode_times=False)
	a=list(dss)
	units=dss[a[0]].units
	Tarr=np.empty(ndat,dtype=int)
	for it in range(ndat):
		Tarr[it]=1901+it

	for it in range(T):
		f.write("cpt:field="+vari+", cpt:T="+str(Tarr[it])+"-01, cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		f.write("\t")
		np.savetxt(f, Xarr, fmt="%.1f",newline='\t')
		f.write("\n") #next line
		for iy in range(H):
			np.savetxt(f,np.r_[Yarr[iy],var[it,iy,0:]],fmt="%.4f", newline='\t')  #excise extra line
			f.write("\n") #next line
	f.close()

	#write CPT for observation
	outfile='obs_precip_'+mon+'_wk'+str(wk)+'_training.tsv'
	f = open(outfile, 'w')
	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
	f.write("cpt:nfields=1\n")
	W=nlon
	H=nlat
	Xarr=lons
	Yarr=lats[::-1]
	vari='tp'
	var=np.flip(y[0:lit,:,:], axis=1)
	var[np.isnan(var)]=-999. #use CPT missing value
	dss=xr.open_dataset('../input/obs_precip_'+mon+'_wk'+str(wk)+'_hc.nc',decode_times=False)
	a=list(dss)
	units=dss[a[0]].units
	T1=lit
	for it in range(T1):
		f.write("cpt:field="+vari+", cpt:T="+str(Tarr[it])+"-01, cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		f.write("\t")
		np.savetxt(f, Xarr, fmt="%.1f",newline='\t')
		f.write("\n") #next line
		for iy in range(H):
			np.savetxt(f,np.r_[Yarr[iy],var[it,iy,0:]],fmt="%.4f", newline='\t')  #excise extra line
			f.write("\n") #next line
	f.close()

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
	outfile=model+'_precip_'+mon+'_wk'+str(wk)+'_elr_verification.txt'
	f = open(outfile, 'w')
	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
	f.write("cpt:nfields=1\n")
	f.write("cpt:ncats=3\n")
	W=nlon
	H=nlat
	ds=xr.open_dataset('../input/'+model+'_precip_'+mon+'_wk'+str(wk)+'.nc',decode_times=False)
	T=ndat-lit
	Tarr1=Tarr[lit:]
	Xarr=lons
	Yarr1=lats
	Yarr=Yarr1[::-1] #Y should from N to S
	vari='tp'
	var=np.flip(elr_fc, axis=1)*100
	var[np.isnan(var)]=-1.0 #use CPT missing value

	for it in range(T):
		f.write("cpt:field="+vari+", cpt:C=1, cpt:clim_prob=0.33333333333300003, cpt:T="+str(Tarr1[it])+"-01, cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units=probability (%), cpt:missing=-1.0000000000000000\n")
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
	outfile='obs_precip_'+mon+'_wk'+str(wk)+'_verification.tsv'
	f = open(outfile, 'w')
	f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n")
	f.write("cpt:nfields=1\n")
	W=nlon
	H=nlat
	Xarr=lons
	Yarr=lats[::-1]
	vari='tp'
	#var=np.flip(y[int(ndat/2):,:,:], axis=1)
	var=np.flip(y[lit:,:,:], axis=1)
	var[np.isnan(var)]=-999. #use CPT missing value
	dss=xr.open_dataset('../input/obs_precip_'+mon+'_wk'+str(wk)+'_hc.nc',decode_times=False)
	a=list(dss)
	units=dss[a[0]].units
	#T1=int(ndat/2)
	T1=ndat-lit
	for it in range(T1):
		f.write("cpt:field="+vari+", cpt:T="+str(Tarr1[it])+"-01, cpt:nrow="+str(H)+", cpt:ncol="+str(W)+", cpt:row=Y, cpt:col=X, cpt:units="+units+", cpt:missing=-999.\n")
		f.write("\t")
		np.savetxt(f, Xarr, fmt="%.1f",newline='\t')
		f.write("\n") #next line
		for iy in range(H):
			np.savetxt(f,np.r_[Yarr[iy],var[it,iy,0:]],fmt="%.4f", newline='\t')  #excise extra line
			f.write("\n") #next line
	f.close()




