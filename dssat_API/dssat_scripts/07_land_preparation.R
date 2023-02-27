###############################################################################################
## This script creates a couple of functions ("land_preparation_day" and summary_table) to   ##
## get, for a certain number of crop simulation scenarios, the day of the year when the soil ##
## water content is ideal to start land preparation before planting. Additionally, the       ##
## script allows getting descriptive statistics of land preparation dates.                   ##
##                                                                                           ##
## Created by: Camilo Barrios-Perez (c.barrios@cgiar.org)                                    ##
## Postdoc: Alliance CIAT-Bioversity International                                           ##
###############################################################################################

###############
## Libraries ##
###############

library (readr)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)

################################################################################
## Create the "land_preparation_day" function to get, for a certain number of ##
## crop simulation scenarios, the day of the year when the soil water content ##
## is ideal to start land preparation before planting.                        ##
################################################################################

land_preparation_day = function(path_Info.out, path_SoilWat.out) { # path_Info.out: Path of "INFO.OUT" file
                                                                   # path_SoilWat.out: Path of "SoilWat.OUT" file
  
    ##################################################################
    ## Import Info.OUT file: This script session imports the        ##
    ## Info.OUT file to extract the table with the soil properties. ##
    ##################################################################
    
    Info.out = read_lines (path_Info.out)
    
    pos_head <- grep("DUL", Info.out)
    
    Ini = pos_head[1]      # The first row of the soil profile table
    End = pos_head[1]+5    # The last row of the soil profile table
    
    ## Extract the soil data as data frame ##
    Soil.Info = fread (path_Info.out, header=T,check.names = T, skip=(pos_head[1]), sep = " ",dec=".", nrows = End - Ini)
    
    ## Write the column names ##
    var_names_Soil.Info = c("LYR", "DS", "DLAYR", "LL", "DUL", "SAT", "Root.Grow", "BD", "OC", "CLAY", "SILT", "SAND", "pH")
    names(Soil.Info)[1:13] = var_names_Soil.Info

    ## Extract soil layers within the first 45 cm depth ##
    Soil.Info = subset(Soil.Info, DS <= 45) # 45 cm soil depth can be an input parameter

    ### Calculates the average DRAINED UPPER LIMIT (DUL) for the selected layers ##
    ### DUL is analogous to the definition of field capacity.                    ##
    Mean_DUL = mean(Soil.Info$DUL)

    ### Calculates the average DRAINED LOWER LIMIT (DLL) for the selected layer ##
    ### DLL is analogous to the definition of permanent wilting point.          ##
    Mean_LL = mean(Soil.Info$LL)

    ### Calculates the average available soil water content ###
    Mean_AW = Mean_DUL - Mean_LL

    ### Define the depletion rate for wheat ###
    #DR = 0.40
    DR = 0.30

    ### Soil water content from which land preparation can begin ###
    soil.water.limit = Mean_DUL - (Mean_AW * DR)

    ##############################################################
    ## Import SoilWat.OUT file: This script session imports the ##
    ## Soilwat.out file to split the water balance information  ##
    ## for each crop simulation.                                ##
    ##############################################################
    
    scenarios_Soilwater = list() ## Lists for saving soil water balance data for each of the 99 simulations.
    
    SoilWater_1 = read_lines (path_SoilWat.out)
    
    pos_head <- grep("@YEAR", SoilWater_1)

      for (i in 1:length(pos_head)){
        #i=99
        if(i < length(pos_head)){
          
          Ini = pos_head[i]      # The first row of the soil water balance information
          End = pos_head[i+1]-12 # The last row of the soil water balance information
          
          Water = fread (path_SoilWat.out, header=T,check.names = T,skip=(pos_head[i]-1),sep = " ",dec=".", nrows = End - Ini)
          
          scenarios_Soilwater[[i]] = Water
          
        } else { ## the case to read the weather for the last simulation, i.e. simulation number 99 ##
          
          Ini = pos_head[i]         # The first row of the soil water balance information
          End = length(SoilWater_1) # The last row of the soil water balance information
          
          Water = fread (path_SoilWat.out, header=T,check.names = T,skip=(pos_head[i]-1),sep = " ",dec=".", nrows = End - Ini)
          
          scenarios_Soilwater[[i]] = Water
          
        }
        
      }

    ##################################################################
    ## Create an empty matrix (Dimension: 99X2) to store the day of ## 
    ## the year that land preparation should be done.               ##
    ##################################################################
    
    LPT.DOY = as.data.frame(matrix(NA, nrow = length(scenarios_Soilwater), ncol = 2))
    names(LPT.DOY)[1:2] = c("Run", "DOY")
    
    for (i in 1:length(scenarios_Soilwater)){
      
        if (nrow(Soil.Info) == 1){
    
          data = scenarios_Soilwater[[i]]
          DAS = which(data$SW1D<=soil.water.limit) #Days after simulation when the soil water content is lower or equal to the depletion rate
          LPT = data$DOY[DAS[1]]                   #Day of year for land preparation
          
          LPT.DOY[i,1] = paste("Run_",i,sep="")
          LPT.DOY[i,2] = LPT
          
        } else if (nrow(Soil.Info) == 2){
          
          data = scenarios_Soilwater[[i]]
          DAS = which(data$SW2D<=soil.water.limit) #Days after simulation when the soil water content is lower or equal to the depletion rate
          LPT = data$DOY[DAS[1]]                   #Day of year for land preparation
          
          LPT.DOY[i,1] = paste("Run_",i,sep="")
          LPT.DOY[i,2] = LPT
          
        } else if (nrow(Soil.Info) == 3){
          
          data = scenarios_Soilwater[[i]]
          DAS = which(data$SW3D<=soil.water.limit) #Days after simulation when the soil water content is lower or equal to the depletion rate
          LPT = data$DOY[DAS[1]]                   #Day of year for land preparation
          
          LPT.DOY[i,1] = paste("Run_",i,sep="")
          LPT.DOY[i,2] = LPT
          
          
        } else if (nrow(Soil.Info) == 4){
          
          data = scenarios_Soilwater[[i]]
          DAS = which(data$SW4D<=soil.water.limit) #Days after simulation when the soil water content is lower or equal to the depletion rate
          LPT = data$DOY[DAS[1]]                   #Day of year for land preparation
          
          LPT.DOY[i,1] = paste("Run_",i,sep="")
          LPT.DOY[i,2] = LPT
          
          
        } else {
          
          data = scenarios_Soilwater[[i]]
          DAS = which(data$SW5D<=soil.water.limit) #Days after simulation when the soil water content is lower or equal to the depletion rate
          LPT = data$DOY[DAS[1]]                   #Day of year for land preparation
          
          LPT.DOY[i,1] = paste("Run_",i,sep="")
          LPT.DOY[i,2] = LPT
          
        }
    
    }
    
    return(LPT.DOY)

}

######################################################
## Create the "summary_table" function to get       ##
## descriptive statistics of land preparation dates ##
######################################################

summary_table = function(LPT.DOY){ ## LPT.DOY is the matrix (Dimension: 99X2) containing the simulation number in the 
                                   ## first column and the land preparation day (DOY) in the second column.
  
  conf_lower <- function(var){
    
    t.test(var)$conf.int[1]
  }
  
  conf_upper <- function(var){
    
    t.test(var)$conf.int[2]
  }
  
  avg = round(mean(LPT.DOY$DOY, na.rm = T), 0)
  median = round(median(LPT.DOY$DOY, na.rm = T), 0)
  min = round(min(LPT.DOY$DOY, na.rm = T), 0)
  max = round(max(LPT.DOY$DOY, na.rm = T), 0)
  quar_1 = round(quantile(LPT.DOY$DOY, 0.25, na.rm = T), 0)
  quar_2 = round(quantile(LPT.DOY$DOY, 0.50, na.rm = T), 0)
  quar_3 = round(quantile(LPT.DOY$DOY, 0.75, na.rm = T), 0)
  conf_lower = 0#round(conf_lower(LPT.DOY$DOY), 0)
  conf_upper = 0#round(conf_upper(LPT.DOY$DOY), 0)
  sd = round(sd (LPT.DOY$DOY, na.rm = T), 0)
  perc_5 = round(quantile(LPT.DOY$DOY, 0.05, na.rm = T), 0)
  perc_95 = round(quantile(LPT.DOY$DOY, 0.95, na.rm = T), 0)
  coef_var = round((sd/avg)*100, 0)
  
  statistics = c(avg, median, min, max, quar_1, quar_2, quar_3, conf_lower, conf_upper, sd, perc_5, perc_95, coef_var)
  names(statistics)[1:13] = c("avg", "median", "min", "max", "quar_1", "quar_2", "quar_3", "conf_lower", "conf_upper",  "sd", "perc_5", "perc_95", "coef_var")
  return(statistics)
    
}
    
###############################
## An implementation example ##
###############################

## Define file paths ##

#path_Info.out = "D:/OneDrive - CGIAR/Desktop/Land Preparetation/INFO.OUT"
#path_SoilWat.out = "D:/OneDrive - CGIAR/Desktop/Land Preparetation/SoilWat.OUT"

##Run functions ##

#LPT.DOY = land_preparation_day (path_Info.out, path_SoilWat.out)
#Final_Result = summary_table (LPT.DOY)

#Gets landing_preparation for all planting dates
land_preparation_all <- function(data_files_all){

  land_preparation_list <- list()

  data <- mclapply(1:length(data_files_all), function(i) {
    data_files <- paste0(data_files_all[1])

    current_info <- paste0(data_files, 'INFO.OUT')
    current_soil_wat <- paste0(data_files, 'SoilWat.OUT')

    current_land_preparation <- summary_table(land_preparation_day(current_info, current_soil_wat))

    ## -1 means that the soil has a lot of moisture and land preparation cannot be done
    if(is.na(current_land_preparation[1])) current_land_preparation[1:13] = -1
    
    land_preparation_list <- append(land_preparation_list, current_land_preparation)
  
  }, mc.cores = no_cores, mc.preschedule = F)
  return(map(data, bind_rows))

}


