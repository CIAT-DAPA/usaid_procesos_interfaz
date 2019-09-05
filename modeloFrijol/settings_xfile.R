## Make a R-markdown for a summary that show how to write a x-file with some parameters (Most used parameters in another projects)
## Change the next lines for new initial conditions
## make parameter for condiciones iniciales upper limit or lower limit (since looks wilt point)
## add C function tu use multiple opciones is required during to run DSSAT
## add read soil information from (file SOIL) to determine de upper limit for initial conditions
## test
## out_file <- "./JBID.RIX"
# overwrite <- F


# Proof make_details

# Parameters
# details <- '*EXP.DETAILS: CALB1501MZ Calibracion Buga Valle 2015B' # recordar cambiar este campo solo para el proyecto de USAID
# details <- '*EXP.DETAILS: USAID-CIAT project Agroclimatic forecasts'
# people <- "CIAT"
# The others parameters its not necessary to have outside the function


make_details <- function(details, people){

  general <- list(DETAILS = details,
                  PEOPLE = people,
                  addres = "CIAT",
                  site = "CALI")
  
  return(general)
  
}


# make_details(details, people)



# Parameters
# For now make_treatments make a configuration for one combination to run into the experimental

# IC <- 0         # 1 if its necesarry run the experimental with initial conditions, 0 if its not necessary to run
#                   the experimental with initial conditions

# MI <- 0         # 1 turn on field for irrigation level, 0 turn off field for irrigation level
# MF <- 0         # 1 turn on field for fertilizier level, 0 turn off field for fertilizier level
# MH <- 0         # 1 turn on field for harvest level, 0 turn off field for harvest level
# FL <- 1:200

make_treatments <- function(IC, MI, MF, MH){

  # Treatments
  
  # *TREATMENTS                        -------------FACTOR LEVELS------------
  # @N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM
  #  1 1 0 0 P1                         1  1  0  1  1  0  1  0  0  0  0  1  1
  
  treatments <- data.frame(N = 1:99, R = 1, O = 0, C = 0, TNAME = "USAID",
                           CU = 1, FL = 1:99, SA = 0, IC, MP = 1,
                           MI, MF, MR = 0, MC = 0, MT = 0, ME = 0, MH, SM = 1)
  
  
  return(treatments)
  
}

# make_treatments(IC, MI, MF, MH, FL)

# Parameters
# CR <- 'MZ'    # Crop Code, you need to search this parameter for de manual DSSAT (its different by crop)
# INGENO <- 'CI0027' # Cultivar indentifier, this is the code for cultivar to run depend of crop
# CNAME <- 'PIO 30F35HRB_'  # Whatever code to identify the cultivar ran, maybe no too long string

make_cultivars <- function(CR, INGENO, CNAME){
  
  
  cultivars <- data.frame(C = 1 , CR, INGENO, CNAME)
  
  return(cultivars)
  
}

# make_cultivars(CR, INGENO, CNAME)


# Parameters
# WSTA <- 'CCCR8000' # Weather Station Code, its the same code to using in WTH file
# ID_SOIL <- 'CCBuga0001' # Id soil to using in the SOIL.SOl



make_fields <- function(WSTA, ID_SOIL){
  
  
  # *FIELDS
  # @L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME
  #  1 -99      CCBR1502   -99   -99 DR000   -99   -99     0 SL      30  CCBuga0001 Calibracion
  # 
  fields <- data.frame(L = 1:99, ID_FIELD = "USAID", WSTA, FLSA = -99, FLOB = -99, FLDT = "DR000",
                               FLDD = -99, FLDS = -99, FLST = -99, SLTX = -99, SLDP = -99, ID_SOIL,
                               FLNAME = "FIELD01", XCRD = -99, YCRD = -99, ELEV = -99, AREA = -99, SLEN=-99,
                               FLWR = -99, SLAS = -99, FLHST = -99, FHDUR=-99)
  

  return(fields)
  
}

# make_fields(WSTA, ID_SOIL) 



# Parameters


# Initial Conditions
# 1 ) you are going to use initial conditions for default (soil characteristics) (see code for running DSSAT on BID project)
# 2 ) you need to use lower limit or upper limit (para la siembra, esto mas que todo se utiliza cuando se desea sembrar automaticamente)
# 3 ) you are goint to give the information to make Initial Conditions
# Por ahora se utilizará condiciones iniciales dadas por el usuario

## data frame to Initial Conditions
# ICDAT the same from PDATE (planting date)
# *INITIAL CONDITIONS
# @C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME
# 1    MZ 80085   -99     0     1     1   -99     0     0     0   100    15 -99


# ICBL <- c(25, 45, 95)
# SH20 <- -99
# SNH4 <- c(4.2, 4.4, 4.5)    # estas variables se pueden investigar cuales utilizar cuando no se tiene condiciones iniciales
# SNO3 <- c(11.9, 12.4, 7.6)  # estas variables se pueden investigar cuales utilizar cuando no se tiene condiciones iniciales
# ICDAT <- -99 #  for now

make_IC <- function(ICBL, SH20, SNH4, SNO3){
  
  IC_field <- data.frame(C = 1, PCR = 'MZ', ICDAT = -99, ICRT = -99, ICND = -99, ICRN = -99, ICRE = -99,
                                       ICWD = -99, ICRES = -99, ICREN = -99, ICREP = -99, ICRIP = -99, ICRID = -99,
                                       ICNAME = -99)
  
  IC_values <- data.frame(C= rep(1,length(SNH4)),ICBL, SH20,SNH4,
                          SNO3)
  

  
  initial_conditions <- list(field = IC_field, values = IC_values)
  
  return(initial_conditions)
  
}


# make_IC(ICBL, SH20, SNH4, SNO3)


# Parameters

# FDATE = c(21, 21, 35, 35, 58)  ## Dias de la aplicacion +
# FMCD = c('FE006', 'FE016', 'FE005', 'FE016', 'FE005') ## Investigar acerca de este parametro
# FACD = 'AP002' ## Investigar acerca de este parametro
# FDEP = 4       ## Profundidad de la aplicacion del nitrogeno
# FAMN = c(33.3, 0, 63.9, 0, 63.9)
# FAMP = c(29.1, 0, 0, 0, 0) ## Investigar mas acerca de este parametro
# FAMK = c(0, 36, 0, 39.2, 0)
# FAMC = 0
# FAMO = 0
# FOCD = 0
# FERNAME = -99
# FERTI = 'D' ## D = dias despues de la siembra


# make all parameters in a object list for R

# input_fertilizer <- list()
# input_fertilizer$FDATE = c(21, 21, 35, 35, 58)  ## Dias de la aplicacion +
# input_fertilizer$FMCD = c('FE006', 'FE016', 'FE005', 'FE016', 'FE005') ## Investigar acerca de este parametro
# input_fertilizer$FACD = 'AP002' ## Investigar acerca de este parametro
# input_fertilizer$FDEP = 4       ## Profundidad de la aplicacion del nitrogeno
# input_fertilizer$FAMN = c(33.3, 0, 63.9, 0, 63.9)
# input_fertilizer$FAMP = c(29.1, 0, 0, 0, 0) ## Investigar mas acerca de este parametro
# input_fertilizer$FAMK = c(0, 36, 0, 39.2, 0)
# input_fertilizer$FAMC = 0
# input_fertilizer$FAMO = 0
# input_fertilizer$FOCD = 0
# input_fertilizer$FERNAME = -99
# input_fertilizer$FERTI = 'D' ## D = dias despues de la siembra, es necesario actualizar con las otras opciones que tiene este parametro



make_MF <- function(input_fertilizer){
  
  FDATE <- input_fertilizer$FDATE 
  FMCD <- input_fertilizer$FMCD
  FACD <- input_fertilizer$FACD
  FDEP <- input_fertilizer$FDEP 
  FAMN <- input_fertilizer$FAMN 
  FAMP <- input_fertilizer$FAMP 
  FAMK <- input_fertilizer$FAMK 
  FAMC <- input_fertilizer$FAMC 
  FAMO <- input_fertilizer$FAMO
  FOCD <- input_fertilizer$FOCD
  FERNAME <- input_fertilizer$FERNAME
  FERTI <- input_fertilizer$FERTI
  
  fertilizer <- data.frame(F = 1, FDATE, FMCD, FACD, FDEP, FAMN, FAMP, FAMK,
                                   FAMC, FAMO, FOCD, FERNAME)
  
  return(fertilizer)
  
}


# make_MF(input_fertilizer)



# Planting Details


## doing a comment that explain all parameters
# input_pDetails <- list()
# input_pDetails$PDATE <- 80092 # Planting date
# input_pDetails$SDATE <- pmax(input_pDetails$PDATE - 20, 0)   ## Starting simulation. 20 before planting date
# input_pDetails$plant <- 'R'  # R = planting on reporting date
## Remember Simulation date starts 20 days before planting date
# input_pDetails$EDATE <- -99
# input_pDetails$PPOP <- 6.25
# input_pDetails$PPOE <- 6.25
# input_pDetails$PLME <- 'S'
# input_pDetails$PLDS <- 'R'
# input_pDetails$PLRS <- 80
# input_pDetails$PLRD <- 90
# input_pDetails$PLDP <- 4
## Variables como PLWT, PAGE, PENV, PLPH, SPRL con -99

make_pDetails <- function(input_pDetails){
  

  PDATE <- input_pDetails$PDATE 
  SDATE <- input_pDetails$SDATE    
  plant <- input_pDetails$plant   
  EDate <- input_pDetails$EDATE  
  PPOP <- input_pDetails$PPOP  
  PPOE <- input_pDetails$PPOE 
  PLME <- input_pDetails$PLME  
  PLDS <- input_pDetails$PLDS  
  PLRS <- input_pDetails$PLRS  
  PLRD <- input_pDetails$PLRD  
  PLDP <- input_pDetails$PLDP  
  
  
  planting <- data.frame( P = 1, PDATE, EDATE = -99, PPOP, PPOE, PLME, 
                                  PLDS, PLRS = 80, PLRD, PLDP,
                                  PLWT = -99, PAGE = -99, PENV = -99, PLPH = -99, SPRL = -99)
  
  
  return(planting)

  
}

# make_pDetails(input_pDetails)

# Simulation Controls

# input_sControls <- list()
# input_sControls$NYERS <- 20 ## Years for simulation
# input_sControls$SMODEL <- 'MZCER046' # model to use
# input_sControls$WATER <- 'N'   ## Y = Utiliza balance Hidrico, N = No utiliza balance hidrico
# input_sControls$NITRO <-  'N'  ## Y = utiliza balance nitrogeno, N =  no utiliza balance nitrogeno
# input_sControls$PLANT <- 'R'  # R = planting on reporting date ## Add the other options
# input_sControls$IRRIG <- 'R'  ##  on reporting date, A automatically irragated, N Nothing, add the other options
# input_sControls$FERTI = 'N' ## add more options
# input_sControls$SDATE <- pmax(input_pDetails$PDATE - 20, 0)

make_sControls <- function(input_sControls, PDATE){
  
  ## 
  
  NYERS <- input_sControls$NYERS 
  SMODEL <-input_sControls$SMODEL
  WATER <- input_sControls$WATER 
  NITRO <- input_sControls$NITRO
  PLANT <- input_sControls$PLANT
  IRRIG <- input_sControls$IRRIG
  FERTI <- input_sControls$FERTI 
  SDATE <- input_sControls$SDATE

  
  sim_ctrl <- data.frame(N = 1, GENERAL = "GE", NYERS, NREPS = 1, START = "S", SDATE,
                                 RSEED = 2150, SNAME = "simctr1", SMODEL,
                                 OPTIONS = "OP", WATER, NITRO, SYMBI = "N",
                                 PHOSP = "N", POTAS = "N", DISES = "N", CHEM = "N", TILL = "N",
                                 CO2 = "N", METHODS = "ME", WTHER = "M", INCON = "M", LIGHT = "E",
                                 EVAPO = "R", INFIL = "S", PHOTO = "C", HYDRO = "R",
                                 NSWIT = 1, MESOM = "G", MESEV = "S", MESOL =2, MANAGEMENT = "MA",
                                 PLANT, IRRIG,
                                 FERTI, RESID = "R", HARVS = "M", OUTPUTS = "OU", FNAME = "N",
                                 OVVEW = "N", SUMRY = "Y", FROPT = 1, GROUT = "Y", CAOUT = "N",
                                 WAOUT = "N", NIOUT = "N", MIOUT = "N",
                                 DIOUT = "N", VBOSE = "N", CHOUT = "N", OPOUT = "N")

 
  return(sim_ctrl)
   
}


# make_sControls(input_sControls, input_pDetails$PDATE)

# if is necessary to specify sowing dates 
# PFRST <- -99
# PLAST <- -99
# system <- 'irrigation'  ## Solo se habilita con IC = 1 (irrigation es como condiciones inicial del suelo menos drasticas) 
# este campo para futuras simulaciones o proyectos


make_Amgmt <- function(PFRST, PLAST){
  # PSTMX PSTMN
  auto_mgmt <- data.frame(N = 1, PLANTING = 'PL', PFRST, PLAST, PH2OL = 50, PH2OU = 100,
                                  PH2OD = 30, PSTMX = 40, PSTMN = 10, IRRIGATION = "IR", IMDEP =30, ITHRL = 50, 
                                  ITHRU =100, IROFF = "GS000", IMETH = "IR001", IRAMT = 10, IREFF = 1,
                                  NITROGEN = "NI", NMDEP = 30, NMTHR = 50, NAMNT = 25, NCODE = "FE001",
                                  NAOFF = "GS000", RESIDUES = "RE", RIPCN = 100, RTIME = 1, RIDEP = 20, 
                                  HARVEST = "HA", HFRST = 0, HLAST = 00001, HPCNP = 100, HPCNR = 0)
 
  return(auto_mgmt) 
}


# make_Amgmt(PFRST, PLAST)





