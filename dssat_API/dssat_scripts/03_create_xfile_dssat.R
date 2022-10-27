### Create Experimental file DSSAT - xfile --->> Aclimate - EDACaP
# Author: Rodriguez-Espinoza J. Mesa-Diez J.
# https://github.com/jrodriguez88/
# 2022


#Crea archivo X file  en el directorio especifico de la simulacion


#planting_details <- read_tsv(clipboard()) %>% pivot_wider(names_from = name)
##add FERT switch 
#
#crop <- "maize"
#path <- "inputs/setups/test_wheat/"
#id_name <- "CIAT0001"
#cultivar <- c("AW0071","Yecora_Rojo")    #same as .CUL file @VAR#  VAR-NAME  = c("AW0071","Yecora_Rojo")
#soil <- "IB00000004"
#wth_station <- "CIAT0001"
#irri = F
#fert_in = NULL
#start_date <- input_dates$DATE[[1]]
#planting_date <- input_dates$DATE[[1]]
#emergence_date = NULL
#treatments_number <- length(wth_station)
#xfile <- crop_name_setup(id_name, crop)[[3]]
#

write_exp_dssat <- function(path, id_name, crop, cultivar, soil, wth_station, planting_details, 
                irri, fert_in, start_date, planting_date, emergence_date, treatments_number){
  
  options(encoding = "UTF-8")
  

  
  # Function to write date into DSSAT format
  # 'var' must be object date class
  date_for_dssat <- function(date) {
    stopifnot(class(date)=="Date")
    stopifnot(require(lubridate))
    
    yr <- str_sub(year(date), -2)
    doy <- yday(date)
    
    paste0(yr, sprintf("%.3d", doy))
    
  }
  
  
  
  
  #############################
  #*EXP.DETAILS:
  #*############################
  
  description <- list(details = "*AgroclimR - DSSAT X File",
                      people = "Rodriguez-Espinoza, J., Mesa-Diez J., Ramirez-Villegas, J.",
                      address = "CIAT-Colombia, Climate Action",
                      site = "https://github.com/jrodriguez88/agroclimR")
  
  
  
  write_details <- function(name_exp, description){
    
    
    
    cat(paste0(description$details, "\n"), file = name_exp)
    cat("\n",file = name_exp)
    cat("*GENERAL\n@PEOPLE\n", file = name_exp)
    cat(paste(sprintf("%-12s", as.character(description$people)), "\n", sep = ""), file = name_exp)
    cat("@ADDRESS\n", file = name_exp)
    cat(paste(sprintf("%-12s", as.character(description$address)), "\n", sep = ""), file = name_exp)
    cat("@SITE\n", file = name_exp)
    cat(paste(sprintf("%-12s", as.character(description$site)), "\n", sep = ""), file = name_exp)
    
    cat("@ PAREA  PRNO  PLEN  PLDR  PLSP  PLAY HAREA  HRNO  HLEN  HARM.........
    -99   -99   -99   -99   -99   -99   -99   -99   -99   -99", file = name_exp, sep = "\n")
    cat("\n", file = name_exp)
    
    
  }
  
  ########################
  #*TREATMENTS 
  #*######################
  #*
  # IC <- 0         # 1 if its necesarry run the experimental with initial conditions, 0 if its not necessary to run
  #                   the experimental with initial conditions
  
  # MI <- 0         # 1 turn on field for irrigation level, 0 turn off field for irrigation level
  # MF <- 0         # 1 turn on field for fertilizier level, 0 turn off field for fertilizier level
  # MH <- 0         # 1 turn on field for harvest level, 0 turn off field for harvest level
  # FL <- 1:200
  
  IC <- 0   
  MI <- 0 #if(isTRUE(irri)){1} else {0} # 
  MF <- if(is.null(fert_in)){0} else {1}
  MH <- 0
  
  leng_tb <- 1:treatments_number
  
  treatments <- data.frame(N = leng_tb, R = 1, O = 0, C = 0, TNAME = "CIAT_",
                           CU = 1, FL = leng_tb, SA = 0, IC, MP = 1,
                           MI, MF, MR = 0, MC = 0, MT = 0, ME = 0, MH, SM = 1)
  
  
  
  
  write_treatments <- function(name_exp, treatments){
    
    cat("*TREATMENTS                        -------------FACTOR LEVELS------------\n", file = name_exp)
    cat("@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM\n", file = name_exp)
    for (i in 1:nrow(treatments)) {
      
      cat(paste(sprintf("%1$2d%2$2d%3$2d%4$2d",as.integer(treatments$N[i]),as.integer(treatments$R[i]),
                        as.integer(treatments$O[i]),as.integer(treatments$C[i])),
                " ",sprintf("%1$-25s%2$3d%3$3d%4$3d%5$3d%6$3d%7$3d%8$3d%9$3d%10$3d%11$3d%12$3d%13$3d%14$3d",treatments$TNAME[i],
                            as.integer(treatments$CU[i]),as.integer(treatments$FL[i]),as.integer(treatments$SA[i]),
                            as.integer(treatments$IC[i]),as.integer(treatments$MP[i]),as.integer(treatments$MI[i]),
                            as.integer(treatments$MF[i]),as.integer(treatments$MR[i]),as.integer(treatments$MC[i]),
                            as.integer(treatments$MT[i]),as.integer(treatments$ME[i]),as.integer(treatments$MH[i]),
                            as.integer(treatments$SM[i])),
                "\n", sep = ""), file = name_exp)
      
    }
    cat("\n", file = name_exp)
    
  }
  
  #####################################
  #*CULTIVARS
  #*###################################
  
  # Parameters
  # CR <- 'MZ'    # Crop Code, you need to search this parameter for de manual DSSAT (its different by crop)
  # INGENO <- 'CI0027' # Cultivar indentifier, this is the code for cultivar to run depend of crop
  # CNAME <- 'PIO 30F35HRB_'  # Whatever code to identify the cultivar ran, maybe no too long string
  
  CR <- crop_name_setup(id_name, crop)[["CR"]]
  
  cultivars <- data.frame(C = 1 , CR, INGENO = cultivar[1], CNAME = cultivar[2])
  
  
  write_cultivars <- function(name_exp, cultivars){
    
    cat("*CULTIVARS\n", file = name_exp)
    cat("@C CR INGENO CNAME\n", file = name_exp)
    for (i in 1:nrow(cultivars)) {
      cat(paste(sprintf("%2d",as.integer(cultivars$C[i]))," ",sprintf("%2s", cultivars$CR[i]),
                " ", sprintf("%6s",cultivars$INGENO[i])," ",sprintf("%-12s",cultivars$CNAME[i]),
                "\n", sep = ""), file = name_exp)
    }
    cat("\n", file = name_exp)
    
  }
  
  #################################
  #*FIELDS
  #*###############################
  
  # Parameters
  # WSTA <- 'CCCR8000' # Weather Station Code, its the same code to using in WTH file
  # ID_SOIL <- 'CCBuga0001' # Id soil to using in the SOIL.SOl
  
  # *sim_ctrl
  # @L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME
  #  1 -99      CCBR1502   -99   -99 DR000   -99   -99     0 SL      30  CCBuga0001 Calibracion
  # 
  
  
  fields <- data.frame(L = leng_tb, ID_FIELD = "CIAT_", WSTA = wth_station, FLSA = -99, FLOB = -99, FLDT = -99,
                         FLDD = -99, FLDS = -99, FLST = -99, SLTX = -99, SLDP = -99, ID_SOIL = soil,
                         FLNAME = "FIELD01", XCRD = -99, YCRD = -99, ELEV = -99, AREA = -99, SLEN=-99,
                         FLWR = -99, SLAS = -99, FLHST = -99, FHDUR=-99)
  
  
  
  
  write_fields <- function(name_exp, fields){
    
    # fields
    cat("*FIELDS\n", file = name_exp)
    cat("@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME\n", file = name_exp)
    for (i in 1:nrow(fields)) {
      
      cat(paste(sprintf("%2d",as.integer(fields$L[i]))," ",sprintf("%-8s",fields$ID_FIELD[i]),
                " ",sprintf("%-8s",fields$WSTA[i]),sprintf("%6d",as.integer(fields$FLSA[i])),
                sprintf("%6d",as.integer(fields$FLOB[i])),sprintf("%6s",fields$FLDT[i]),
                sprintf("%6d",as.integer(fields$FLDD[i])),sprintf("%6s",as.integer(fields$FLDS[i])),
                sprintf("%6d",as.integer(fields$FLST[i]))," ",sprintf("%-4d",as.integer(fields$SLTX[i])),
                sprintf("%6d",as.integer(fields$SLDP[i])),"  ",sprintf("%-10s",fields$ID_SOIL[i])," ",
                sprintf("%-12s",fields$FLNAME[i]),"\n", sep=""),file=name_exp)
      
    }
    
    cat("\n", file = name_exp)
    
    
    cat("@L ..........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR\n",file=name_exp)
    # 
    # for (i in 1:nrow(fields)) { 
    #   
    #   
    #   cat(paste(sprintf("%2d",as.integer(fields$L[i]))," ",sprintf("%15.3f",fields$XCRD[i])," ",
    #             sprintf("%15.3f",fields$YCRD[i])," ",sprintf("%9d",as.integer(fields$ELEV[i]))," ",
    #             sprintf("%17d",as.integer(fields$AREA[i]))," ",sprintf("%5d",as.integer(fields$SLEN[i]))," ",
    #             sprintf("%5d",as.integer(fields$FLWR[i]))," ",sprintf("%5d",as.integer(fields$SLAS[i]))," ",
    #             sprintf("%5d",as.integer(fields$FLHST[i]))," ",sprintf("%5d",as.integer(fields$FHDUR[i])),
    #             "\n",sep=""),file=name_exp)
    #   
    #   
    # }
    
    for (i in 1:nrow(fields)) { 
      
      
      cat(paste(sprintf("%2d",as.integer(fields$L[i]))," ",sprintf("%14.d",fields$XCRD[i])," ",
                sprintf("%15.d",fields$YCRD[i])," ",sprintf("%9d",as.integer(fields$ELEV[i]))," ",
                sprintf("%17d",as.integer(fields$AREA[i]))," ",sprintf("%5d",as.integer(fields$SLEN[i]))," ",
                sprintf("%5d",as.integer(fields$FLWR[i]))," ",sprintf("%5d",as.integer(fields$SLAS[i]))," ",
                sprintf("%5d",as.integer(fields$FLHST[i]))," ",sprintf("%5d",as.integer(fields$FHDUR[i])),
                "\n",sep=""), file=name_exp)
      
      
    }
    
    
    cat("\n",file=name_exp)
    
  }
  
  
  ###############################
  #*PLANTING DETAILS
  #*#############################
  
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
  
  
  
  PDATE <- date_for_dssat(planting_date)
  EDATE <- if(is.Date(emergence_date)){date_for_dssat(emergence_date)} else {-99} 
  PPOP <- planting_details$PPOP  
  PPOE <- planting_details$PPOE 
  PLME <- planting_details$PLME  
  PLDS <- planting_details$PLDS  
  PLRS <- planting_details$PLRS  
  PLRD <- planting_details$PLRD  
  PLDP <- planting_details$PLDP  
  PLNAME <- "JRE"  
  
  planting <- data.frame( P = 1, PDATE, EDATE , PPOP, PPOE, PLME, 
                          PLDS, PLRS, PLRD, PLDP,
                          PLWT = -99, PAGE = -99, PENV = -99, PLPH = -99, SPRL = -99, PLNAME)
  
  
  
  write_planting <- function(name_exp, planting){
    
    #planting details
    cat("*PLANTING DETAILS\n",file = name_exp)
    cat("@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME\n",file=name_exp)
    cat(paste(sprintf("%2d",as.integer(planting$P))," ",sprintf("%5s",planting$PDATE),
              " ",sprintf("%5s",planting$EDATE)," ",sprintf("%5s",as.character(planting$PPOP)),
              " ",sprintf("%5s",as.numeric(as.character(planting$PPOE)))," ",sprintf("%5s",planting$PLME),
              " ",sprintf("%5s",planting$PLDS)," ",sprintf("%5s",as.character(planting$PLRS)),
              " ",sprintf("%5s",as.character(planting$PLRD))," ",sprintf("%5s",as.character(planting$PLDP)),
              " ",sprintf("%5d",as.integer(planting$PLWT))," ",sprintf("%5d",as.integer(planting$PAGE)),
              " ",sprintf("%5d",as.integer(planting$PENV))," ",sprintf("%5d",as.integer(planting$PLPH)),
              " ",sprintf("%5d",as.integer(planting$SPRL))," ",sprintf("%29s",planting$PLNAME),
              "\n", sep = ""), file = name_exp)
    cat("\n", file = name_exp)
    
  }
  
  
  #########################################
  #*IRRIGATION AND WATER MANAGEMENT
  #*#######################################
  
  #*IRRIGATION AND WATER MANAGEMENT
  #@I  EFIR  IDEP  ITHR  IEPT  IOFF  IAME  IAMT IRNAME
  #1     1    30    50   100 GS000 IR001    10 -99
  #@I IDATE  IROP IRVAL
  #1 97032   -99   -99
  
  
  
  #*FERTILIZERS (INORGANIC) -- 
  #*
  
  ## Fertilizer or NOt
  if(!is.null(fert_in)){
    FERT <- 'D'
  } else{
    
    FERT <- 'N'
  }
  
  #make_MF <- function(input_fertilizer){
  #  
  #  FDATE <- fert_in$FDATE 
  #  FMCD <-  fert_in$FMCD
  #  FACD <-  fert_in$FACD
  #  FDEP <-  fert_in$FDEP 
  #  FAMN <-  fert_in$FAMN 
  #  FAMP <-  fert_in$FAMP 
  #  FAMK <-  fert_in$FAMK 
  #  FAMC <-  fert_in$FAMC 
  #  FAMO <-  fert_in$FAMO
  #  FOCD <-  fert_in$FOCD
  #  FERNAME <- fert_in$FERNAME
  #  FERTI <- fert_in$FERTI
  #  
  #  fertilizer <- data.frame(F = 1, FDATE, FMCD, FACD, FDEP, FAMN, FAMP, FAMK,
  #                           FAMC, FAMO, FOCD, FERNAME)
  #  
  #  return(fertilizer)
  #  
  #}
  
  write_fertilizer <- function(name_exp, fert_in){
    
    cat("*FERTILIZERS (INORGANIC)\n", file = name_exp)
    cat("@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME                       \n", file = name_exp)
    
    for(i in 1:dim(fert_in)[1]){
      
      cat(paste(sprintf("%2d %5d %5s %5s %5d %5d %5d %5d %5d %5d %5d %5s", 1, fert_in$FDATE[i], fert_in$FMCD[i],
                        fert_in$FACD[i], fert_in$FDEP[i], fert_in$FAMN[i], fert_in$FAMP[i], 
                        fert_in$FAMK[i], fert_in$FAMC[i], fert_in$FAMO[i], 
                        fert_in$FOCD[i], fert_in$FERNAME[i]), '\n'), file = name_exp)
      
    }
    
    cat("\n", file = name_exp)
    
  }
  
  
  
  #*FERTILIZERS (INORGANIC)
  #@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME
  ##1     1 FE005 AP001     1    30   -99   -99   -99   -99   -99 fertApp
  ##1    25 FE006 AP001     1    10    20   -99   -99   -99   -99 fertApp
  ##1    40 FE028 AP001     1    10    30    10   -99   -99   -99 fertApp
  ##1    65 FE027 AP001     1    15    15    15   -99   -99   -99 fertApp
  
  
  
  #####################################
  #*SIMULATION CONTROLS
  #*###################################
  
  ## IRRIGATION or RAINFED
  if(isTRUE(irri)){
    IRR <- 'A'
  } else{
    
    IRR <- 'N'
  }
  
  ## Simulation Controls
  input_sControls <- list()
  input_sControls$NYERS <- 1 ## Years for simulation
  input_sControls$SMODEL <- '' # model to use
  input_sControls$WATER <- 'Y'   ## Y = Utiliza balance Hidrico, N = No utiliza balance hidrico
  input_sControls$NITRO <-  'Y'  ## Y = utiliza balance nitrogeno, N =  no utiliza balance nitrogeno
  input_sControls$PLANT <- 'R'  # R = planting on reporting date ## Add the other options
  input_sControls$IRRIG <- IRR  ##  R =  on reporting date, A automatically irragated, N Nothing, add the other options
  input_sControls$FERTI = FERT ## add more options
  #input_sControls$SDATE <- SDATE
  
  
  NYERS <- input_sControls$NYERS 
  SMODEL <-input_sControls$SMODEL
  WATER <- input_sControls$WATER 
  NITRO <- input_sControls$NITRO
  PLANT <- input_sControls$PLANT
  IRRIG <- input_sControls$IRRIG
  FERTI <- input_sControls$FERTI 
  SDATE <- date_for_dssat(start_date)
  
  
  sim_ctrl <- data.frame(N = 1, GENERAL = "GE", NYERS, NREPS = 1, START = "S", SDATE,
                         RSEED = 2150, SNAME = "simctr1", SMODEL,
                         OPTIONS = "OP", WATER, NITRO, SYMBI = "N",
                         PHOSP = "N", POTAS = "N", DISES = "N", CHEM = "N", TILL = "N",
                         CO2 = "N", METHODS = "ME", WTHER = "M", INCON = "M", LIGHT = "E",
                         EVAPO = "R", INFIL = "S", PHOTO = "C", HYDRO = "R",
                         NSWIT = 1, MESOM = "G", MESEV = "S", MESOL =2, MANAGEMENT = "MA",
                         PLANT, IRRIG,
                         FERTI, RESID = "R", HARVS = "M", OUTPUTS = "OU", FNAME = "N",
                         OVVEW = "Y", SUMRY = "Y", FROPT = 1, GROUT = "Y", CAOUT = "N",
                         WAOUT = "Y", NIOUT = "N", MIOUT = "N",
                         DIOUT = "N", VBOSE = "Y", CHOUT = "N", OPOUT = "N")
  
  
  
  
  
  write_sim_control <- function(name_exp, sim_ctrl){
    
    #simulation controls
    cat("*SIMULATION CONTROLS\n", file = name_exp)
    cat("@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL\n", file = name_exp)
    cat(paste(sprintf("%2d",as.integer(sim_ctrl$N))," ",sprintf("%-11s",sim_ctrl$GENERAL),
              " ",sprintf("%5d",as.integer(sim_ctrl$NYERS))," ",sprintf("%5d",as.integer(sim_ctrl$NREPS)),
              " ",sprintf("%5s",sim_ctrl$START)," ",sprintf("%5s",sim_ctrl$SDATE),
              " ",sprintf("%5d",as.integer(sim_ctrl$RSEED))," ",sprintf("%-25s",sim_ctrl$SNAME),
              " ",sprintf("%-6s",sim_ctrl$SMODEL),"\n",sep=""),file=name_exp)
    cat("@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2\n",file=name_exp)
    cat(paste(sprintf("%2d",as.integer(sim_ctrl$N))," ",sprintf("%-11s",sim_ctrl$OPTIONS),
              " ",sprintf("%5s",sim_ctrl$WATER)," ",sprintf("%5s",sim_ctrl$NITRO),
              " ",sprintf("%5s",sim_ctrl$SYMBI)," ",sprintf("%5s",sim_ctrl$PHOSP),
              " ",sprintf("%5s",sim_ctrl$POTAS)," ",sprintf("%5s",sim_ctrl$DISES),
              " ",sprintf("%5s",sim_ctrl$CHEM)," ",sprintf("%5s",sim_ctrl$TILL),
              " ",sprintf("%5s",sim_ctrl$CO2),"\n",sep=""),file=name_exp)
    cat("@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL\n",file=name_exp)
    cat(paste(sprintf("%2d",as.integer(sim_ctrl$N))," ",sprintf("%-11s",sim_ctrl$METHODS),
              " ",sprintf("%5s",sim_ctrl$WTHER)," ",sprintf("%5s",sim_ctrl$INCON),
              " ",sprintf("%5s",sim_ctrl$LIGHT)," ",sprintf("%5s",sim_ctrl$EVAPO),
              " ",sprintf("%5s",sim_ctrl$INFIL)," ",sprintf("%5s",sim_ctrl$PHOTO),
              " ",sprintf("%5s",sim_ctrl$HYDRO)," ",sprintf("%5d",as.integer(sim_ctrl$NSWIT)),
              " ",sprintf("%5s",sim_ctrl$MESOM)," ",sprintf("%5s",sim_ctrl$MESEV),
              " ",sprintf("%5d",as.integer(sim_ctrl$MESOL)),"\n",sep=""),file=name_exp)
    cat("@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS\n",file=name_exp)
    cat(paste(sprintf("%2d",as.integer(sim_ctrl$N))," ",sprintf("%-11s",sim_ctrl$MANAGEMENT),
              " ",sprintf("%5s",sim_ctrl$PLANT)," ",sprintf("%5s",sim_ctrl$IRRIG),
              " ",sprintf("%5s",sim_ctrl$FERTI)," ",sprintf("%5s",sim_ctrl$RESID),
              " ",sprintf("%5s",sim_ctrl$HARVS),"\n",sep=""),file=name_exp)
    cat("@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT\n",file=name_exp)
    cat(paste(sprintf("%2d",as.integer(sim_ctrl$N))," ",sprintf("%-11s",sim_ctrl$OUTPUTS),
              " ",sprintf("%5s",sim_ctrl$FNAME)," ",sprintf("%5s",sim_ctrl$OVVEW),
              " ",sprintf("%5s",sim_ctrl$SUMRY)," ",sprintf("%5s",sim_ctrl$FROPT),
              " ",sprintf("%5s",sim_ctrl$GROUT)," ",sprintf("%5s",sim_ctrl$CAOUT),
              " ",sprintf("%5s",sim_ctrl$WAOUT)," ",sprintf("%5s",sim_ctrl$NIOUT),
              " ",sprintf("%5s",sim_ctrl$MIOUT)," ",sprintf("%5s",sim_ctrl$DIOUT),
              " ",sprintf("%5s",sim_ctrl$VBOSE)," ",sprintf("%5s",sim_ctrl$CHOUT),
              " ",sprintf("%5s",sim_ctrl$OPOUT),"\n",sep=""),file=name_exp)
    cat("\n", file = name_exp)
    
    
  }
  
  #######################################
  #@  AUTOMATIC MANAGEMENT
  #######################################
  
  # PFRST <- -99
  # PLAST <- -99
  # system <- 'irrigation'  ## Solo se habilita con IC = 1 (irrigation es como condiciones inicial del suelo menos drasticas) 
  # este campo para futuras simulaciones o proyectos
  
  
  PFRST <- -99
  PLAST <- -99
  
  simulation_options <- data.frame(N = 1, PLANTING = 'PL', PFRST, PLAST, PH2OL = 50, PH2OU = 100,
                                   PH2OD = 30, PSTMX = 40, PSTMN = 10, IRRIGATION = "IR", IMDEP =30, ITHRL = 50, 
                                   ITHRU =100, IROFF = "GS000", IMETH = "IR001", IRAMT = 10, IREFF = 1,
                                   NITROGEN = "NI", NMDEP = 30, NMTHR = 50, NAMNT = 25, NCODE = "FE001",
                                   NAOFF = "GS000", RESIDUES = "RE", RIPCN = 100, RTIME = 1, RIDEP = 20, 
                                   HARVEST = "HA", HFRST = 0, HLAST = 00001, HPCNP = 100, HPCNR = 0)
  
  
  
  write_sim_setup <- function(name_exp, simulation_options){
    
    
    cat("@  AUTOMATIC MANAGEMENT\n", file = name_exp)
    cat("@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN\n", file = name_exp)
    
    cat(paste(sprintf("%2d",as.integer(simulation_options$N))," ",sprintf("%-11s",simulation_options$PLANTING),
              " ",sprintf("%5s",simulation_options$PFRST)," ",sprintf("%5s",simulation_options$PLAST),
              " ",sprintf("%5d",as.integer(simulation_options$PH2OL))," ",sprintf("%5d",as.integer(simulation_options$PH2OU)),
              " ",sprintf("%5d",as.integer(simulation_options$PH2OD))," ",sprintf("%5d",as.integer(simulation_options$PSTMX)),
              " ",sprintf("%5d",as.integer(simulation_options$PSTMN)),"\n",sep=""),file=name_exp)
    
    cat("@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF\n",file=name_exp)
    cat(paste(sprintf("%2d",as.integer(simulation_options$N))," ",sprintf("%-11s",simulation_options$IRRIGATION),
              " ",sprintf("%5d",as.integer(simulation_options$IMDEP))," ",sprintf("%5d",as.integer(simulation_options$ITHRL)),
              " ",sprintf("%5d",as.integer(simulation_options$ITHRU))," ",sprintf("%5s",simulation_options$IROFF),
              " ",sprintf("%5s",simulation_options$IMETH)," ",sprintf("%5d",as.integer(simulation_options$IRAMT)),
              " ",sprintf("%5d",as.integer(simulation_options$IREFF)),"\n",sep=""),file=name_exp)
    
    cat("@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF\n",file=name_exp)
    cat(paste(sprintf("%2d",as.integer(simulation_options$N))," ",sprintf("%-11s",simulation_options$NITROGEN),
              " ",sprintf("%5d",as.integer(simulation_options$NMDEP))," ",sprintf("%5d",as.integer(simulation_options$NMTHR)),
              " ",sprintf("%5d",as.integer(simulation_options$NAMNT))," ",sprintf("%5s",simulation_options$NCODE),
              " ",sprintf("%5s",simulation_options$NAOFF),"\n",sep=""),file=name_exp)
    cat("@N RESIDUES    RIPCN RTIME RIDEP\n",file=name_exp)
    cat(paste(sprintf("%2d",as.integer(simulation_options$N))," ",sprintf("%-11s",simulation_options$RESIDUES),
              " ",sprintf("%5d",as.integer(simulation_options$RIPCN))," ",sprintf("%5d",as.integer(simulation_options$RTIME)),
              " ",sprintf("%5d",as.integer(simulation_options$RIDEP)),"\n",sep=""),file=name_exp)
    cat("@N HARVEST     HFRST HLAST HPCNP HPCNR\n",file=name_exp)
    cat(paste(sprintf("%2d",as.integer(simulation_options$N))," ",sprintf("%-11s",simulation_options$HARVEST),
              " ",sprintf("%5d",as.integer(simulation_options$HFRST))," ",sprintf("%5d",as.integer(simulation_options$HLAST)),
              " ",sprintf("%5d",as.integer(simulation_options$HPCNP))," ",sprintf("%5d",as.integer(simulation_options$HPCNR)),
              "\n",sep=""),file=name_exp)
    
    cat("\n", file = name_exp)
    
  }
  
  name_exp <- paste0(path, "/", crop_name_setup(id_name, crop)[["ext"]])
  
  xfile <- file(name_exp, open = "w")
  
  write_details(xfile, description)
  write_treatments(xfile, treatments)  ## the parameter FL its to identify the run with a specific .WTH
  write_cultivars(xfile, cultivars)
  write_fields(xfile,fields)
  write_planting(xfile, planting)
  if(FERT == 'D'){write_fertilizer(xfile, fert_in)}
  write_sim_control(xfile, sim_ctrl)
  write_sim_setup(xfile, simulation_options)
  close(xfile)
  
  
  
  
}



