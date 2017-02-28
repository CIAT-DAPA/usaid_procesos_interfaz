
## Function to make x-file for DSSAT v4.6 from code R
































# To test

information <- list()

## Parameters to organize then 

information$NITRO <-  'N'  ## Y = utiliza balance nitrogeno, N =  no utiliza balance nitrogeno
information$WATER <- 'Y'   ## Y = Utiliza balance Hidrico, N = No utiliza balance hidrico


## Fields
information$NYERS <- 20 ## Years for simulation
information$name <- 'CALB.MZX' # Only specify four characters  an then .MZX or depend to crop
information$exp_details <- '*EXP.DETAILS: CALB1501MZ Calibracion Buga Valle 2015B'
information$WSTA <- 'CCCR8000'  ## add with year of start simulation
information$ID_SOIL <- 'CCBuga0001' # name of ID soil
information$smodel <- 'MZCER045' # model to use

## Cultivars

information$INGENO <- 'CI0027'
information$CR <- 'MZ'
information$CNAME <- 'USAID_Union'





## data frame to Initial Conditions
# ICDAT the same from PDATE (planting date)
# *INITIAL CONDITIONS
# @C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME
# 1    MZ 80085   -99     0     1     1   -99     0     0     0   100    15 -99


information$ICBL <- c(25, 45, 95)
information$SH20 <- -99
information$SNH4 <- c(4.2, 4.4, 4.5)
information$SNO3 <- c(11.9, 12.4, 7.6)

# Planting Details


## doing a comment that explain all parameters
information$PDATE <- 80092
information$SDATE <- pmax(information$PDATE - 20, 0)   ## Starting simulation
information$plant <- 'R'  # R = planting on reporting date
## Remember Simulation date starts 20 days before planting date
information$EDATE <- -99
information$PPOP <- 6.25
information$PPOE <- 6.25
information$PLME <- 'S'
information$PLDS <- 'R'
information$PLRS <- 80
information$PLRD <- 90
information$PLDP <- 4
## Variables como PLWT, PAGE, PENV, PLPH, SPRL con -99


## Date of application of irrigation

information$IRRIG <- 'R'  ## R on reporting date, A automatically irragated, N Nothing
information$IROP <- 'IR001'  ## 
information$IDATE <- c(15289, 15293, 15296, 15304, 15345, 15362, 16020, 16036)
information$IRVAL <- c(36, 35.59, 39.53, 21.13, 27.77, 32.65, 37.10, 41.47)



# if is necessary to specify sowing dates 
information$pfrst <- -99
information$plast <- -99
information$IC <- 1 # 1 = True or  0 = False  (when you want to use Initial Condigions)
information$system <- 'irrigation'  ## Solo se habilita con IC = 1 (irrigation es como condiciones inicial del suelo menos drasticas)

## Fertilizers

information$FDATE = c(21, 21, 35, 35, 58)  ## Dias de la aplicacion +
information$FMCD = c('FE006', 'FE016', 'FE005', 'FE016', 'FE005') ## Investigar acerca de este parametro
information$FACD = 'AP002' ## Investigar acerca de este parametro
information$FDEP = 4       ## Profundidad de la aplicacion del nitrogeno
information$FAMN = c(33.3, 0, 63.9, 0, 63.9)
information$FAMP = c(29.1, 0, 0, 0, 0) ## Investigar mas acerca de este parametro
information$FAMK = c(0, 36, 0, 39.2, 0)
information$FAMC = 0
information$FAMO = 0
information$FOCD = 0
information$FERNAME = -99
information$FERTI = 'D' ## D = dias despues de la siembra





## Maybe to add a function recognice when SDATE = 0 so this function should a new clima datasets for repet the same year of 
## Simulation only for start simulation another year (the last of the first year)

Xfile <- function(information, pixel, initial) {
  
  
  # Cultivars
  
  INGENO <- information$INGENO 
  CR <- information$CR
  CNAME <- information$CNAME

  ## Fields
  
  WSTA <- information$WSTA
  ID_SOIL <- information$ID_SOIL
  SMODEL <- paste(information$smodel)
  
  ## Initial Conditions Must be a data frame
  
  
  ICBL <- information$ICBL
  SH20 <- information$SH20
  SNH4 <- information$SNH4
  SNo3 <- information$SNO3
  
  # Planting Details
  
  PDATE <- information$PDATE   
  PLANT <- information$plant
  ## Maybe to add a function recognice when SDATE = 0 so this function should a new clima datasets for repet the same year of 
  ## Simulation only for start simulation another year (the last of the first year)
  SDATE <- information$SDATE
  PPOP <- information$PPOP
  PPOE <- information$PPOE
  PLME <- paste(information$PLME)
  PLDS <- paste(information$PLDS)
  PLRD <- information$PLRD 
  PLDP <- information$PLDP 
  
  
  ## Date of application of irrigation

  IRRIG <- information$IRRIG
  IROP <- information$IROP
  IDATE <- information$IDATE
  IRVAL <- information$IRVAL
  
  
  
  ## Fertilizers
  
  
  FDATE <- information$FDATE
  FMCD <- information$FMCD
  FACD = information$FACD
  FDEP = information$FDEP
  FAMN = information$FAMN
  FAMP = information$FAMP
  FAMK = information$FAMK
  FAMC = information$FAMC
  FAMO = information$FAMO
  FOCD = information$FOCD
  FERNAME = information$FERNAME
  FERTI = information$FERTI
  
  ### AUTOMATIC MANAGEMENT
  
  PFRST <-  information$pfrst
  PLAST <- information$plast
  
  
  # C = 1  ## What is C??
  
  # PLANTING = A
  
  IC <- information$IC 
  NYERS <- information$NYERS
  NITRO <- information$NITRO 
  WATER <- information$WATER 
  
  
  
  ## Defining the Experiment
  in_data <- list()
   # IC <- IC
  
  ## General data of the Experiment
  
  in_data$general <- list(PEOPLE = "Leonardo Ordoñez and Jeison Mesa", ADDRESS = "CIAT", SITE = "CALI")
  
  
  ## Definition simulate treatment
  in_data$treatments <- data.frame(N = 1, R = 1, O = 0, C = 0, TNAME = "USAID", CU = 1, FL = 1, SA = 0, IC = IC, MP = 1,
                                   MI = 0, MF = 1, MR = 0, MC = 0, MT = 0, ME = 0, MH = 0, SM = 1)
  
  ## Definition simulate cultivar
  
  in_data$cultivars <- data.frame(C = 1 , CR, INGENO, CNAME)
  
  ## Field
  
  in_data$fields <- data.frame(L = 1, ID_FIELD = "USAID", WSTA, FLSA = -99, FLOB = -99, FLDT = "DR000",
                               FLDD = -99, FLDS = -99, FLST = -99, SLTX = -99, SLDP = -99, ID_SOIL,
                               FLNAME = "FIELD01", XCRD = -99, YCRD = -99, ELEV = -99, AREA = -99, SLEN=-99,
                               FLWR = -99, SLAS = -99, FLHST = -99, FHDUR=-99)
  
  ## initial conditions of the experiment
  ## Aqui investigar acerca de ICDAT
  ## Segun el manual Initial Conditions Measurement date, year + days
  # in_data$ini_cond_properties <- data.frame(C = 1, PCR = information$CR, ICDAT = "50001", ICRT = -99, ICND = -99, ICRN = -99, ICRE = -99,
  # ICWD = -99, ICRES = -99, ICREN = -99, ICREP = -99, ICRIP = -99, ICRID = -99,
  # ICNAME = "inicond1")
  
  #in_data$ini_cond_profile <- data.frame(C=rep(1,5),ICBL=rep(-99,5),SH2O=rep(-99,5),SNH4=rep(-99,5),
  #                                       SNO3=rep(-99,5))
  
  
  ## Initial Conditions
  
  in_data$ini_cond_field <- data.frame(C = 1, PCR = information$CR, ICDAT = PDATE, ICRT = -99, ICND = -99, ICRN = -99, ICRE = -99,
                                  ICWD = -99, ICRES = -99, ICREN = -99, ICREP = -99, ICRIP = -99, ICRID = -99,
                                  ICNAME = -99)
  
  in_data$ini_cond_values <- data.frame(C= rep(1,length(SNH4)),ICBL, SH20,SNH4,
                                        SNo3)
  
  ## Fetilizer Details
  # print('Here')
  in_data$fertilizer <- data.frame(F = 1, FDATE, FMCD, FACD, FDEP, FAMN, FAMP, FAMK,
                                   FAMC, FAMO, FOCD, FERNAME)
  
  ## Planting Details
  in_data$planting <- data.frame( P = 1, PDATE, EDATE = -99, PPOP, PPOE, PLME, 
                                  PLDS, PLRS = 80, PLRD, PLDP,
                                  PLWT = -99, PAGE = -99, PENV = -99, PLPH = -99, SPRL = -99)
  

  
  ## Simulation Control 
  in_data$sim_ctrl <- data.frame(N = 1, GENERAL = "GE", NYERS, NREPS = 1, START = "S", SDATE, 
                                 RSEED = 2150, SNAME = "simctr1", SMODEL, 
                                 OPTIONS = "OP", WATER, NITRO, SYMBI = "N",
                                 PHOSP = "N", POTAS = "N", DISES = "N", CHEM = "N", TILL = "N", 
                                 CO2 = "M", METHODS = "ME", WTHER = "M", INCON = "M", LIGHT = "E", 
                                 EVAPO = "R", INFIL = "S", PHOTO = "C", HYDRO = "R",
                                 NSWIT = 1, MESOM = "G", MESEV = "S", MESOL =2, MANAGEMENT = "MA", 
                                 PLANT, IRRIG,
                                 FERTI, RESID = "R", HARVS = "M", OUTPUTS = "OU", FNAME = "N", 
                                 OVVEW = "Y", SUMRY = "Y", FROPT = 1, GROUT = "Y", CAOUT = "Y", 
                                 WAOUT = "Y", NIOUT = "Y", MIOUT = "Y",
                                 DIOUT = "Y", VBOSE = "Y", CHOUT = "Y", OPOUT = "Y")
  
  ## AUTOMATIC MANAGEMENT 
  
  in_data$auto_mgmt <- data.frame(N = 1, PLANTING = 'PL', PFRST, PLAST, PH2OL = 50, PH2OU = 100,
                                  PH2OD = 30, PSTMX = 40, PSTMN = 10, IRRIGATION = "IR", IMDEP =30, ITHRL = 50, 
                                  ITHRU =100, IROFF = "GS000", IMETH = "IR001", IRAMT = 10, IREFF = 1,
                                  NITROGEN = "NI", NMDEP = 30, NMTHR = 50, NAMNT = 25, NCODE = "FE001",
                                  NAOFF = "GS000", RESIDUES = "RE", RIPCN = 100, RTIME = 1, RIDEP = 20, 
                                  HARVEST = "HA", HFRST = 0, HLAST = 00001, HPCNP = 100, HPCNR = 0)
  
  
  

  
  # Make Xfile
  
  ## test
  ## out_file <- "./JBID.RIX"
  # overwrite <- F
  
  make_xfile <- function(in_data, out_file, overwrite = F) {
    #open file in write mode
    if (file.exists(out_file)) {
      if (overwrite) {
        pf <- file(out_file, open = "w")
      } else {
        rnum <- round(runif(1, 10000, 20000), 0)
        tmpvar <- unlist(strsplit(out_file, "/", fixed = T))
        pth_ref <- paste(tmpvar[1:(length(tmpvar) - 1)], collapse = "/")
        out_file <- paste(pth_ref, "/copy-", rnum, "_", tmpvar[length(tmpvar)], sep = "")
        pf <- file(out_file, open = "w")
      }
    } else {
      pf <- file(out_file,open="w")
    }
    
    # Write header and stuff
    # pf <- file(out_file,open="w")
    
    cat(paste0(information$exp_details, "\n"), file = pf)
    cat("\n",file = pf)
    
    # General stuff
    
    cat("*GENERAL\n@PEOPLE\n", file = pf)
    cat(paste(sprintf("%-12s", as.character(in_data$general$PEOPLE)), "\n", sep = ""), file = pf)
    cat("@ADDRESS\n", file = pf)
    cat(paste(sprintf("%-12s", as.character(in_data$general$ADDRESS)), "\n", sep = ""), file = pf)
    cat("@SITE\n", file = pf)
    cat(paste(sprintf("%-12s", as.character(in_data$general$SITE)), "\n", sep = ""), file = pf)
    
    # Treatments
    cat("*TREATMENTS                        -------------FACTOR LEVELS------------\n", file = pf)
    cat("@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM\n", file = pf)
    for (i in 1:nrow(in_data$treatments)) {
      
      cat(paste(sprintf("%1$2d%2$2d%3$2d%4$2d",as.integer(in_data$treatments$N[i]),as.integer(in_data$treatments$R[i]),
                        as.integer(in_data$treatments$O[i]),as.integer(in_data$treatments$C[i])),
                " ",sprintf("%1$-25s%2$3d%3$3d%4$3d%5$3d%6$3d%7$3d%8$3d%9$3d%10$3d%11$3d%12$3d%13$3d%14$3d",in_data$treatments$TNAME[i],
                            as.integer(in_data$treatments$CU[i]),as.integer(in_data$treatments$FL[i]),as.integer(in_data$treatments$SA[i]),
                            as.integer(in_data$treatments$IC[i]),as.integer(in_data$treatments$MP[i]),as.integer(in_data$treatments$MI[i]),
                            as.integer(in_data$treatments$MF[i]),as.integer(in_data$treatments$MR[i]),as.integer(in_data$treatments$MC[i]),
                            as.integer(in_data$treatments$MT[i]),as.integer(in_data$treatments$ME[i]),as.integer(in_data$treatments$MH[i]),
                            as.integer(in_data$treatments$SM[i])),
                "\n", sep = ""), file = pf)
      
    }
    cat("\n", file = pf)
    
    # cultivars
    cat("*CULTIVARS\n", file = pf)
    cat("@C CR INGENO CNAME\n", file = pf)
    for (i in 1:nrow(in_data$cultivars)) {
      cat(paste(sprintf("%2d",as.integer(in_data$cultivars$C[i]))," ",sprintf("%2s", in_data$cultivars$CR[i]),
                " ", sprintf("%6s",in_data$cultivars$INGENO[i])," ",sprintf("%-12s",in_data$cultivars$CNAME[i]),
                "\n", sep = ""), file = pf)
    }
    cat("\n", file = pf)
    
    # fields
    cat("*FIELDS\n", file = pf)
    cat("@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME\n", file = pf)
    for (i in 1:nrow(in_data$treatments)) { 
      
      cat(paste(sprintf("%2d",as.integer(in_data$fields$L[i]))," ",sprintf("%-8s",in_data$fields$ID_FIELD[i]),
                " ",sprintf("%-8s",in_data$fields$WSTA[i]),sprintf("%6d",as.integer(in_data$fields$FLSA[i])),
                sprintf("%6d",as.integer(in_data$fields$FLOB[i])),sprintf("%6s",in_data$fields$FLDT[i]),
                sprintf("%6d",as.integer(in_data$fields$FLDD[i])),sprintf("%6s",as.integer(in_data$fields$FLDS[i])),
                sprintf("%6d",as.integer(in_data$fields$FLST[i]))," ",sprintf("%-4d",as.integer(in_data$fields$SLTX[i])),
                sprintf("%6d",as.integer(in_data$fields$SLDP[i])),"  ",sprintf("%-10s",in_data$fields$ID_SOIL[i])," ",
                sprintf("%-12s",in_data$fields$FLNAME[i]),"\n", sep=""),file=pf)
      
    }
    
    cat("\n", file = pf)
    
    cat("@L ..........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR\n",file=pf)
    
    for (i in 1:nrow(in_data$treatments)) { 
      
      
      cat(paste(sprintf("%2d",as.integer(in_data$fields$L[i]))," ",sprintf("%15.3f",in_data$fields$XCRD[i])," ",
                sprintf("%15.3f",in_data$fields$YCRD[i])," ",sprintf("%9d",as.integer(in_data$fields$ELEV[i]))," ",
                sprintf("%17d",as.integer(in_data$fields$AREA[i]))," ",sprintf("%5d",as.integer(in_data$fields$SLEN[i]))," ",
                sprintf("%5d",as.integer(in_data$fields$FLWR[i]))," ",sprintf("%5d",as.integer(in_data$fields$SLAS[i]))," ",
                sprintf("%5d",as.integer(in_data$fields$FLHST[i]))," ",sprintf("%5d",as.integer(in_data$fields$FHDUR[i])),
                "\n",sep=""),file=pf)
      
      
      }
    
    
    cat("\n",file=pf)
    
    #initial conditions
    cat("*INITIAL CONDITIONS\n",file=pf)
    cat("@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$ini_cond_properties$C))," ",sprintf("%5s",in_data$ini_cond_properties$PCR),
             " ",sprintf("%5s",in_data$ini_cond_properties$ICDAT)," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRT)),
             " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICND))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRN)),
             " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRE))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICWD)),
             " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRES))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICREN)),
             " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICREP))," ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRIP)),
             " ",sprintf("%5d",as.integer(in_data$ini_cond_properties$ICRID))," ",sprintf("%-12s",in_data$ini_cond_properties$ICNAME),
             "\n",sep=""),file=pf)
    cat("@C  ICBL  SH2O  SNH4  SNO3\n",file=pf)
    
    for (i in 1:nrow(in_data$ini_cond_values)) {
      cat(paste(sprintf("%2d %5d %5d %5.2f %5.2f", 1, in_data$ini_cond_values[i, 'ICBL'], in_data$ini_cond_values[i, 'SH20'],
                        in_data$ini_cond_values[i, 'SNH4'], in_data$ini_cond_values[i , 'SNo3'])), "\n", file = pf)
    }
    cat("\n",file=pf)
    
     # Initial Conditions
     # if(exists("ini_cond")) {
     # 
     #   cat("*INITIAL CONDITIONS\n", file = pf)
     #   cat("@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME\n", file = pf)
     #   cat(sprintf("%2d %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %2s", 1, "MZ", -99, 1, -99, 1, 1, -99,  -99, -99, -99, -99, -99, -99), "\n", file = pf)
     # 
     #   cat("@C  ICBL  SH2O  SNH4  SNO3\n", file = pf)
     #   for(i in 1:dim(ini_cond)[1]){
     # 
     #     if(information$system  == "rainfed"){
     #       ## CAmbio en las Condiciones iniciales de Nitrogeno 5 Octubre 2015
     #       #cat(paste(sprintf("%2d %5d %5.2f %5.2f %5.2f", 1, in_conditions[i, 1], in_conditions[i, 2], SNH4[i], SNO3[i])), "\n", file = pf)
     #       cat(paste(sprintf("%2d %5d %5d %5.2f %5.2f", 1, ini_cond[i, 'SLB'], ini_cond[i, 'SDUL'], ini_cond[i, 'SLOC'], ini_cond[i , 'SLOC'])), "\n", file = pf)
     #       
     #     }
     # 
     #     if(information$system  == "irrigation"){
     #       #cat(paste(sprintf("%2d %5d %5.0f %5.2f %5.2f", 1, in_conditions[i, 1], in_conditions[i, 2], SNH4[i], SNO3[i])), "\n", file = pf)
     #       cat(paste(sprintf("%2d %5d %5.2f %5.2f %5.2f", 1, ini_cond[i, 'SLB'], ini_cond[i, 'SDUL'], ini_cond[i, 'SLOC'], ini_cond[i , 'SLOC'])), "\n", file = pf)
     #     }
     # 
     # 
     #   }
    
    
     # }
    
    
    
    cat("\n", file = pf)
    #planting details
    cat("*PLANTING DETAILS\n",file = pf)
    cat("@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$planting$P))," ",sprintf("%5s",in_data$planting$PDATE),
              " ",sprintf("%5s",in_data$planting$EDATE)," ",sprintf("%5d",as.integer(in_data$planting$PPOP)),
              " ",sprintf("%5d",as.integer(in_data$planting$PPOE))," ",sprintf("%5s",in_data$planting$PLME),
              " ",sprintf("%5s",in_data$planting$PLDS)," ",sprintf("%5d",as.integer(in_data$planting$PLRS)),
              " ",sprintf("%5d",as.integer(in_data$planting$PLRD))," ",sprintf("%5d",as.integer(in_data$planting$PLDP)),
              " ",sprintf("%5d",as.integer(in_data$planting$PLWT))," ",sprintf("%5d",as.integer(in_data$planting$PAGE)),
              " ",sprintf("%5d",as.integer(in_data$planting$PENV))," ",sprintf("%5d",as.integer(in_data$planting$PLPH)),
              " ",sprintf("%5d",as.integer(in_data$planting$SPRL))," ",sprintf("%29s",in_data$planting$PLNAME),
              "\n", sep = ""), file = pf)
    cat("\n", file = pf)
    
    
    cat("*FERTILIZERS (INORGANIC)\n", file = pf)
    cat("@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME                       \n", file = pf)
    
    for(i in 1:dim(in_data$fertilizer)[1]){
    
      cat(paste(sprintf("%2d %5i %5s %5i %5i %5.2f %5.2f %5.2f %5i %5i %5i %5-i", 1, in_data$fertilizer$FDATE[i], in_data$fertilizer$FMCD[i],
                        in_data$fertilizer$FACD[i], in_data$fertilizer$FDEP[i], in_data$fertilizer$FAMN[i], in_data$fertilizer$FAMP[i], 
                        in_data$fertilizer$FAMK[i], in_data$fertilizer$FAMC[i], in_data$fertilizer$FAMO[i], 
                        in_data$fertilizer$FOCD[i], in_data$fertilizer$FERNAME[i]), '\n'), file = pf)
      
    }
    
   
    cat("\n", file = pf)
    
    # ## Details Fertilization
    # cat("*FERTILIZERS (INORGANIC)\n", file = pf)
    # cat("@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME                       \n", file = pf)
    # for(i in 1:dim(information$nitrogen_aplication$amount)[2]){
    #   if(!is.na(information$nitrogen_aplication$amount[, i])) {
    #     
    #     
    #     if(i == 1){
    #       
    #       cat(sprintf("%2s %5s %4s %5s %5i %5.1f %5i %5i %5i %5i %5i %1i", 1, information$nitrogen_aplication$day_app[, i], "FE005", "AP002", 
    #                   4, information$nitrogen_aplication$amount[, i], 0, -99, -99, -99, -99, -99), "\n", file = pf)
    #       
    #     } else{
    #       cat(sprintf("%2s %5s %4s %5s %5i %5.1f %5i %5i %5i %5i %5i %1i", 1, information$nitrogen_aplication$day_app[, i], "FE005", "AP002", 
    #                   0, information$nitrogen_aplication$amount[, i], 0, -99, -99, -99, -99, -99), "\n", file = pf)
    #     }
    #     
    #   }
    #   
    # }
    # cat("\n", file = pf)
    
    #simulation controls
    cat("*SIMULATION CONTROLS\n", file = pf)
    cat("@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL\n", file = pf)
    cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$GENERAL),
              " ",sprintf("%5d",as.integer(in_data$sim_ctrl$NYERS))," ",sprintf("%5d",as.integer(in_data$sim_ctrl$NREPS)),
              " ",sprintf("%5s",in_data$sim_ctrl$START)," ",sprintf("%5s",in_data$sim_ctrl$SDATE),
              " ",sprintf("%5d",as.integer(in_data$sim_ctrl$RSEED))," ",sprintf("%-25s",in_data$sim_ctrl$SNAME),
              " ",sprintf("%-6s",in_data$sim_ctrl$SMODEL),"\n",sep=""),file=pf)
    cat("@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$OPTIONS),
              " ",sprintf("%5s",in_data$sim_ctrl$WATER)," ",sprintf("%5s",in_data$sim_ctrl$NITRO),
              " ",sprintf("%5s",in_data$sim_ctrl$SYMBI)," ",sprintf("%5s",in_data$sim_ctrl$PHOSP),
              " ",sprintf("%5s",in_data$sim_ctrl$POTAS)," ",sprintf("%5s",in_data$sim_ctrl$DISES),
              " ",sprintf("%5s",in_data$sim_ctrl$CHEM)," ",sprintf("%5s",in_data$sim_ctrl$TILL),
              " ",sprintf("%5s",in_data$sim_ctrl$CO2),"\n",sep=""),file=pf)
    cat("@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$METHODS),
              " ",sprintf("%5s",in_data$sim_ctrl$WTHER)," ",sprintf("%5s",in_data$sim_ctrl$INCON),
              " ",sprintf("%5s",in_data$sim_ctrl$LIGHT)," ",sprintf("%5s",in_data$sim_ctrl$EVAPO),
              " ",sprintf("%5s",in_data$sim_ctrl$INFIL)," ",sprintf("%5s",in_data$sim_ctrl$PHOTO),
              " ",sprintf("%5s",in_data$sim_ctrl$HYDRO)," ",sprintf("%5d",as.integer(in_data$sim_ctrl$NSWIT)),
              " ",sprintf("%5s",in_data$sim_ctrl$MESOM)," ",sprintf("%5s",in_data$sim_ctrl$MESEV),
              " ",sprintf("%5d",as.integer(in_data$sim_ctrl$MESOL)),"\n",sep=""),file=pf)
    cat("@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$MANAGEMENT),
              " ",sprintf("%5s",in_data$sim_ctrl$PLANT)," ",sprintf("%5s",in_data$sim_ctrl$IRRIG),
              " ",sprintf("%5s",in_data$sim_ctrl$FERTI)," ",sprintf("%5s",in_data$sim_ctrl$RESID),
              " ",sprintf("%5s",in_data$sim_ctrl$HARVS),"\n",sep=""),file=pf)
    cat("@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$sim_ctrl$N))," ",sprintf("%-11s",in_data$sim_ctrl$OUTPUTS),
              " ",sprintf("%5s",in_data$sim_ctrl$FNAME)," ",sprintf("%5s",in_data$sim_ctrl$OVVEW),
              " ",sprintf("%5s",in_data$sim_ctrl$SUMRY)," ",sprintf("%5s",in_data$sim_ctrl$FROPT),
              " ",sprintf("%5s",in_data$sim_ctrl$GROUT)," ",sprintf("%5s",in_data$sim_ctrl$CAOUT),
              " ",sprintf("%5s",in_data$sim_ctrl$WAOUT)," ",sprintf("%5s",in_data$sim_ctrl$NIOUT),
              " ",sprintf("%5s",in_data$sim_ctrl$MIOUT)," ",sprintf("%5s",in_data$sim_ctrl$DIOUT),
              " ",sprintf("%5s",in_data$sim_ctrl$VBOSE)," ",sprintf("%5s",in_data$sim_ctrl$CHOUT),
              " ",sprintf("%5s",in_data$sim_ctrl$OPOUT),"\n",sep=""),file=pf)
    cat("\n", file = pf)
    
    #automatic management
    cat("@  AUTOMATIC MANAGEMENT\n", file = pf)
    cat("@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN\n", file = pf)
    cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$PLANTING),
              " ",sprintf("%5s",in_data$auto_mgmt$PFRST)," ",sprintf("%5s",in_data$auto_mgmt$PLAST),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$PH2OL))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$PH2OU)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$PH2OD))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$PSTMX)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$PSTMN)),"\n",sep=""),file=pf)
    cat("@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$IRRIGATION),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$IMDEP))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$ITHRL)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$ITHRU))," ",sprintf("%5s",in_data$auto_mgmt$IROFF),
              " ",sprintf("%5s",in_data$auto_mgmt$IMETH)," ",sprintf("%5d",as.integer(in_data$auto_mgmt$IRAMT)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$IREFF)),"\n",sep=""),file=pf)
    cat("@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$NITROGEN),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$NMDEP))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$NMTHR)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$NAMNT))," ",sprintf("%5s",in_data$auto_mgmt$NCODE),
              " ",sprintf("%5s",in_data$auto_mgmt$NAOFF),"\n",sep=""),file=pf)
    cat("@N RESIDUES    RIPCN RTIME RIDEP\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$RESIDUES),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$RIPCN))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$RTIME)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$RIDEP)),"\n",sep=""),file=pf)
    cat("@N HARVEST     HFRST HLAST HPCNP HPCNR\n",file=pf)
    cat(paste(sprintf("%2d",as.integer(in_data$auto_mgmt$N))," ",sprintf("%-11s",in_data$auto_mgmt$HARVEST),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$HFRST))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$HLAST)),
              " ",sprintf("%5d",as.integer(in_data$auto_mgmt$HPCNP))," ",sprintf("%5d",as.integer(in_data$auto_mgmt$HPCNR)),
              "\n",sep=""),file=pf)
    
    
    #close file
    close(pf)
    
    #output
    
    return(out_file)
  }
  
  make_xfile(in_data, out_file = information$name, overwrite = T)
}










## September 2015
## CIAT, Code's Jeison
## Change the dates of the WTH files generated by Marksim

## PAckages Neccesary

# library(lubridate)
# 
# ## New dates
# 
# initial <- 1999
# final <- 1999
# 
# ## path
# 
# path <- "C:/Runs_DSSAT/675574/"
# out_dir <- "C:/Runs_DSSAT/675574/"
# archivo <- "01680101.WTG"

