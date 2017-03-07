## es necesario que esta funcion escriba los parametros dado la localidad a generar el pronostico agroclimatico
# funcion que va generar los parametros por region 

# El parametro region genera la informacion necesaria para ajustar los parametros de entrada para DSSAT v 4.6 necesarios a correr por
# las regiones del proyecto con USAID


# add source functions


# path_functions <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/'
# source(paste0(path_functions, 'settings_xfile.R'))
# source(paste0(path_functions, 'functions_xfile.R'))

# dir_run <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Proof_run/'
# filename <- 'proof.MZX'
# WSTA <- 'USAID001'
# region <- "LaUnion"
# PDATE <- 17274
# SDATE <- 17274
# cultivar <- 'CI0027'
# ID_SOIL <- 'CCBuga0001'
  
# make_xfile_region(region, WSTA, paste0(dir_run, filename), PDATE, SDATE)


## esta funcion se necesita agregar el condicional si el archivo existe en tal caso es mejor yo creo eliminarlo (make_xfile)



make_xfile_region <- function(dir_parameters, WSTA, filename, PDATE, SDATE, cultivar, ID_SOIL){
  
  
  # if(region == "LaUnion"){
    
    
    ## Parameters necessary to write experimental file
    
  

    out_file <- filename     #./proof.MZX
    overwrite <- F
    details <- '*USAID-CIAT project Agroclimatic forecasts'
    people <- "Leonardo Ordonez and Jeison Mesa and Julian Ramirez"
    
   
    
    
    IC <- 0  # Inital conditions
    MI <- 0  # input if you are going to use a irrigation, 1 = TRUE, 0 = FALSe 
    MF <- 0 # Fertilization field, 1 = TRUE, 0 = FALSE
    MH <- 0 # its necessary to include harvest date when you turn on this parameter
    
    # # depends of the inputs
    # if(file.exists(paste0(dir_parameters, 'irrigation.csv'))){
    #   
    #   irrigation_head <- read_csv(paste0(dir_parameters, 'irrigation_head.csv'))
    #   irrigation <- read_csv(paste0(dir_parameters, 'irrigation.csv'))
    #   MI <- 0
    #   
    # }

    
    
    CR <- 'MZ'    # Crop Code, you need to search this parameter for de manual DSSAT (its different by crop)
    INGENO <- cultivar # Cultivar indentifier, this is the code for cultivar to run depend of crop
    CNAME <- 'PIO 30F35HRB_'  # Whatever code to identify the cultivar to run, maybe no too long string
  
    # ICBL <- c(25, 45, 95)
    # SH20 <- -99
    # SNH4 <- c(4.2, 4.4, 4.5)    # estas variables se pueden investigar cuales utilizar cuando no se tiene condiciones iniciales
    # SNO3 <- c(11.9, 12.4, 7.6)  # estas variables se pueden investigar cuales utilizar cuando no se tiene condiciones iniciales
    # ICDAT <- -99 #  for now
    
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
    # 
    
    
    ## doing a comment that explain all parameters
    
    
    input_pDetails <- as.data.frame(read_planting(dir_parameters)) %>%
      frame_list()
    
    
    ## IRRIGATION or RAINFED
    if(input_pDetails$IRR == 'YES'){
      IRR <- 'A'
    } else{
      
      IRR <- 'N'
    }
    
    input_pDetails$PDATE <- PDATE
    input_pDetails$SDATE <- SDATE
      
    # initial_conditions <- setNames(split(input_pDetails[,2], seq(nrow(input_pDetails))), input_pDetails[,1])
    
    # input_pDetails <- list()
    # input_pDetails$PDATE <- PDATE # Planting date
    # input_pDetails$SDATE <- SDATE
    # input_pDetails$plant <- 'R'  # R = planting on reporting date
    # input_pDetails$EDATE <- -99
    # input_pDetails$PPOP <- 6.25
    # input_pDetails$PPOE <- 6.25
    # input_pDetails$PLME <- 'S'
    # input_pDetails$PLDS <- 'R'
    # input_pDetails$PLRS <- 80
    # input_pDetails$PLRD <- 90
    # input_pDetails$PLDP <- 4
    
    
    ## Simulation Controls
    input_sControls <- list()
    input_sControls$NYERS <- 1 ## Years for simulation
    input_sControls$SMODEL <- 'MZCER046' # model to use
    input_sControls$WATER <- 'N'   ## Y = Utiliza balance Hidrico, N = No utiliza balance hidrico
    input_sControls$NITRO <-  'N'  ## Y = utiliza balance nitrogeno, N =  no utiliza balance nitrogeno
    input_sControls$PLANT <- 'R'  # R = planting on reporting date ## Add the other options
    input_sControls$IRRIG <- IRR  ##  R =  on reporting date, A automatically irragated, N Nothing, add the other options
    input_sControls$FERTI = 'N' ## add more options
    input_sControls$SDATE <- SDATE

    
    PFRST <- -99
    PLAST <- -99
    
    ## escritura del archivo experimental 
    options(encoding = "UTF-8")
    
    proof <- make_archive(out_file, overwrite = F,  encoding = "UTF-8") 
    
    write_details(proof, make_details(details, people))
    write_treatments(proof, make_treatments(IC, MI, MF, MH))  ## the parameter FL its to identify the run with a specific .WTH
    write_cultivars(proof, make_cultivars(CR, INGENO, CNAME))
    write_fields(proof, make_fields(WSTA, ID_SOIL))
    # Las corridas serán entonces de acuerdo al potencial en rendimiento que puedan alcanzar las plantas
    # write_IC(proof, make_IC(ICBL, SH20, SNH4, SNO3)) # posiblemente este campo no se necesite durante la corrida de los pronosticos
    # write_MF(proof, make_MF(input_fertilizer))         # sin requerimientos por fertilizantes dejarlo con potencial
    write_pDetails(proof, make_pDetails(input_pDetails))     
    write__sControls(proof, make_sControls(input_sControls))
    write_Amgmt(proof, make_Amgmt(PFRST, PLAST))
    close(proof)
    
    
  # }
  
}









