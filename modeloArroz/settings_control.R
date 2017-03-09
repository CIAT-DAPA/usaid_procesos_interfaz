## settings control file


## make the head control to the *.DAT
settings_head <- function(CONTROLFILE, STRUN, ENDRUN){
  
  data.frame(CONTROLFILE, STRUN, ENDRUN)
  
}
# information <- settings_head('CONTROL', 1, 1)

## settings parameters of control 
# FILEON = name of output file
# FILEOL = name of log file
# FILEIR = name of Rerun file
# next parameters can be specify by the Rerun file

ctrl_params <- function(FILEON, FILEOL, FILEIR){
  
  
  data.frame(FILEON, FILEOL, FILEIR)

  
}

# information <- ctrl_params('RES.DAT', 'MODEL.LOG', 'USAID.rer')

## Weather station information 
# CNTR = Weather station name
#  ISTN = weather station number


ctrl_wther <- function(CNTR, ISTN){
  
  data.frame(CNTR, ISTN)
  
  
}

# ctrl_wther('USAID', 1)




