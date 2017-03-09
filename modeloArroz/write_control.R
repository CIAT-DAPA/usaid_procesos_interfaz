## make a file control.dat necessary to run ORYZA v.3

# out_file <-  'D:/CIAT/USAID/Oryza/usaid_forecast_rice/Prueba/'

## final_run is if you need turn on ENDRUN

# information <- settings_head('CONTROL', 1, 1)
# proof <- write_head(out_file, information)


write_head <- function(out_file, information, final_run = FALSE){
  
  CONTROLFILE <- information$CONTROLFILE 
  STRUN <- information$STRUN
  ENDRUN <- information$ENDRUN
  
  name_control <- paste0(out_file, CONTROLFILE, '.DAT')
  
  if(final_run == FALSE){
    
    sink(name_control, append = F)
    
    cat(paste0('*CONTROLFILE = ',  CONTROLFILE, '.DAT'), sep = '\n')
    cat(paste('STRUN =', STRUN), sep = '\n')
    cat(paste('*ENDRUN =', ENDRUN), sep = '\n')
    cat(sep = '\n')
    
    sink()
    
  } else{
    
    sink(name_control, append = F)
    
    paste('*CONTROLFILE =',  CONTROLFILE, sep = '\n')
    paste('STRUN =', STRUN, sep = '\n')
    paste('ENDRUN =', ENDRUN, sep = '\n')
    
  }
  
  return(name_control)
  
}

# information <- ctrl_params('RES.DAT', 'MODEL.LOG', 'USAID.rer')
# write_ctrl_params(proof, information)

write_ctrl_params <- function(name_control, information){
  
  FILEON <- information$FILEON
  FILEOL <- information$FILEOL
  FILEIR <- information$FILEIR

  sink(name_control, append = T)
  
  cat('*----------------------------------------------------------------------*', sep = '\n')
  cat('* control file for ORYZA2000 model AUTOCOLIBRATION                     *', sep = '\n')
  cat('*----------------------------------------------------------------------*', sep = '\n')
  
  cat(paste0('FILEON = ', "'", FILEON, "'"), sep = '\n')
  cat(paste0('FILEOL = ', "'", FILEOL, "'"), sep = '\n')
  cat(paste('FILEIT =', "' '"), sep = '\n')
  cat(paste('FILEI1 =', "' '"), sep = '\n')
  cat(paste0('FILEIR = ', "'", FILEIR, "'"), sep = '\n')
  cat(paste('FILEI2 =', "' '"), sep = '\n')
  cat(sep = '\n')
  
  sink()
}

#  information <- ctrl_wther('USAID', 1)
#  write_wther(proof, information)

write_wther <- function(name_control, information){
  
  
  CNTR <- information$CNTR
  ISTN <- information$ISTN
  
  sink(name_control, append = T)
  
  cat('*----------------------------------------------------------------------*', sep = '\n')
  cat('* Optionally, Weather Station information can be provided here         * ', sep = '\n')
  cat('* It is useful for large amount of simulations under same management   *', sep = '\n')
  cat('*----------------------------------------------------------------------*', sep = '\n')
  
#   cat('*----------------------------------------------------------------------*
# * Optionally, Weather Station information can be provided here         * 
# * It is useful for large amount of simulations under same management   *
# *----------------------------------------------------------------------*', sep = '\n')
  
  cat(paste('WTRDIR =', "' '"), sep = '\n')
  cat(paste0('CNTR = ', "'", CNTR, "'"), sep = '\n')
  cat(paste0('ISTN = ', ISTN), sep = '\n')
  cat(paste('MULTIY =', "'YES'"), sep = '\n')
  cat(sep = '\n')
  
  sink()
  
  
} 

## write_options_out(proof)

write_options_out <- function(name_control){
  
  sink(name_control, append = T)
  cat('*----------------------------------------------------------------------*
* Rice monoculture cropping system                                     *
* the default for SOILKILL is "YES", soil will be reinitiated every    *
* crop season, and all processes in soil will also stop as growth stop.*
* if "NO", soil will only be initiated at the starting date of simulation*
* and all processes in soil will continue after growth stop.           *
*----------------------------------------------------------------------*', sep = '\n')
  # cat(sep = '\n')
  
  cat(paste0('*SOILKILL = ', "'NO'", '   !* Whether the soil processes continue after crop maturity'), sep = '\n')
  
  # cat(sep = '\n')
  
  cat('*----------------------------------------------------------------------*
* Set output/print options                                             *
*----------------------------------------------------------------------*', sep = '\n')
  
  # cat(sep = '\n')
  
  cat(paste0('PRDEL = ', 1, '.', ' ! Output time step (day), too much reruns, omit detail outputs!'), sep = '\n')
  
  # cat(sep = '\n')
  
  cat(paste0('IPFORM = ', 5, '   ! Code for output table format:'), sep = '\n')
  
  # cat(sep = '\n')
  
  cat(paste0('COPINF = ', "'N'", '   ! Switch variable whether to copy the input files to the output'), sep = '\n')
  
  # cat(sep = '\n')
  
  cat(paste0('DELTMP = ', "'N'", '   ! Switch variable what should be done with the temporary output'), sep = '\n')
  
  # cat(sep = '\n')
  
  cat(paste0("IFLAG  = ", 1100, '  ! Indicates where weather error and warnings go'), sep = '\n')
 
  # cat(sep = '\n')
  
  cat(paste0('*PRSEL = ', "'WAGT'", ',', "'WRR14'", ',', "'WSO'"), sep = '\n')
  cat(paste0('*        ', "'LAI'", ',', " 'LAI_OBS'"), sep = '\n')
  
  cat(paste0('*OPSTRING = ', "'DAE, WRR14, WAGT'", ' !* It is for the output variables in op.dat'), sep = '\n')
  cat(paste0('*IOBSD = '), '2008,161')
  
  sink()
}
  