##############################################################################################################
#  Create batch file for CSM (.v46 file) 
##############################################################################################################


# to test
# its necessary to add dir_run into a funtion than is goint to run DSSAT with all specification and run into a particular folder
# dir_run <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Proof_run/'
# crop <- "MAIZE"
# name <- "proof.MZX"  # for linux ./proof.MZX, for windows proof.MZX USAID
# filename <- "DSSBatch.v46"  # filename
# filename <- "D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Proof_run/LaUnion/1/LaUnion/1/"

# CSMbatch(crop, name, paste0(dir_run, filename))

CSMbatch <- function(crop, name, filename) {
  
  outbatch <- rbind(
    rbind(
      # Batchfile headers            
      paste0("$BATCH(", crop, ")"),            
      "!",            
      cbind(sprintf("%6s %92s %6s %6s %6s %6s", "@FILEX", "TRTNO", "RP", "SQ", "OP", 
                    "CO"))),            
    cbind(sprintf("%6s %89s %6i %6i %6i %6i",            
                  paste0(name),
                  1:99,  # Variable for treatment number            
                  1,  # Default value for RP element            
                  0,  # Default value for SQ element            
                  1,  # Default value for OP element            
                  0)))  # Default value for CO element 
  
  # Write the batch file to the selected folder  
  write(outbatch, file = filename, append = F)
  
}





