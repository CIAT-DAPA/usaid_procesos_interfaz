cat("\n Aclimate CPT - Prediction Start")

aclimate_cpt_log = paste0(dirLogs,"aclimate_cpt.log")

aclimate_cpt_start <- Sys.time()

command = paste(
    "aclimate_cpt", "-C", currentCountry, "-p", dirWorkdir, "-c", no_cores
)

aclimate_output <- system(command, intern = TRUE)

aclimate_cpt_end <- Sys.time()

aclimate_cpt_time <- as.numeric(difftime(aclimate_cpt_end, aclimate_cpt_start, units = "mins"))

message <- paste("The duration of the aclimate_cpt process was:", aclimate_cpt_time, "minutes")


file <- paste0(dirLogs, "aclimate_cpt_time.txt")

writeLines(aclimate_output, con = aclimate_cpt_log)
writeLines(message, con = file)

cat("\n Aclimate CPT - Prediction End")