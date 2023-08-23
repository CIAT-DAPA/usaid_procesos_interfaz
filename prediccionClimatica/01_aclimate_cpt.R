cat("\n Aclimate CPT - Prediction Start")

aclimate_cpt_log = paste0(dirLogs,"aclimate_cpt.log")

aclimate_cpt_start <- Sys.time()

system(paste(
    "aclimate_cpt", "-C", currentCountry, "-p", dirWorkdir, "-c", no_cores, ">",  aclimate_cpt_log, "2>&1"
))

aclimate_cpt_end <- Sys.time()

aclimate_cpt_time <- as.numeric(difftime(aclimate_cpt_end, aclimate_cpt_start, units = "mins"))

message <- paste("The duration of the acclimate_cpt process was:", aclimate_cpt_time, "minutes")


file <- paste0(dirLogs, "aclimate_cpt_time.txt")

writeLines(message, con = file)

cat("\n Aclimate CPT - Prediction End")