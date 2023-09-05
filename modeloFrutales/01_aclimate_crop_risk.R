cat("\n Aclimate Crop Risk - Start")

aclimate_crop_risk_log = paste0(dirLogs,"aclimate_crop_risk.log")

aclimate_crop_risk_start <- Sys.time()

command = paste(
    "aclimate_crop_risk", "-C", currentCountry, "-p", dirWorkdir, "-s", no_cores, "-c", crop
)

aclimate_output <- system(command, intern = TRUE)

aclimate_crop_risk_end <- Sys.time()

aclimate_crop_risk_time <- as.numeric(difftime(aclimate_crop_risk_end, aclimate_crop_risk_start, units = "mins"))

message <- paste("The duration of the aclimate_crop_risk process was:", aclimate_crop_risk_time, "minutes")


file <- paste0(dirLogs, "aclimate_crop_risk_time.txt")

writeLines(aclimate_output, con = aclimate_crop_risk_log)
writeLines(message, con = file)

cat("\n Aclimate Crop Risk - End")