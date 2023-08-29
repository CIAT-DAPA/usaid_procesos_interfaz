cat("\n Aclimate Resampling - Start")

aclimate_resampling_log = paste0(dirLogs,"aclimate_resampling.log")

aclimate_resampling_start <- Sys.time()

year = Sys.Date()
year = as.numeric(format(year, "%Y"))

command = paste(
    "aclimate_resampling", "-C", currentCountry, "-p", dirWorkdir, "-c", no_cores, "-m", "1", "-y", year, 
)

aclimate_output <- system(command, intern = TRUE)

aclimate_resampling_end <- Sys.time()

aclimate_resampling_time <- as.numeric(difftime(aclimate_resampling_end, aclimate_resampling_start, units = "mins"))

message <- paste("The duration of the aclimate resampling process was:", aclimate_resampling_time, "minutes")


file <- paste0(dirLogs, "aclimate_resampling_time.txt")

writeLines(aclimate_output, con = aclimate_resampling_log)
writeLines(message, con = file)

cat("\n Aclimate Resampling - End")