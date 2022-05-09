
#http://localhost:8600/geoserver/rest/imports
url_geoserver_api <- "https://geo.aclimate.org/geoserver/rest/imports" #"http://localhost:8600/geoserver/rest/imports"
#url_geoserver_api <- "http://localhost:8600/geoserver/rest/imports"
#url_geoserver_api2 <- "http://172.17.0.4:8080/geoserver/rest/imports"
usr <- Sys.getenv("GEO_USER")
pwd <- Sys.getenv("GEO_PWD")
#Upload and import the raster files to Geosever
#The first POST() upload the raster files and prepare them to be import
#The second POST() import the raster files
upload_raster_files_to_geoserver <- function(url_geoserver_api){

  require(httr)
  workspaceName <- paste0("aclimate_", countries_ids[currentCountry]) #aclimate_61e59d829d5d2486e18d2ea9"
  remote_location <- "data/aclimate_et/" #"/data/aclimate_et/"

  body <- list(
      import = list(targetWorkspace= list(workspace=list(name=workspaceName)), data=list(type="directory", location=remote_location))
      )

  response <- POST(url_geoserver_api, content_type_json(), body = jsonlite::toJSON(body,auto_unbox=TRUE), authenticate(usr, pwd))
  import_id <- content(response)$import$id
  POST(paste0(url_geoserver_api, "/", import_id), authenticate(usr, pwd))
}

upload_raster_files_to_geoserver(url_geoserver_api)

cat("\n Raster files imported in Geoserver \n")