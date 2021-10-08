
url_geoserver_api <- Sys.getenv("GEOSERVER_API") #"http://172.17.0.2:8080/geoserver/rest/imports"
#Upload and import the raster files to Geosever
#The first POST() upload the raster files and prepare them to be import
#The second POST() import the raster files
upload_raster_files_to_geoserver <- function(geoserver_url_api){

  require(httr)

  body <- '{
    "import": {
        "targetWorkspace": {
          "workspace": {
              "name": "Rasters"
          }
        },
        "data": {
          "type": "directory",
          "location": "/opt/geoserver/data_dir/data"
        }
    }
  }'
  response <- POST(geoserver_url_api, content_type_json(), body = body)
  import_id <- content(response)$import$id
  POST(paste0(geoserver_url_api, "/", import_id))
}

upload_raster_files_to_geoserver(url_geoserver_api)

cat("\n Archivos raster importados en Geoserver \n")