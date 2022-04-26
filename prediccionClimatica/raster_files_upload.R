
#http://localhost:8600/geoserver/rest/imports
#url_geoserver_api <- "https://geo.aclimate.org/" #"http://localhost:8600/geoserver/rest/imports"
url_geoserver_api <- "http://localhost:8600/geoserver/rest/imports"
url_geoserver_api2 <- "http://172.17.0.3:8080/geoserver/rest/imports"
#Upload and import the raster files to Geosever
#The first POST() upload the raster files and prepare them to be import
#The second POST() import the raster files
upload_raster_files_to_geoserver <- function(geoserver_url_api){

  require(httr)
  #workspaceName <- paste0("aclimate_", countries_ids[currentCountry]) #aclimate_61e59d829d5d2486e18d2ea9"
  #initialization <-
  storeName <- "Seasonal"
  
  body <- '{
    "import": {
        "targetWorkspace": {
          "workspace": {
              "name":"', workspaceName,'"
          }
        },
        "data": {
          "type": "directory",
          "location": "/opt/geoserver/data_dir/data"
        }
    }
  }'

  # body <- paste0('{
  #   "import": {
  #       "targetStore": {
  #       "dataStore": {
  #         "name":"'storeName, '"
  #       }
  #     },
  #       "targetWorkspace": {
  #         "workspace": {
  #             "name":"', workspaceName,'"
  #         }
  #       },
  #       "data": {
  #         "type": "directory",
  #         "location": "/data/aclimate_et/"
  #       }
  #   }
  # }')
  response <- POST(geoserver_url_api, content_type_json(), body = body, authenticate("admin", "geoserver"))
  import_id <- content(response)$import$id
  POST(paste0(geoserver_url_api, "/", import_id), authenticate("admin", "geoserver"))
}

upload_raster_files_to_geoserver(url_geoserver_api2)

cat("\n Raster files imported in Geoserver \n")