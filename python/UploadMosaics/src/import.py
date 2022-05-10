import os
import sys
from tools import GeoserverClient


stores_aclimate = ["seasonal_country_probabilistic_above", "seasonal_country_probabilistic_normal",
                   "seasonal_country_probabilistic_below", "seasonal_country_deterministic"]
folder_root = "/forecast/workdir/usaid_procesos_interfaz/python/UploadMosaics/"
folder_data = os.path.join(folder_root, "data")
folder_layers = os.path.join(folder_data, "layers")
folder_properties = os.path.join(folder_layers, "properties")
folder_tmp = os.path.join(folder_data, "tmp")
geo_url = "https://geo.aclimate.org/geoserver/rest/"
geo_user = os.environ['GEO_USER']
geo_pwd = os.environ['GEO_PWD']
workspace_name = sys.argv[1]
# Connecting
geoclient = GeoserverClient(geo_url, geo_user, geo_pwd)

for current_store in stores_aclimate:

    current_layer = current_store.split("_")[-1]
    current_rasters_folder = os.path.join(folder_layers, current_layer)
    rasters_files = os.listdir(current_rasters_folder)

    store_name = current_store
    print("Importing")
    geoclient.connect()
    geoclient.get_workspace(workspace_name)

    for r in rasters_files:
        store = geoclient.get_store(store_name)
        layer = os.path.join(current_rasters_folder, r)
        if not store:
            print("Creating mosaic")
            geoclient.create_mosaic(
                store_name, layer, folder_properties, folder_tmp)
        else:
            print("Updating mosaic")
            geoclient.update_mosaic(
                store, layer, folder_properties, folder_tmp)

print("Process completed")
