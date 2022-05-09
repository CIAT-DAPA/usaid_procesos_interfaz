import os
import sys
from tools import GeoserverClient

current_layer = sys.argv[2]
folder_root = "/forecast/workdir/usaid_procesos_interfaz/python/UploadMosaics/"
folder_data = os.path.join(folder_root, "data")
folder_layers = os.path.join(folder_data, "layers")
folder_properties = os.path.join(folder_layers, "properties")
folder_tmp = os.path.join(folder_data, "tmp")
current_rasters = os.path.join(folder_layers, current_layer)

layer = os.path.join(
    current_rasters, "6273d5bda31f58003091e701_jas_202208.tif")

#dir_list = os.listdir(path)

geo_url = "https://geo.aclimate.org/geoserver/rest/"
geo_user = "dguzman"
geo_pwd = "Mvu6ygSjQ#}hYW"

workspace_name = "aclimate_et"
# ["seasonal_country_probabilistic_above", "seasonal_country_probabilistic_normal", "seasonal_country_probabilistic_below", "seasonal_country_deterministic"]
store_name = "seasonal_country_deterministic"
############
geoclient = GeoserverClient(geo_url, geo_user, geo_pwd)

# for s in store_name:
print("Importing")
geoclient.connect()
geoclient.get_workspace(workspace_name)
store = geoclient.get_store(store_name)
# geoclient.check(store)

if not store:
    print("Creating mosaic")
    geoclient.create_mosaic(store_name, layer, folder_properties, folder_tmp)
else:
    print("Updating mosaic")
    geoclient.update_mosaic(store, layer, folder_properties, folder_tmp)

    #files = list()
    # if "above" in s:
    # files =

    # result = map()
