import os
import sys
from tools import GeoserverClient


geo_url = "https://geo.aclimate.org/geoserver/rest/"
geo_user = os.environ['GEO_USER']
geo_pwd = os.environ['GEO_PWD']
workspace_name = sys.argv[1]
forecast_type = sys.argv[2]
country_iso = workspace_name.split("_")[1]

folder_root = "/forecast/usaid_procesos_interfaz/python/UploadMosaics/"
folder_data = os.path.join(folder_root, "data")
folder_layers = os.path.join(folder_data, "layers")
folder_properties = os.path.join(folder_layers, forecast_type+"_properties")
folder_tmp = os.path.join(folder_data, "tmp")


stores_aclimate = [forecast_type+"_country_"+country_iso+"_probabilistic_above", forecast_type+"_country_"+country_iso+"_probabilistic_normal",
                   forecast_type+"_country_"+country_iso+"_probabilistic_below", forecast_type+"_country_"+country_iso+"_deterministic"]

stores_aclimate_dominant = [forecast_type+"_country_"+country_iso+"_dominant"]

# Connecting
geoclient = GeoserverClient(geo_url, geo_user, geo_pwd)

# uploading above, below, normal and deterministic
for current_store in stores_aclimate:
    
    try:
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
    except Exception as e:
        print(str(e))
        continue


# uploading dominants
for current_store in stores_aclimate_dominant:

    current_layer = forecast_type+"_"+current_store.split("_")[-1]
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

# Deletes .tif files
geoclient.delete_folder_content(os.path.join(folder_layers, "above"))
geoclient.delete_folder_content(os.path.join(folder_layers, "below"))
geoclient.delete_folder_content(os.path.join(folder_layers, "normal"))
geoclient.delete_folder_content(os.path.join(folder_layers, "deterministic"))
geoclient.delete_folder_content(os.path.join(folder_layers, "seasonal_dominant"))
geoclient.delete_folder_content(os.path.join(folder_layers, "subseasonal_dominant"))

print("Process completed")
