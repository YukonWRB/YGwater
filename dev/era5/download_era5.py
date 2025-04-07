import cdsapi
from tqdm import tqdm
from pathlib import Path
import warnings
import os

warnings.filterwarnings("ignore")
os.chdir("dev/era5")
data_dir = Path(".data")

for year in tqdm(range(2000,2024)):
    for month in range(1,13):
        filename = data_dir / "raw" / f"era5_reanalysis_snow_{year}-{str(month).zfill(2)}.zip"
        if filename.exists():
            print(f"File {filename} already exists. Skipping download.")
            continue
        else:
            dataset = "reanalysis-era5-land" #dataset = "reanalysis-era5-single-levels"
            request = {
                #"product_type": ["reanalysis"],
                "variable": [
                    "snow_depth_water_equivalent",
                    #"convective_snowfall",
                    #"convective_snowfall_rate_water_equivalent",
                    #"large_scale_snowfall_rate_water_equivalent",
                    #"large_scale_snowfall",
                    #"snow_albedo",
                    #"snow_density", #
                    #"snow_depth", #
                    #"snow_evaporation",
                    #"snowfall",
                    #"snowmelt",
                    #"temperature_of_snow_layer",
                    #"total_column_snow_water" #
                ],
                "year": [str(year)],
                "month": [str(month).zfill(2)],
                "day": [str(d).zfill(2) for d in range(1, 32)],
                "time": [
                    "00:00",#["{str(h).zfill(2)}:00" for h in range(1, 24)]
                ],
                "data_format": "netcdf",
                "download_format": "zip",
                "area": [72, -150, 50, -120]
            }

            client = cdsapi.Client(verify=False)
            client.retrieve(dataset, request, target=filename)