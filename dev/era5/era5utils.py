import cdsapi
from tqdm import tqdm
from pathlib import Path
import warnings
import os
import netCDF4
import numpy as np
import xarray as xr
import pandas as pd
from netCDF4 import num2date
import rasterio as rio
import geopandas as gpd
import zipfile

warnings.filterwarnings("ignore")


data_dir = Path(".data")

from metadata import param_md

def download_from_cds(
        years = range(2000,2024),
        months = range(1,13),
        days = range(1, 32),
        times = ["00:00"],
        area = [72, -150, 50, -120],
        data_dir = Path(".data"),
):
    """
    Download ERA5 snow water equivalent data from the Copernicus Climate Data Store (CDS).
    The data is downloaded in zip format and saved in the specified directory.
    """

    for year in tqdm(years):
        for month in months:
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
                    "day": [str(day).zfill(2) for day in days],
                    "time": times,
                    "data_format": "netcdf",
                    "download_format": "zip",
                    "area": [72, -150, 55, -120]
                }

                client = cdsapi.Client(verify=False, key="5815cfa9-2642-46bd-9a7f-9ac2099b32f4")
                client.retrieve(dataset, request, target=filename)


def concatenate_to_xda(data_dir, param, freq=None, resampling_function="mean"):
    RESAMPLING_FUNCTIONS = ["mean", "sum", "max", "min", "median"]
    if resampling_function not in RESAMPLING_FUNCTIONS:
        raise ValueError(f"Resampling function {resampling_function} not supported. Supported functions are: {RESAMPLING_FUNCTIONS}")
    
    FREQUENCIES = {"hourly": "1H", "daily": "1D", "monthly": "1M", "yearly": "1Y"}
    if freq not in FREQUENCIES.keys() and freq is not None:
        raise ValueError(f"Frequency {freq} not supported. Supported frequencies are: {FREQUENCIES.keys()}")
    
    data_arrays = []

    if param not in param_md.loc[:,"shortName"].values:
        raise ValueError(f"Parameter {param} not found in metadata; update metadata (defs/metadata) or fix the parameter name")
    if param in param_md.loc[:,"name"].values:
        raise ValueError(f"Parameter {param} is a long name; use the short name instead")

    units = param_md.loc[param_md["shortName"] == param, "units"].values[0]
    long_name = param_md.loc[param_md["shortName"] == param, "name"].values[0]

    for dir in (data_dir).iterdir():
        filename = dir / "data_0.nc"
        # Open the extracted NetCDF file
        with netCDF4.Dataset(filename, "r") as netcdf_file:            
            # extract data for called parameter
            data = netcdf_file.variables[param][:]

            # extract and fix datetime array
            time_units = netcdf_file.variables["valid_time"].units
            time_calendar = netcdf_file.variables["valid_time"].calendar if hasattr(netcdf_file.variables["valid_time"], "calendar") else "standard"
            valid_time_datetime = num2date(netcdf_file.variables["valid_time"][:], units=time_units, calendar=time_calendar)

            # Create an xarray DataArray
            xda = xr.DataArray(
                data.astype(np.float32),  # Specify data type to conserve RAM
                coords=[
                valid_time_datetime,
                netcdf_file.variables["latitude"][:],
                netcdf_file.variables["longitude"][:]
                ], 
                dims=["time", "latitude", "longitude"], 
                name=param,
                attrs=param_md.loc[param_md["shortName"] == param, :].to_dict(),
            )    
            # Append to the list
            if freq is not None:
                xda = xda.resample(time=FREQUENCIES[freq]).reduce(getattr(np, resampling_function)) 
            
            #xda["time"] = pd.to_datetime(xda["time"].values).tz_localize("UTC")
            data_arrays.append(xda)

    # Combine all DataArrays into a single xarray DataArray
    xda = xr.concat(data_arrays, dim="time")
    # this code is very ugly, but 'resample' really hates numpy datetimes, and for reason this convoluted timetype is the only one that works
    # so we convert to ISO format, then to pandas timestamps, then to UTC
    xda["time"] = [pd.to_datetime(x.isoformat()) for x in xda["time"].values]
    #xda = xda.assign_coords(time=pd.to_datetime(xda.time.values))
    #xda["time"] = pd.to_datetime(xda['time'].values).tz_localize('UTC')

    if param == "sd":
        xda.values = xda.values * 1000 # Convert from m to mm
        xda.attrs["units"] = "mm"
    return xda



def zip_to_nc(source_dir:str|Path, destination_dir:str|Path) -> bool:
    """
    Unzips all zip files in the source directory to the destination directory.
    """
    # Create the destination directory if it doesn't exist
    if not isinstance(source_dir, Path):
        source_dir = Path(source_dir)
    if not isinstance(destination_dir, Path):
        destination_dir = Path(destination_dir)

    destination_dir.mkdir(exist_ok=True)
    # Iterate through all files in the source directory
    for filename in tqdm(source_dir.iterdir(), desc="Unzipping files"):
        if filename.suffix == ".zip":
            with zipfile.ZipFile(filename, 'r') as zip_ref:
                zip_ref.extractall(destination_dir / filename.stem)
    return