

import rasterio as rio
from rasterio.features import geometry_mask
from rasterio.transform import from_bounds

import geopandas as gpd
import xarray as xr
from pathlib import Path

def xda_to_transform(xda):
    """
    Convert xarray DataArray to rasterio transform and shape.
    """
    # Get the coordinates and dimensions
    lon = xda.longitude.values
    lat = xda.latitude.values

    # Create the transform
    transform = from_bounds(lon.min(), lat.max(), lon.max(), lat.min(), len(lon), len(lat))

    # Get the shape of the data
    shape = (len(lat), len(lon))

    return transform, shape

def poly_to_mask(reference_data:str|Path|xr.DataArray, poly:gpd.GeoDataFrame) -> dict:
    """
    Convert polygons to masks for a raster.
    """
    if isinstance(reference_data, str):
        reference_data = Path(reference_data)

    if isinstance(reference_data, xr.DataArray):
        transform, shape = xda_to_transform(reference_data)

    elif isinstance(reference_data, Path):
        if reference_data.suffix not in [".nc", ".tif", ".tiff"]:
            raise ValueError("Unsupported file format. Only NetCDF (.nc) and GeoTIFF (.tif/.tiff) are supported.")
        with rio.open(reference_data) as src:
            transform = src.transform
            shape = src.shape
    else:
        raise ValueError("Unsupported data type. Must be an xarray.DataArray or a file path.")

    # Create masks for each polygon in the GeoDataFrame
    masks = {}
    for key, geom in poly["geometry"].items():
        masks[key] = geometry_mask([geom], transform=transform, invert=True, all_touched=False, out_shape=shape)

    return masks