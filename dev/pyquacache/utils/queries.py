import os
import pandas as pd
import geopandas as gpd
from sqlalchemy import create_engine, text
from shapely import wkt
from pathlib import Path
import sys
import warnings
import numpy as np
import xarray as xr
import geopandas as gpd

from ..defs.config import db_url_prod as db_url
from ..defs import DEFAULT_CRS


def fetch_table(table_name, db_url=db_url):
    engine = create_engine(db_url)
    with engine.connect() as connection:
        table_result = connection.execute(
            text(f"SELECT * FROM {table_name}")
        )
    return pd.DataFrame(table_result)

def fetch_parameters(db_url=db_url):
    return fetch_table(table_name="parameters", db_url=db_url).set_index("parameter_id")

def fetch_locations(db_url=db_url):
    df = fetch_table(table_name="locations", db_url=db_url).set_index("location_id")
    return gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.longitude, df.latitude), crs=DEFAULT_CRS)

def invert_dict(d):
    return {v: k for k, v in d.items()}

def fetch_drainage_basin_boundaries(db_url=db_url):
    """
    Downloads and processes drainage basin boundary data from a database.

    This function connects to a database using a SQLAlchemy engine, retrieves
    geometry and attribute data for drainage basins, and constructs a 
    GeoPandas GeoDataFrame containing the data. The resulting GeoDataFrame 
    is indexed by the "feature_name" column and uses the EPSG:4269 coordinate 
    reference system.

    Returns:
        geopandas.GeoDataFrame: A GeoDataFrame containing the drainage basin 
        boundaries with their associated attributes and geometries.

    Raises:
        sqlalchemy.exc.SQLAlchemyError: If there is an issue with the database 
        connection or query execution.
        ValueError: If the geometry data cannot be parsed or processed correctly.
    """
    engine = create_engine(db_url)

    basins = {}
    res = []
    polys = []

    # Test the connection by listing tables
    with engine.connect() as connection:
        geom_results = connection.execute(
            text("SELECT ST_AsText(geom) as geom FROM vectors WHERE layer_name = 'Drainage basins';")
        )
        results = connection.execute(
            text("SELECT * FROM vectors WHERE layer_name = 'Drainage basins';")
        )

        for geom_row, row in zip(geom_results, results):
            polys.append(wkt.loads(geom_row)[0])
            res.append(row)

    basins = gpd.GeoDataFrame(res, geometry=polys, crs="epsg:4269")
    basins.set_index("feature_name", inplace=True)
    return basins

def upsample_xda(xda, factor=2):
    """
    Upsample the xarray DataArray by a given factor using linear interpolation.

    Parameters:
        xda (xarray.DataArray): The input DataArray to be upsampled.
        factor (int): The factor by which to upsample the latitude and longitude dimensions.

    Returns:
        xarray.DataArray: The upsampled DataArray.

    Raises:
        TypeError: If the input is not an xarray DataArray.
        ValueError: If the DataArray does not have 'latitude' and 'longitude' dimensions.
    """
    # Check if the input is an xarray DataArray
    if not isinstance(xda, xr.DataArray):
        raise TypeError("Input must be an xarray DataArray.")

    # Check if the DataArray has latitude and longitude dimensions
    if "latitude" not in xda.dims or "longitude" not in xda.dims:
        raise ValueError("DataArray must have 'latitude' and 'longitude' dimensions.")

    new_lat = np.linspace(xda.latitude.min(), xda.latitude.max(), xda.sizes["latitude"] * factor)
    new_lon = np.linspace(xda.longitude.min(), xda.longitude.max(), xda.sizes["longitude"] * factor)

    # Perform the interpolation
    xda_upsampled = xda.interp(latitude=new_lat, longitude=new_lon, method="linear")
    return xda_upsampled
