
# ============================================================
# does not work yet
# HYDROBASINS LEVEL 5 - BUFFERED DEM TILE GENERATION
# Yukon regional hydrological processing workflow
# ============================================================

import geopandas as gpd
import rasterio
import numpy as np
import pandas as pd

from rasterio.mask import mask
from pathlib import Path


# ------------------------------------------------------------
# 1. PROJECT SETTINGS
# ------------------------------------------------------------

project_dir = Path(r"H:\cmfmacne\basins\code")

source_dir = project_dir / "source_data"

dem_file = source_dir / "regional_dem.tif"

basin_file = source_dir / "hydrobasins_level5.gpkg"

# If the GeoPackage has multiple layers, specify the layer name.
# None reads the default/first layer.
basin_layer = None

output_dir = project_dir / "basin_tiles"

polygon_dir = output_dir / "polygons"
dem_dir = output_dir / "dem_tiles"

polygon_dir.mkdir(parents=True, exist_ok=True)
dem_dir.mkdir(parents=True, exist_ok=True)


# ------------------------------------------------------------
# 2. PROCESSING PARAMETERS
# ------------------------------------------------------------

buffer_distance = 40000  # metres

target_crs = "EPSG:3979"

id_field = "HYBAS_ID"

# Initial technical test:
# Set to 5 to generate five tiles.
# Change to None to process every polygon.
max_tiles = 5

# Alternatively, specify particular HYBAS_ID values.
# Leave as None to use the first max_tiles polygons.
selected_hybas_ids = None


# ------------------------------------------------------------
# 3. CHECK INPUT FILES
# ------------------------------------------------------------

if not dem_file.exists():
    raise FileNotFoundError(f"DEM not found: {dem_file}")

if not basin_file.exists():
    raise FileNotFoundError(f"HydroBASINS file not found: {basin_file}")

print("Input files found.")


# ------------------------------------------------------------
# 4. READ HYDROBASINS POLYGONS
# ------------------------------------------------------------

if basin_layer is None:
    basins = gpd.read_file(basin_file)
else:
    basins = gpd.read_file(
        basin_file,
        layer=basin_layer
    )

print(f"HydroBASINS polygons loaded: {len(basins)}")

if id_field not in basins.columns:
    raise ValueError(
        f"Required field {id_field} not found."
    )

if basins.crs is None:
    raise ValueError("HydroBASINS polygons have no CRS.")

# ------------------------------------------------------------
# 5. CHECK DEM CRS AND RESOLUTION
# ------------------------------------------------------------

with rasterio.open(dem_file) as src:

    if src.crs is None:
        raise ValueError("DEM has no CRS.")

    dem_crs = src.crs

    print("\nDEM information:")
    print(f"CRS: {src.crs}")
    print(f"Resolution: {src.res}")
    print(f"Dimensions: {src.width} x {src.height}")
    print(f"Bounds: {src.bounds}")
    print(f"Data type: {src.dtypes[0]}")

    if src.count != 1:
        raise ValueError(
            "This script expects a single-band DEM."
        )

    if str(src.crs) != target_crs:
        raise ValueError(
            f"Expected {target_crs}, found {src.crs}. "
            "Reproject the source data before proceeding."
        )

# Reproject polygons if necessary
if basins.crs != dem_crs:
    print("\nReprojecting polygons to DEM CRS...")
    basins = basins.to_crs(dem_crs)


# ------------------------------------------------------------
# 6. CLEAN AND IDENTIFY POLYGONS
# ------------------------------------------------------------

basins = basins[
    basins.geometry.notnull()
    & ~basins.geometry.is_empty
].copy()

# Repair invalid geometries where necessary
invalid = ~basins.geometry.is_valid

if invalid.any():
    print(f"Repairing {invalid.sum()} invalid geometries...")
    basins.loc[invalid, "geometry"] = (
        basins.loc[invalid, "geometry"].buffer(0)
    )

# Create consistent integer IDs
basins["tile_id"] = basins[id_field].apply(
    lambda x: str(int(round(float(x))))
)

if basins["tile_id"].duplicated().any():
    raise ValueError("Duplicate HYBAS_ID values detected.")

# Sort consistently
basins = basins.sort_values("tile_id").reset_index(drop=True)


# ------------------------------------------------------------
# 7. SELECT POLYGONS FOR PROCESSING
# ------------------------------------------------------------

if selected_hybas_ids is not None:

    selected_ids = [
        str(int(round(float(x))))
        for x in selected_hybas_ids
    ]

    basins = basins[
        basins["tile_id"].isin(selected_ids)
    ].copy()

    missing_ids = set(selected_ids) - set(basins["tile_id"])

    if missing_ids:
        raise ValueError(
            f"Selected basin IDs not found: {missing_ids}"
        )

elif max_tiles is not None:

    basins = basins.head(max_tiles).copy()

print(f"\nTiles selected for processing: {len(basins)}")


# ------------------------------------------------------------
# 8. CREATE BUFFERED POLYGONS
# ------------------------------------------------------------

print("\nBuffering HydroBASINS polygons...")

buffered = basins.copy()

buffered.geometry = buffered.geometry.buffer(
    buffer_distance
)

buffered["buffer_m"] = buffer_distance

buffered["original_area_km2"] = (
    basins.geometry.area / 1_000_000
).values

buffered["buffered_area_km2"] = (
    buffered.geometry.area / 1_000_000
)

print("Buffering complete.")


# ------------------------------------------------------------
# 9. SAVE ORIGINAL AND BUFFERED POLYGONS
# ------------------------------------------------------------

polygon_file = (
    polygon_dir /
    f"hydrobasins_level5_buffered_{int(buffer_distance/1000)}km.gpkg"
)

if polygon_file.exists():
    polygon_file.unlink()

basins.to_file(
    polygon_file,
    layer="original_basins",
    driver="GPKG"
)

buffered.to_file(
    polygon_file,
    layer="buffered_basins",
    driver="GPKG",
    mode="a"
)

print(f"\nSaved polygon GeoPackage: {polygon_file.name}")


# ------------------------------------------------------------
# 10. CLIP DEM FOR EACH BUFFERED BASIN
# ------------------------------------------------------------

inventory = []

with rasterio.open(dem_file) as src:

    source_bounds = src.bounds

    from shapely.geometry import box

    dem_extent = box(
        source_bounds.left,
        source_bounds.bottom,
        source_bounds.right,
        source_bounds.top
    )

    for index, (_, basin) in enumerate(
        buffered.iterrows(),
        start=1
    ):

        tile_id = basin["tile_id"]

        tile_name = f"hb5_{tile_id}_dem.tif"

        tile_file = dem_dir / tile_name

        geometry = basin.geometry

        print("\n" + "=" * 55)
        print(f"Processing tile {index}/{len(buffered)}")
        print(f"HYBAS_ID: {tile_id}")

        # ----------------------------------------------------
        # Check whether polygon intersects DEM extent
        # ----------------------------------------------------

        if not geometry.intersects(dem_extent):

            print("WARNING: Basin does not intersect DEM.")

            inventory.append({
                "HYBAS_ID": tile_id,
                "status": "outside_dem",
                "tile_file": None
            })

            continue

        # ----------------------------------------------------
        # Clip DEM to buffered polygon
        # ----------------------------------------------------

        try:

            clipped, clipped_transform = mask(
                src,
                [geometry.__geo_interface__],
                crop=True,
                filled=False,
                all_touched=False
            )

        except ValueError as error:

            print(f"Clipping failed: {error}")

            inventory.append({
                "HYBAS_ID": tile_id,
                "status": "clip_failed",
                "tile_file": None
            })

            continue

        # ----------------------------------------------------
        # Convert masked cells to Float32 NoData
        # ----------------------------------------------------

        nodata_value = -9999.0

        clipped_data = np.ma.filled(
            clipped,
            nodata_value
        ).astype("float32")

        # ----------------------------------------------------
        # Update output raster profile
        # ----------------------------------------------------

        profile = src.profile.copy()

        profile.update(
            driver="GTiff",
            height=clipped_data.shape[1],
            width=clipped_data.shape[2],
            transform=clipped_transform,
            crs=src.crs,
            count=1,
            dtype="float32",
            nodata=nodata_value,
            compress="deflate",
            predictor=3,
            tiled=True,
            blocxsize=256,
            blockysize=256,
            BIGTIFF="IF_SAFER"
        )

        # ----------------------------------------------------
        # Write individual DEM tile
        # ----------------------------------------------------

        with rasterio.open(
            tile_file,
            "w",
            **profile
        ) as dst:

            dst.write(clipped_data)

        # ----------------------------------------------------
        # Calculate tile statistics
        # ----------------------------------------------------

        valid_cells = np.count_nonzero(
            clipped_data[0] != nodata_value
        )

        valid_area_km2 = (
            valid_cells * abs(
                clipped_transform.a * clipped_transform.e
                - clipped_transform.b * clipped_transform.d
            ) / 1_000_000
        )

        tile_size_mb = tile_file.stat().st_size / 1_000_000

        print(f"Saved: {tile_name}")
        print(
            f"Raster dimensions: "
            f"{clipped_data.shape[2]} x {clipped_data.shape[1]}"
        )
        print(f"Valid area: {valid_area_km2:,.1f} km2")
        print(f"File size: {tile_size_mb:.1f} MB")

        inventory.append({
            "HYBAS_ID": tile_id,
            "status": "complete",
            "original_area_km2": basin["original_area_km2"],
            "buffered_area_km2": basin["buffered_area_km2"],
            "valid_area_km2": valid_area_km2,
            "width": clipped_data.shape[2],
            "height": clipped_data.shape[1],
            "resolution_x": clipped_transform.a,
            "resolution_y": abs(clipped_transform.e),
            "buffer_m": buffer_distance,
            "tile_file": str(tile_file),
            "file_size_mb": round(tile_size_mb, 2)
        })


# ------------------------------------------------------------
# 11. SAVE PROCESSING INVENTORY
# ------------------------------------------------------------

inventory_df = pd.DataFrame(inventory)

inventory_file = output_dir / "tile_inventory.csv"

inventory_df.to_csv(
    inventory_file,
    index=False
)


# ------------------------------------------------------------
# 12. SUMMARY
# ------------------------------------------------------------

print("\n" + "=" * 55)
print("TILE GENERATION COMPLETE")

print(f"Tiles processed: {len(inventory_df)}")

if not inventory_df.empty:
    print(
        inventory_df["status"].value_counts()
    )

print(f"\nPolygon GeoPackage: {polygon_file}")
print(f"DEM tile directory: {dem_dir}")
print(f"Inventory: {inventory_file}")
