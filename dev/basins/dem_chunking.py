
# ============================================================
# HYDROBASINS LEVEL 5 - BUFFERED DEM TILE GENERATION
# Yukon regional hydrological processing workflow
# ============================================================

import geopandas as gpd
import rasterio
import numpy as np
import pandas as pd
from rasterio.windows import from_bounds, Window
from rasterio.features import geometry_mask
from rasterio.enums import Resampling
from affine import Affine
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

buffer_distance = 30000  # metres

target_crs = "EPSG:3979"

id_field = "HYBAS_ID"

# Maximum  number of basins with valid DEM data to process.print
# Set to None to process all qualifying basins.
max_tiles = 5

# Alternatively, specify particular HYBAS_ID values.
# Leave as None to screen all available basins.
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

# ------------------------------------------------------------
# 5. CHECK DEM CRS, RESOLUTION AND EXTENT
# ------------------------------------------------------------

from shapely.geometry import box

with rasterio.open(dem_file) as src:

    if src.crs is None:
        raise ValueError("DEM has no CRS.")

    dem_crs = src.crs
    dem_bounds = src.bounds

    dem_extent = box(
        dem_bounds.left,
        dem_bounds.bottom,
        dem_bounds.right,
        dem_bounds.top
    )

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

print(f"\nCandidate basins for screening: {len(basins)}")


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
# 8B. CALCULATE DEM OVERLAP
# ------------------------------------------------------------

print("\nCalculating DEM overlap for each basin...")

# Calculate the area of each buffered polygon
buffered["buffered_area_km2"] = (
    buffered.geometry.area / 1_000_000
)

# Calculate the area of each buffer intersecting the DEM extent
buffered["dem_overlap_area_km2"] = (
    buffered.geometry.intersection(dem_extent).area
    / 1_000_000
)

# Calculate percentage of buffered area covered by DEM
buffered["dem_coverage_pct"] = (
    buffered["dem_overlap_area_km2"]
    / buffered["buffered_area_km2"]
) * 100

# Identify basins with any overlap
buffered["intersects_dem"] = (
    buffered["dem_overlap_area_km2"] > 0
)

# Print summary
print("\nDEM overlap summary:")

print(f"Total basins: {len(buffered)}")

print(
    f"Intersecting DEM: "
    f"{buffered['intersects_dem'].sum()}"
)

print(
    f"Outside DEM: "
    f"{(~buffered['intersects_dem']).sum()}"
)

print(
    f"Mean DEM coverage: "
    f"{buffered['dem_coverage_pct'].mean():.1f}%"
)

# Keep only basins with DEM overlap
buffered = buffered[
    buffered["intersects_dem"]
].copy()

print(
    f"\nBasins retained for DEM processing: {len(buffered)}"
)

# ------------------------------------------------------------
# 8C. SCREEN BASINS FOR VALID DEM DATA (LOW-RESOLUTION)
# ------------------------------------------------------------

print("\nScreening basins for valid DEM data...")

valid_tile_ids = []

with rasterio.open(dem_file) as src:

    for index, (_, basin) in enumerate(
        buffered.iterrows(),
        start=1
    ):

        tile_id = basin["tile_id"]

        print(f"\nChecking basin {index}/{len(buffered)}: {tile_id}")

        # Find the part of the DEM covered by the basin buffer
        window = from_bounds(
            *basin.geometry.bounds,
            transform=src.transform
        )

        # Restrict the window to the actual DEM dimensions
        window = window.intersection(
            Window(0, 0, src.width, src.height)
        )

        # Read a much smaller version of the DEM validity mask
        sample_width = min(512, max(1, int(np.ceil(window.width))))
        sample_height = min(512, max(1, int(np.ceil(window.height))))

        sampled_mask = src.read_masks(
            1,
            window=window,
            out_shape=(sample_height, sample_width),
            resampling=Resampling.max
        )

        # Calculate the transform for the smaller sampled raster
        sample_transform = (
            src.window_transform(window)
            * Affine.scale(
                window.width / sample_width,
                window.height / sample_height
            )
        )

        # Identify which sampled pixels overlap the basin buffer
        inside_basin = geometry_mask(
            [basin.geometry.__geo_interface__],
            out_shape=(sample_height, sample_width),
            transform=sample_transform,
            invert=True,
            all_touched=True
        )

        # Check for valid DEM data inside the basin buffer
        has_valid_data = np.any(
            (sampled_mask > 0) & inside_basin
        )

        if has_valid_data:
            valid_tile_ids.append(tile_id)
            print("Valid DEM data found. Basin retained.")
        else:
            print("No valid DEM data found. Basin skipped.")

        # Stop once we have enough candidate basins
        if max_tiles is not None and len(valid_tile_ids) >= max_tiles:
            break

# Keep only basins with valid DEM data
buffered = buffered[
    buffered["tile_id"].isin(valid_tile_ids)
].copy()

basins = basins[
    basins["tile_id"].isin(valid_tile_ids)
].copy()

print("\nDEM screening complete.")
print(f"Candidate basins retained: {len(buffered)}")

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
            blockxsize=256,
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
