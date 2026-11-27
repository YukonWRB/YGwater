
# ============================================================
# BUFFERED DEM TILE GENERATION
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
from rasterio.features import geometry_mask, geometry_window
from rasterio.windows import Window
from rasterio.errors import WindowError


# ------------------------------------------------------------
# 1. PROJECT SETTINGS
# ------------------------------------------------------------

project_dir = Path(r"H:\cmfmacne\basins\code")

source_dir = project_dir / "source_data"

dem_file = source_dir / "regional_dem.tif"

dem_tiles = source_dir / "dem_tiles_3979.gpkg"

# If the GeoPackage has multiple layers, specify the layer name.
# None reads the default/first layer.
basin_layer = None

output_dir = project_dir / "basin_tiles_2"

polygon_dir = output_dir / "polygons_2"
dem_dir = output_dir / "dem_tiles_2"

polygon_dir.mkdir(parents=True, exist_ok=True)
dem_dir.mkdir(parents=True, exist_ok=True)


# ------------------------------------------------------------
# 2. PROCESSING PARAMETERS
# ------------------------------------------------------------

buffer_distance = 10000  # metres

target_crs = "EPSG:3979"

id_field = "id"

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

if not dem_tiles.exists():
    raise FileNotFoundError(f"Tile file not found: {dem_tiles}")

print("Input files found.")


# ------------------------------------------------------------
# 4. READ TILE POLYGONS
# ------------------------------------------------------------

if basin_layer is None:
    basins = gpd.read_file(dem_tiles)
else:
    basins = gpd.read_file(
        dem_tiles,
        layer=basin_layer
    )

print(f"Tile polygons loaded: {len(basins)}")

if id_field not in basins.columns:
    raise ValueError(
        f"Required field {id_field} not found."
    )

if basins.crs is None:
    raise ValueError("Tile polygons have no CRS.")

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
    raise ValueError("Duplicate id values detected.")

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
            f"Selected tile IDs not found: {missing_ids}"
        )

print(f"\nCandidate tiles for screening: {len(basins)}")


# ------------------------------------------------------------
# 8. CREATE BUFFERED POLYGONS
# ------------------------------------------------------------

print("\nBuffering tile polygons...")

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
# 8C. SCREEN BASINS FOR VALID DEM DATA (CHUNKED)
# ------------------------------------------------------------

print("\nScreening basins for valid DEM data...")

valid_tile_ids = []

# Size of each DEM chunk (pixels)
chunk_size = 1024

with rasterio.open(dem_file) as src:

    for index, (_, basin) in enumerate(
        buffered.iterrows(),
        start=1
    ):

        tile_id = basin["tile_id"]

        print(
            f"\nChecking basin {index}/{len(buffered)}: {tile_id}"
        )

        geometry = basin.geometry

        has_valid_data = False

        try:

            # Find the raster window covering the basin buffer
            window = geometry_window(
                src,
                [geometry.__geo_interface__]
            )

            # Restrict the window to the DEM dimensions
            window = window.intersection(
                Window(0, 0, src.width, src.height)
            )

            # Read the window in small chunks
            for row_off in range(
                int(window.row_off),
                int(window.row_off + window.height),
                chunk_size
            ):

                for col_off in range(
                    int(window.col_off),
                    int(window.col_off + window.width),
                    chunk_size
                ):

                    # Calculate the dimensions of this chunk
                    chunk_window = Window(
                        col_off,
                        row_off,
                        min(
                            chunk_size,
                            int(window.col_off + window.width) - col_off
                        ),
                        min(
                            chunk_size,
                            int(window.row_off + window.height) - row_off
                        )
                    )

                    # Read the DEM validity mask for this chunk
                    validity = src.read_masks(
                        1,
                        window=chunk_window
                    )

                    # Identify pixels inside the basin buffer
                    inside_basin = geometry_mask(
                        [geometry.__geo_interface__],
                        out_shape=validity.shape,
                        transform=src.window_transform(chunk_window),
                        invert=True,
                        all_touched=True
                    )

                    # Check for at least one valid DEM pixel
                    if np.any(
                        (validity > 0) & inside_basin
                    ):

                        has_valid_data = True
                        break

                # Stop checking chunks once valid data is found
                if has_valid_data:
                    break

        except (ValueError, WindowError):
            has_valid_data = False

        # Retain or skip the basin
        if has_valid_data:

            valid_tile_ids.append(tile_id)

            print("Valid DEM data found. Basin retained.")

        else:

            print("No valid DEM data found. Basin skipped.")

        # Stop once we have enough qualifying basins
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
    f"dem_tiles_buffered_{int(buffer_distance/1000)}km.gpkg"
)

if polygon_file.exists():
    polygon_file.unlink()

basins.to_file(
    polygon_file,
    layer="original_tiles",
    driver="GPKG"
)

buffered.to_file(
    polygon_file,
    layer="buffered_tiles",
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

        tile_name = f"tile_{tile_id}_dem.tif"

        tile_file = dem_dir / tile_name

        geometry = basin.geometry

        print("\n" + "=" * 55)
        print(f"Processing tile {index}/{len(buffered)}")
        print(f"TILE_ID: {tile_id}")

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
                "TILE_ID": tile_id,
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
