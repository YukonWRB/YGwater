
# ============================================================
# Generates candidate pour points
# AUTOMATED BASIN OUTLET CANDIDATE IDENTIFICATION
# Yukon basin delineation experiment
# ============================================================

import geopandas as gpd
import rasterio
import numpy as np
import pandas as pd

from pathlib import Path
from rasterio.features import geometry_mask
from shapely.geometry import Point


# ------------------------------------------------------------
# 1. PROJECT SETTINGS
# ------------------------------------------------------------

project_dir = Path(r"H:\cmfmacne\basins\code")

basin_file = project_dir / "test_basins.gpkg"

stream_dir = project_dir / "stream_networks"

output_dir = project_dir / "outlet_analysis"
output_dir.mkdir(exist_ok=True)

output_file = output_dir / "outlet_candidates_2500.gpkg"


# ------------------------------------------------------------
# 2. BASIN CONFIGURATION
# ------------------------------------------------------------

basins_config = {
    "dtm_1": {
        "name": "steep",
        "hybas_id": 8050259520
    },
    "dtm_2": {
        "name": "flat",
        "hybas_id": 8050112340
    },
    "dtm_3": {
        "name": "upland",
        "hybas_id": 8050219130
    }
}


# ------------------------------------------------------------
# 3. OUTLET SEARCH PARAMETERS
# ------------------------------------------------------------

stream_threshold = 2500

# Distance from original polygon boundary (metres)
boundary_tolerance = 300

# Minimum spacing between candidate points (metres)
candidate_spacing = 1000

# Maximum candidates retained per basin
max_candidates = 20

# Expected coordinate system
target_crs = "EPSG:3979"


# ------------------------------------------------------------
# 4. READ ORIGINAL BASIN POLYGONS
# ------------------------------------------------------------

basins = gpd.read_file(basin_file, layer="hybas_reprojected")

basins = basins.to_crs(target_crs)

# Convert HYBAS_ID to numeric for matching
basins["_hybas_numeric"] = pd.to_numeric(
    basins["HYBAS_ID"],
    errors="coerce"
)

print("Original basin polygons loaded:")
print(f"Number of polygons: {len(basins)}")
print(f"CRS: {basins.crs}")


# ------------------------------------------------------------
# 5. READ MANUAL POUR POINTS
# ------------------------------------------------------------

manual_points_list = []

for dem_name, config in basins_config.items():

    pour_number = dem_name.replace("dtm_", "pour")

    pour_file = project_dir / f"{pour_number}.gpkg"

    if pour_file.exists():

        points = gpd.read_file(pour_file)

        points = points.to_crs(target_crs)

        points["basin"] = config["name"]
        points["source"] = "manual"
        points["manual_id"] = pour_number

        manual_points_list.append(points)

    else:
        print(f"WARNING: Missing manual point file: {pour_file}")


if manual_points_list:
    manual_points = gpd.GeoDataFrame(
        pd.concat(manual_points_list, ignore_index=True),
        crs=target_crs
    )
else:
    raise FileNotFoundError("No manual pour-point files found.")


# ------------------------------------------------------------
# 6. PROCESS EACH BASIN
# ------------------------------------------------------------

candidate_records = []
basin_records = []

for dem_name, config in basins_config.items():

    basin_name = config["name"]
    target_id = config["hybas_id"]

    print("\n" + "=" * 55)
    print(f"Processing basin: {basin_name}")
    print(f"DEM: {dem_name}")
    print(f"Target HYBAS_ID: {target_id}")

    # --------------------------------------------------------
    # Find matching HydroBASINS polygon
    # --------------------------------------------------------

    differences = (
        basins["_hybas_numeric"] - target_id
    ).abs()

    matches = basins.loc[differences <= 1500].copy()

    if len(matches) != 1:
        raise ValueError(
            f"Expected one polygon for {target_id}, "
            f"but found {len(matches)}. "
            "Check HYBAS_ID matching."
        )

    basin_row = matches.iloc[0]
    basin_geometry = basin_row.geometry

    print(f"Matched stored HYBAS_ID: {basin_row['HYBAS_ID']}")

    basin_output = basin_row.drop(
        labels=["geometry", "_hybas_numeric"]
    ).to_dict()

    basin_output["basin"] = basin_name
    basin_output["target_id"] = str(target_id)
    basin_output["geometry"] = basin_geometry

    basin_records.append(basin_output)

    # --------------------------------------------------------
    # Identify input rasters
    # --------------------------------------------------------

    accum_file = project_dir / f"{dem_name}_flow_accum.tif"

    stream_file = (
        stream_dir /
        f"{dem_name}_streams_{stream_threshold}.tif"
    )

    if not accum_file.exists():
        raise FileNotFoundError(accum_file)

    if not stream_file.exists():
        raise FileNotFoundError(stream_file)

    print(f"Flow accumulation: {accum_file.name}")
    print(f"Stream network: {stream_file.name}")

    # --------------------------------------------------------
    # Read rasters and identify candidate cells
    # --------------------------------------------------------

    with rasterio.open(accum_file) as accum_src, \
         rasterio.open(stream_file) as stream_src:

        # Check raster alignment
        if accum_src.shape != stream_src.shape:
            raise ValueError("Raster dimensions do not match.")

        if accum_src.transform != stream_src.transform:
            raise ValueError("Raster transforms do not match.")

        if accum_src.crs != stream_src.crs:
            raise ValueError("Raster coordinate systems do not match.")

        if str(accum_src.crs) != target_crs:
            raise ValueError(
                f"Unexpected raster CRS: {accum_src.crs}"
            )

        transform = accum_src.transform

        # Read accumulation and stream data
        accum_data = accum_src.read(1, masked=True)

        stream_data = stream_src.read(1, masked=True)

        accum_values = accum_data.astype("float64").filled(np.nan)

        stream_values = stream_data.filled(0)

        accum_valid = (
            ~np.ma.getmaskarray(accum_data)
            & np.isfinite(accum_values)
        )

        stream_valid = ~np.ma.getmaskarray(stream_data)

        # ----------------------------------------------------
        # Create boundary search area
        # ----------------------------------------------------

        search_geometry = basin_geometry.boundary.buffer(
            boundary_tolerance
        )

        boundary_mask = geometry_mask(
            [search_geometry],
            out_shape=accum_src.shape,
            transform=transform,
            invert=True,
            all_touched=True
        )

        # ----------------------------------------------------
        # Identify stream cells near basin boundary
        # ----------------------------------------------------

        candidate_mask = (
            boundary_mask
            & accum_valid
            & stream_valid
            & (stream_values > 0)
        )

        rows, cols = np.where(candidate_mask)

        print(f"Candidate stream cells found: {len(rows):,}")

        if len(rows) == 0:
            print("No candidates found for this basin.")
            continue

        # ----------------------------------------------------
        # Rank cells by flow accumulation
        # ----------------------------------------------------

        values = accum_values[rows, cols]

        sort_order = np.argsort(values)[::-1]

        rows = rows[sort_order]
        cols = cols[sort_order]

        # Calculate raster cell area in km2
        cell_area_km2 = abs(
            transform.a * transform.e -
            transform.b * transform.d
        ) / 1_000_000

        # ----------------------------------------------------
        # Retain spatially separated candidates
        # ----------------------------------------------------

        selected_locations = []

        basin_candidates = []

        for row, col in zip(rows, cols):

            x, y = rasterio.transform.xy(
                transform,
                row,
                col,
                offset="center"
            )

            point = Point(x, y)

            # Enforce minimum distance between candidates
            if any(
                point.distance(existing) < candidate_spacing
                for existing in selected_locations
            ):
                continue

            selected_locations.append(point)

            accumulation = accum_values[row, col]

            # Distance to nearest manually created point
            manual_basin = manual_points[
                manual_points["basin"] == basin_name
            ]

            manual_distance = manual_basin.geometry.distance(
                point
            ).min()

            basin_candidates.append({
                "basin": basin_name,
                "target_id": str(target_id),
                "threshold": stream_threshold,
                "acc_cells": int(accumulation),
                "area_km2": round(
                    accumulation * cell_area_km2, 2
                ),
                "manual_dist_m": round(manual_distance, 1),
                "row": int(row),
                "col": int(col),
                "geometry": point
            })

            if len(basin_candidates) >= max_candidates:
                break

        # ----------------------------------------------------
        # Assign candidate rankings
        # ----------------------------------------------------

        for rank, record in enumerate(basin_candidates, start=1):

            record["rank"] = rank

            candidate_records.append(record)

        print(f"Candidates retained: {len(basin_candidates)}")

        for record in basin_candidates[:5]:

            print(
                f"Rank {basin_candidates.index(record) + 1}: "
                f"Accumulation = {record['acc_cells']:,} cells, "
                f"Area = {record['area_km2']:,.1f} km2"
            )


# ------------------------------------------------------------
# 7. CREATE OUTPUT GEODATAFRAMES
# ------------------------------------------------------------

if not candidate_records:
    raise RuntimeError(
        "No candidate outlets were generated. "
        "Check the stream rasters and boundary overlap."
    )

candidates_gdf = gpd.GeoDataFrame(
    candidate_records,
    geometry="geometry",
    crs=target_crs
)

manual_output = manual_points.copy()

basins_output = gpd.GeoDataFrame(
    basin_records,
    geometry="geometry",
    crs=target_crs
)


# ------------------------------------------------------------
# 8. WRITE OUTPUT GEOPACKAGE
# ------------------------------------------------------------

# Remove previous output to avoid stale layers
if output_file.exists():
    output_file.unlink()

candidates_gdf.to_file(
    output_file,
    layer="candidate_outlets",
    driver="GPKG"
)

manual_output.to_file(
    output_file,
    layer="manual_outlets",
    driver="GPKG",
    mode="a"
)

basins_output.to_file(
    output_file,
    layer="original_basins",
    driver="GPKG",
    mode="a"
)


# ------------------------------------------------------------
# 9. SUMMARY
# ------------------------------------------------------------

print("\n" + "=" * 55)
print("OUTLET CANDIDATE IDENTIFICATION COMPLETE")

print(f"\nOutput: {output_file}")

print("\nCandidates per basin:")

print(
    candidates_gdf.groupby("basin").size()
)

print("\nHighest accumulation candidates:")

print(
    candidates_gdf.sort_values(
        ["basin", "rank"]
    )[
        ["basin", "rank", "acc_cells", "area_km2", "manual_dist_m"]
    ].to_string(index=False)
)

print("\nLoad the output GeoPackage into QGIS for inspection.")
