
# ============================================================
# YUKON BASIN DELINEATION
# Test using highest-ranked automated pour points
# ============================================================
import rasterio
from rasterio.features import shapes
from shapely.geometry import shape
import whitebox_workflows as wbw
import geopandas as gpd
import pandas as pd

from pathlib import Path
import gc


# ------------------------------------------------------------
# 1. PROJECT SETTINGS
# ------------------------------------------------------------

wbe = wbw.WbEnvironment()

project_dir = Path(r"H:\cmfmacne\basins\code")

candidate_file = (
    project_dir /
    "outlet_analysis" /
    "outlet_candidates_2500.gpkg"
)

basin_file = project_dir / "test_basins.gpkg"

output_dir = project_dir / "delineated_basins"
output_dir.mkdir(exist_ok=True)


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
# 3. READ CANDIDATE POUR POINTS
# ------------------------------------------------------------

candidates = gpd.read_file(
    candidate_file,
    layer="candidate_outlets"
)

print("Candidate outlets loaded:")
print(candidates.groupby("basin").size())


# ------------------------------------------------------------
# 4. READ ORIGINAL BASIN POLYGONS
# ------------------------------------------------------------

original_basins = gpd.read_file(
    basin_file,
    layer="hybas_reprojected"
)

original_basins = original_basins.to_crs("EPSG:3979")

original_basins["_hybas_numeric"] = pd.to_numeric(
    original_basins["HYBAS_ID"],
    errors="coerce"
)


# ------------------------------------------------------------
# 5. DELINEATE EACH BASIN
# ------------------------------------------------------------

for dem_name, config in basins_config.items():

    basin_name = config["name"]

    print("\n" + "=" * 55)
    print(f"Processing: {basin_name}")

    # --------------------------------------------------------
    # Select rank 1 candidate
    # --------------------------------------------------------

    selected = candidates[
        (candidates["basin"] == basin_name) &
        (candidates["rank"] == 1)
    ].copy()

    if len(selected) != 1:
        raise ValueError(
            f"Expected one rank 1 pour point for {basin_name}; "
            f"found {len(selected)}"
        )

    print("Selected pour point:")
    print(selected[
        ["rank", "acc_cells", "area_km2", "geometry"]
    ])

    # --------------------------------------------------------
    # Save selected pour point
    # --------------------------------------------------------

    pour_file = output_dir / f"{basin_name}_pour_point.gpkg"

    if pour_file.exists():
        pour_file.unlink()

    selected.to_file(
        pour_file,
        driver="GPKG"
    )

    # --------------------------------------------------------
    # Read matching D8 pointer raster
    # --------------------------------------------------------

    flow_dir_file = (
        project_dir /
        f"{dem_name}_flow_dir.tif"
    )

    if not flow_dir_file.exists():
        raise FileNotFoundError(flow_dir_file)

    print(f"Reading: {flow_dir_file.name}")

    d8_pointer = wbe.read_raster(str(flow_dir_file))

    # --------------------------------------------------------
    # Read pour point vector
    # --------------------------------------------------------

    pour_points = wbe.read_vector(str(pour_file))

    # --------------------------------------------------------
    # Run watershed delineation
    # --------------------------------------------------------

    print(f"Delineating {basin_name} watershed...")

    watershed = wbe.hydrology.watersheds_basins.watershed(
        d8_pntr = d8_pointer,
        pour_pts = pour_points
    )

    # --------------------------------------------------------
    # Save watershed raster (intermediate)
    # --------------------------------------------------------

    watershed_file = (
        output_dir /
        f"{basin_name}_watershed.tif"
    )

    wbe.write_raster(
        watershed,
        str(watershed_file)
    )

    print(f"Saved watershed raster: {watershed_file.name}")

    # --------------------------------------------------------
    # Convert watershed raster to polygon
    # --------------------------------------------------------

    print("Converting watershed raster to polygon...")

    with rasterio.open(watershed_file) as src:

        watershed_data = src.read(1, masked=True)

        valid_mask = ~watershed_data.mask

        # Extract watershed polygons
        polygon_features = []

        for geom, value in shapes(
            watershed_data.filled(0),
            mask=valid_mask,
            transform=src.transform
        ):

            polygon_features.append({
                "geometry": shape(geom),
                "value": int(value)
            })

        watershed_gdf = gpd.GeoDataFrame(
            polygon_features,
            crs=src.crs
        )

    # --------------------------------------------------------
    # Save watershed polygon
    # --------------------------------------------------------

    # Retain only valid watershed features
    watershed_gdf = watershed_gdf[
        watershed_gdf.geometry.notnull()
        & ~watershed_gdf.geometry.is_empty
    ]

    polygon_file = (
        output_dir /
        f"{basin_name}_watershed.gpkg"
    )

    if polygon_file.exists():
        polygon_file.unlink()

    watershed_gdf.to_file(
        polygon_file,
        driver="GPKG"
    )

    print(f"Saved watershed polygon: {polygon_file.name}")

    # --------------------------------------------------------
    # Clean up memory
    # --------------------------------------------------------

    del watershed
    del d8_pointer
    del pour_points

    wbe.clear_raster_memory()
    gc.collect()

    print(f"Completed: {basin_name}")


# ------------------------------------------------------------
# 6. COMPLETION
# ------------------------------------------------------------

print("\n" + "=" * 55)
print("ALL THREE WATERSHEDS DELINEATED")

print(f"Output directory: {output_dir}")
