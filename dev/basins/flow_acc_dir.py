#1

import rasterio
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path
import whitebox_workflows as wbw

wbe = wbw.WbEnvironment()

#project settings
project_dir = Path(r"H:\cmfmacne\basins\code")

tile_dir = project_dir / "basin_tiles" / "dem_tiles"

output_dir = project_dir / "basin_tiles"

flow_dir_dir = output_dir / "flow_direction"
flow_accum_dir = output_dir / "flow_accumulation"

flow_dir_dir.mkdir(parents=True, exist_ok=True)
flow_accum_dir.mkdir(parents=True, exist_ok=True)

#tile = 1 #None for all

#breaches, makes flow dir and acc, and prints along the way because it is so darn slow
def cond_dem(dem_file):
    print("ENTERED FUNCTION")
    print(dem_file.name)

    dem = wbe.read_raster(str(dem_file))

    print("READ COMPLETE")

    # print("Breaching depressions...")
    # breached = wbe.hydrology.depressions_storage.breach_depressions_least_cost(
    #     dem=dem,
    #     max_dist=10,
    #     fill_deps=True,
    #     minimize_dist=True
    # )

    # print("BREACH COMPLETE")
    stem = dem_file.stem

    print("Creating D8 pointer...")
    d8_pointer = wbe.hydrology.flow_routing.d8_pointer(
        dem=dem
    )

    print("Creating flow accumulation...")
    flow_accum = wbe.hydrology.flow_routing.d8_flow_accum(
        input=d8_pointer,
        input_is_pointer=True,
        out_type="cells"
    )

    print("writing flow accumulation...")
    wbe.write_raster(
        flow_accum,
        str(flow_accum_dir / f"{stem}_flow_accum.tif")
    )

    print("Writing flow direction...")
    wbe.write_raster(
        d8_pointer,
        str(flow_dir_dir /  f"{stem}_flow_dir.tif")
    )

    print(f"Finished {dem_file.name}")    

tile = tile_dir / "hb5_8050000010_dem.tif"
print("starting test")
cond_dem(tile)
print("done")

#loops through some or all tiles
# for i, dem_file in enumerate(sorted(tile_dir.glob("hb5_*.tif"))):

#     if max_tiles is not None and i >= max_tiles:
#         break

#     cond_dem(dem_file)
with rasterio.open(tile) as src:
    print("Width:", src.width)
    print("Height:", src.height)
    print("Cells:", src.width * src.height)
    print("Data type:", src.dtypes[0])