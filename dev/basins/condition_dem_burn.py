##does not work - flow acc rasters super weird

# [x for x in dir(wbe.hydrology) if "burn" in x.lower()]
# [x for x in dir(wbe) if "burn" in x.lower()]

# import inspect

# print(inspect.signature(wbe.burn_streams))
# print(wbe.burn_streams.__doc__)
# print(wbe.hydrology.burn_streams.__doc__)

# [x for x in dir(wbe.hydrology) if "fill" in x.lower()]
# [x for x in dir(wbe.hydrology.depressions_storage) if "fill" in x.lower()]
import math
import rasterio
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path
import whitebox_workflows as wbw

wbe = wbw.WbEnvironment()

project_dir = Path(r"H:\cmfmacne\basins\code\burn")

def cond_dem_burn(dem_file):

    stem = dem_file.stem
    suffix = stem.split("_")[-1]

    stream_file = project_dir / f"streams_{suffix}.gpkg"

    print(f"Processing {stem}")
    print(f"Using {stream_file.name}")

    # Read inputs
    dem = wbe.read_raster(str(dem_file))
    streams = wbe.read_vector(str(stream_file))

    # Fill depressions
    filled = wbe.hydrology.depressions_storage.fill_depressions_wang_and_liu(
        dem=dem
    )

    wbe.write_raster(
        filled,
        str(project_dir / f"{stem}_filled.tif")
    )

    # Burn streams (10 m)
    burned = wbe.hydrology.depressions_storage.burn_streams(
        dem=filled,
        streams=streams,
        decrement_value=10.0,
        gradient_distance=5
    )

    wbe.write_raster(
        burned,
        str(project_dir / f"{stem}_fillburn10.tif")
    )

    # D8 pointer
    d8_pointer = wbe.hydrology.flow_routing.d8_pointer(
        dem=burned
    )

    # Flow accumulation
    flow_accum = wbe.hydrology.flow_routing.d8_flow_accum(
        input=d8_pointer,
        input_is_pointer=True,
        out_type="cells"
    )

    wbe.write_raster(
        flow_accum,
        str(project_dir / f"{stem}_fillburn10_flow_accum.tif")
    )

    print(f"Finished {stem}")

for dem_file in sorted(project_dir.glob("dtm_*.tif")):

    if "_filled" in dem_file.stem:
        continue

    if "_fillburn" in dem_file.stem:
        continue

    if "_flow_accum" in dem_file.stem:
        continue

    cond_dem_burn(dem_file)



########################################################
#just to take a look
flow_file = project_dir / "dtm_3_fillburn10_flow_accum.tif"

with rasterio.open(flow_file) as src:

    data = src.read(
        1,
        out_shape=(1500, 1500)
    )

    nodata = src.nodata

display = data.astype(float)

display[data == nodata] = np.nan

plt.figure(figsize=(10,10))

plt.imshow(
    np.log10(display),
    cmap="Blues"
)

plt.colorbar()
plt.title(flow_file.stem)

plt.show()