import math
import rasterio
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path
import whitebox_workflows as wbw

wbe = wbw.WbEnvironment()

project_dir = Path(r"H:\cmfmacne\basins\code")

def cond_dem_breach(dem_file):

    dem = wbe.read_raster(str(dem_file))

    breached = wbe.hydrology.depressions_storage.breach_depressions_least_cost(
        dem=dem,
        max_dist=100,
        fill_deps=True,
        minimize_dist=True
    )

    stem = dem_file.stem

    wbe.write_raster(
        breached,
        str(project_dir / f"{stem}_breached.tif")
    )

    d8_pointer = wbe.hydrology.flow_routing.d8_pointer(
        dem=breached
    )

    flow_accum = wbe.hydrology.flow_routing.d8_flow_accum(
        input=d8_pointer,
        input_is_pointer=True,
        out_type="cells"
    )

    wbe.write_raster(
        flow_accum,
        str(project_dir / f"{stem}_flow_accum.tif")
    )

    wbe.write_raster(
        d8_pointer,
        str(project_dir / f"{stem}_flow_dir.tif")
    )    


for dem_file in sorted(project_dir.glob("dtm_*.tif")):
    cond_dem_breach(dem_file)



#just to take a look
flow_file = project_dir / "dtm_1_flow_accum.tif"

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



