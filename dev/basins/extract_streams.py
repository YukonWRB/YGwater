#2
#extract streams

import whitebox_workflows as wbw
from pathlib import Path
import gc

wbe = wbw.WbEnvironment()

project_dir = Path(r"H:\cmfmacne\basins\code")

stream_dir = project_dir / "stream_networks"
stream_dir.mkdir(exist_ok=True)

thresholds = [500, 1000, 2500, 5000]


def extract_stream_networks(flow_accum_file):

    basin_name = flow_accum_file.stem.replace("_flow_accum", "")

    print(f"\nStarting basin: {basin_name}")

    # Monitor memory before processing
    print(f"Raster memory: {wbe.raster_memory_bytes() / 1e9:.2f} GB")

    flow_accum = wbe.read_raster(str(flow_accum_file))

    for threshold in thresholds:

        print(f"  Extracting threshold: {threshold}")

        streams = wbe.streams.network_extraction.extract_streams(
            flow_accumulation=flow_accum,
            threshold=threshold
        )

        output_file = stream_dir / f"{basin_name}_streams_{threshold}.tif"

        wbe.write_raster(streams, str(output_file))

        print(f"  Saved: {output_file.name}")

        # Release the stream raster after writing
        del streams

    # Release input raster
    del flow_accum

    # Clear tracked raster memory
    wbe.clear_raster_memory()

    gc.collect()

    print(f"Completed: {basin_name}")
    print(f"Remaining raster memory: {wbe.raster_memory_bytes() / 1e9:.2f} GB")


flow_files = sorted(project_dir.glob("*_flow_accum.tif"))

for flow_accum_file in flow_files:
    extract_stream_networks(flow_accum_file)

print("\nAll stream extraction complete.")



