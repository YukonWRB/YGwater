import requests
from bs4 import BeautifulSoup
from pathlib import Path
import pandas as pd

# download basin shapefiles from WSC and merge them into a single GeoDataFrame
output_dir = Path().resolve() / "data/basins"
os.makedirs(output_dir, exist_ok=True)

url = "https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/HydrometricNetworkBasinPolygons/shp/"
# Send a GET request to the URL with SSL verification disabled
response = requests.get(url, verify=False)

# Parse the HTML content
soup = BeautifulSoup(response.text, 'html.parser')

# Find all links on the page
links = soup.find_all('a')

# Extract and print file names
files = [link.get('href') for link in links if link.get('href') and link.get('href').endswith('.zip')]
# Extract numbers from file names

file_selection = [7, 8, 9, 10]
files = [file for file in files if int(''.join(filter(str.isdigit, file))) in file_selection]


for file in files:
    file_path = os.path.join(output_dir, file)
    if not os.path.exists(file_path):
        file_url = os.path.join(url, file)
        response = requests.get(file_url, verify=False)
        with open(file_path, 'wb') as f:
            f.write(response.content)

# Load the shapefile from the zip file
# Load all shapefiles from the zip files in the data/basins directory
gdfs = []
for file in files:
    file_path = output_dir / file
    gdf = gpd.read_file(f"zip://{file_path}", layer=0)
    gdfs.append(gdf)

# Merge all GeoDataFrames into a single GeoDataFrame
basins = gpd.GeoDataFrame(pd.concat(gdfs, ignore_index=True))