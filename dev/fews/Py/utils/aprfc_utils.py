import requests
from defs.aprfc import XML_FILE_URL, PEM_FILE

def download_aprfc_yukon_flows(save_dir):
    response = requests.get(XML_FILE_URL, verify=PEM_FILE)
    if response.status_code == 200:
        with open(f"{save_dir}/aprfc_yukonflows.xml", "wb") as file:
            file.write(response.content)
    else:
        print(f"Failed to download file. Status code: {response.status_code}")


