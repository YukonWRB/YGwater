
import sys
import os

#sys.path.append(os.path.join(os.path.dirname(__file__), '..'))


from utils.aprfc_utils import download_aprfc_yukon_flows

download_aprfc_yukon_flows(save_dir="dev/everett/Py/downloads")