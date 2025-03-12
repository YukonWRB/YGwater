import matplotlib
import matplotlib.cbook

# ...existing code...

def get_data_path():
    if hasattr(matplotlib, 'get_data_path'):
        return matplotlib.get_data_path()
    else:
        return matplotlib.cbook.get_sample_data_path()

# ...existing code...

data_path = get_data_path()
# ...existing code...
