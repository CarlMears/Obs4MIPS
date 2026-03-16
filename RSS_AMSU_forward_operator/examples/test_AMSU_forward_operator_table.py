from rss_amsu_forward_operator import AMSUForwardOperatorTable
from rss_amsu_forward_operator.check_model_data import check_model_data
from era5 import era5_monthly_files,read_era5_data_monthly
import numpy as np
import xarray as xr
from pathlib import Path
# for graphical debugging
from rss_plotting.global_map import plot_global_map
import matplotlib.pyplot as plt

# This not needed, but a lot problems can arise if the wrong Python environment is being used,
# so this is just a sanity check to print the Python executable being used.
import sys
print("Python executable being used: ")
print(sys.executable)
print()

if __name__ == "__main__":
    month = 7
    year = 2024
    OxygenAbs_index = 5
    path_to_era5 = Path('/mnt/n/data/model/ERA5/monthly')

    # find a list of the ERA5 files needed
    era5_files = era5_monthly_files(year_to_do=year, month_to_do=month, path_to_era5=path_to_era5)

    # read the ERA5 data for the specified month and year.  Model data is a dictionary a 2D and 3D numpy arrays
    model_data = read_era5_data_monthly(era5_files) 

    # some simple checks on the data
    result = check_model_data(model_data, verbose=False)

    # intialize the AMSU forward operator for choosen channel
    amsu_op = AMSUForwardOperatorTable(AMSU_channel=5,OxygenAbs_index=OxygenAbs_index)

    #compute the brightness temperatures for the specified month and year
    brightness_temperatures = amsu_op.compute_tbs(model_data)

    channels_present = list(brightness_temperatures.keys())  # For AMSU Channel 5, we compute TLT and TMT. 
                                                             # For AMSU Channel 7, we compute TTS. 
                                                             # For AMSU Channel 9, we compute TLS.
    print(f"Channels present in output: {channels_present}")
    for channel, tb in brightness_temperatures.items():
        plot_global_map(tb,vmin=200.0,vmax=300.0, plt_colorbar=True, title=f'{channel} TB', cmap='viridis')
    plt.show()
    print()