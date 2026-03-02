import numpy as np
import xarray as xr
import matplotlib.pyplot as plt
# import sys
# sys.path.append('/mnt/m/Obs4MIPs/make_RTM_tables/build_linux')
import sys
import os
from pathlib import Path
print(f"Current Directory: {os.getcwd()}")
print(f"Python Path: {sys.path}")
print("Python executable being used: ")
print(sys.executable)

import compute_absorption_tables_amsu

#from compute_absorption_tables_amsu.make_absorption_table_t_q import compute_absorption_tables_amsu_dsb, compute_absorption_tables_amsu


num_T = int(200)
num_p = int(110)
num_q = int(150)

T0 = float(140.0)
Delta_T = float(1.0)
Delta_P = float(10.0)
Delta_q = float(0.001)

ivap = int(4)
for ioxy in [4,5]:
    ioxy = int(ioxy)
    for channel in [5,7,9]:
        channel = int(channel)
        if channel in [5,10]:
                print(f"Computing absorption tables for AMSU channel {channel} with ivap={ivap} and ioxy={ioxy}...")
                abs_table, abs_table_per_Pa = compute_absorption_tables_amsu.make_absorption_table_t_q.compute_absorption_tables_amsu_dsb(
                    channel, ivap, ioxy,
                    num_T, num_p, num_q,
                    T0, Delta_T, Delta_P, Delta_q)
        elif channel in [1,2,3,4,6,7,8,9]:
                print(f"Computing absorption tables for AMSU channel {channel} with ivap={ivap} and ioxy={ioxy}...")
                abs_table, abs_table_per_Pa = compute_absorption_tables_amsu.make_absorption_table_t_q.compute_absorption_tables_amsu(
                    channel, ivap, ioxy,
                    num_T, num_p, num_q,
                    T0, Delta_T, Delta_P, Delta_q)
        else:
                raise ValueError(f"Invalid AMSU channel: {channel}. Must be 1-10. Quad Bands not supported yet.")

        print("Setting abs for P=0 to 0")
        abs_table[:,0,:] = 0
        abs_table_per_Pa[:,0,:] = 0
        print("Absorption table shape: ", abs_table.shape)
        print("Absorption table per Pa shape: ", abs_table_per_Pa.shape)

        abs_table_ds = xr.Dataset(
            {"absorption": (["T", "p", "q"], abs_table)},
            coords={"T": np.arange(num_T+1)*Delta_T + T0,
                    "p": np.arange(num_p+1)*Delta_P,
                    "q": np.arange(num_q+1)*Delta_q},
            attrs={"AMSU_channel": channel, "ivap": ivap, "ioxy": ioxy}
        )
        nc_file = f"amsu_{channel:02d}_abs_table_q.{ivap}.{ioxy}.nc"
        nc_path = Path(__file__).parent.parent / 'data' / 'abs_tables' / nc_file
        print(f"Saving absorption table to {nc_path}")
        nc_path.parent.mkdir(parents=True, exist_ok=True)
        abs_table_ds.to_netcdf(nc_path)

        abs_table_per_Pa_ds = xr.Dataset(
            {"absorption_per_Pa": (["T", "p", "q"], abs_table_per_Pa)},
            coords={"T": np.arange(num_T+1)*Delta_T + T0,
                    "p": np.arange(num_p+1)*Delta_P,
                    "q": np.arange(num_q+1)*Delta_q},
            attrs={"AMSU_channel": channel, "ivap": ivap, "ioxy": ioxy}
        )
        nc_file_per_Pa = f"amsu_{channel:02d}_abs_table_per_Pa_q.{ivap}.{ioxy}.nc"
        nc_path_per_Pa = Path(__file__).parent.parent / 'data' / 'abs_tables' / nc_file_per_Pa
        print(f"Saving absorption table per Pa to {nc_path_per_Pa}")
        nc_path_per_Pa.parent.mkdir(parents=True, exist_ok=True)
        abs_table_per_Pa_ds.to_netcdf(nc_path_per_Pa)
        print()
