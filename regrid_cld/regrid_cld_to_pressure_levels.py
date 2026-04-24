import xarray as xr
import matplotlib.pyplot as plt
import pandas as pd
import xarray as xr
import xcdat as xc 
#from rss_plotting.global_map import plot_global_map

pressure_levels_default =[100000.0,
                  92500.0,
                  85000.0,
                  70000.0,
                  60000.0,
                  50000.0,
                  40000.0,
                  30000.0,
                  25000.0,
                  20000.0,
                  15000.0,
                  10000.0,
                  7000.0,
                  5000.0,
                  3000.0,
                  2000.0,
                  1000.0,
                  500.0,
                  100.0]

def hybrid_coordinate(p0, a, b, ps, **kwargs):
    return_val = a * p0 + b * ps
    
    return return_val

def hybrid_coordinate_2(ap, b, ps, **kwargs):
    return_val = ap + b * ps
    
    return return_val




def regrid_cld_to_pressure_levels(cld, pressure_levels):
    pass

if __name__ == "__main__":

    from pathlib import Path
    import glob
    path_to_data = Path('/mnt/n/data/model')
    model_name = 'MPI-ESM1-2-LR'
    run_id = 'r1i1p1f1'
    experiment_name = 'historical'

    cloud_stem = 'clw_Amon'
    search_path = path_to_data / model_name / experiment_name / run_id / f'{cloud_stem}_{model_name}_{experiment_name}*.nc'
    file_list = glob.glob(str(search_path))

    # Figure the pressure levels from the ta file.
    ta_file = file_list[0].replace(cloud_stem, 'ta_Amon')
    ds_ta = xr.open_dataset(ta_file)
    pressure_levels = ds_ta['plev'].values
    print('pressure levels: ', pressure_levels)
    for file in file_list:
        ds_cl = xr.open_dataset(file)
        print('processing file: ', file)

        try:
            pressure = hybrid_coordinate(**ds_cl.data_vars)
        except Exception as e:
            print('Error in hybrid_coordinate: ', e, ' trying hybrid_coordinate_2')
            pressure = hybrid_coordinate_2(**ds_cl.data_vars)
            print('hybrid_coordinate_2 successful')

        print('constructing new pressure grid for regridding')
        new_pressure_grid = xc.create_grid(
            z=xc.create_axis("lev", pressure_levels))
        
        print('regridding cloud variable to pressure levels')
        output_cl = ds_cl.regridder.vertical(
                "clw",
                new_pressure_grid,
                method="linear",
                target_data=pressure)

        # fill any nans with 0.0
        output_cl['clw'] = output_cl['clw'].fillna(0.0)
        
        #copy attributes from original dataset
        output_cl["clw"].attrs.update(ds_cl["clw"].attrs)
        
        #write out the output file as the source location
        out_file = file.replace(cloud_stem, f'{cloud_stem}_pressure_levels')
        print('writing output file: ', out_file)
        output_cl.to_netcdf(out_file)
        
        print()
