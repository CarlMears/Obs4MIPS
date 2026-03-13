import numpy as np
import xarray as xr
import rss_surface_emiss as emiss
import importlib.resources
import matplotlib.pyplot as plt
from pathlib import Path

from MSU_constants import MSU_NOM_EIAS,MSU_VIEW_ANGLES
from MSU_constants import MSU_POLARIZATION,MSU_FREQ,MSU_FREQ_SPLIT_1,MSU_BANDWIDTH

from AMSU_constants import AMSU_NOM_EIAS,AMSU_VIEW_ANGLES
from AMSU_constants import AMSU_A_POLARIZATION,AMSU_A_FREQ,AMSU_A_FREQ_SPLIT_1,AMSU_A_FREQ_SPLIT_2,AMSU_A_BANDWIDTH
       
def compute_sounder_freq_weights(freq, bandwidth, freq_split_1, num_freq=14):
    
    if freq_split_1 <= 0.00001:
        # This is not a DSB channel, so we can just return the center frequency and a weight of 1.
        # Just do one range of frequencies around the center frequency.

        sounder_freq_arr = np.zeros(num_freq, dtype=np.float32)
        sounder_freq_wt = np.zeros(num_freq, dtype=np.float32)

        for freq_index in range(num_freq):
            sounder_freq_arr[freq_index] = freq - bandwidth / 2.0 + freq_index * bandwidth / (num_freq - 1)
            if (freq_index == 0) or (freq_index == num_freq - 1):
                sounder_freq_wt[freq_index] = 0.5 / (num_freq - 1.0)
            else:
                sounder_freq_wt[freq_index] = 1.0 / (num_freq - 1.0)

    else:

        num_per_side = num_freq // 2

        center_freq_lower = freq - freq_split_1
        center_freq_upper = freq + freq_split_1

        sounder_freq_arr = np.zeros(num_freq, dtype=np.float32)
        sounder_freq_wt = np.zeros(num_freq, dtype=np.float32)

        for freq_index in range(num_per_side):
            sounder_freq_arr[freq_index] = center_freq_lower - bandwidth / 2.0 + freq_index * bandwidth / (num_per_side - 1)
            sounder_freq_arr[freq_index + num_per_side] = (
                center_freq_upper - bandwidth / 2.0 + freq_index * bandwidth / (num_per_side - 1)
            )

            # these weights perform trapezoidal integration.
            if (freq_index == 0) or (freq_index == num_per_side - 1):
                sounder_freq_wt[freq_index] = 0.25 / (num_per_side - 1.0)
                sounder_freq_wt[freq_index + num_per_side] = 0.25 / (num_per_side - 1.0)
            else:
                sounder_freq_wt[freq_index] = 0.5 / (num_per_side - 1.0)
                sounder_freq_wt[freq_index + num_per_side] = 0.5 / (num_per_side - 1.0)
        

    try:
        assert np.isclose(np.sum(sounder_freq_wt), 1.0), (
            f"Frequency weights do not sum to 1. Sum is {np.sum(sounder_freq_wt)}"
        )
    except AssertionError as e:
        print(e)
        print("weights do not sum to 1.0)")
        print("sounder_freq_arr: ", sounder_freq_arr)
        print("sounder_freq_wt: ", sounder_freq_wt)
        raise

    return sounder_freq_arr, sounder_freq_wt


def compute_ocean_emissivity_from_freq_weights(polarization,
                                               num_T,
                                               num_W,
                                               T0,
                                               Delta_T,
                                               W0,
                                               Delta_W,
                                               nom_eias,
                                               view_angles,
                                               freqs,
                                               weights):
    
    num_views = len(nom_eias)

    emiss_table_by_pol = np.zeros((num_T + 1, num_W + 1, num_views, 2), dtype=np.float32)
    emiss_table = np.zeros((num_T + 1, num_W + 1, num_views), dtype=np.float32)

    path_to_data = importlib.resources.files('geomod10') / 'data'
    emiss.geomod10b.init1(str(path_to_data) + '/')

    num_freq = freqs.shape[0]

    T = T0 + Delta_T * np.arange(num_T + 1)
    sst = T - 273.16
    phir_arr = np.full_like(sst, -999.0)
    for fov in range(num_views):
        tht = nom_eias[fov]
        tht_arr = np.full_like(sst, tht)
        for freq_index in range(num_freq):
            freq = freqs[freq_index]
            freq_grid = np.full_like(sst, freq)
            for W_index in range(num_W + 1):
                W = W0 + Delta_W * W_index
                W_arr = np.full_like(sst, W)
                emiss_vh, emiss_phi = emiss.wind_emiss(freq_grid, tht_arr, sst, W_arr, phir_arr)
                emiss_table_by_pol[:, W_index, fov, :] = (
                    emiss_table_by_pol[:, W_index, fov, :] + emiss_vh * weights[freq_index]
                )

    # combine polarizations according to view angle
    for fov in range(num_views):
        theta_view = view_angles[fov] * np.pi / 180.0
        cos_sqr_theta_view = np.cos(theta_view) * np.cos(theta_view)
        sin_sqr_theta_view = np.sin(theta_view) * np.sin(theta_view)

        if polarization == 1:  # V-pol
            emiss_table = (
                emiss_table_by_pol[:, :, :, 0] * cos_sqr_theta_view
                + emiss_table_by_pol[:, :, :, 1] * sin_sqr_theta_view
            )
        elif polarization == 2:  # H-pol
            emiss_table = (
                emiss_table_by_pol[:, :, :, 0] * sin_sqr_theta_view
                + emiss_table_by_pol[:, :, :, 1] * cos_sqr_theta_view
            )
        else:
            raise ValueError(
                f"Invalid polarization value: {polarization}. Expected 1 for V-pol or 2 for H-pol."
            )

    return emiss_table


def compute_ocean_emissivity_tables_amsu(channel,
                                             num_T,
                                             num_W,
                                             T0,
                                             Delta_T,
                                             W0,
                                             Delta_W):
    channel_index = channel - 1
    if AMSU_A_FREQ_SPLIT_2[channel_index] > 0.00001:
        raise NotImplementedError(
            f"AMSU channel {channel} has a second frequency split, which is not currently supported by this code."
        )
    polarization = AMSU_A_POLARIZATION[channel_index]
    amsu_freq_arr, amsu_freq_wt = compute_sounder_freq_weights(
        AMSU_A_FREQ[channel_index],
        AMSU_A_BANDWIDTH[channel_index],
        AMSU_A_FREQ_SPLIT_1[channel_index]
    )
    return compute_ocean_emissivity_from_freq_weights(
        polarization,
        num_T,
        num_W,
        T0,
        Delta_T,
        W0,
        Delta_W,
        AMSU_NOM_EIAS,
        AMSU_VIEW_ANGLES,
        amsu_freq_arr,
        amsu_freq_wt
    )

def compute_ocean_emissivity_tables_msu(channel,
                                             num_T,
                                             num_W,
                                             T0,
                                             Delta_T,
                                             W0,
                                             Delta_W):
    channel_index = channel - 1
    polarization = MSU_POLARIZATION[channel_index]
    msu_freq_arr, msu_freq_wt = compute_sounder_freq_weights(
        MSU_FREQ[channel_index],
        MSU_BANDWIDTH[channel_index],
        MSU_FREQ_SPLIT_1[channel_index]
    )
    return compute_ocean_emissivity_from_freq_weights(
        polarization,
        num_T,
        num_W,
        T0,
        Delta_T,
        W0,
        Delta_W,
        MSU_NOM_EIAS,
        MSU_VIEW_ANGLES,
        msu_freq_arr,
        msu_freq_wt
    )

if __name__ == "__main__":
     
    
    num_T = 200
    Delta_T = 1.0
    T0 = 140.0

    num_W = 30
    Delta_W = 1.0
    W0 = 0.0

    do_MSU = True
    do_AMSU = True

    if do_MSU:
        for msu_channel in range(1, 5):
            print(f"Computing ocean emissivity table for MSU channel {msu_channel}...")
            emiss_table = compute_ocean_emissivity_tables_msu(msu_channel,
                                                        num_T,
                                                        num_W, 
                                                        T0, 
                                                        Delta_T,
                                                        W0, Delta_W)
            num_views = emiss_table.shape[2]
            emiss_table = emiss_table.astype(np.float32)
            ds = xr.Dataset(
                data_vars={
                    'emissivity': (('temperature', 'wind_speed', 'fov'), emiss_table)
                },
                coords={
                    'temperature': T0 + Delta_T*np.arange(num_T+1),
                    'wind_speed': W0 + Delta_W*np.arange(num_W+1),
                    'fov': np.arange(num_views)
                }
            )

            ds['emissivity'].attrs.update({
                'standard_name': 'surface_emissivity',
                'long_name': 'Surface emissivity',
                'units': '1',
                'valid_min': 0.0,
                'valid_max': 1.0,
                'description': f'MSU channel {msu_channel} ocean surface emissivity as a function of sea surface temperature and wind speed',
            })

            ds['temperature'].attrs.update({
                'standard_name': 'sea_surface_temperature',
                'long_name': 'Sea surface temperature',
                'units': 'K',
                'valid_min': float(T0),
                'valid_max': float(T0 + Delta_T * num_T)
            })
            ds['wind_speed'].attrs.update({
                'standard_name': 'wind_speed',
                'long_name': 'Surface wind speed',
                'units': 'm s-1',
                'valid_min': float(W0),
                'valid_max': float(W0 + Delta_W * num_W)
            })
            ds['fov'].attrs.update({
                'long_name': 'Field of view index',
                'units': '1',
                'valid_min': 0,
                'valid_max': num_views - 1
            })

            output_path = Path('./make_RTM_tables/data/ocean_emiss_tables/')
            nc_filename  = f'ocean_emissivity_table_MSU_channel_{msu_channel:02d}.nc'
            ds.to_netcdf(output_path / nc_filename)

            print('Emissivity table saved to: ', output_path / nc_filename)
            print() 

    if do_AMSU:
        for channel in range(1, 11):
            print(f"Computing ocean emissivity table for AMSU channel {channel}...")
            emiss_table = compute_ocean_emissivity_tables_amsu(channel,
                                                        num_T,
                                                        num_W, 
                                                        T0, 
                                                        Delta_T,
                                                        W0, Delta_W)
            num_views = emiss_table.shape[2]
            emiss_table = emiss_table.astype(np.float32)
            ds = xr.Dataset(
                data_vars={
                    'emissivity': (('temperature', 'wind_speed', 'fov'), emiss_table)
                },
                coords={
                    'temperature': T0 + Delta_T*np.arange(num_T+1),
                    'wind_speed': W0 + Delta_W*np.arange(num_W+1),
                    'fov': np.arange(num_views)
                }
            )

            ds['emissivity'].attrs.update({
                'standard_name': 'surface_emissivity',
                'long_name': 'Surface emissivity',
                'units': '1',
                'valid_min': 0.0,
                'valid_max': 1.0,
                'description': f'AMSU channel {channel} ocean surface emissivity as a function of sea surface temperature and wind speed',
            })

            ds['temperature'].attrs.update({
                'standard_name': 'sea_surface_temperature',
                'long_name': 'Sea surface temperature',
                'units': 'K',
                'valid_min': float(T0),
                'valid_max': float(T0 + Delta_T * num_T)
            })
            ds['wind_speed'].attrs.update({
                'standard_name': 'wind_speed',
                'long_name': 'Surface wind speed',
                'units': 'm s-1',
                'valid_min': float(W0),
                'valid_max': float(W0 + Delta_W * num_W)
            })
            ds['fov'].attrs.update({
                'long_name': 'Field of view index',
                'units': '1',
                'valid_min': 0,
                'valid_max': num_views - 1
            })

            output_path = Path('./make_RTM_tables/data/ocean_emiss_tables/')
            nc_filename  = f'ocean_emissivity_table_AMSU_channel_{channel:02d}.nc'
            ds.to_netcdf(output_path / nc_filename)

            print('Emissivity table saved to: ', output_path / nc_filename)
            print()

        


