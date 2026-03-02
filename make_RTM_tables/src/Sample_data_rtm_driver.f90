program sample_data_RTM_Driver


	! program calcalates Tbs and weighting functions ccm3 hourly maps
	! Correction is made for finite altitude at land sites.

	use nan_support
	use date_and_time


	use calc_tb_multiview
	use msu_constants

	use sample_data
	use rtm_tables
	use rtm

	implicit none

	character(len=120) :: output_file

	real(4),dimension(0:num_levels) :: pressure
	real(4),dimension(0:num_levels) :: temperature
	real(4),dimension(0:num_levels) :: height
	real(4),dimension(0:num_levels) :: specific_humidity
	real(4)                            :: wind


	real(4)                            :: land_fraction
	real(4)                            :: elevation

	integer(4)                        :: month
	integer(4)                        :: level
	integer(4)                        :: ilon
	integer(4)                        :: ilat
	real(4)                            :: lon
	real(4)                            :: lat

	integer(4)                        :: error
	integer(4)                        :: file_num
	integer(4)                        :: time_step
	integer(4)                        :: day_num
	integer(4)                        :: msu_channel = 2


	real(4),dimension(num_lons,num_lats,n_fov,num_months)        :: surf_wt_map
	real(4),dimension(num_lons,num_lats,n_fov,num_months)        :: space_wt_map
	real(4),dimension(num_lons,num_lats,n_fov,num_months)        :: tb_map    

	real(4),dimension(2,N_FOV)                        :: surf_wt
	real(4),dimension(2,N_FOV)                        :: space_wt
	real(4),dimension(2,N_FOV)                        :: tb
	real(4),dimension(2,N_FOV)                        :: emissivity
	real(4),dimension(N_FOV)                        :: surf_wt_c
	real(4),dimension(N_FOV)                        :: space_wt_c
	real(4),dimension(N_FOV)                        :: tb_c
	real(4),dimension(N_FOV)                        :: emissivity_c

	integer(4)                    :: i
  
	call init_nan
	call read_abs_table_q(msu_channel)
	call read_cld_abs_table(msu_channel)
	call read_ocean_emiss_table(msu_channel)
	call read_sample_data(error)


    
	do month = 1,num_months
		print *,month,curr_date_string(error)
		do ilon = 1,num_lons
			do ilat = 1,num_lats
                
				! assemble the profile
				pressure(0)        = ps(ilon,ilat,month)
				temperature(0)    = ts(ilon,ilat,month)
                
				level = 1
				do i = 1,num_levels
					if (plev(i) .lt. pressure(0)) then
						pressure(level) = plev(i)
						temperature(level) = ta(ilon,ilat,i,month) 
						specific_humidity(level) = hus(ilon,ilat,i,month)
						level = level+1
					endif
				enddo
				level = level-1

				! extrapolate specific humidity down to surface

				specific_humidity(0) = specific_humidity(1) + (pressure(0)-pressure(1))*(specific_humidity(1)-specific_humidity(2))/(pressure(1)-pressure(2))
				! call the tb routine

				wind = 8.0
				call calc_tb_multiview_table(    temperature(0:level),          &  ! temperature
												pressure(0:level),             &    ! pressure
												specific_humidity(0:level),    &    ! specific humidity
												level,             &  ! number of levels in this profile
												msu_channel,     &  ! Frequency (GHz)
												msu_eia,         &  ! EIA angle (degrees)  array(1:num_views)
												n_fov,             &  ! number of views
												wind,             &  ! windspeed
												emissivity,         &    ! emissivity for each pol, surface, view
												surf_wt,         &  ! weight from surface emission for each surface,view
												space_wt,         &  ! weight from space
												tb,              &    ! brightness temperature at TOA for each surface,view
												error)

				land_fraction = 0.01*sftlf(ilon,ilat)

				surf_wt_c = land_fraction*surf_wt(2,:)+(1.0-land_fraction)*surf_wt(1,:)
				space_wt_c = land_fraction*space_wt(2,:)+(1.0-land_fraction)*space_wt(1,:)
				tb_c = land_fraction*tb(2,:) + (1.0-land_fraction)*tb(1,:)

				tb_map(ilon,ilat,:,month)                       = tb_c
				surf_wt_map(ilon,ilat,:,month)                 = surf_wt_c
			enddo
		enddo
	enddo
    

	print *,curr_date_string(error)
	! write out map

	output_file = "E:\AMSU_MSU_simulation\wt_func_compare\sample_data_tbs.dat"
	open(unit= 15,file = output_file,form = 'BINARY')
	write(15)tb_map,surf_wt_map
	close(15)

        
end program sample_data_RTM_Driver