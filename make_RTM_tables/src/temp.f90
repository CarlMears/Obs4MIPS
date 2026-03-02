	! truncate the profile at surface elevations, if non-zero

			if (height(1) .lt. elevation) then

				! figure out between which two NCEP levels this height is

				level_found = .false.
				do level = 1,num_levels-1
					if ((height(level) .lt. elevation) .and. &
						(height(level+1) .ge. elevation)) then

						level_found = .true.

						! assume temperature is changing linearly

						temp_at_surface = temperature(level) +		&
											(elevation-height(level))*(temperature(level+1)-temperature(level))/(height(level+1) - height(level))
				
						! assume pressure is falling exponentially with scale height given by average temperature over the layer

						ave_temp = 0.5*(temp_at_surface+temperature(level+1))
						scale_height = R_gas*ave_temp/(M_W_Air*G_0)
						p_at_surface = pressure(level+1)*exp(-(elevation-height(level+1))/scale_height)

						! assume pv is falling exponentially with scale height 3 Km

						pv_at_surface = pv(level+1)*exp(-(elevation-height(level+1))/3000.0)

						! replace observation at level = level with surface level numbers

						height(level) = elevation
						temperature(level) = temp_at_surface
						pressure(level) = p_at_surface
						pv(level) = pv_at_surface

						! remove levels lower than level

						if (level .gt. 1) then
							num_levels = num_levels - (level-1)
							height(1:num_levels) = height(level:num_levels+(level-1))
							temperature(1:num_levels) = temperature(level:num_levels+(level-1))
							pressure(1:num_levels) = pressure(level:num_levels+(level-1))
							pv(1:num_levels) = pv(level:num_levels+(level-1))
						endif

						exit

					endif
				enddo
				if (.not. level_found) then
					print *,'Error finding level'
					stop
				endif
			else ! replace 1000 mb level with surface level

				! assume temperature is changing linearly

				temp_at_surface = temperature(1) +		&
								(elevation-height(1))*(temperature(2)-temperature(1))/(height(2) - height(1))
				
				! assume pressure is falling exponentially with scale height given by average temperature over the layer

				ave_temp = 0.5*(temp_at_surface+temperature(1))
				scale_height = R_gas*ave_temp/(M_W_Air*G_0)
				p_at_surface = pressure(1)*exp(-(elevation-height(1))/scale_height)

				! assume pv is falling exponentially with scale height 3 Km

				pv_at_surface = pv(1)*exp(-(elevation-height(1))/3000.0)

				! replace observation at level = level with surface level numbers

				height(1) = elevation
				temperature(1) = temp_at_surface
				pressure(1) = p_at_surface
				pv(1) = pv_at_surface

			endif


		!	level_found = .false.
		!	do level = 1,num_levels
		!		if (abs(pressure(level) - 925.0) .lt. 1.0) then
		!			level_found = .true.
		!			exit
		!		endif
		!	enddo
					 

		!	if ((level .gt. 1) .and. (level_found)) then ! remove 925 mb observation if it's not at the bottom 
		!		height(level:num_levels) = height(level+1:num_levels+1)
		!		pressure(level:num_levels) = pressure(level+1:num_levels+1)
		!		temperature(level:num_levels) = temperature(level+1:num_levels+1)
		!		pv(level:num_levels) = pv(level+1:num_levels+1)
		!		num_levels = num_levels - 1
		!	endif

			level_found = .false.
			do level = 1,num_levels
				if (abs(pressure(level) - 600.0) .lt. 1.0) then
					level_found = .true.
					exit
				endif
			enddo
					 

			if ((level .gt. 1) .and. (level_found)) then ! remove 600 mb observation if it's not at the bottom 
				height(level:num_levels) = height(level+1:num_levels+1)
				pressure(level:num_levels) = pressure(level+1:num_levels+1)
				temperature(level:num_levels) = temperature(level+1:num_levels+1)
				pv(level:num_levels) = pv(level+1:num_levels+1)
				num_levels = num_levels - 1
			endif

	
				
			! calculate weighting function
				
			do surface = 1,2
				do view_number = 1,6
			
					call calc_weighting_function(height,					&  ! height
												temperature,				&  ! temperature
												pressure*100.0,				&  ! pressure
												pv*100.0,					&  ! water vapor partial pressure
												num_levels-1,				    &  ! number of levels
												MSU_FREQUENCY(channel),	    &  ! Frequency (GHz)
												MSU_EIA(view_number),		&  ! EIA angle (degrees)
												MSU_VIEW(view_number),      &  ! View angle (degrees)
												polarization,				&  ! 
												surface,					&  ! OCEAN = 1, LAND = 2
												8.0,						&  ! windspeed
												emissivity,					&  ! 
												wt_function(surface,view_number,:),				&  ! weight at each level  (m-1)
												wt_function_by_level(surface,view_number,:),		&  ! weight assigned to each level
												surf_wt(surface,view_number),					&  ! weight from surface emission
												space_wt(surface,view_number),					&  ! weight from surface emission
												tb(surface,view_number),							&  ! brightness temperature at TOA
												error)

			    enddo ! view
			enddo !surface

			if (pressure(2) .lt. 699.9) then 

	 			pressure(3:num_levels+1) = pressure(2:num_levels)
	 			pressure(2) = 700.0

	 			wt_function_by_level(:,:,2:num_levels+1) = wt_function_by_level(:,:,1:num_levels)
	 			wt_function_by_level(:,:,1) = 99.9

				num_levels = num_levels + 1
			endif

			if (pressure(2) .lt. 849.9) then 

	 			pressure(3:num_levels+1) = pressure(2:num_levels)
	 			pressure(2) = 850.0

	 			wt_function_by_level(:,:,2:num_levels+1) = wt_function_by_level(:,:,1:num_levels)
	 			wt_function_by_level(:,:,1) = 99.9

				num_levels = num_levels + 1
			endif

			if (pressure(2) .lt. 924.9) then 

	 			pressure(3:num_levels+1) = pressure(2:num_levels)
	 			pressure(2) = 850.0

	 			wt_function_by_level(:,:,2:num_levels+1) = wt_function_by_level(:,:,1:num_levels)
	 			wt_function_by_level(:,:,1) = 99.9

				num_levels = num_levels + 1
			endif

			wt_function_map(ilon,ilat,:,:,1:num_levels) = wt_function_by_level
			wt_function_map(ilon,ilat,:,:,0) = surf_wt
			wt_function_map(ilon,ilat,:,:,num_levels+1) = space_wt
			
		enddo !ilat
	enddo !ilon

		write(output_file,9002)time_step
9002    format('\\dell-p1700d\j\NCEP_Modeled_AMSU_Tbs\Channel_MSU2\ncep_msu_wt_func_LTM_w925_',i2.2,'.dat')

		open(unit=3,file=output_file,form='binary')
		print *,'writing ',output_file
		write(3)wt_function_map
		close(3)

	enddo     !month