program make_ocean_emissivity_table

	use nan_support
	use rtm
	use amsu_constants
	use NESDIS_SEAICE_PHYEM_MODULE

	implicit none

	
	integer(4)			:: num_T 
	real(4)				:: T0 
	real(4)				:: Delta_T 

	integer(4)			:: num_W
	real(4)				:: W0
	real(4)	 			:: delta_W

	

	real(4),dimension(0:200,1:15,1:2)  :: emiss_table       ! temperature,fov,pol (vpol = 1 hpol = 2)
	real(4),dimension(0:200,1:15)      :: emiss_table_comb  ! temperature,fov

	integer(4)							:: polarization
	integer(4),parameter				:: amsu_num_band = 2
	integer(4),parameter				:: amsu_num_freq = 13
	integer(4)							:: freq_index,band_index
 	real(4),dimension(amsu_num_freq,amsu_num_band) :: amsu_freq_arr


	integer(4)							:: fov,T_index,W_index,channel
	real(4)								:: tht,theta_view,freq,T,W,Ts_ice,salinity
	real(4),dimension(2)				:: emiss
	real(4)								:: emissivity_v,emissivity_h

	character(len = 120)				:: file


	do channel = 1,2

		polarization = AMSU_A_Polarization(channel)

		! construct an array of frequencies where the calc is performed
		do band_index = 0,amsu_num_band-1 
			do freq_index = 0,amsu_num_freq-1
				amsu_freq_arr(freq_index+1,band_index+1) =						&
						AMSU_A_Freq(channel) -									&
						2.0*(band_index-0.5)*AMSU_A_Freq_Split_1(channel) - 	&			
						AMSU_A_BANDWIDTH(channel)/2.0 +							&
						(1+2*freq_index)*AMSU_A_BANDWIDTH(channel)/(2.0*amsu_num_freq)
			enddo
		enddo

		print *,amsu_freq_arr


		num_T = 200
		T0 = 140.0
		Delta_T = 1.0

		emiss_table = 0.0


		do fov = 1,15
			tht = AMSU_NOM_EIAS(fov)
			do band_index = 1,amsu_num_band
			do freq_index = 1,amsu_num_freq
				freq = amsu_freq_arr(freq_index,band_index)
				print*,freq	
				do T_index = 0,num_T
				   T = T0+Delta_T*T_index
				   Ts_ice = T
				   if (Ts_ice .gt. 271.0) Ts_ice = 271.0
				   Salinity = 30
				   call NESDIS_SIce_Phy_EM(Freq,                                                 &  ! INPUT
									tht,                                                     &  ! INPUT
									Ts_ice,                                                    &  ! INPUT
									Salinity,                                                  &  ! INPUT
									Emissivity_H,                                              &  ! OUTPUT
									Emissivity_V)
				   emiss(1) = Emissivity_V
				   emiss(2) = Emissivity_H 
				   emiss_table(T_index,fov,:) = emiss_table(T_index,fov,:) + emiss
				enddo
			enddo
			enddo
		enddo

		emiss_table = emiss_table/(amsu_num_freq*amsu_num_band)

		! combine polarizations according to view angle

		do fov = 1,15
			theta_view = AMSU_VIEW_ANGLES(fov)
			do T_index = 0,num_T
				if (polarization .eq. 1) then !V-pol
				emiss_table_comb(T_index,fov)	=	 &
						emiss_table(T_index,fov,1)*cosd(theta_view)*cosd(theta_view) + &
		 				emiss_table(T_index,fov,2)*sind(theta_view)*sind(theta_view)
				else if (polarization .eq. 2) then !H-pol
				emiss_table_comb(T_index,fov)	=	  &
						emiss_table(T_index,fov,2)*cosd(theta_view)*cosd(theta_view) + &
		 				emiss_table(T_index,fov,1)*sind(theta_view)*sind(theta_view)
				endif
				
			enddo
		enddo
		
		write(file,777) channel
777     format('C:\idl_library\MSU_AMSU_simulation\method_2\data\emiss_tables\amsu_',i2.2,'_emiss_table_sea_ice.dat')
		print *, file
		open(unit= 15,file = file,form = 'BINARY')
		write(15)num_t,T0,Delta_T
		write(15)emiss_table_comb
		close(15)
enddo




end program
