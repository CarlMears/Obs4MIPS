program make_ocean_emissivity_table

	use nan_support
	use rtm
	use msu_constants
	use amsu_constants

	implicit none

	
	integer(4)			:: num_T 

	real(4)				:: T0 
	real(4)				:: Delta_T 

	integer(4)			:: num_W
	real(4)				:: W0
	real(4)	 			:: delta_W



	

	real(4),dimension(0:200,0:30,1:15,1:2)  :: emiss_table  ! temperature,fov,pol (vpol = 1 hpol = 2)
	real(4),dimension(0:200,0:30,1:15)      :: emiss_table_comb  ! temperature,fov

	integer(4)							:: polarization
	integer(4),parameter				:: amsu_num_freq = 13
	integer(4)							:: freq_index
	real(4),dimension(amsu_num_freq)     :: amsu_freq_arr

	integer(4)							:: fov,T_index,W_index,amsu_channel
	real(4)								:: tht,theta_view,freq,T,W,sst
	real(4),dimension(2)				:: emiss

	character(len = 120)				:: file


	do amsu_channel = 1,2
		polarization = AMSU_A_Polarization(amsu_channel)


		! construct an array of frequencies where the calc is performed
		do freq_index = 0,amsu_num_freq-1
			amsu_freq_arr(freq_index+1) =					&
				AMSU_A_Freq(amsu_channel) -					&
				AMSU_A_BANDWIDTH(amsu_channel)/2.0 +				&
				(1+2*freq_index)*AMSU_A_BANDWIDTH(amsu_channel)/(2.0*amsu_num_freq)
		enddo


		num_T = 200
		T0 = 140.0
		Delta_T = 1.0

		num_W = 30
		W0 = 0.0
		Delta_W = 1.0

		emiss_table = 0.0


		do fov = 1,15
			tht = AMSU_NOM_EIAS(fov)
			do freq_index = 1,amsu_num_freq
				freq = amsu_freq_arr(freq_index)
				print*,freq,fov	
				do T_index = 0,num_T
				   T = T0+Delta_T*T_index
				   sst = T - 273.16
				   do W_index = 0,num_W
					  W = W0 + Delta_W*W_index
					  call surterm(freq,tht,sst,w, emiss)
					  !emiss_table(T_index,fov,:) = emiss_table(T_index,fov,:) + emiss
					  emiss_table(T_index,W_index,fov,:) = emiss_table(T_index,W_index,fov,:) + emiss
				   enddo
				enddo
			enddo
		enddo

		emiss_table = emiss_table/amsu_num_freq

		! combine polarizations according to view angle

		do fov = 1,15
			theta_view = AMSU_VIEW_ANGLES(fov)
			do T_index = 0,num_T
				do W_index = 0,num_W
					if (polarization .eq. 1) then !V-pol
						emiss_table_comb(T_index,W_index,fov)	=	 &
							emiss_table(T_index,W_index,fov,1)*cosd(theta_view)*cosd(theta_view) + &
		 					emiss_table(T_index,W_index,fov,2)*sind(theta_view)*sind(theta_view)
					else if (polarization .eq. 2) then !H-pol
						emiss_table_comb(T_index,W_index,fov)	=	  &
							emiss_table(T_index,W_index,fov,2)*cosd(theta_view)*cosd(theta_view) + &
		 					emiss_table(T_index,W_index,fov,1)*sind(theta_view)*sind(theta_view)
					endif
				enddo
			enddo
		enddo

	
		write(file,112) amsu_channel
112     format('C:\idl_library\MSU_AMSU_simulation\method_2\data\emiss_tables\amsu_',i2.2,'_emiss_table_W.temp.dat')

		print *,file
		open(unit= 15,file = file,form = 'BINARY')
		write(15)num_t,T0,Delta_T,num_w,W0,Delta_W,amsu_channel
		write(15)emiss_table_comb
		write(15)emiss_table
		close(15)
	enddo




end program
