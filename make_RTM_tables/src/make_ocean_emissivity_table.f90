program make_ocean_emissivity_table

	use nan_support
	use rtm
	use msu_constants

	implicit none

	
	integer(4)			:: num_T 

	real(4)				:: T0 
	real(4)				:: Delta_T 

	

	real(4),dimension(0:200,1:6,1:2)  :: emiss_table  ! temperature,fov,pol (vpol = 1 hpol = 2)
	real(4),dimension(0:200,1:6)  :: emiss_table_comb  ! temperature,fov

	integer(4)							:: polarization
	integer(4),parameter				:: msu_num_freq = 13
	integer(4)							:: freq_index
	real(4),dimension(msu_num_freq)     :: msu_freq_arr

	integer(4)							:: fov,T_index,channel
	real(4)								:: tht,theta_view,freq,T,sst
	real(4),dimension(2)				:: emiss

	character(len = 120)				:: file


	channel = 4
	polarization = MSU_Polarization(channel)


	! construct an array of frequencies where the calc is performed
	do freq_index = 0,msu_num_freq-1
		msu_freq_arr(freq_index+1) =					&
			MSU_Frequency(channel) -					&
			MSU_BANDWIDTHS(channel)/2.0 +				&
			(1+2*freq_index)*MSU_BANDWIDTHS(channel)/(2.0*msu_num_freq)
	enddo


	num_T = 200
	T0 = 140.0
	Delta_T = 1.0

	emiss_table = 0.0


	do fov = 1,6
		tht = MSU_EIA(fov)
		do freq_index = 1,msu_num_freq
			freq = msu_freq_arr(freq_index)
			print*,freq	
			do T_index = 0,num_T
			   T = T0+Delta_T*T_index
			   sst = T - 273.16
			   call surterm(freq,tht,sst,8.0, emiss)
			   emiss_table(T_index,fov,:) = emiss_table(T_index,fov,:) + emiss
			enddo
		enddo
	enddo

	emiss_table = emiss_table/13.0

	! combine polarizations according to view angle

	do fov = 1,6
		theta_view = MSU_VIEW(fov)
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

			
	file = 'E:\AMSU_MSU_simulation\emiss_tables\msu_4_emiss_table_2.dat'
	open(unit= 15,file = file,form = 'BINARY')
	write(15)num_t,T0,Delta_T,channel
	write(15)emiss_table_comb
	close(15)




end program
