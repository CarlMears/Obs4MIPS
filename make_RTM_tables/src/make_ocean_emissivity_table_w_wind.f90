program make_ocean_emissivity_table

	use nan_support
	use rtm
	use msu_constants

	implicit none
	
	integer(4)			:: num_T 
	real(4)				:: T0 
	real(4)				:: Delta_T 

	integer(4)			:: num_W
	real(4)				:: W0
	real(4)	 			:: delta_W
	
	integer(4)          :: theta_i
	integer(4)          :: delta_theta

	real(4),dimension(0:200,0:30,1:6,1:2)  :: emiss_table             ! temperature,wind,fov,pol (vpol = 1 hpol = 2)
	real(4),dimension(0:200,0:30,1:6)      :: emiss_table_comb        ! temperature,wind,fov
	real(4),dimension(0:200,0:30,1:6)      :: emiss_table_comb_deriv  ! temperature,wind,fov
	real(4),dimension(0:200,0:30,1:6,1:2,1:2)  :: emiss_table_delta_theta  ! temperature,wind,fov,(vpol = 1 hpol = 2),delta_Theta
	real(4),dimension(0:200,0:30,1:6,1:2)  :: emiss_table_comb_delta_theta  ! temperature,wind,fov,(vpol = 1 hpol = 2),delta_Theta

	integer(4)							:: polarization
	integer(4),parameter				:: msu_num_freq = 13
	integer(4)							:: freq_index
	real(4),dimension(msu_num_freq)     :: msu_freq_arr

	integer(4)							:: fov,T_index,W_index,channel
	real(4)								:: tht,theta_view,freq,T,W,sst
	real(4),dimension(2)				:: emiss

	character(len = 120)				:: file

	channel = 4
	polarization = MSU_Polarization(channel)

	! construct an array of frequencies where the calc is performed
	do freq_index = 0,msu_num_freq-1
		msu_freq_arr(freq_index+1) =					&
				MSU_Frequency(channel) -					&
				MSU_BANDWIDTHS(channel)/2.0 +		&
				(1+2*freq_index)*MSU_BANDWIDTHS(channel)/(2.0*msu_num_freq)
	enddo

	num_T = 200
	T0 = 140.0
	Delta_T = 1.0

	num_W = 30
	W0 = 0.0
	Delta_W = 1.0

	emiss_table = 0.0

	do fov = 1,6
		tht = MSU_EIA(fov)
		do freq_index = 1,msu_num_freq
			freq = msu_freq_arr(freq_index)
			print*,freq	
			do T_index = 0,num_T
			   T = T0+Delta_T*T_index
			   do W_index = 0,num_W
			      W = W0 + Delta_W*W_index
				  sst = T - 273.16
				  call surterm(freq,tht,sst,W, emiss)
				  emiss_table(T_index,W_index,fov,:) = emiss_table(T_index,W_index,fov,:) + emiss
			   enddo
			enddo
		enddo
	enddo

	emiss_table = emiss_table/msu_num_freq

	! combine polarizations according to view angle

	do fov = 1,6
		theta_view = MSU_VIEW(fov)
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
	
	emiss_table_delta_theta = 0.0

    do theta_i = 1,2
	    do fov = 1,6
		    tht = MSU_EIA(fov) + (theta_i - 1.5)*1.0
		    do freq_index = 1,msu_num_freq
			    freq = msu_freq_arr(freq_index)
			    print*,freq	
			    do T_index = 0,num_T
			       T = T0+Delta_T*T_index
			       do W_index = 0,num_W
			          W = W0 + Delta_W*W_index
				      sst = T - 273.16
				      call surterm(freq,tht,sst,W, emiss)
				      emiss_table_delta_theta(T_index,W_index,fov,:,theta_i) = emiss_table_delta_theta(T_index,W_index,fov,:,theta_i) + emiss
			       enddo
			    enddo
		    enddo
		enddo
	enddo

	emiss_table_delta_theta = emiss_table_delta_theta/msu_num_freq
	
    ! combine polarizations according to view angle
    
    do theta_i = 1,2
	    do fov = 1,6
		    theta_view = MSU_VIEW(fov)
		    do T_index = 0,num_T
			    do W_index = 0,num_W
				    if (polarization .eq. 1) then !V-pol
				    emiss_table_comb_delta_theta(T_index,W_index,fov,theta_i)	=	 &
						    emiss_table_delta_theta(T_index,W_index,fov,1,theta_i)*cosd(theta_view)*cosd(theta_view) + &
		 				    emiss_table_delta_theta(T_index,W_index,fov,2,theta_i)*sind(theta_view)*sind(theta_view)
				    else if (polarization .eq. 2) then !H-pol
				    emiss_table_comb_delta_theta(T_index,W_index,fov,theta_i)	=	  &
						    emiss_table_delta_theta(T_index,W_index,fov,2,theta_i)*cosd(theta_view)*cosd(theta_view) + &
		 				    emiss_table_delta_theta(T_index,W_index,fov,1,theta_i)*sind(theta_view)*sind(theta_view)
				    endif
			    enddo
		    enddo
	    enddo
	enddo
	
	! calculate derivatives
	
    do fov = 1,6
	    theta_view = MSU_VIEW(fov)
	    do T_index = 0,num_T
		    do W_index = 0,num_W
			    emiss_table_comb_deriv(T_index,W_index,fov)	=	 &
			        emiss_table_comb_delta_theta(T_index,W_index,fov,2) - &
			        emiss_table_comb_delta_theta(T_index,W_index,fov,1)
		    enddo
	    enddo
    enddo
			
	file = 'B:\Users\mears\IDL_6_code\AMSU_MSU_simulation\emiss_tables\msu_4_emiss_table_W.dat'
	open(unit= 15,file = file,form = 'BINARY')
	write(15)num_t,T0,Delta_T,num_w,W0,Delta_W,2
	write(15)emiss_table_comb
	write(15)emiss_table
	write(15)emiss_table_comb_deriv
	close(15)
end program
