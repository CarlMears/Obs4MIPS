program make_absorption_table

	use nan_support
	use rtm
	use msu_constants

	implicit none

	integer(4)			:: num_T 
	integer(4)			:: num_p 
	integer(4)			:: num_q 

	real(4)				:: T0 
	real(4)				:: Delta_T 
	real(4)				:: Delta_P 
	real(4)				:: Delta_q 

	real(4),dimension(0:200,0:110,0:150)  :: abs_table
	real(4),dimension(0:200,0:110,0:150)  :: abs_table_per_Pa

	integer(4)						:: T_index,P_index,q_index
	real(4)							:: T,P,PV,q

	integer(4)						:: ioxy = 5
	integer(4)						:: ivap = 4


	real(4)							:: ao,av,total_abs,freq

	integer(4)						:: channel
	integer(4),parameter			:: msu_num_freq = 13  !3  ! should be 13 or so
	integer(4)						:: freq_index
	real(4),dimension(msu_num_freq) :: msu_freq_arr
	
	real(4),dimension(0:msu_num_freq-1,0:200,0:110,0:150)  :: abs_table_by_freq
	real(4),dimension(0:msu_num_freq-1,0:200,0:110,0:150)  :: abs_table_per_Pa_by_freq

	real(8),parameter				:: M_W_air = 2.8966D-2
	real(8),parameter				:: M_W_H2O  = 1.8015324d-2    ! kg/mol
	real(8),parameter				:: R_GAS    = 8.3145112d0
	real(8),parameter				:: g        = 9.80665

	real(8)							:: c_air
	real(8)							:: c_h2o
	real(8)							:: rho_dry
	real(8)							:: rho_vap

		
	character(len=120)		:: file

	c_air = R_GAS/M_W_AIR
	c_h2o = R_GAS/M_W_H2O
	
	!call FDABSCOEFF(4,4,MSU_Frequency(2),850.,270.,10.0, av,ao)
    !print *,av,ao
	!call FDABSCOEFF(4,1,MSU_Frequency(2),850.,270.,10.0, av,ao)
    !print *,av,ao
    !call FDABSCOEFF(1,1,MSU_Frequency(2),850.,270.,10.0, av,ao)
    !print *,av,ao

    !stop
    
	channel = 2

	! construct an array of frequencies where the calc is performed
	do freq_index = 0,msu_num_freq-1
		msu_freq_arr(freq_index+1) =					&
				MSU_Frequency(channel) -					&
				MSU_BANDWIDTHS(channel)/2.0 +		&
				(1+2*freq_index)*MSU_BANDWIDTHS(channel)/(2.0*msu_num_freq)
	enddo




	num_T = 200
	num_p = 110
	num_q = 150

	T0 = 140.0
	Delta_T = 1.0
	Delta_P = 10
	Delta_q = 0.001

	
    abs_table = 0.0
	abs_table_per_Pa = 0.0
	do freq_index = 1,msu_num_freq
		freq = msu_freq_arr(freq_index)
		print*,freq	
		do T_index = 0,num_T
		   T = T0+Delta_T*T_index
		   do P_index = 0,num_p
				P = Delta_P*P_index
				do q_index = 0,num_q
					q = Delta_q*q_index
					total_abs = 0.0

					PV = p*q/(0.622+0.378*q)

					call FDABSCOEFF(ivap,ioxy,freq,P,T,PV, av,ao)
					!print *,av,ao
					!call FDABSCOEFF(4,4,freq,P,T,PV, av,ao)
					!print *,av,ao
					total_abs = av + ao
					abs_table(T_index,P_index,q_index) = abs_table(T_index,P_index,q_index) + total_abs				
					abs_table_by_freq(freq_index-1,T_index,P_index,q_index) = total_abs
					!print *,t,p,q,total_Abs
					! find density

					rho_dry = (p-pv)/(c_air*T)
					rho_vap = pv/(c_h2o*T)
					abs_table_per_Pa(T_index,P_index,q_index) = abs_table_per_Pa(T_index,P_index,q_index) + &
																total_abs*0.001/((rho_dry + rho_vap) * g)
	               	abs_table_per_Pa_by_freq(freq_index-1,T_index,P_index,q_index) = total_abs*0.001/((rho_dry + rho_vap) * g)

				
				enddo
			enddo
		enddo
	enddo

	abs_table = abs_table/msu_num_freq
	abs_table_per_Pa = abs_table_per_Pa/msu_num_freq
	
	write(file,111)channel,ioxy,ivap
	print *,file
	111 format('B:\idl_library\MSU_AMSU_simulation\method_2\data\abs_tables\msu_',i1,'_abs_table_q.',i1,'.',i1,'.dat')

	!file = 'B:\idl_library\MSU_AMSU_simulation\method_2\data\abs_tables\msu_2_abs_table_q.4.4.dat'
	open(unit= 15,file = file,form = 'BINARY')
	write(15)num_t,num_p,num_q,T0,Delta_T,Delta_P,Delta_q,ivap,ioxy,channel
	write(15)abs_table
	close(15)

	!file = 'B:\idl_library\MSU_AMSU_simulation\method_2\data\abs_tables\msu_2_abs_table_q_per_Pa.4.4.dat'
	write(file,112)channel,ioxy,ivap
	print *,file
	112 format('B:\idl_library\MSU_AMSU_simulation\method_2\data\abs_tables\msu_',i1,'_abs_table_q_per_Pa.',i1,'.',i1,'.dat')

	open(unit= 15,file = file,form = 'BINARY')
	write(15)num_t,num_p,num_q,T0,Delta_T,Delta_P,Delta_q,ivap,ioxy,channel
	write(15)abs_table_per_Pa
	close(15)

	write(file,113)channel,ioxy,ivap
	print *,file
	113 format('B:\idl_library\MSU_AMSU_simulation\method_2\data\abs_tables\msu_',i1,'_abs_table_q_by_freq.',i1,'.',i1,'.dat')
	!file = 'B:\idl_library\MSU_AMSU_simulation\method_2\data\abs_tables\msu_2_abs_table_q_by_freq.4.4.dat'
	open(unit= 15,file = file,form = 'BINARY')
	write(15)num_t,num_p,num_q,T0,Delta_T,Delta_P,Delta_q,ivap,ioxy,channel
	write(15)msu_num_freq,msu_freq_arr
	write(15)abs_table_by_freq
	
	close(15)

	!file = 'B:\idl_library\MSU_AMSU_simulation\method_2\data\abs_tables\msu_2_abs_table_q_by_freq_per_Pa.4.4.dat'
	write(file,114)channel,ioxy,ivap
	114 format('B:\idl_library\MSU_AMSU_simulation\method_2\data\abs_tables\msu_',i1,'_abs_table_q_by_freq_per_Pa.',i1,'.',i1,'.dat')
    print *,file
	open(unit= 15,file = file,form = 'BINARY')
	write(15)num_t,num_p,num_q,T0,Delta_T,Delta_P,Delta_q,ivap,ioxy,channel
	write(15)msu_num_freq,msu_freq_arr
	write(15)abs_table_per_Pa_by_freq
	close(15)

end program