program make_absorption_table

	use nan_support
	use rtm
	use msu_constants
	use amsu_constants

	implicit none

	integer(4)						:: T_index,P_index,q_index
	real(4)							:: T,P,PV,q

	integer(4)						:: ivap = 4
	integer(4)						:: ioxy = 1

	real(4)							:: ao,av,total_abs,freq,ao2,av2,ao3
	integer(4)						:: freq_index

	real(8),parameter				:: M_W_air = 2.8966D-2
	real(8),parameter				:: M_W_H2O  = 1.8015324d-2    ! kg/mol
	real(8),parameter				:: R_GAS    = 8.3145112d0
	real(8),parameter				:: g        = 9.80665

	real(8)							:: c_air
	real(8)							:: c_h2o
	real(8)							:: rho_dry
	real(8)							:: rho_vap

	real(4),dimension(600,3)        :: abs_array

		
	character(len=120)		:: file

	c_air = R_GAS/M_W_AIR
	c_h2o = R_GAS/M_W_H2O

	T = 210.0
	P = 100.0
	q = 0.0



	do freq_index = 1,600
		freq = 53.0 + 0.01*freq_index
		PV = p*q/(0.622+0.378*q)

		call FDABSCOEFF(ivap,1,freq,P,T,PV, av,ao)
		call FDABSCOEFF(ivap,2,freq,P,T,PV, av,ao2)
		call FDABSCOEFF(ivap,3,freq,P,T,PV, av,ao3)

		write(*,*) freq,ao,ao2,ao3
		abs_array(freq_index,1) = ao
		abs_array(freq_index,2) = ao2
		abs_array(freq_index,3) = ao3

	enddo

	file = 'E:\AMSU_MSU_simulation\abs_tables\abs_array_by_freq_P_100.dat'
	open(unit= 15,file = file,form = 'BINARY')
	write(15)T,P,q
	write(15)abs_array
	close(15)


end program