module atmos_abs_tables

	implicit none

	integer(4),parameter,private			:: num_t = 200
	integer(4),parameter,private			:: num_p = 110
	integer(4),parameter,private			:: num_q = 150

	real(4),dimension(0:num_t,0:num_p,0:num_q)	:: abs_table_q
	real(4),dimension(0:num_t)				:: cld_abs_table
    real(4)									:: T0,delta_t
	real(4)									:: delta_p
	real(4)									:: delta_q

	integer(4)								:: msu_channel_loaded = -1
	integer(4)								:: msu_channel_loaded_cloud = -1

contains

	subroutine read_abs_table_q(msu_channel)


		implicit none

		integer(4)							:: msu_channel

		character(len = 120)				:: file
		integer(4)							:: numt,nump,numq
		integer(4)							:: ivap,ioxy,channel
		

		write(file,100) msu_channel
100     format('E:\AMSU_MSU_simulation\abs_tables\msu_',i1,'_abs_table_q_per_Pa.dat')

		open(unit= 15,file = file,form = 'BINARY')
		read(15)numt,nump,numq,T0,Delta_t,Delta_p,Delta_q,ivap,ioxy,channel
		read(15)abs_table_q
		close(15)

		msu_channel_loaded = msu_channel
		return
	end subroutine read_abs_table_q

	
	subroutine read_cld_abs_table(msu_channel)


		implicit none

		integer(4)							:: msu_channel

		character(len = 120)				:: file
		integer(4)							:: numt,nump,numq
		integer(4)							:: ivap,ioxy,channel
		

		write(file,100) msu_channel
100     format('E:\AMSU_MSU_simulation\abs_tables\msu_',i2.2,'_cld_abs_table.dat')

		open(unit= 15,file = file,form = 'BINARY')
		read(15)T0,Delta_t,numt
		read(15)cld_abs_table
		close(15)

		msu_channel_loaded_cloud = msu_channel

		return
	end subroutine read_cld_abs_table

	real(4) function find_abs_q(t,p,q,error)

		! this routine finds the absorption coefficient (in nepers/Pa) as
		! a function of temperature t (Kelvin), pressure p (hPa), and specific humidity q (kg/kg)
		! by interpolating a table.  The subroutine read_abs_table_q must be called before this
		! routine is used.  

		use NAN_support

		real(4)				:: t    ! temperature
		real(4)				:: p    ! pressure
		real(4)				:: q    ! spec. humidity (kg h20/kg (h20 + dry air))
		integer(4)			:: error

		integer(4)			:: t1,p1,q1
		real(4)				:: t_scaled,p_scaled,q_scaled
		real(4)				:: wt,wp,wq

		real(4)				:: abs_interp

		if (msu_channel_loaded < 1) then
			error = -1
			find_abs_q = NAN
			return
		endif

		error = 0

		t_scaled = (t-t0)/delta_t
		t1 = floor(t_scaled)
		wt = t_scaled - t1

		if (t1 < 0) then
			t1 = 0
			wt = 0.0
			error = ior(error,1)
		endif

		if (t1 > (num_t - 1)) then   ! extrapolate -- set error to +1
			t1 = num_t -1
			wt = t_scaled - t1
			error = ior(error,1)
		endif

		p_scaled = p/delta_p
		p1 = floor(p_scaled)
		wp = p_scaled - p1

		if (p1 < 0) then 
			p1 = 0
			wp = 0.0
			error = ior(error,2)
		endif

		if (p1 > (num_p-1)) then   ! extrapolate
			p1 =num_p-1
			wp = p_scaled - p1
			error = ior(error,2)
		endif

		q_scaled = q/delta_q
		q1 = floor(q_scaled)
		wq = q_scaled - q1

		if (q1 < 0) then 
			q1 = 0
			wq = 0.0
			error = ior(error,4)
		endif

		if (q1 > (num_q-1)) then   ! extrapolate
			q1 = num_q-1
			wq = q_Scaled - q1
			error = ior(error,4)
		endif

		abs_interp = (1.0-wt)*((1.0-wp)*((1.0-wq)*abs_table_q(t1,p1,    q1) + wq*abs_table_q(t1  ,p1,  q1+1)) +     &
									 wp*((1.0-wq)*abs_table_q(t1,p1+1,  q1) + wq*abs_table_q(t1  ,p1+1,q1+1))) +    &
						   wt*((1.0-wp)*((1.0-wq)*abs_table_q(t1+1,p1,  q1) + wq*abs_table_q(t1+1,p1,  q1+1)) +     &
									 wp*((1.0-wq)*abs_table_q(t1+1,p1+1,q1) + wq*abs_table_q(t1+1,p1+1,q1+1)))

		find_abs_q = abs_interp

		return

	end function find_abs_q

	real(4) function find_cld_abs(T,rho_cld,error)
		
		real(4)					:: T
		real(4)					:: rho_cld
		integer(4)				:: error

		real(4)					:: t_scaled
		integer(4)				:: t1
		real(4)					:: wt

		real(4)					:: abs_interp

		t_scaled = (t-t0)/delta_t
		t1 = floor(t_scaled)
		wt = t_scaled - t1

		if (t1 < 0) then
			t1 = 0 
			wt = 0.0
			error = 1
		endif

		if (t1 > num_t-1) then ! extrapolate
			t1 = num_t-1
			wt = t_scaled - t1
			error = 1
		endif 

		abs_interp = (1.0-wt)*cld_abs_table(t1) + wt*cld_abs_table(t1+1)

		! now multiply by rho in g/cm^3 to get absorbtion in nepers/cm

		abs_interp = abs_interp*rho_cld

		find_cld_abs = abs_interp

		return

	end function find_cld_abs

end module atmos_abs_tables

		
