program test_rtm_tables

	use rtm_tables
	use nan_support

	real(4)				:: t = 290.4
	real(4)				:: p = 973.0
	real(4)				:: q = 0.0054
	real(4)				:: w = 7.6
	real(4)				:: cld = 0.002

	integer(4)			:: msu_channel = 2
	integer(4)			:: fov = 1


	real(4)				:: abs 
	real(4)				:: cld_abs
	real(4)				:: emiss
	integer(4)			:: error

	call init_nan


	call read_abs_table_q(msu_channel)
	call read_cld_abs_table(msu_channel)
	call read_ocean_emiss_table(msu_channel)

	abs = find_abs_q(t,p,q,error)
	print *,abs
	cld_abs = find_cld_abs(t,cld,error)
	print *, cld_abs
	emiss = find_emiss(t,w,fov,error)
	print *,emiss


	end

	

