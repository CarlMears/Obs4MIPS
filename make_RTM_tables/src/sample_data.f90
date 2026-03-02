module sample_data

	integer(4),parameter,private		:: num_months = 12
	integer(4),parameter		:: num_lons = 256
	integer(4),parameter		:: num_lats = 128
	integer(4),parameter		:: num_levels = 17

	real(4),dimension(num_lons,num_lats,num_months)					:: ps
	real(4),dimension(num_levels)									:: plev
	real(4),dimension(num_lons,num_lats,num_levels,num_months)		:: ta
	real(4),dimension(num_lons,num_lats,num_months)					:: ts
	real(4),dimension(num_lons,num_lats,num_levels,num_months)      :: hus
	real(4),dimension(num_lons,num_lats)							:: sftlf
	real(4),dimension(num_lons,num_lats,num_months)					:: sic

contains


	subroutine read_sample_data(error)

		integer(4)			:: error
		character(len = 120) :: file

		error = 0

		file = 'E:\AMSU_MSU_simulation\Compare_Weighting_Algorithm_w_PCMDI\model_output\sample_Data.dat'
		open(unit=3,file = file,form='BINARY')

		read(3)ps,plev,ta,ts,hus,sftlf,sic
		close(3)

	end subroutine read_sample_data
end module sample_data