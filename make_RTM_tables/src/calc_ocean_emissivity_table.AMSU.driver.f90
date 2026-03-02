program calc_ocean_emissivity_table_AMSU_driver
    
    use calc_ocean_emissivity_table_AMSU, only: calc_ocean_emissivity_table_amsu
    implicit none

    integer(4),parameter :: num_T = 200
    integer(4),parameter :: num_W = 30

    real(4),parameter :: T0 = 140.0
    real(4),parameter :: Delta_T = 1.0

    real(4),parameter :: W0 = 0.0
    real(4),parameter :: Delta_W = 1.0

    integer(4) :: channel
    real(4), dimension(0:200,0:30,1:15) :: emiss_table_comb

    character(len=120) :: file

    channel = 5
    call calc_ocean_emissivity_table_amsu_dsb(channel,  &
                                              num_T, num_W, &
                                              T0, Delta_T, W0, Delta_W, &
                                              emiss_table_comb)

	write(file,111)channel		
111 format('/mnt/m/Obs4MIPs/make_RTM_tables/data/ocean_emiss_tables/amsu_',i2.2,'_ocean_emiss_table_W.dat')
    print *,file
	open(unit= 15,file = file,, access='stream', form='unformatted')
	write(15)num_t,T0,Delta_T,num_w,W0,Delta_W,channel
	write(15)emiss_table_comb
	close(15)

end program calc_ocean_emissivity_table_AMSU_driver