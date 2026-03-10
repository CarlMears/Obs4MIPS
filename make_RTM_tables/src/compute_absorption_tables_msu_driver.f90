! driver program for AMSU RTM tables
program make_absorption_table

    use nan_support
    use msu_constants
    use amsu_constants

    use make_absorption_table_t_q, only: compute_absorption_tables_msu

    implicit none

    integer(4), parameter :: num_T = 200
    integer(4), parameter :: num_p = 110
    integer(4), parameter :: num_q = 150

    real(4), parameter :: T0 = 140.0
    real(4), parameter :: Delta_T = 1.0
    real(4), parameter :: Delta_P = 10.0
    real(4), parameter :: Delta_q = 0.001

    integer(4), parameter :: ivap = 4
    integer(4), parameter :: ioxy = 4
    integer(4), parameter :: channel = 5

    real(4), dimension(0:num_T,0:num_p,0:num_q) :: abs_table
    real(4), dimension(0:num_T,0:num_p,0:num_q) :: abs_table_per_Pa

    character(len=120) :: file

    call compute_absorption_tables_msu(channel, ivap, ioxy, num_T, num_p, num_q, T0, Delta_T, Delta_P, Delta_q, &
                                abs_table, abs_table_per_Pa)

    write(file,111) channel, ioxy, ivap
111   format('/mnt/m/Obs4MIPs/make_RTM_tables/data/abs_tables/msu_',i2.2,'_abs_table_q.',i1,'.',i1,'.dat')
    print *, 'Writing absorption table to file: ', trim(file)
    open(unit=15, file=file, access='stream', form='unformatted')
    write(15) num_T, num_p, num_q, T0, Delta_T, Delta_P, Delta_q, ivap, ioxy, channel
    write(15) abs_table
    close(15)

    write(file,112) channel, ioxy, ivap
112   format('/mnt/m/Obs4MIPs/make_RTM_tables/data/abs_tables/msu_',i2.2,'_abs_table_q_per_Pa.',i1,'.',i1,'.dat')
    print *, 'Writing absorption table per Pa to file: ', trim(file)
    open(unit=15, file=file,access='stream', form='unformatted')
    write(15) num_T, num_p, num_q, T0, Delta_T, Delta_P, Delta_q, ivap, ioxy, channel
    write(15) abs_table_per_Pa
    close(15)

    print *, 'Done.'

end program make_absorption_table