    Module calc_tb_multiview

        use rtm_tables
        use rtm
        use MSU_Constants

        integer(4),parameter        :: OCEAN = 1
        integer(4),parameter        :: LAND  = 2



    contains
    
        subroutine calc_tb_multiview_table(        t,               &  ! temperature
                                                p,               &    ! pressure
                                                q,               &    ! specific humidity
                                                num_levels,      &  ! number of levels
                                                msu_channel,     &  ! MSU channel number (1-4)
                                                theta,             &  ! EIA angle (degrees)  array(1:num_views)
                                                num_views,         &  ! number of views
                                                wind,             &  ! windspeed
                                                emissivity,         &    ! emissivity for each pol, surface, view
                                                surf_wt,         &  ! weight from surface emission for each surface,view
                                                space_wt,         &  ! weight from space
                                                tb,              &    ! brightness temperature at TOA for each surface,view
                                                error)

            implicit none
             
            integer(4)                             :: num_levels 
            real(4),dimension(0:num_levels)      :: t        
            real(4),dimension(0:num_levels)      :: p        
            real(4),dimension(0:num_levels)      :: q
            integer(4)                             :: msu_channel
            integer(4)                             :: num_views
            integer(4)                             :: view_num
            real(4),dimension(num_views)         :: theta
            real(4),dimension(num_views)         :: theta_view
            real(4)                                 :: wind
            real(4),dimension(2,num_views)         :: surf_wt        
            real(4),dimension(2,num_views)         :: space_wt

            real(4),dimension(2,num_views)         :: tb
            
                
             real(4),dimension(0:num_levels)      :: transmittance_up   ! 
            real(4),dimension(0:num_levels)      :: transmittance_down ! from bottom of each level
            real(4)                                 :: tbup
            real(4)                                 :: tbdw
            integer(4)                             :: level
            integer(4)                             :: error

            real(4),dimension(2,num_views)         :: emissivity

            real(4),dimension(0:NUM_LEVELS)         :: scl_interface_pressure
            real(4),dimension(0:NUM_LEVELS)         :: Total_abs

            !real(4),dimension(2,num_views)       :: tot_wt
            integer(4)                             :: surf_type
            real(4)                                 :: sst


            ! find the absorbtion coefficients for each level
            ! these are independent of angle, so they only need to be calculated 
            ! once for each profile/frequency

            if (maxval(p) .gt. 2000) then
                scl_interface_pressure = p/100.0
            else
                scl_interface_pressure = p
            endif

            ! calculate absorption coefficients at each level
            ! "4" means ivap = 4: abh2o_rk_modified (modified Rosenkranz -- used in skytemp2 8/2009
            ! "1" means ioxy = 1: ABO2_RK Rosenkranz 1998
            ! absorption coefficients are returned in nepers per Km

            do level = 0,num_levels
                  total_abs(level) = find_abs_q(t(level),scl_interface_pressure(level),q(level),error)
            enddo

            do view_num = 1,num_views     
                call ATM_TRAN_P(    num_levels,                &
                                    theta(view_num),        &
                                    t,                        &
                                    scl_interface_pressure, &
                                    total_Abs,                &
                                    transmittance_up,        &
                                    transmittance_down,        &
                                    tbdw,                    &
                                    tbup)

                ! calculate surface emissivities
                emissivity(OCEAN,view_num) =  find_emiss(t(0),wind,view_num,error)
                emissivity(LAND,view_num)  =  0.9  ! land polarization set to 0.9

                tb(OCEAN,view_num) = ((tbdw*(1.0-emissivity(OCEAN,view_num)))+emissivity(OCEAN,view_num)*t(0))*transmittance_up(0) + tbup
                tb(LAND,view_num) = ((tbdw*(1.0-emissivity(LAND,view_num)))+emissivity(LAND,view_num)*t(0))*transmittance_up(0) + tbup
                
                surf_wt(OCEAN,view_num) = transmittance_up(0) * emissivity(OCEAN,view_num)
                surf_wt(LAND, view_num) = transmittance_up(0) * emissivity(LAND ,view_num)

                space_wt(OCEAN,view_num) = (1.0-emissivity(OCEAN,view_num)) *transmittance_up(0)*transmittance_down(num_levels)
                 space_wt(LAND,view_num) =  (1.0-emissivity(LAND,view_num)) *transmittance_up(0)*transmittance_down(num_levels)
            enddo
            error = 0

        end subroutine calc_tb_multiview_table
    end module calc_tb_multiview