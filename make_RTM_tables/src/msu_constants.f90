
module msu_constants

    use date_and_time

    real(8),parameter        :: A_SMALL_NUMBER = 1.0d-10
    real(8),parameter        :: TWO_PI = 6.283185307d0
    real(8),parameter        :: PI = 3.141592654d0

    real,dimension(4),parameter  :: MSU_FREQUENCY = (/50.299178,53.740796,54.960951,57.949882/)
    real,dimension(4),parameter     :: MSU_BANDWIDTH = (/0.200,0.200,0.200,0.200/)
    real,dimension(6),parameter  :: MSU_EIA =  (/0.0,10.71,21.51,32.51,43.91,56.19/)
    real,dimension(6),parameter  :: MSU_VIEW = (/0.0,9.47,18.94,28.41,37.88,47.35/)
    real,dimension(6),parameter  :: COS2_MSU_VIEW = (/1.0,0.9729,0.8946,0.7736,0.6230,0.4590/)

    integer(4),parameter,dimension(4) :: MSU_Polarization = & ! 1 = V, 2 = H
                (/1,2,1,2/)


    real(8),parameter            :: MSU_SCAN_TIME = 2.962963d-4 !typical interscan time
    real(8),parameter            :: MSU_ORBIT_TIME = 0.070799d0

    integer(4),parameter        :: FILE_LEN = 120
    integer(4),parameter        :: MAX_SCANS_PER_ORBIT=300

    character(len=100),parameter  :: MSU_L1B_PATH_TEST    = '\\dell-p1700d\f\msu_level_0_test\'
    character(len=100),parameter  :: MSU_L1B_PATH        = '\\dell-p1700d\g\msu_level_0\' !is operational directory

    character(len=100),parameter  :: MSU_L2A_PATH_TEST    = '\\dell-p1700d\f\msu_level_1A_test\'
    character(len=100),parameter  :: MSU_L2A_PATH        = '\\Nasserver5\MSU_AMSU_Data\MSU_L2A\' !is operational directory

    ! MSU array constants

    integer,parameter       ::  NUM_SATS = 9

    integer,parameter        ::  TIROS_N  = 1
    integer,parameter        ::  NOAA_06  = 2
    integer,parameter        ::  NOAA_07  = 3
    integer,parameter        ::  NOAA_08  = 4
    integer,parameter        ::  NOAA_09  = 5
    integer,parameter        ::  NOAA_10  = 6
    integer,parameter        ::  NOAA_11  = 7
    integer,parameter        ::  NOAA_12  = 8
    integer,parameter        ::  NOAA_14  = 9


    integer,parameter        ::    NUM_CHANNELS = 4 
    integer,parameter        ::    NUM_FOVS = 11
    integer,parameter        ::  N_FOV = 6
    integer,parameter        ::  NUM_NODES = 2
    integer,parameter        ::  CENTER_FOV = 6
    integer,parameter        ::    NUM_VIEW_ANGLES = 5
    integer,parameter        ::    NUM_SCAN_POSITIONS = 14
    integer,parameter        ::    SUBTRACK = 6
    integer,parameter        ::    REC_LEN  = 336
    integer,parameter        ::  MAX_COUNTS = 4095  !????

    integer,parameter        ::  MSU_YEAR_OFFSET = 1978
    integer,parameter        ::  NUM_MONTHS = 12
    integer,parameter        ::  NUM_YEARS = 3
    integer,parameter        ::  NUM_DAYS = 366

    integer(4),parameter,dimension(NUM_SATS)  :: START_YEAR = (/1978,1979,1981,1983,1984,1986,1988,1991,1995/)
    integer(4),parameter,dimension(NUM_SATS)  :: START_ODAY = (/294,0,0,0,0,0,0,0,0/)

    integer(4),parameter,dimension(NUM_SATS)  :: FIRST_PENTAD = (/59,110,255,389,509,651,785,977,1243/)
    integer(4),parameter,dimension(NUM_SATS)  :: FIRST_DAY_NUM_1978 = (/294,546,1272,1941,2541,3251,3923,4885,6211/)

    integer(4),parameter,dimension(NUM_SATS)  :: END_YEAR   = (/1981,0,0,0,0,0,0,0,0/)
    integer(4),parameter,dimension(NUM_SATS)  :: END_ODAY    = (/58,0,0,0,0,0,0,0,0/)

    character(20),dimension(NUM_SATS),parameter :: sat_names =(/'TIROS-N','NOAA-6 ', 'NOAA-7 ', 'NOAA-8 ','NOAA-9 ', &
                                                                'NOAA-10','NOAA-11','NOAA-12','NOAA-14'/)
    character(7),dimension(NUM_SATS),parameter :: sat_names_2 =(/'TIROS-N','NOAA-06', 'NOAA-07', 'NOAA-08','NOAA-09', &
                                                                 'NOAA-10','NOAA-11','NOAA-12','NOAA-14'/)

    integer(4),dimension(NUM_SATS),parameter           :: MAX_ORBITS = (/12138,38399,18842,12840,20067,24985,49715,51124,40373/)



    ! constants associated with input/output files
       integer,parameter        ::    MSU_LU            = 2
    integer,parameter        ::  TS_LU            = 3
    integer,parameter        ::  MSU_1B_LU        = 4
    integer,parameter        ::  MSU_1B_GEO_LU    = 5
    !integer,parameter        ::  LOG_LU            = 7
    integer,parameter        ::  LU_MSU_TBS      = 8

    integer,parameter        ::  MSU_L1B_BASE    = 10
    integer,parameter        ::  MSU_L1C_BASE    = 15
    integer,parameter        ::  GRID_PT_BASE_LU    = 25
    integer,parameter        ::  ZONAL_LU_BASE    = 30
    integer,parameter        ::  TEMP_LU         = 35

    character(80),dimension(NUM_SATS),parameter    ::    stage1_filename =(/'\\DELL-p333A\Z\MSU_Level_1A\filter1_msu_01.dat', &
                                                                       '\\dell-p333a\Z\MSU_Level_1A\filter1_msu_02.dat', &
                                                                       '\\dell-p333a\Z\MSU_Level_1A\filter1_msu_03.dat', &
                                                                       '\\dell-p333a\Z\MSU_Level_1A\filter1_msu_04.dat', &
                                                                       '\\dell-p333a\Z\MSU_Level_1A\filter1_msu_05.dat', &        
                                                                       '\\dell-p333a\Z\MSU_Level_1A\filter1_msu_06.dat', &
                                                                       '\\dell-p333a\Z\MSU_Level_1A\filter1_msu_07.dat', &
                                                                       '\\dell-p333a\Z\MSU_Level_1A\filter1_msu_08.dat', &
                                                                       '\\dell-p333a\Z\MSU_Level_1A\filter1_msu_09.dat'/)


     character(80),dimension(NUM_SATS),parameter    :: Level_1b_geolocation_filename =(/ &
                                                                       '\\Dell-p866a\f\msu_level_1b\TIROS-N_GEO.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-06_GEO.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-07_GEO.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-08_GEO.L1B', &        
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-09_GEO.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-10_GEO.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-11_GEO.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-12_GEO.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-14_GEO.L1B'/)


     character(80),dimension(NUM_SATS),parameter    :: Level_1b_calibration_filename =(/ &
                                                                       '\\Dell-p866a\f\msu_level_1b\TIROS-N_CAL.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-06_CAL.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-07_CAL.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-08_CAL.L1B', &        
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-09_CAL.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-10_CAL.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-11_CAL.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-12_CAL.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-14_CAL.L1B'/)

       character(80),dimension(NUM_SATS),parameter    :: Level_1C_geolocation_filename =(/ &
                                                                       '\\Dell-p866a\f\msu_level_1c\TIROS-N_GEO.L1C', &
                                                                       '\\Dell-p866a\f\msu_level_1c\NOAA-06_GEO.L1C', &
                                                                       '\\Dell-p866a\f\msu_level_1c\NOAA-07_GEO.L1C', &
                                                                       '\\Dell-p866a\f\msu_level_1c\NOAA-08_GEO.L1C', &        
                                                                       '\\Dell-p866a\f\msu_level_1c\NOAA-09_GEO.L1C', &
                                                                       '\\Dell-p866a\f\msu_level_1c\NOAA-10_GEO.L1C', &
                                                                       '\\Dell-p866a\f\msu_level_1c\NOAA-11_GEO.L1C', &
                                                                       '\\Dell-p866a\f\msu_level_1c\NOAA-12_GEO.L1C', &
                                                                       '\\Dell-p866a\f\msu_level_1c\NOAA-14_GEO.L1C'/)

     character(80),dimension(NUM_SATS),parameter    :: Level_1b_v2_geolocation_filename = (/ &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\TIROS-N_GEO.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-06_GEO.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-07_GEO.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-08_GEO.L1Bv2', &        
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-09_GEO.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-10_GEO.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-11_GEO.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-12_GEO.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-14_GEO.L1Bv2'/)


     character(80),dimension(NUM_SATS),parameter    ::  Level_1b_v2_calibration_filename = (/ &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\TIROS-N_CAL.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-06_CAL.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-07_CAL.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-08_CAL.L1Bv2', &        
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-09_CAL.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-10_CAL.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-11_CAL.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-12_CAL.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-14_CAL.L1Bv2'/)

      character(80),dimension(NUM_SATS),parameter    ::  Level_1b_channel_1_filename = (/&
                                                                       '\\Dell-p866a\f\msu_level_1b\TIROS-N_CH1.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-06_CH1.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-07_CH1.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-08_CH1.L1B', &        
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-09_CH1.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-10_CH1.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-11_CH1.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-12_CH1.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-14_CH1.L1B'/)


      character(80),dimension(NUM_SATS),parameter    ::  Level_1b_channel_2_filename = (/&
                                                                       '\\Dell-p866a\f\msu_level_1b\TIROS-N_CH2.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-06_CH2.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-07_CH2.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-08_CH2.L1B', &        
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-09_CH2.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-10_CH2.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-11_CH2.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-12_CH2.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-14_CH2.L1B'/)
    character(80),dimension(NUM_SATS),parameter    :: Level_1b_v2_channel_1_filename =(/&
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\TIROS-N_CH1.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-06_CH1.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-07_CH1.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-08_CH1.L1Bv2', &        
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-09_CH1.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-10_CH1.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-11_CH1.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-12_CH1.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-14_CH1.L1Bv2'/)

    character(80),dimension(NUM_SATS),parameter    :: Level_1b_v2_channel_2_filename =(/&
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\TIROS-N_CH2.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-06_CH2.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-07_CH2.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-08_CH2.L1Bv2', &        
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-09_CH2.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-10_CH2.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-11_CH2.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-12_CH2.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-14_CH2.L1Bv2'/)


    character(80),dimension(NUM_SATS),parameter    :: Level_1b_v2_channel_3_filename =(/&
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\TIROS-N_CH3.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-06_CH3.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-07_CH3.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-08_CH3.L1Bv2', &        
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-09_CH3.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-10_CH3.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-11_CH3.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-12_CH3.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-14_CH3.L1Bv2'/)

       character(80),dimension(NUM_SATS),parameter    :: Level_1b_v2_channel_4_filename =(/&
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\TIROS-N_CH4.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-06_CH4.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-07_CH4.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-08_CH4.L1Bv2', &        
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-09_CH4.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-10_CH4.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-11_CH4.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-12_CH4.L1Bv2', &
                                                                       '\\Dell-p1700d\MSU_Data\msu_level_1b\NOAA-14_CH4.L1Bv2'/)


    character(80),dimension(NUM_SATS),parameter    :: Level_1b_channel_3_filename =(/&
                                                                       '\\Dell-p866a\f\msu_level_1b\TIROS-N_CH3.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-06_CH3.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-07_CH3.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-08_CH3.L1B', &        
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-09_CH3.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-10_CH3.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-11_CH3.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-12_CH3.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-14_CH3.L1B'/)

    character(80),dimension(NUM_SATS),parameter    :: Level_1b_channel_4_filename =(/&
                                                                       '\\Dell-p866a\f\msu_level_1b\TIROS-N_CH4.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-06_CH4.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-07_CH4.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-08_CH4.L1B', &        
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-09_CH4.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-10_CH4.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-11_CH4.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-12_CH4.L1B', &
                                                                       '\\Dell-p866a\f\msu_level_1b\NOAA-14_CH4.L1B'/)



    character(80),dimension(NUM_SATS),parameter    :: Level_1c_channel_2_filename =(/&
                                                                       '\\Dell-p600a\msu_level_1c\TIROS-N_CH2.L1C', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-06_CH2.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-07_CH2.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-08_CH2.L1c', &        
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-09_CH2.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-10_CH2.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-11_CH2.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-12_CH2.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-14_CH2.L1c'/)

     character(80),dimension(NUM_SATS),parameter    :: Level_1c_channel_2_filename_hgt_corr =(/&
                                                                       '\\Dell-p600a\msu_level_1c\TIROS-N_CH2_HGT.L1C', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-06_CH2_HGT.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-07_CH2_HGT.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-08_CH2_HGT.L1c', &        
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-09_CH2_HGT.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-10_CH2_HGT.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-11_CH2_HGT.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-12_CH2_HGT.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-14_CH2_HGT.L1c'/)

 
     character(80),dimension(NUM_SATS),parameter    :: Level_1c_channel_2_filename_roll_corr =(/&
                                                                       '\\Dell-p600a\msu_level_1c\TIROS-N_CH2_R.L1C', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-06_CH2_R.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-07_CH2_R.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-08_CH2_R.L1c', &        
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-09_CH2_R.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-10_CH2_R.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-11_CH2_R.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-12_CH2_R.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-14_CH2_R.L1c'/)


     character(80),dimension(NUM_SATS),parameter    :: Level_1c_channel_2_filename_diur_corr =(/&
                                                                       '\\Dell-p600a\msu_level_1c\TIROS-N_CH2_DNL.L1C', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-06_CH2_DNL.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-07_CH2_DNL.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-08_CH2_DNL.L1c', &        
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-09_CH2_DNL.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-10_CH2_DNL.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-11_CH2_DNL.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-12_CH2_DNL.L1c', &
                                                                       '\\Dell-p600a\msu_level_1c\NOAA-14_CH2_DNL.L1c'/)




    character(80),dimension(NUM_SATS),parameter    ::    Log_filename    =(/'\\micron-p400c\e\msu\logs\log_01.txt', &
                                                                       '\\micron-p400c\e\msu\logs\log_02.txt', &
                                                                       '\\micron-p400c\e\msu\logs\log_03.txt', &
                                                                       '\\micron-p400c\e\msu\logs\log_04.txt', &
                                                                       '\\micron-p400c\e\msu\logs\log_05.txt', &        
                                                                       '\\micron-p400c\e\msu\logs\log_06.txt', &
                                                                       '\\micron-p400c\e\msu\logs\log_07.txt', &
                                                                       '\\micron-p400c\e\msu\logs\log_08.txt', &
                                                                       '\\micron-p400c\e\msu\logs\log_09.txt'/)

    character(80),dimension(NUM_SATS),parameter    ::    TS_FILENAME    = (/'\\micron-p400c\e\msu\time_series\ts_01.dat', &
                                                                       '\\micron-p400c\e\msu\time_series\ts_02.dat', &
                                                                       '\\micron-p400c\e\msu\time_series\ts_03.dat', &
                                                                       '\\micron-p400c\e\msu\time_series\ts_04.dat', &
                                                                       '\\micron-p400c\e\msu\time_series\ts_05.dat', &        
                                                                       '\\micron-p400c\e\msu\time_series\ts_06.dat', &
                                                                       '\\micron-p400c\e\msu\time_series\ts_07.dat', &
                                                                       '\\micron-p400c\e\msu\time_series\ts_08.dat', &
                                                                       '\\micron-p400c\e\msu\time_series\ts_09.dat'/)


      character(120),dimension(NUM_CHANNELS),parameter    ::    MERGE_PARAM_FILENAME    = (/'\\dell-p1700d\msu_data\MSU_Level_1A\merging_parameters\chan_1_merging_parameters.txt', &
                                                                                    '\\dell-p1700d\msu_data\MSU_Level_1A\merging_parameters\chan_2_merging_parameters.txt', &
                                                                                    '\\dell-p1700d\msu_data\MSU_Level_1A\merging_parameters\chan_3_merging_parameters.txt', &
                                                                                    '\\dell-p1700d\msu_data\MSU_Level_1A\merging_parameters\chan_4_merging_parameters.txt'/)



    
    ! constants for doing the non-linearity correction
    ! see NL_corr_notes.txt for sources of constants

    integer,parameter        :: NUM_NL_COEFFS = 3

    ! see cal_coef_notes.txt in \\micron-p400c\e\msu\memos for discussion of the selection of these coeffs

                                                                                        !TIROS_N        NOAA_6            NOAA-7            NOAA-8            NOAA-9            NOAA-10            NOAA-11            NOAA-12            NOAA-14

    real(8),parameter,dimension(NUM_SATS,NUM_NL_COEFFS)    :: NL_COEFFS_CH2  =  RESHAPE((/    26.8499755859D0,22.0407867432D0,26.0610046387D0,18.7100067139D0,20.5899963379D0,38.4689941406D0,58.5272064209D0,200.0448D0,        75.3933563232D0, &
                                                                                          0.9399644136D0,    0.9275690913D0,    0.9437999725D0,    0.9588999748D0,    0.9661999941D0,    0.9546149969D0,    0.9424229860D0,    0.855130D0,        0.9399530292D0, &
                                                                                        0.0000172642D0,    0.0000167910D0,    0.0000162600D0,    0.0000128004D0,    0.0000096619D0,    0.0000110622D0,    0.0000134850D0,    0.000025935D0,    0.0000116106D0/),(/9,3/))
   
    ! constants associated with PRT calculations

    integer,parameter        :: NUM_PRTS = 4

        ! Locations in raw count matrix:
    
    integer,parameter        ::    A1_SCAN_POS = 8
    integer,parameter        ::    A1_CHAN_POS = 2
    
    integer,parameter        ::    A2_SCAN_POS = 9
    integer,parameter        ::    A2_CHAN_POS = 2    
    
    integer,parameter        ::    B1_SCAN_POS = 8
    integer,parameter        ::    B1_CHAN_POS = 3    
    
    integer,parameter        ::    B2_SCAN_POS = 9
    integer,parameter        ::    B2_CHAN_POS = 3    
    
    integer,parameter        ::    CAL_LO_1_SCAN_POS    = 1
    integer,parameter        ::    CAL_LO_1_CHAN_POS    = 2

    integer,parameter        ::    CAL_HI_1_SCAN_POS    = 2
    integer,parameter        ::    CAL_HI_1_CHAN_POS    = 2

    integer,parameter        ::    CAL_LO_2_SCAN_POS    = 1
    integer,parameter        ::    CAL_LO_2_CHAN_POS    = 3

    integer,parameter        ::    CAL_HI_2_SCAN_POS    = 2
    integer,parameter        ::    CAL_HI_2_CHAN_POS    = 3

    integer,parameter        ::  OTHER_1_SCAN_POS    = 3
    integer,parameter        ::  OTHER_1_CHAN_POS    = 2

    integer,parameter        ::    OTHER_2_SCAN_POS    = 3
    integer,parameter        ::    OTHER_2_CHAN_POS    = 3

    integer,parameter        ::    LO_1_SCAN_POS        = 4
    integer,parameter        ::    LO_1_CHAN_POS        = 2

    integer,parameter        ::    LO_2_SCAN_POS        = 4
    integer,parameter        ::    LO_2_CHAN_POS        = 3

    integer,parameter        ::    LO_3_SCAN_POS        = 5
    integer,parameter        ::    LO_3_CHAN_POS        = 2

    integer,parameter        ::    LO_4_SCAN_POS        = 5
    integer,parameter        ::    LO_4_CHAN_POS        = 3

    integer,parameter        ::    DICKE_1_SCAN_POS    = 6
    integer,parameter        ::    DICKE_1_CHAN_POS    = 2

    integer,parameter        ::    DICKE_2_SCAN_POS    = 6
    integer,parameter        ::    DICKE_2_CHAN_POS    = 3

    integer,parameter        ::    DICKE_3_SCAN_POS    = 7
    integer,parameter        ::    DICKE_3_CHAN_POS    = 2

    integer,parameter        ::    DICKE_4_SCAN_POS    = 7
    integer,parameter        ::    DICKE_4_CHAN_POS    = 3

    integer,parameter        ::    ANT_1_SCAN_POS        = 10
    integer,parameter        ::    ANT_1_CHAN_POS        = 2

    integer,parameter        ::    ANT_2_SCAN_POS        = 10
    integer,parameter        ::    ANT_2_CHAN_POS        = 3

    integer,parameter        ::    MOTOR_1_SCAN_POS    = 11
    integer,parameter        ::    MOTOR_1_CHAN_POS    = 2

    integer,parameter        ::    MOTOR_2_SCAN_POS    = 11
    integer,parameter        ::    MOTOR_2_CHAN_POS    = 3

    integer,parameter        ::    CHASSIS_1_SCAN_POS    = 12
    integer,parameter        ::    CHASSIS_1_CHAN_POS    = 2

    integer,parameter        ::    CHASSIS_2_SCAN_POS    = 12
    integer,parameter        ::    CHASSIS_2_CHAN_POS    = 3


        ! resistance calculating constants:

    integer,parameter        ::    A1 = 1
    integer,parameter        ::    A2 = 2
    integer,parameter        ::    B1 = 3
    integer,parameter        ::    B2 = 4

        ! Note that the k0 and k1 parameters are all the same for now....  I include the possibility of them being different just in case
        ! Numbers are from Spencer Christy and Grody, Journal of Climate, October 1990, Page 1127.

                                                                                !TIROS_N        NOAA_6        NOAA-7        NOAA-8        NOAA-9        NOAA-10        NOAA-11        NOAA-12        NOAA-14
    real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_k0 =    RESHAPE((/    495.6D0,        495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    &        ! Target A1
                                                                                495.6D0,        495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    &        ! Target A2
                                                                                495.6D0,        495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    &        ! Target B1
                                                                                495.6D0,        495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0,    495.6D0/),&        ! Target B2
                                                                                (/9,4/))

    real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_k1 =    RESHAPE((/    107.8D0,        107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    &        ! Target A1
                                                                                107.8D0,        107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    &        ! Target A2
                                                                                107.8D0,        107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    &        ! Target B1
                                                                                107.8D0,        107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0,    107.8D0/),  &        ! Target B2
                                                                                (/9,4/))

    ! target temperature calculating constants

    real(8),parameter        ::    UPPER_TARGET_TEMP_LIMIT = 300.0D0
    real(8),parameter        ::    LOWER_TARGET_TEMP_LIMIT = 240.0D0
    real(8),parameter        ::    TARGET_TEMP_DIFF_LIMIT    =    5.0D0


    !The R to T coefficents are identical to those used by matthias.  These are slightly different that those used by Spencer, Christly and Grody, 1990 for
    !NOAA-6.  Their values are still here, but commented out.
                                                                                   !TIROS_N    NOAA_6        NOAA-7    NOAA-8        NOAA-9    NOAA-10    NOAA-11    NOAA-12    NOAA-14

    !real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_e0 =    RESHAPE((/    29.10,        29.3171,    28.976,    29.4857,    0.000,    0.000,    0.000,    0.000,    0.000,    &        ! Target A1
    !                                                                            27.55,        29.2983,    28.995,    29.3123,    0.000,    0.000,    0.000,    0.000,    0.000,    &        ! Target A2
    !                                                                            29.05,        29.2823,    28.945,    29.3471,    0.000,    0.000,    0.000,    0.000,    0.000,    &        ! Target B1
    !                                                                            28.95,        29.3631,    28.964,    27.9719,    0.000,    0.000,    0.000,    0.000,    0.000/),&        ! Target B2
    !                                                                            (/9,4/))

                                                                                  !TIROS_N    NOAA_6    NOAA-7      NOAA-8     NOAA-9         NOAA-10   NOAA-11     NOAA-12    NOAA-14
    !real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_e0 =    RESHAPE((/    29.1000D0,  29.3271D0,  28.9760D0,  29.48570D0,  29.429400D0,  29.2589D0,  29.1014D0,  29.3970D0,  28.74552D0,  & !29.003197D0,
    !                                                                            27.5500D0,  29.3083D0,  28.9950D0,  29.31230D0,  29.263900D0,  29.4633D0,  28.9481D0,  28.4527D0,  28.86749D0,  &  !28.514892D0,
    !                                                                            29.0500D0,  29.2923D0,  28.9450D0,  29.34710D0,  29.541600D0,  29.2350D0,  28.9854D0,  28.1854D0,  28.73856D0,  &  !28.455605D0,
    !                                                                            28.9500D0,  29.3731D0,  28.9640D0,  27.97190D0,  29.577800D0,  29.4680D0,  29.0424D0,  28.9588D0,  28.67430D0/),&  !29.113998D0/), &
    !                                                                            (/9,4/))

                                                                                    !TIROS_N    NOAA_6    NOAA-7      NOAA-8     NOAA-9         NOAA-10   NOAA-11     NOAA-12    NOAA-14
    real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_e0 =    RESHAPE((/    29.1000D0,  29.3271D0,  28.9760D0,  29.48570D0,  29.429400D0,  29.2589D0,  29.1014D0,  29.3970D0,  29.003197D0,      &
                                                                                27.5500D0,  29.3083D0,  28.9950D0,  29.31230D0,  29.263900D0,  29.4633D0,  28.9481D0,  28.4527D0,  28.514892D0,      &
                                                                                29.0500D0,  29.2923D0,  28.9450D0,  29.34710D0,  29.541600D0,  29.2350D0,  28.9854D0,  28.1854D0,  28.455605D0,   &
                                                                                28.9500D0,  29.3731D0,  28.9640D0,  27.97190D0,  29.577800D0,  29.4680D0,  29.0424D0,  28.9588D0,  29.113998D0/), &
                                                                                (/9,4/))
  
                                                                                  !TIROS_N    NOAA_6        NOAA-7        NOAA-8        NOAA-9    NOAA-10    NOAA-11    NOAA-12    NOAA-14

    !real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_e1 =    RESHAPE((/    0.42596,    0.4252700,    0.42694,    0.4247172,    0.000,    0.000,    0.000,    0.000,    0.000,    &        ! Target A1
    !                                                                            0.42787,    0.4260801,    0.42627,    0.4326488,    0.000,    0.000,    0.000,    0.000,    0.000,    &        ! Target A2
    !                                                                            0.42710,    0.4261320,    0.42628,    0.4247339,    0.000,    0.000,    0.000,    0.000,    0.000,    &        ! Target B1
    !                                                                            0.42836,    0.4255765,    0.42498,    0.4285466,    0.000,    0.000,    0.000,    0.000,    0.000/),&        ! Target B2
    !                                                                            (/9,4/))


                                                                                 !TIROS_N      NOAA_6        NOAA-7         NOAA-8          NOAA-9       NOAA-10       NOAA-11         NOAA-12      NOAA-14
    !real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_e1 =    RESHAPE((/    .4259600D0,  .4254270D0,  .4269400D0,  .4247172D0,  .4283514D0,  .4245799D0,  .4242637D0,  .4256561D0,  .4278088D0, & !  .4293630
    !                                                                            .4278700D0,  .4260801D0,  .4262700D0,  .4326844D0,  .4338180D0,  .4250778D0,  .4277704D0,  .4293800D0,  .4263018D0, & !  .4280371
    !                                                                            .4271000D0,  .4261320D0,  .4262800D0,  .4247339D0,  .4248521D0,  .4289328D0,  .4269018D0,  .4302196D0,  .4335265D0, & !  .4284300
    !                                                                            .4283600D0,  .4255765D0,  .4249800D0,  .4285466D0,  .4243732D0,  .4275379D0,  .4274570D0,  .4276377D0,  .4282688D0/), & !  .4257680
    !                                                                            (/9,4/))


     real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_e1 =    RESHAPE((/    .4259600D0,  .4254270D0,  .4269400D0,  .4247172D0,  .4283514D0,  .4245799D0,  .4242637D0,  .4256561D0,  .4293630D0,    &
                                                                                .4278700D0,  .4260801D0,  .4262700D0,  .4326844D0,  .4338180D0,  .4250778D0,  .4277704D0,  .4293800D0,  .4280371D0,    &
                                                                                .4271000D0,  .4261320D0,  .4262800D0,  .4247339D0,  .4248521D0,  .4289328D0,  .4269018D0,  .4302196D0,  .4284300D0,    &
                                                                                .4283600D0,  .4255765D0,  .4249800D0,  .4285466D0,  .4243732D0,  .4275379D0,  .4274570D0,  .4276377D0,  .4257680D0/), &
                                                                                (/9,4/))



                                                                                   !TIROS_N        NOAA_6            NOAA-7            NOAA-8            NOAA-9    NOAA-10    NOAA-11    NOAA-12    NOAA-14

!    real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_e2 =    RESHAPE((/    0.00003187,        0.00003165629,    0.000031737,    0.000032096,    0.000,    0.000,    0.000,    0.000,    0.000,    &        ! Target A1
!                                                                                0.00003185,        0.0000314991,    0.000031775,    0.00003282283,    0.000,    0.000,    0.000,    0.000,    0.000,    &        ! Target A2
!                                                                                0.00003206,        0.0000315241,    0.000031603,    0.00003165123,    0.000,    0.000,    0.000,    0.000,    0.000,    &        ! Target B1
!                                                                                0.00003175,        0.0000317602,    0.000031389,    0.00003095837,    0.000,    0.000,    0.000,    0.000,    0.000/),&        ! Target B2
!                                                                                (/9,4/))

                                                                                   !TIROS_N        NOAA_6           NOAA-7         NOAA-8            NOAA-9           NOAA-10          NOAA-11          NOAA-12         NOAA-14

!    real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_e2 =    RESHAPE((/    .3187000D-04,  .3165629D-04,  .3173700D-04,  .3209600D-04,  .3223351D-04,  .3169871D-04,  .3162073D-04,  .3238697D-04,  .3056633D-04, & !  .3144484D-04
!                                                                                .3185000D-04,  .3149910D-04,  .3177500D-04,  .3282283D-04,  .3334592D-04,  .3192864D-04,  .3185110D-04,  .2974601D-04,  .3083285D-04, & !  .3028803D-04 
!                                                                                .3206000D-04,  .3152410D-04,  .3160300D-04,  .3165123D-04,  .3188156D-04,  .3232162D-04,  .3170732D-04,  .2920356D-04,  .3147607D-04, & !  .3021258D-04
!                                                                                .3175000D-04,  .3176020D-04,  .3138900D-04,  .3095837D-04,  .3264424D-04,  .3221571D-04,  .3191209D-04,  .3033068D-04,  .3054580D-04/), & !  .3159801D-04
!                                                                                (/9,4/))

    real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_e2 =    RESHAPE((/    .3187000D-04,  .3165629D-04,  .3173700D-04,  .3209600D-04,  .3223351D-04,  .3169871D-04,  .3162073D-04,  .3238697D-04,  .3144484D-04,    &
                                                                                .3185000D-04,  .3149910D-04,  .3177500D-04,  .3282283D-04,  .3334592D-04,  .3192864D-04,  .3185110D-04,  .2974601D-04,  .3028803D-04,    & 
                                                                                .3206000D-04,  .3152410D-04,  .3160300D-04,  .3165123D-04,  .3188156D-04,  .3232162D-04,  .3170732D-04,  .2920356D-04,  .3021258D-04,    &
                                                                                .3175000D-04,  .3176020D-04,  .3138900D-04,  .3095837D-04,  .3264424D-04,  .3221571D-04,  .3191209D-04,  .3033068D-04,  .3159801D-04/),    &
                                                                                (/9,4/))



                                                                                 !TIROS_N        NOAA_6        NOAA-7        NOAA-8        NOAA-9         NOAA-10    NOAA-11         NOAA-12    NOAA-14

    real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_error =     RESHAPE((/    0.000D0,        0.370D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    &        ! Target A1
                                                                                -1.036D0,        0.405D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    &        ! Target A2
                                                                                -0.440D0,        0.370D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    &        ! Target B1
                                                                                0.000D0,        0.389D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0/),&        ! Target B2
                                                                                (/9,4/))

!    real(8),parameter,dimension(NUM_SATS,NUM_PRTS)    ::    PRT_error =     RESHAPE((/    0.000D0,        0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    &        ! Target A1
!                                                                                0.000D0,        0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    &        ! Target A2
!                                                                                0.000D0,        0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    &        ! Target B1
!                                                                                0.000D0,        0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0,    0.000D0/),&        ! Target B2
!                                                                                (/9,4/))


                                                                                             

     ! constants associated with Tb calculations

    real(8),parameter        :: T_SPACE        = 2.73D0
    integer,parameter        :: SPACE        = 12
    integer,parameter        :: BLACK_BODY    = 13

    ! constants for various bounds checks


    real(8),dimension(NUM_CHANNELS),parameter    :: MAX_REASONABLE_TB = (/350.0D0,350.0D0,350.0D0,350.0D0/)
    real(8),dimension(NUM_CHANNELS),parameter   :: MIN_REASONABLE_TB = (/150.0D0,150.0D0,150.0D0,150.0D0/)
    real(8),dimension(NUM_CHANNELS),parameter   :: MAX_REASONABLE_NEAR_NEIGHBOR_DIFF = (/15.0d0,15.0d0,1.5d0,1.5d0/)
    real(8),parameter                            :: MAX_HEIGHT = 910.0  ! maximum height of satellite allowed
    real(8),parameter                            :: MIN_HEIGHT = 740.0  ! minimum height of satellite allowed

    ! Flag values for Ascending/Descending flag

    integer(4),parameter                        :: ASCENDING  = 1
    integer(4),parameter                        :: DESCENDING = 2
    integer(4),parameter                        :: UNKNOWN    = 0

    ! Flag Values for Correction Level Flag

    integer(4),parameter                        :: RAW_TBS = 0 
    integer(4),parameter                        :: TBS_CORRECTED_FOR_HEIGHT = 1 
    integer(4),parameter                        :: TBS_CORRECTED_FOR_DIURNAL_EFFECTS = 2 
    integer(4),parameter                        :: TBS_CORRECTED_FOR_SAT_TEMP = 3
    integer(4),parameter                        :: TBS_MODELLED_NCEP = 4 
    integer(4),parameter                        :: TBS_CORRECTED_TO_NADIR = 5 
    integer(4),parameter                        :: TBS_MODELLED_NCEP_DIURNAL = 6
    integer(4),parameter                        :: TBS_CORR_FOR_HEIGHT_AND_LAT = 7
    integer(4),parameter                        :: TBS_CORR_TO_NOM_EIA = 8

    ! L1B Qual flag positions sre defined in module msu_qual



    ! pseudo-emunerated types

    ! surface type

    integer(4),parameter                :: SURF_TYPE_LAND = 1
    integer(4),parameter                :: SURF_TYPE_OCEAN = 0

end module msu_constants





