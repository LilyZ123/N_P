      subroutine read_mp_nminr1_parameters()
     implicit none
     integer :: ierr
     logical :: file_named
     REAL :: curyr  ! current year of simulation
     REAL:: hru_dafr    ! fraction of watershed area in HRU
     INTEGER :: ihru     ! HRU number
     INTEGER :: nyskip   ! number of years to skip output summarization and printing
     REAL:: nro  ! sequence number of year in rotation
     INTEGER :: sol_nly  ! number of layers in soil profile
     
     REAL:: cmn    ! none rate factor for humus mineralization on active organic N
     REAL:: idplt       !none land cover code from crop.dat
     REAL:: nactfr     !none nitrogen active pool fraction. The fraction of organic nitrogen in the active pool.
     REAL:: rsdco_pl  !none plant residue decomposition coefficient.
     REAL :: sol_aorgn  !kg N/ha amount of nitrogen stored in the active organic (humic) nitrogen pool
     REAL :: sol_cbn  !% percent organic carbon in soil layer
     REAL :: sol_fon  !kg N/ha amount of nitrogen stored in the fresh organic (residue) pool
     REAL :: sol_fop  !kg N/ha amount of phosphorus stored in the fresh organic (residue) pool
     REAL :: sol_no3  !kg N/ha amount of nitrogen stored in the nitrate pool in soil layer
     REAL :: sol_orgn  !kg N/ha amount of nitrogen stored in the stable organic N pool
     REAL :: sol_orgp  !kg N/ha amount of phosphorus stored in the organic P pool in soil layer
     REAL :: sol_rsd  !kg N/ha amount of organic matter in the soil classified as residue
     REAL :: sol_solp  !kg N/ha amount of phosphorus stored in solution
     REAL :: sol_st  !mm H2O  |amount of water stored in the soil layer on the current day
     REAL :: sol_fc  !mm H2O  |amount of water available to plants in soil
     REAL :: sol_tmp !deg C  | daily average temperature of soil layer
     REAL :: sol_ul  !mm H2O | amount of water held in the soil layer at saturation
     REAL :: wshd_dnit   !kg N/ha average annual amount of nitrogen lost from nitrate pool due to denitrification in watershed
     REAL :: wshd_hmn    !kg N/ha average annual amount of nitrogen moving from active organic to nitrate pool in watershed
     REAL :: wshd_hmp    !kg N/ha average annual amount of phosphorus moving from organic to labile pool in watershed
     REAL :: wshd_rmn    !kg N/ha average annual amount of nitrogen moving from fresh organic (residue) to nitrate and active organic pools in watershed
     REAL :: wshd_rmp    !kg N/ha average annual amount of phosphorus moving from fresh organic (residue) to labile and organic pools in watershed
     REAL :: wshd_rwn    !kg N/ha average annual amount of nitrogen moving from active organic to stable organic pool in watershed
     
     NAMELIST / noahmp_nminr1_parameters / cmn idplt nactfr rsdco_pl sol_aorgn &
     sol_fon sol_fop sol_no3 sol_orgn sol_orgp sol_rsd sol_st sol_fc sol_tmp sol_ul wshd_dnit &
     wshd_hmn wshd_hmp wshd_rmn wshd_rmp wshd_rwn
     
     cmn_table = -1.E36 
     idplt_table = -1.E36
     nactfr_table = -1.E36
     rsdco_pl_table = -1.E36
     sol_aorgn_table = -1.E36
     sol_fon_table = -1.E36
     sol_fop_table = -1.E36
     sol_no3_table = -1.E36
     sol_orgn_table = -1.E36
     sol_orgp_table = -1.E36
     sol_rsd_table = -1.E36
     sol_st_table = -1.E36
     sol_fc_table = -1.E36
     sol_tmp_table = -1.E36
     sol_ul_table = -1.E36
     wshd_dnit_table = -1.E36
     wshd_hmn_table = -1.E36
     wshd_hmp_table = -1.E36
     wshd_rmn_table = -1.E36
     wshd_rmp_table = -1.E36
     wshd_rwn_table = -1.E36
     
     inquire( file='NPCYCLE.TBL', exist=file_named )
         if ( file_named ) then
           open(15, file="NPCYCLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
         else
           open(15, status='old', form='formatted', action='read', iostat=ierr)
         end if
     
         if (ierr /= 0) then
            write(*,'("WARNING: Cannot find file NPCYCLE.TBL")')
            call wrf_error_fatal("STOP in Noah-MP read_mp_nminr1_parameters")
         endif
     
         read (15,noahmp_nminr1_parameters)
         close(15)

     cmn_table = cmn 
     idplt_table = idplt
     nactfr_table = nactfr
     rsdco_pl_table = rsdco_pl
     sol_aorgn_table(1:4) = sol_aorgn(1:4)
     sol_fon_table(1:4) = sol_fon(1:4)
     sol_fop_table(1:4) = sol_fop(1:4)
     sol_no3_table(1:4) = sol_no3(1:4)
     sol_orgn_table(1:4) = sol_orgp(1:4)
     sol_orgp_table(1:4) = sol_orgp(1:4)
     sol_rsd_table(1:4) = sol_rsd(1:4)
     sol_st_table(1:4) = sol_st(1:4)
     sol_fc_table(1:4) = sol_fc(1:4)
     sol_tmp_table(1:4) = sol_tmp(1:4)
     sol_ul_table(1:4) = sol_ul(1:4)
     wshd_dnit_table = wshd_dnit
     wshd_hmn_table = wshd_hmn
     wshd_hmp_table = wshd_hmp
     wshd_rmn_table = wshd_rmn
     wshd_rmp_table = wshd_rmp
     wshd_rwn_table = wshd_rwn
     
     end subroutine read_mp_nminr1_parameters
    