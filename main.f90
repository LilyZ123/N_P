      include 'modparm.f90'
      program main
      use parm

      implicit none
      call readsub

      call varinit

        !! perform management operations
        ! if (yr_skip(j) == 0)
         call operatn

      !  !! apply fertilizer/manure in continuous fert operation
      !   if (icfrt(j) == 1) then
      !     ndcfrt(j) = ndcfrt(j) + 1
      !     call confert
      !   end if


!! compute nitrogen and phosphorus mineralization

   !   if (cswat == 0) then
        call nminrl
   !   end if

      !   call nitvol
      !   if (sol_P_model == 1) then
      !       call pminrl
      !   end if
      ! if (cswat == 0) then
      !       call orgn(0)
      ! end if
      !       call psed(0)

      !   !! add nitrate in rainfall to soil profile
      !   call nrain
      !   !! compute nitrate movement leaching
      !   call nlch
      !   !! compute phosphorus movement
      !   call solp

end
