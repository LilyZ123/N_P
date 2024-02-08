subroutine readsub
  use parm
  
  ! Declare variables
  character(len=13) ::  chmfile, mgtfile, solfile

      ! Process each HRU
      ! Read HRU file names
    chmfile = ""
    ! hrufile = ""
    mgtfile = ""
    solfile = ""
    ! Open files
      open(106, file=chmfile)
      open(107, file=solfile)
      ! open(108, file=hrufile)
      open(109, file=mgtfile)
      
      ! Call subroutines to read data from files
      ! call readhru
      call readchm
      call readmgt
      call readsol
      
      ! Close files
      close(106)
      close(107)
      close(108)
      close(109)

  
  ! Close weather generator file
  close(114)
  
  ! Additional processing can be added here
  
  ! Close input file
  close(101)
  
end subroutine readsub
