subroutine read_basis(nBas,nO,nV)

! Read basis set information

  implicit none
  include 'parameters.h'

! Input variables

  integer,intent(in)            :: nO

! Output variables

  integer,intent(out)           :: nBas
  integer,intent(out)           :: nV

!------------------------------------------------------------------------
! Primary basis set information
!------------------------------------------------------------------------

! Open file with basis set specification

  open(unit=2,file='input/basis')
  read(2,*) nBas

! Close file with basis set specification

  close(unit=2)

  write(*,'(A28)') '------------------'
  write(*,'(A28,1X,I16)') 'Number of basis functions',nBas
  write(*,'(A28)') '------------------'
  write(*,*)

! Number of virtual orbitals

  nV = nBas - nO 

end subroutine read_basis
