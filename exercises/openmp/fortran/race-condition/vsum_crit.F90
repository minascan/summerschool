program vectorsum
  use omp_lib
  use iso_fortran_env, only: int64
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 102400_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: asum, psum, sumex
  integer(kind=ik) :: i

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do

  asum = 0

  !$omp parallel &
  !$omp shared(vecA,asum) &
  !$omp private(i,psum)

  psum = 0
  
  !$omp do
  do i = 1, nx
     psum = psum + vecA(i)
  end do
  !$omp end do
  
  !$omp critical(dosum)
  asum = asum + psum
  !$omp end critical(dosum)
  
  !$omp end parallel
  
  write(*,*) 'Sum: ', asum
  
end program vectorsum
