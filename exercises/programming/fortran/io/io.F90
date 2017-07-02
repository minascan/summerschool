module io
    
contains

  ! Reads the temperature distribution from an input file
  subroutine read_field(field, filename)
    implicit none

    real, dimension(:,:), allocatable, intent(out) :: field
    character(len=*), intent(in) :: filename
    integer   :: nx, ny, i, alloc_stat
    integer, parameter :: funit=10
    character(len=1) :: dummy
    
    ! TODO: implement function that will:
    ! open the file
    open (funit, file=filename)
    ! read the first header line to get nx and ny
    read (funit, *) dummy, nx, ny    
    ! allocate matrix called field
    allocate(field(nx,ny), stat=alloc_stat)
    if (alloc_stat /= 0) call abort()   
    ! read rest of the file into field
    do i =1, nx
       read (funit,*) field(i,:)
    enddo    
    ! close the file
    close(funit)

  end subroutine read_field
  

  ! Output routine, saves the temperature distribution as a png image
  subroutine write_field(field, iter)
    use iso_fortran_env, only : REAL64
    use pngwriter
    implicit none

    integer, parameter :: dp = REAL64
    real, intent(in) :: field(:,:)
    integer, intent(in) :: iter

    character(len=85) :: filename
    integer :: nx, ny, stat

    nx = size(field, 1)
    ny = size(field, 2)


    write(filename,'(A5,I4.4,A4,A)')  'heat_', iter, '.png'
    stat = save_png(real(field, kind=dp), nx, ny, filename)

  end subroutine write_field

end module io
