program exer1
  use omp_lib
  implicit none
  integer :: var1, var2
  var1 = 1
  var2 = 2

  !   Test different data sharing clauses here
  
  !$omp parallel shared(var1, var2)
  print *, 'Region 1:       var1=', var1, 'var2=', var2
  var1 = var1 + 1
  var2 = var2 + 1
  !$omp end parallel
  
  print *, 'After region 1: var1=', var1, 'var2=', var2
  print *
  
end program exer1
