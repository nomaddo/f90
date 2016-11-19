program test
  integer n
  integer, dimension(3) :: x = (/ 1,2,3 /)
  integer, dimension(3) :: y = [1,2,3]

  x = f ([n + 1,n,n])
  y = g ((/1,2,3/) + 1)
end program test
