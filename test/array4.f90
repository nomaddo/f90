program test
  integer, dimension(1:5) :: a, b
  a(:2) = 1 + 2
  a(1:2) = b(3:4)
  a(1:5:2) = b(1:3)
  a(:5:2) = b(1:3)
  a(::2) = b(1:5:2)
end program test
