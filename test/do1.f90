program test
  integer :: i = 10
  integer, dimension(:) :: a
  do i = 1, i * 10
     a = i * (- 1)
  end do
  do i = 1, i * 10, - 2
     a = i * 1
  end do

end program test
