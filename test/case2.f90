program test
  integer :: i = 1
  select case (i)
  case (:3)
     a = 1
  case (4:10)
     a = 2
  case (11:)
     a = 3
  end select
end program test
