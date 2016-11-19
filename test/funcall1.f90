program test
  integer :: i = 1
  x = f ()
contains
  function f ()
    return 13
  end function
end program test
