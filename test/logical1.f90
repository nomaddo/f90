program test
  logical :: i = .not. (((.true. .and. .false. .or. .true.) .eqv. .true.) .neqv. .false.)
end program test
