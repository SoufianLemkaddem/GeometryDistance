program Core

	implicit none

call Print
!call Initialize !Testing Changes 2
call Getpoint

	
contains

subroutine Print

print*, 'hello World'

end subroutine


	
subroutine Getpoint

integer, parameter :: Ndist = 6
real(8), parameter :: dist(Ndist) = (/1.0,2.0,3.0,5.0,6.0,2.0/)
real(8) :: p(4,2)

p(1,:) = (/0.0,0.0/)
p(2,:) = (/dist(1),0.0/)

end subroutine 


end program
