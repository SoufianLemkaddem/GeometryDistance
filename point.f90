! **********************************************************************************
! Module containing get point and check point
! **********************************************************************************


module point

    implicit none

public :: Newpoint 

contains

subroutine Newpoint(disti, dist1, dist2, p3)

real(8), intent(in) :: disti, dist1, dist2
real(8), intent(out) :: p3(2)
real(8) :: angle, arg_angle

arg_angle = (dist1**2 + disti**2 - dist2**2)/(2*disti*dist1)
angle = acos(arg_angle)

p3(1) = cos(angle)*dist1
p3(2) = sin(angle)*dist1

end subroutine

end module
