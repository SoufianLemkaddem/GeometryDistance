! **********************************************************************************
! Module containing get point and check point
! **********************************************************************************


module point

    implicit none

public :: Newpoint, CalcDistance, GetAlternativeP3

contains

! Calculates newpoint when 2 distances are given
subroutine Newpoint(disti, dist1, dist2, p3)

real(8), intent(in) :: disti, dist1, dist2
real(8), intent(out) :: p3(2)
real(8) :: angle, arg_angle

if (disti + dist1 > dist2 ) then

	arg_angle = (dist1**2 + disti**2 - dist2**2)/(2*disti*dist1)
	angle = acos(arg_angle)

	p3(1) = cos(angle)*dist1
	p3(2) = sin(angle)*dist1

end if 

end subroutine

subroutine CalcDistance(p1, p2, dist)

real(8), intent(in) :: p1(2), p2(2)
real(8), intent(inout) :: dist

dist = sqrt((p1(1) - p2(1))**2 + (p1(2) - p2(2))**2)

end subroutine

subroutine GetAlternativeP3(RefP, p2, p3)

real(8), intent(in) :: p2(2), p3(2)
real(8), intent(inout) :: RefP(2,3)

RefP(1,1) = p2(1) - p3(1)
RefP(2,1) = p3(2)

RefP(1,2) = p3(1)
RefP(2,2) = -p3(2)

RefP(1,3) = RefP(1,1)
RefP(2,3) = -RefP(2,1)

end subroutine


end module
