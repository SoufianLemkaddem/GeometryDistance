! **********************************************************************************
! Module containing get rotation angle, get translation coordinates and calculate original coordiantes from found coordinates
! **********************************************************************************

module GetOriginalPoints

use point

public :: GetRotATrans, GetOriginalP

contains


! Finds the rotational angle and the translation coordinates

subroutine GetRotATrans(P2found, P1original, P2original, rotangle, TransCoord)

real(8), intent(in) :: P2found(2), P1original(2), P2original(2)
real(8), intent(out) :: rotangle, TransCoord(2)

real(8) :: side(3), dummyP(2), arg_angle

if (P1original(2) < P2original(2)) then
	transCoord = P1original

	side(1) = P2found(1)
	side(2) = P2found(1)
	
	dummyP(1) = P1original(1) + P2original(1)
	dummyP(2) = P1original(2)
	

	call CalcDistance(dummyP(:), P2original(:), side(3))
	
	arg_angle = (side(1)**2 + side(2)**2 - side(3)**2)/(2*side(1)*side(2))
	rotangle = acos(arg_angle)
	

else if (P2original(2) < P1original(2)) then
	transCoord = P2original

	side(1) = P2found(1)
	side(2) = P2found(1)
	
	dummyP(1) = P2original(1) + P2found(1)
	dummyP(2) = P2original(2)
	
	call CalcDistance(dummyP(:), P1original(:), side(3))
	
	arg_angle = (side(1)**2 + side(2)**2 - side(3)**2)/(2*side(1)*side(2))
	rotangle = acos(arg_angle)

else if (P1original(2) == P2original(2)) then

	if (P1original(1) < P2original(1)) then
		transCoord = P1original
	else if (P2original(1) < P1original(1)) then
		transCoord = P2original
	end if

end if

end subroutine




! Pfound are the found xy-coordinates
! P3original are the coordinates of the original third point. 
! Np is the numper of points
! rotangle is the rotation angle
! transCoord are the translation distance in the x and y direction

subroutine GetOriginalP(Pfound, Poriginal, P3original, Np, rotangle, transCoord)

integer, intent(in) :: Np
real(8), intent(in) :: Pfound(Np,2), P3original(2), rotangle, transCoord(2)
real(8), intent(inout) :: Poriginal(Np,2)

real(8) :: CheckC3(2)
integer :: i


Poriginal(1,:) = transCoord(:)

CheckC3(1) = Cos(rotangle)*Pfound(3,1) - Sin(rotangle)*Pfound(3,2) + transCoord(1)
CheckC3(2) = Sin(rotangle)*Pfound(3,1) + Cos(rotangle)*Pfound(3,2) + transCoord(2)


if (floor(CheckC3(1)) == floor(P3original(1)) .AND. floor(CheckC3(2)) == floor(P3original(2))) then

	do i = 2, Np

		Poriginal(i,1) = Cos(rotangle)*Pfound(i,1) - Sin(rotangle)*Pfound(i,2) + transCoord(1)
		Poriginal(i,2) = Sin(rotangle)*Pfound(i,1) + Cos(rotangle)*Pfound(i,2) + transCoord(2)

	end do 

else

	do i = 2, Np

		Poriginal(i,1) = Cos(rotangle)*Pfound(i,1) + Sin(rotangle)*Pfound(i,2) + transCoord(1)
		Poriginal(i,2) = Sin(rotangle)*Pfound(i,1) - Cos(rotangle)*Pfound(i,2) + transCoord(2)

	end do 

end if

end subroutine
