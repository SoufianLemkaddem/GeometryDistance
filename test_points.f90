! **********************************************************************************
! Module containing test points for Plotters.f90
! **********************************************************************************
module testPoints

use plotters 

    implicit none


    public Shapes


contains
! **********************************************************************************   
! Reads 'geometry.params'
subroutine shapes
	real*8, dimension(20,2) :: test_array 

	call circle(test_array)

	call geometricdraw(test_array)

	

end subroutine shapes
!**********************************************************************
!
! Reads in the positions from a file of NumPoint lines
subroutine circle(test_array)
	real*8, intent(inout) :: test_array(:,:)
	real*8 :: radius, xCord, yCord, PI, theta
	integer :: k

	PI = acos(-1.0d0)
	
	radius = 10.0d0

    do k = 1, 20

	theta = 2.0d0*PI*RAND()
	xCord = radius* cos(theta)
	ycord = radius* sin(theta)

	test_array(k,1) = xcord
	test_array(k,2) = ycord
	
    end do    
end subroutine circle
!**********************************************************************

!**********************************************************************
end module
