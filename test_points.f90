! **********************************************************************************
! Module containing test points for Plotters.f90
! **********************************************************************************
module testPoints

use plotters 
use initializers

    implicit none


    public Shapes


contains
! **********************************************************************************   
! Reads 'geometry.params'
subroutine shapes
	real*8, dimension(20,2) :: test_array, dist_array(120)

	call randPoints(test_array, dist_array)

	call geometricdraw(test_array)

	

end subroutine shapes
!**********************************************************************
!
! Reads in the positions from a file of NumPoint lines
subroutine randPoints(test_array, dist_array)
	real*8, intent(inout) :: test_array(:,:), dist_array(:)
	real*8 :: xCord, yCord
	integer :: k


	open(16, file = 'points.dat')
    do k = 1, 20

	xCord = k*rand()
	ycord = k*rand()

	test_array(k,1) = xcord
	test_array(k,2) = ycord

	write(16,*) xcord, ycord
	
	!print*, xcord, ycord, rand()

    end do   

	call FillDistKP(k, test_array, dist_array)

	print*, dist_array
	close(16) 
end subroutine randPoints
!**********************************************************************
!Subroutine finddistance(test_array)
!	real*8, intent(in) :: test_array(:,:)
!	real*8, intent(out) :: dist_array(size(test_array(:,1))
!
!	real*8, :: x1, y1, x2, y2, distance
!
!	integer :: i
!	
!	
!
!	do i=1 , size(test_array(:,1))
!
!		x1 = test_array(i,1)
!		y1 = test_array(i,2)
!
!		do j=1, size(test_array(:,1))
!
!		y2 = test_array(j,2)
!		x2 = test_array(j,1)
!
!		distance = sqrt((x2 - x1)**2 + (y2 - y1)**2)
!
!		dist_array(j)
!end do
!end subroutine


	
	







!**********************************************************************
end module
