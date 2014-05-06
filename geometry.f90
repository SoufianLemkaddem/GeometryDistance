! *****************************************************************************
! May 2014 - ICCP final project
! *****************************************************************************
program geometry
    use initializers
    use point
    use plotters
	use testpoints
    implicit none
    
    ! Variables, see geometry.params for the user-defined parameters
    integer                     :: NumPoints
    
    real(8), allocatable        :: KnownPos(:,:), Dist(:), Pos(:,:)


    call Initialize
	call shapes


	call plot_close
contains

subroutine Initialize
! Main initilizing routine
    call GetParameters(NumPoints)
    
    allocate(KnownPos(2, NumPoints), Dist(NumPoints*(NumPoints-1)/2), Pos(2, NumPoints))
    
end subroutine Initialize

end program geometry
