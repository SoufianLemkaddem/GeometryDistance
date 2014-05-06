! *****************************************************************************
! May 2014 - ICCP final project
! *****************************************************************************
program geometry
    use initializers
    use point
    use plotters
    implicit none
    
    ! Variables, see geometry.params for the user-defined parameters
    integer                     :: NumPoints
    
    real(8), allocatable        :: KnownPos(:,:), Dist(:), Pos(:,:)


    call Initialize


contains

subroutine Initialize
! Main initilizing routine
    call GetParameters(NumPoints)
    
    allocate(KnownPos(NumPoints, NumPoints), Dist(NumPoints), Pos(NumPoints, NumPoints))
    
end subroutine Initialize

end program geometry
