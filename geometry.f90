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
    integer                 :: NumPoints
    
    real(8), allocatable    :: KnownPos(:,:), Dist(:), Pos(:,:)
    real(8), allocatable    :: TrianglePos(:,:)
    integer, allocatable    :: iTrianglePosDist(:,:)
    logical, allocatable    :: DistUsed(:)


    call Initialize
    
    call FindCore
    
    call GeometricDraw(Pos(:,1:4))
    
contains

subroutine Initialize
! Main initilizing routine
    call GetParameters(NumPoints)   
    
    allocate(KnownPos(2, NumPoints), Dist(NumPoints*(NumPoints-1)/2), &
            Pos(2, NumPoints), DistUsed(NumPoints*(NumPoints-1)/2))
    allocate(TrianglePos(2,NumPoints**2))
    allocate(iTrianglePosDist(2,NumPoints**2)) ! TODO size of array?
    
    KnownPos = 0d0
    Dist = 0d0

    call MainSetup(NumPoints, KnownPos, Dist)
    
    DistUsed = .false.
    
end subroutine Initialize

subroutine FindCore
    integer :: iTrianglesStored, iTrianglesChecked, TriangleArrayLength,iConnectingDist
     
    ! Add first segment 
    Pos(:,1) = 0
    Pos(:,2) = [Dist(1), 0.d0]
    DistUsed(1) = .true.
    
    ! Loop through all triangle candidates
    ! If we find that two of the triangles 'match', we have our core.
    TriangleArrayLength = size(TrianglePos, 2) !Max # of triangles due to arraysize
    do iTrianglesStored=1, TriangleArrayLength
        ! Get a new triangle candidate, and store it
        call GetTriangle(   TrianglePos(:,iTrianglesStored), & !New triangle
                            iTrianglePosDist(:,iTrianglesStored), & !New dist indices
                            iTrianglesStored, Pos(:,1), Pos(:,2), Dist, DistUsed) !Inputs
        
        ! Now, try to search for a core in the triangles that are found by now.
        ! If we not succeed in finding a core the loop continues and calculates
        ! an extra triangle.
        do iTrianglesChecked=1, iTrianglesStored-1
            if ( CheckCore( Pos, &
                             TrianglePos(:,iTrianglesStored), &
                             TrianglePos(:,iTrianglesChecked), &
                             iConnectingDist)) then 
                             !TODO CheckMatch Should check for all 4? 
                             ! possible versions of this triangle and 
                             ! overwrite TrianglePos
                             
                ! We have a core!! 
                ! The correct rotation of the second triangle is supplied by the checking
                ! routine
                Pos(:,3) = TrianglePos(:,iTrianglesStored)
                Pos(:,4) = TrianglePos(:,iTrianglesChecked)
                DistUsed(iTrianglePosDist(1, iTrianglesStored)) = .true.
                DistUsed(iTrianglePosDist(2, iTrianglesStored)) = .true.
                DistUsed(iTrianglePosDist(1, iTrianglesChecked)) = .true.
                DistUsed(iTrianglePosDist(2, iTrianglesChecked)) = .true.
                DistUsed(iConnectingDist) = .true.
                return
            end if     
        enddo
    enddo
    print *, 'Core not found: increase array size.'
end subroutine FindCore

function CheckCore(Pos, Point3, Point4, iConnectingDist)
    real(8), intent(in) :: Pos(:,:)
    real(8), intent(inout) :: Point3(:), Point4(:)
    integer, intent(out) :: iConnectingDist
    logical :: CheckCore
    
    
    CheckCore = .false.
end function CheckCore

subroutine GetTriangle(NewTrianglePoint, iNewTriangleDist, iTriangle, Point1, &
                        Point2, Dist, DistUsed)
    real(8), intent(out) :: NewTrianglePoint(2)
    integer, intent(out) :: iNewTriangleDist(2) ! Contains the indexes of the distances corresponding to the new triangle
    integer, intent(in) :: iTriangle 
    real(8), intent(in) :: Point1(:), Point2(:)
    real(8), intent(in) :: Dist(:)
    logical, intent(in) :: DistUsed(:)
    
    integer :: iDist1, iDist2
    
    ! Supply triangle with index # iTriangle
    
    ! Get 2 indices corresponding to distances for the new triangle with index # iTriangle 
    call GetDistIndexCombination(iNewTriangleDist(1), iNewTriangleDist(2), iTriangle, Dist, DistUsed)
    
    call Newpoint(1d0, Dist(iNewTriangleDist(1)), Dist(iNewTriangleDist(2)), NewTrianglePoint)
    
end subroutine GetTriangle

subroutine GetDistIndexCombination(iDist1, iDist2, iTriangle, Dist, DistUsed)
    integer, intent(out) :: iDist1, iDist2
    integer, intent(in) :: iTriangle 
    real(8), intent(in) :: Dist(:)
    logical, intent(in) :: DistUsed(:)
    
    iDist1=2
    iDist2=3
end subroutine GetDistIndexCombination

end program geometry
