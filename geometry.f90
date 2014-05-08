! *****************************************************************************
! May 2014 - ICCP final project
! *****************************************************************************
program geometry
    use initializers
    use point
    use plotters
    use testpoints
    use TempFcns
    implicit none
    
    ! Variables, see geometry.params for the user-defined parameters
    integer                 :: NumPoints
    real(8)                 :: Margin
    
    real(8), allocatable    :: KnownPos(:,:), Dist(:), Pos(:,:)
    real(8), allocatable    :: TrianglePos(:,:)
    logical, allocatable    :: TrianglesValid(:)
    integer, allocatable    :: iTrianglePosDist(:,:)
    logical, allocatable    :: DistUsed(:)
   
    call Initialize
    
    print *, Dist(:)
    call FindCore
    
    call GeometricDraw(Pos)

	call plot_close
    
contains

!******************************************************************************
! Main initilizing routine
subroutine Initialize
! Main initilizing routine
    call GetParameters(NumPoints, Margin)   
    
    allocate(KnownPos(2, NumPoints), Dist(NumPoints*(NumPoints-1)/2), &
            Pos(2, NumPoints), DistUsed(NumPoints*(NumPoints-1)/2))
    allocate(TrianglePos(2,NumPoints**2))
    allocate(iTrianglePosDist(2,size(TrianglePos,2))) ! TODO size of array?
    print *, 
    allocate(TrianglesValid(size(TrianglePos,2)))
    KnownPos = 0d0
    Dist = 0d0

    call MainSetup(NumPoints, KnownPos, Dist)
    
    DistUsed = .false.
    TrianglesValid = .true.

    Pos = 0
    
end subroutine Initialize

!******************************************************************************
! Corefinding routine
subroutine FindCore
    integer :: iTrianglesStored, iTrianglesChecked,iConnectingDist
    logical :: StopSearch = .false., CoreFound = .false.
    integer :: NumPointsFound
     
    ! Add first segment 
    Pos(:,1) = 0
    Pos(:,2) = [Dist(1), 0.d0]
    DistUsed(1) = .true.
    NumPointsFound = 2
    
    ! Loop through all triangle candidates
    ! If we find that two of the triangles 'match', we have our core.
    do iTrianglesStored=1, size(TrianglePos, 2)
        ! Get a new triangle candidate, and add it to TrianglePos (containing 
        ! the third points of the triangle candidates) and store the indices of
        ! the used distances for the new triangle in iTrianglePosDist.
print *, "Finding a Triangle."
        call GetTriangle(   TrianglePos(:,iTrianglesStored), & !New triangle
                            iTrianglePosDist(:,:), & ! dist indices
                            iTrianglesStored, Dist, DistUsed, StopSearch) !Inputs

        if (StopSearch) then
            print *, 'No more triangles available'
            return
        end if
        if (NumPointsFound .eq. NumPoints) then
            print *, 'All points found'
            return
        end if

       
        print *, "Triangle Found" , iTrianglePosDist(1,iTrianglesStored),&
                     ", ",iTrianglePosDist(2,iTrianglesStored)
        !print *, TrianglePos(:,iTrianglesStored)
        
        if (CoreFound .eqv. .false.) then
            ! Now, try to search for a core in the triangles that are found by now.
            ! If we not succeed in finding a core the loop continues and calculates
            ! an extra triangle.
 
            do iTrianglesChecked=1, iTrianglesStored-1
                ! The 2 triangles should not use a same distance! (except for the shared 
                ! distance) Check this:
                ! Also they should satisfy the triangle inequality
                print *, "Checking core with a 2nd triangle."
                if ( CheckTrianglesUseSameDist(iTrianglesChecked, &
                                        iTrianglesStored, iTrianglePosDist) .and. &
                                        TrianglesValid(iTrianglesStored) .and. &
                                        TrianglesValid(iTrianglesChecked)) then

                    print *, "Found a 2nd Triangle!"

                    ! Check whether the 2 triangles form a valid core
                    if ( CheckCore(Pos(:,2),TrianglePos(:,iTrianglesStored), &
                                 TrianglePos(:,iTrianglesChecked), &
                                 iConnectingDist)) then                            
                            NumPointsFound = 4
                            print *, ' We have a core with triangles:', iTrianglesStored, iTrianglesChecked
                            ! The correct rotation of the second triangle is 
                            ! supplied by the 
                            ! CheckCore routine
                            Pos(:,4) = TrianglePos(:,iTrianglesStored)
                            Pos(:,3) = TrianglePos(:,iTrianglesChecked)
                            DistUsed(iTrianglePosDist(1, iTrianglesStored)) = .true.
                            DistUsed(iTrianglePosDist(2, iTrianglesStored)) = .true.
                            DistUsed(iTrianglePosDist(1, iTrianglesChecked)) = .true.
                            DistUsed(iTrianglePosDist(2, iTrianglesChecked)) = .true.
                            DistUsed(iConnectingDist) = .true.
                            print "(4F12.4)", Pos
                            CoreFound = .true.
                            exit
                     end if
                end if
            enddo
        else if (CoreFound .eqv. .true.) then
            print *, ' Checking triangle candidate #', iTrianglesStored
            print *, Pos(:,3)
            print *, TrianglePos(:,iTrianglesStored)
            print *, 'Distances'
            if (TrianglesValid(iTrianglesStored)) then
                print *, 'Triangle valid'
            if (CheckCore(Pos(:,2), Pos(:,3), &
                            TrianglePos(:,iTrianglesStored), &
                                 iConnectingDist) ) then
                print *, ' Found point number ', NumPointsFound                
                NumPointsFound = NumPointsFound + 1
                Pos(:,NumPointsFound) = TrianglePos(:,iTrianglesStored)
                 
                call CorrDistUsed(NumPointsFound, Dist, Pos, DistUsed)
                       if (NumPointsFound .eq. NumPoints) then
                    print *, 'All points found'
                    return
                        end if

                    end if
               endif
        endif
    enddo
    !print *, 'Core not found'
end subroutine FindCore

!******************************************************************************
! Core finding subroutine that checks whether we have found a core
function CheckCore(Point2, Point3, Point4, iConnectingDist)
    real(8), intent(inout) :: Point4(:)
    real(8), intent(in) :: Point2(:), Point3(:)
    integer, intent(out) :: iConnectingDist
    real(8) :: distance, P4(2,4)
    integer :: iP4
    logical :: GoodCore
    logical :: CheckCore
    
    ! Check for all 4 possible versions of the second triangle.
    ! If we have a match, overwrite Point4 with the matching point
    P4(:,1)=Point4 !We already have the first point
    call GetAlternativeP3(P4(:,2:4), Point2, Point4) !get 3 other points
   
                            !TODO CheckMatch Should check for all 4? 
                             ! possible versions of the second triangle and 
                             ! overwrite Point4 of one of the other 3 is a valid point!!
    print *, P4(:,2)
    print *, P4(:,3)
    print *, P4(:,4)
    
    do iP4=1,4
        call CalcDistance(Point3, P4(:,iP4), distance)
        call DistValid(NumPoints, distance, Dist, DistUsed, GoodCore, &
                            iConnectingDist, Margin)
        
        if(GoodCore) then
            Point4=P4(:,iP4)
            CheckCore = .true. ! We found the core!
            return
        end if
    end do
    CheckCore = .false. !Core not found, no match
    
end function CheckCore

!******************************************************************************
! Subroutine of the corefinding: Supplies a new triangle candidate
subroutine GetTriangle(NewTrianglePoint, iTrianglePosDist, iNewTriangle, &
                        Dist, DistUsed, StopSearch)
    real(8), intent(inout) :: NewTrianglePoint(2)
    integer, intent(inout) :: iTrianglePosDist(:,:) ! Contains the indexes of the distances corresponding to all triangles
    integer, intent(in) :: iNewTriangle 
    !real(8), intent(in) :: Point2(:) !Points that form the basis of the triangle
    real(8), intent(in) :: Dist(:)
    logical, intent(in) :: DistUsed(:)
    logical, intent(inout) :: StopSearch
    
    integer :: iPreviousDist1, iPreviousDist2
    iPreviousDist1 = iTrianglePosDist(1,iNewTriangle-1)
    iPreviousDist2 = iTrianglePosDist(2,iNewTriangle-1)
    
    ! Get 2 indices corresponding to distances for the new triangle with index # iTriangle 
    ! so First get new distances indices!
    ! goal: supply 1->[1,2], 2->[1,3] 3->[2,3], 4->[1,4], 5->[2,4], etc
    !
    if (iNewTriangle .eq. 1) then
        
        iTrianglePosDist(1,iNewTriangle) = GetFirstNonUsedDistIndex(1, DistUsed)
        iTrianglePosDist(2,iNewTriangle) = GetFirstNonUsedDistIndex(iTrianglePosDist&
                                            (1,iNewTriangle), DistUsed)
        
        print *,  iTrianglePosDist(1,iNewTriangle), iTrianglePosDist(2,iNewTriangle)
    else if (iPreviousDist1 .eq. iPreviousDist2-1) then
        print *, '2nd if'
        ! We need to start using a new distance!
        if (iPreviousDist2 .eq. size(Dist)) then
            StopSearch = .true.
            return
        end if
        
        iTrianglePosDist(1,iNewTriangle)= &
                    GetFirstNonUsedDistIndex(1, DistUsed)
        iTrianglePosDist(2,iNewTriangle)= &
                    GetFirstNonUsedDistIndex(iPreviousDist2, DistUsed)
        
        if (iTrianglePosDist(1,iNewTriangle) .eq. &
                    iTrianglePosDist(2,iNewTriangle)) then
              iTrianglePosDist(2,iNewTriangle)=GetFirstNonUsedDistIndex(&
                        iTrianglePosDist(1,iNewTriangle), DistUsed)
        endif
                    
        
        !print *, iTrianglePosDist(2,iNewTriangle),  iTrianglePosDist(1,iNewTriangle)
 
    else
        ! Increase the first distance
        iTrianglePosDist(1,iNewTriangle) = &
                    GetFirstNonUsedDistIndex(iPreviousDist1, DistUsed)
        iTrianglePosDist(2,iNewTriangle) = iPreviousDist2
    end if
    
    ! Check triangle inequality
   if (abs((Dist(iTrianglePosDist(1,iNewTriangle)) &
                        - Dist(iTrianglePosDist(2,iNewTriangle)))) .ge. &
                           Dist(1)) then
        print *, 'Triangle ie not satisfied'
        TrianglesValid(iNewTriangle) = .false.
        return
    endif
        
        
        
        ! We now have the two new indices of the new triangle
        ! Find the corresponding point
        !print *, 'Newpoint'
        !print *, iTrianglePosDist(2, iNewTriangle)
        !print *, Dist(1), Dist(iTrianglePosDist(1, iNewTriangle)), Dist(iTrianglePosDist(2, iNewTriangle)), NewTrianglePoint
        call Newpoint(Dist(1), Dist(iTrianglePosDist(1, iNewTriangle)), Dist(iTrianglePosDist(2, iNewTriangle)), NewTrianglePoint)
    
    
    
end subroutine GetTriangle
!**********************************************************************
end program geometry
