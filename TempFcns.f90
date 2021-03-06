! *********************************************************************
! Module containing temporary functions for geometry.f90
! *********************************************************************
module TempFcns
    use point
    implicit none

    private

    public DistValid
    public CheckTrianglesUseSameDist
    public GetFirstNonUsedDistIndex
    public CorrDistUsed

contains
!**********************************************************************
!
! Checks if triangles use the same distance (T if DONT, F is do)
function CheckTrianglesUseSameDist(iTriangle1, iTriangle2, iTrianglePosDist)
    integer, intent(in) :: iTriangle1, iTriangle2
    integer, intent(in) :: iTrianglePosDist(:,:)
    logical CheckTrianglesUseSameDist
    
    CheckTrianglesUseSameDist = .false.
    
    ! checks none of the 4 distance indicies are equal to another
    if (iTrianglePosDist(1,iTriangle1) /= iTrianglePosDist(2,iTriangle1)) then
        if (iTrianglePosDist(2,iTriangle1) /= iTrianglePosDist(1,iTriangle2)) then
        if (iTrianglePosDist(1,iTriangle2) /= iTrianglePosDist(2,iTriangle2)) then
        if (iTrianglePosDist(2,iTriangle1) /= iTrianglePosDist(2,iTriangle2)) then
        if (iTrianglePosDist(1,iTriangle1) /= iTrianglePosDist(2,iTriangle2)) then
        if (iTrianglePosDist(1,iTriangle1) /= iTrianglePosDist(1,iTriangle2)) then
            CheckTrianglesUseSameDist = .true.
        end if
        end if
        end if
        end if
        end if
    end if
end function CheckTrianglesUseSameDist
!**********************************************************************
!
! Reads 'geometry.params'
subroutine DistValid(NumPoints, DistGiven, Dist, DistUsed, CanUse, FinalIndex, Margin)
    integer, intent(in) :: NumPoints
    real(8), intent(in) :: DistGiven, Margin !Distance to check
    real(8), intent(in) :: Dist(:)
    logical, intent(in) :: DistUsed(:)
    logical, intent(inout) :: CanUse !True if distance is avaliable
    integer, intent(inout) :: FinalIndex !Index of the found Pt.
    integer :: TotLen, i
    integer :: Start, Finish, RemRange, Midpt

    TotLen = NumPoints*(NumPoints-1)/2
    Start = 1
    Finish = TotLen
    RemRange = Finish - Start
    Midpt = (Start+Finish)/2

    do while(((Dist(Midpt) > DistGiven*(1.0d0+Margin)).or.(Dist(Midpt) < DistGiven*(1.0d0-Margin))).and.(RemRange > 0))
!print *, 'Max',DistGiven*(1.0d0+Margin)
!print *, 'MIDPT',Dist(Midpt)
!print *, 'Min',DistGiven*(1.0d0-Margin)
!    do i = 1, TotLen
!        if (((Dist(i) < DistGiven*(1.0d0+Margin)).and.(Dist(i) > DistGiven*(1.0d0-Margin)))) then
!            CanUse = .false.
!            exit
!        end if
        if(DistGiven>Dist(Midpt))then
            Start = Midpt+1
        else
            Finish = Midpt-1
        end if
        RemRange = Finish-Start
        Midpt = (Start+Finish)/2
    end do

!print *, DistGiven
!print *, Dist(Midpt)

    if((DistUsed(Midpt)) .or. ((Dist(Midpt) > DistGiven*(1.0d0+Margin)).or.(Dist(Midpt) < DistGiven*(1.0d0-Margin))))then
        CanUse = .false.
        FinalIndex = 0
    else
        CanUse = .true.
        FinalIndex = Midpt
        print *, 'Valid Distance found! Data:', Midpt, Dist(Midpt)
        print *, 'Calc distance:', DistGiven
    end if
end subroutine DistValid
!**********************************************************************
!
! Checks if triangles use the same distance (T if DONT, F is do)
function GetFirstNonUsedDistIndex(iPreviousDist2, DistUsed)
    integer, intent(in) :: iPreviousDist2
    logical, intent(in) :: DistUsed(:)
    integer :: GetFirstNonUsedDistIndex
    integer :: MaxLen, NUDIter

    MaxLen = size(DistUsed, 1)

    GetFirstNonUsedDistIndex = 1

    do NUDIter = iPreviousDist2+1, MaxLen
        if(.not.(DistUsed(NUDIter))) then
            GetFirstNonUsedDistIndex = NUDIter
            exit
        end if
    end do
end function GetFirstNonUsedDistIndex
!**********************************************************************
!
! Determine all used distances
subroutine CorrDistUsed(NumPointsFound, Dist, Pos, DistUsed, Margin)
    integer, intent(in) :: NumPointsFound
    real(8), intent(in) :: Dist(:), Pos(:,:), Margin
    logical, intent(inout) :: DistUsed(:)
    integer :: CDUIter
    real(8) :: DistTemp
    integer :: i, Start, Finish, RemRange, Midpt

    Start = 1
    Finish = size(Dist, 1)
    RemRange = Finish - Start
    Midpt = (Start+Finish)/2

    do CDUIter = 1, NumPointsFound-1
        call CalcDistance(Pos(:,NumPointsFound),Pos(:,CDUIter), DistTemp)

        do i = 1, Finish
            if (Dist(i) < DistTemp*(1.0d0+Margin) .and. Dist(i)>DistTemp*(1.0d0-Margin)) then
                print *, 'Distance eliminated', i
                DistUsed(i)=.true.
            end if
        end do
    end do
end subroutine CorrDistUsed
!**********************************************************************
end module
