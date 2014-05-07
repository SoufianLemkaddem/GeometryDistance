! **********************************************************************************
! Module containing temporary functions for geometry.f90
! **********************************************************************************
module TempFcns
    implicit none

    private

    public DistValid
    public CheckTrianglesUseSameDist

contains
!
! Checks if triangles use the same distance (T if DONT, F is do)
function CheckTrianglesUseSameDist(iTriangle1, iTriangle2, iTrianglePosDist)
    integer, intent(in) :: iTriangle1, iTriangle2
    integer, intent(in) :: iTrianglePosDist(:,:)
    logical CheckTrianglesUseSameDist
    
    CheckTrianglesUseSameDist = .true.
end function CheckTrianglesUseSameDist
!**********************************************************************
!**********************************************************************************   
! Reads 'geometry.params'
subroutine DistValid(NumPoints, DistGiven, Dist, DistUsed, CanUse, FinalIndex)
    integer, intent(in) :: NumPoints
    real(8), intent(in) :: DistGiven !Distance to check
    real(8), intent(in) :: Dist(:)
    logical, intent(in) :: DistUsed(:)
    logical, intent(inout) :: CanUse !True if distance is avaliable
    integer, intent(inout) :: FinalIndex !Index of the found Pt.
    integer :: TotLen
    integer :: Start, Finish, RemRange, Midpt

    TotLen = NumPoints*(NumPoints-1)/2
    Start = 1
    Finish = TotLen
    RemRange = Finish - Start
    Midpt = (Start+Finish)/2

    do while((Dist(Midpt) /= DistGiven).and.(RemRange > 0))
        if(DistGiven>Dist(Midpt))then
            Start = Midpt+1
        else
            Finish = Midpt-1
        end if
        RemRange = Finish-Start
        Midpt = (Start+Finish)/2
    end do

    if((DistUsed(Midpt)) .or. (Dist(Midpt) /= DistGiven))then
        CanUse = .false.
        FinalIndex = 0
    else
        CanUse = .true.
        FinalIndex = Midpt
    end if
end subroutine DistValid
!**********************************************************************
end module
