! **********************************************************************************
! Module containing temporary functions for geometry.f90
! **********************************************************************************
module initializers
    implicit none

    private

    public DistValid

contains
! **********************************************************************************   
! Reads 'geometry.params'
subroutine DistValid(NumPoints, DistGiven, Dist, DistUsed, CanUse)
    integer, intent(in) :: NumPoints
    real(8), intent(in) :: DistGiven
    real(8), intent(in) :: Dist(:)
    logical, intent(in) :: DistUsed(:)
    logical, intent(inout) :: CanUse
    integer :: TotLen = NumPoints*(NumPoints-1)/2
    integer :: Start, Finish, RemRange, Midpt

    Start = 1
    Finish = TotLen
    RemRange = Finish - Start
    Midpt = (Start+Finish)/2

    do while((Dist(Midpt)/=DistGiven).and.(RemRange>0))

    end do
end subroutine DistValid
!**********************************************************************
end module
