! **********************************************************************************
! Module containing initilizing functions for geometry.f90
! **********************************************************************************
module initializers
    implicit none
    private

    public GetParameters
    public GetKnownPos
    public FillDistKP
    public FillDistTxt
    public SortDist

contains
! **********************************************************************************   
! Reads 'geometry.params'
subroutine GetParameters(NumPoints)
    integer, intent(out) :: NumPoints

    open(12, file="geometry.params")
    read(12,*) NumPoints
    close(12)
end subroutine GetParameters
!**********************************************************************
!
! Reads in the positions from a file of NumPoint lines
subroutine GetKnownPos(NumPoints, KnownPos)
    integer, intent(in) :: NumPoints
    real(8), intent(out) :: KnownPos(2,NumPoints)
    integer :: KPIter

    open(24, file='PosInput.txt')
    do KPIter = 1, NumPoints
        read(24,*) KnownPos(1,KPIter), KnownPos(2,KPIter)
    end do    

    close(24)
end subroutine GetKnownPos
!**********************************************************************
!
! Fills the distance from a 2,NumPoints array of X,Y coords
subroutine FillDistKP(NumPoints, KnownPos, Dist)
    integer, intent(in) :: NumPoints
    real(8), intent(in) :: KnownPos(2,NumPoints)
    real(8), intent(out) :: Dist(NumPoints*(NumPoints-1)/2)
    integer :: FDIter1, FDIter2, DistCount

    DistCount = 1
    do FDIter1 = 1, NumPoints-1
        do FDIter2 = FDIter1+1, NumPoints
            Dist(DistCount) = SQRT(((KnownPos(1,FDIter1)-KnownPos(1,FDIter2))**2)+((KnownPos(2,FDIter1)-KnownPos(2,FDIter2))**2))

            DistCount = DistCount + 1
        end do
    end do
end subroutine FillDistKP
!**********************************************************************
!
! Reads in the distances from a file of NumPoint*(NumPoint-1)/2 lines
subroutine FillDistTxt(NumPoints, Dist)
    integer, intent(in) :: NumPoints
    real(8), intent(out) :: Dist(NumPoints*(NumPoints-1)/2)
    integer :: FDTIter

    open(25, file='PosInput.txt')
    do FDTIter = 1, NumPoints*(NumPoints-1)/2
        read(25,*) Dist(FDTIter)
    end do    

    close(25)
end subroutine FillDistTxt
!**********************************************************************
!
! Sorts Array from smallest to largest
subroutine SortDist(NumPoints, Dist)
    integer, intent(in) :: NumPoints
    real(8), intent(out) :: Dist(NumPoints*(NumPoints-1)/2)
    integer :: SDIter1, SDIter2
    real(8) :: SDTemp

    do SDIter1 = 1, NumPoints-1
        do SDIter2 = 1, NumPoints-SDIter1
            if (Dist(SDIter1) > Dist(SDIter2)) then
                SDTemp = Dist(SDIter1)
                Dist(SDIter1) = Dist(SDIter2)
                Dist(SDIter2) = SDTemp
            end if
        end do
    end do
end subroutine SortDist
!**********************************************************************
end module
