! **********************************************************************************
! Module containing initilizing functions for geometry.f90
! **********************************************************************************
module initializers
    implicit none

    private

    public GetKnownPos
    public FillDistKP
    public FillDistTxt
    public SortDist
    public GetParameters
    public MainSetup

contains
! **********************************************************************************   
! Reads 'geometry.params'
subroutine GetParameters(NumPoints, Margin)
    integer, intent(inout) :: NumPoints
    real(8), intent(inout) :: Margin

    open(12, file="geometry.params")
    read(12,*) NumPoints
    read(12,*) Margin
    close(12)
end subroutine GetParameters
!**********************************************************************
!
! Reads in the positions from a file of NumPoint lines
subroutine GetKnownPos(NumPoints, PosFile, KnownPos)
    integer, intent(in) :: NumPoints
    character (len=50), intent(in) :: PosFile
    real(8), intent(inout) :: KnownPos(:,:)
    integer :: KPIter

    open(24, file=PosFile)
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
    real(8), intent(in) :: KnownPos(:,:)
    real(8), intent(inout) :: Dist(:)
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
subroutine FillDistTxt(NumPoints, DistFile, Dist)
    integer, intent(in) :: NumPoints
    character (len=50), intent(in) :: DistFile
    real(8), intent(inout) :: Dist(:)
    integer :: FDTIter

    open(25, file=DistFile)
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
    real(8), intent(inout) :: Dist(:)
    integer :: SDIter1, SDIter2
    real(8) :: SDTemp

    do SDIter1 = 1, (NumPoints*(NumPoints-1)/2)-1
        do SDIter2 = 1, (NumPoints*(NumPoints-1)/2)-SDIter1
            if (Dist(SDIter2) > Dist(SDIter2+1)) then
                SDTemp = Dist(SDIter2)
                Dist(SDIter2) = Dist(SDIter2+1)
                Dist(SDIter2+1) = SDTemp
            end if
        end do
    end do
end subroutine SortDist
!**********************************************************************
!
! Completly initalizes KnownPos & Dist
subroutine MainSetup(NumPoints, KnownPos, Dist)
    integer, intent(in) :: NumPoints
    real(8), intent(inout) :: KnownPos(:,:)
    real(8), intent(inout) :: Dist(:)
    logical :: UseDFile
    character (len=50) :: PosFile, DistFile

    open(26, file='initalizers.params')
    read(26,*) UseDFile
    read(26,*) PosFile
    read(26,*) DistFile
    close(26)

    if (UseDFile) then
        call FillDistTxt(NumPoints, DistFile, Dist)
    else
        call GetKnownPos(NumPoints, PosFile, KnownPos)
        call FillDistKP(NumPoints, KnownPos, Dist)
    end if

    call SortDist(NumPoints, Dist)
end subroutine MainSetup
!**********************************************************************
end module
