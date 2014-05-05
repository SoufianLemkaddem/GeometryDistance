! **********************************************************************************
! Module containing initilizing functions for geometry.f90
! **********************************************************************************
module initializers
    implicit none
    private

    public GetParameters
contains
! **********************************************************************************   
! Reads 'geometry.params'
subroutine GetParameters(NumPoints)
        integer, intent(out) :: NumPoints

        open(12, file="geometry.params")
        read(12,*) NumPoints
        close(12)
end subroutine GetParameters

end module
