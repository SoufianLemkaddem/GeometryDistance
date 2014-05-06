! **********************************************************************************
! Module containing plotter functions for geometry.f90
! **********************************************************************************
module plotters
	use plplot
	implicit none
    public GeometricDraw, plot_positions, Plot_init


contains

!***********************************************************************************
!Initialize 

! **********************************************************************************
! Main plotting routine
	subroutine GeometricDraw(pointPosition)
    	real(8), intent(in) :: pointPosition(:,:)
        call plot_init(pointPosition)
        call plot_positions(pointPosition)
    end subroutine GeometricDraw
    
    
! **********************************************************************************
! Plots polymer
    subroutine plot_Positions (Pointpos)
        real(8), intent(in) :: Pointpos(:, :)
        integer :: i
        
        !call pljoin(0d0, 0d0, -10d0, 0d0)
        !call pljoin(-10d0, 0d0, -10d0, -10d0)
        !call pljoin(-10d0, -10d0, 0d0, -10d0)
        !call pljoin(0d0, -10d0, 0d0, 0d0)
        
        ! Draw molecules
        do i = 1, size(PointPos, 1)
            call plcol0(13)
            call plpoin([PointPos(i,1)], [PointPos(i,2)], 21)
        end do

        ! Draw lines
        !do i = 2, size(polymerPosPlot, 1)
          !  call plcol0(9)
         !   call pljoin(polymerPosPlot(i-1,1), polymerPosPlot(i-1,2), &
        !                polymerPosPlot(i,1), polymerPosPlot(i,2))
       ! end do

        !call plflush()
        
    end subroutine plot_Positions
    
! **********************************************************************************
! Initilizes plot
    subroutine plot_init(PointPos)
        real(8), intent(in) :: PointPos(:, :)
        real(8) :: maxX, maxY, minX, minY, margin
        call plsdev("xcairo")
        call plinit()
        !call plcol0(0)
        !You can find default colors at
        !http://plplot.sourceforge.net/docbook-manual/plplot-html-5.9.9/plcol0.html

        call plscol0(0, 255, 255, 255)  ! white
        !call plscol0(1, 255, 0, 0)      ! red
        !call plscol0(2, 255, 77, 0)     ! orange
        !call plscol0(3, 255, 255, 0)    ! yellow
        !call plscol0(4, 0, 255, 0)      ! green
        !call plscol0(5, 0, 0, 255)      ! blue
        !call plscol0(6, 0, 255, 255)    ! cyan
        !call plscol0(7, 255, 0, 255)    ! magenta
        !call plscol0(8, 128, 128, 128)  ! gray
        !call plscol0(9, 0, 0, 0)        ! black
        maxX = maxval([PointPos(:,1)])
        minX = minval([PointPos(:,1)])
        maxY = maxval([PointPos(:,2)])
        minY = minval([PointPos(:,2)])
        margin = 10d0
    
        call plenv(minX-margin, maxX+margin, minY-margin, maxY+margin, 1, 0)
    end subroutine plot_init

! **********************************************************************************
! Closes plot
  subroutine plot_close()
    call plspause(.true.)
    call plend()
  end subroutine plot_close

end module plotters

