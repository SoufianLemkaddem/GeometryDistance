program fft

	implicit none

	integer, parameter :: c = 300, lambda_0 = 800, num = 2**12, range = 2000
	real, parameter :: pi = 4.d0*datan(1.d0), omega_0 = 2*pi*c/lambda_0, delta_t = range/num, delta_om = 2*pi/num*delta_t

	integer, parameter :: w = 100, T = 10000000
	real(8), parameter ::  Pave = 0.001, Po = Pave*T/w
	real(8) :: et, Inten

	integer :: i

	do i = 1, 250

		et = sqrt(Po)*Exp(-2.0*Log(2.0)*(i/w)**2)
		print*, et
		
		!Inten = et**2
		
		!print*, i, Inten

	end do

end program