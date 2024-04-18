program lab6_2
real :: y = 1, dy, x1 = 0, x2 = 0, e
integer :: n
write(*, *) '[*] y = (0.5x^3 + 2x^2 - 2)cos(x-3)'
write(*, *) '[*] Enter the epsilon:'
read(*, *) e

x1 = -1.8
y = 1
do while(abs(y) > e)
  n = n + 1
  dy = (1.5*x1**2 + 4*x1)*cos(x1-3) - sin(x1-3)*(0.5*x1**3 + 2*x1**2 -2)
  y = (0.5*x1**3 + 2*x1**2 - 2)*cos(x1 - 3)
  x1 = x1 - y/dy
end do
x2 = -1.4
y = 1
do while(abs(y) > e)
  n = n + 1
  dy = (1.5*x2**2 + 4*x2)*cos(x2-3) - sin(x2-3)*(0.5*x2**3 + 2*x2**2 -2)
  y = (0.5*x2**3 + 2*x2**2 - 2)*cos(x2 - 3)
  x2 = x2 - y/dy
end do
write(*, *) 'x_1 = ', x1, 'x_2 = ', x2, 'count interations = ', n

end program lab6_2