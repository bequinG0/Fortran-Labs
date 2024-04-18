program lab6_1
real :: e, y = 1, l, r, x1 = 0, x2 = 0
integer :: n = 0

write(*, *) '[*] y = (0.5x^3 + 2x^2 - 2)cos(x-3)'
write(*, *) '[*] Enter the epsilon:'
read(*, *) e
l = -2.0
r = -1.5
do while(abs(y) > e)
  n = n + 1
  x1 = (l + r)/2
  y = (0.5*x1**3 + 2*x1**2 - 2)*cos(x1-3)
  if(y > 0) then
    l = x1
  else if (y < 0) then
    r = x1
  endif
end do
y = 1
l = -1.5
r = -1.0
do while(abs(y) > e)
  n = n + 1
  x2 = (l + r)/2
  y = (0.5*x2**3 + 2*x2**2 - 2)*cos(x2-3)
  if(y < 0) then
    l = x2
  else if (y > 0) then
    r = x2
  endif
end do

write(*, *) 'x_1 = ', x1, 'x_2 = ', x2, 'count interations = ', n

end program lab6_1