program lab1
  integer :: n = 0
  real :: x, res = 0
  
  write(*, *) '[*] Enter x: '
  read(*, *) x
  write(*, *) '[*] Enter n: '
  read(*, *) n
  do while (n < 2 .or. n > 5)
    write(*, *) '[ER] value n is incorrect!'
    write(*, *) '[*] Enter n: '
    read(*, *) n
  end do
  write(*, *) '[*] x =', x
  write(*, *) '[*] n =', n
  
  if(n == 2) then
    res = 0.5*(2*cos(x) + 1)
    write(*, *) '[*] Result: ', res
  else if(n == 3) then
    res = 0.25*(cos(3*x) + 3*cos(x))
    write(*, *) '[*] Result: ', res
  else if(n == 4) then
    res = 0.125*(cos(4*x) + 4*cos(2*x) + 3)
    write(*, *) '[*] Result: ', res
  else if(n == 5) then
    res = 0.0625*(cos(5*x) + 5*cos(3*x) + 10*cos(x))
    write(*, *) '[*] Result: ', res
  else
    write(*, *) '[*] Result: ', res
  endif
end program lab1