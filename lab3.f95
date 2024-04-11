program lab3
integer :: mode = 0
real*8 :: p_8 = 2, oldpi_8 = 1, ep_8 = 0
real*16 :: p_16 = 2, oldpi_16 = 1, ep_16 = 0, n = 1

write(*, *) '[*] Please select the mode: '
write(*, *) '[1] e = 10^(-7)'
write(*, *) '[2] e = 10^(-15)'
read(*, *) mode
do while (mode < 1 .or. mode > 2)
  write(*, *) '[ER] value mode is incorrect!'
  write(*, *) '[*] Please select the mode: '
  write(*, *) '[1] e = 1e-7'
  write(*, *) '[2] e = 1e-15'
  read(*, *) mode
  if(mode == 1 .or. mode == 2) then
    write(*, *) mode
  endif
end do
if (mode == 1) then
  ep_8 = 1e-7
  do while (abs( p_8 - oldpi_8 ) > ep_8)
    oldpi_8 = p_8
    p_8 = p_8 * ( (4*(n**2))/((4*(n**2) - 1)) )
    n = n + 1
  end do
  
  write(*, *) 'Value pi: ', p_8
else
  ep_16 = 1e-15
  do while (abs(oldpi_16 - p_16) .gt. ep_16)
    oldpi_16 = p_16
    p_16 = p_16 * ((4*(n**2))/(4*(n**2) - 1) )
    n = n + 1
  end do

  write(*, *) 'Value pi: ', p_16
endif
end program lab3