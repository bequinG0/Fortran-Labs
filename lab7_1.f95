
program lab7_1
integer, allocatable :: vec(:)
integer :: i = 2, n = 0, a = 0, x = 0

write(*, *) '[*] Enter the n: '
read(*, *) n
write(*, *) '[*] Enter the x: '
read(*, *) x
allocate(vec(n+1))

vec(0) = 1
vec(1) = 2*x

do while(i < n )
  vec(i) = 2*x*vec(i-1) - 2*(i-1)*vec(i-2)
  i = i + 1
end do

i = 0
do while(i < n)
  write(*, *) vec(i)
  i = i + 1
end do

end program lab7_1