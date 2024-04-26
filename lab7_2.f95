program lab7_2
  integer :: x = 0, n = 0, out = 0
  write(*, *) '[*] Enter the n: '
  read(*, *) n
  write(*, *) 'Enter the x: '
  read(*, *) x
  call hermit(n, x, out)
  write(*, *) out
  
end program lab7_2

recursive subroutine hermit(n, x, out)
  implicit  none
  integer, intent(in) :: n, x
  integer, intent(inout) :: out
  integer :: out1, out2
  if (n == 0) then  
    out = 1
    return
  endif
  if (n == 1) then
    out = 2*x
    return
  endif
  call hermit(n-1, x, out1)
  call hermit(n-2, x, out2)
  out = 2*x*out1 - 2*(n-1)*out2
end subroutine