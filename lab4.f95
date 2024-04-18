program lab4
  real*8 :: x_0 = 0, x_k = 0, dx = 0, u_x = 0, sm_x = 0, ep_8 = 0
  real*16 :: ep_16 = 0
  real*8, allocatable :: x_vec(:), y_vec(:), n_vec(:)
  integer*16 :: i = 0, i_max = 0, fact = 0, n = 2

  write(*, *) '[*] Enter the x_0, x_k, dx (|x| <= 1): '
  read(*, *) x_0, x_k, dx
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
  i_max = real((x_k - x_0)/dx + 0.5)
  allocate(x_vec(i_max + 1))
  allocate(y_vec(i_max + 1))
  allocate(n_vec(i_max + 1))
  do while(i < i_max + 1)
    x_vec(i) = x_0 + i*dx
    i = i + 1
  end do
  if (mode == 1) then
    ep_8 = 1e-7
    i = 0
    do while(i <= i_max)
      n = 2
      sm_x = x_vec(i)
      u_x =  real(2*x_vec(i)**2)
      do while( abs(u_x/sm_x) > ep_8)
        call factorial(n-1, fact)
        u_x = ((-1)**(n-1))*(2**(n-1)/real(fact))*(x_vec(i)**n)
        sm_x = sm_x + u_x
        n = n + 1
      end do
      n_vec(i) = n
      y_vec(i) = sm_x
      i = i + 1
    end do
    i = 0
    do while(i <= i_max)
      write(*, *) i,  x_vec(i), n_vec(i), y_vec(i), y_vec(i)-x_vec(i)/exp(2*x_vec(i))
      i = i + 1
    end do
  else
    ep_16 = 1e-15
    i = 0
    do while(i <= i_max)
      n = 2
      sm_x = x_vec(i)
      u_x =  real(2*x_vec(i)**2)
      do while( abs(u_x/sm_x) > ep_16)
        call factorial(n-1, fact)
        u_x = ((-1)**(n-1))*(2**(n-1)/real(fact))*(x_vec(i)**n)
        sm_x = sm_x + u_x
        n = n + 1
      end do
      n_vec(i) = n
      y_vec(i) = sm_x
      i = i + 1
    end do
    i = 0
    do while(i <= i_max)
      write(*, *)  i,  x_vec(i), n_vec(i), y_vec(i), y_vec(i)-x_vec(i)/exp(2*x_vec(i))
      i = i + 1
    end do
  endif
end program lab4

subroutine factorial(n, fact)
  implicit none
  integer*16, intent(in) :: n
  integer*16, intent(out) :: fact
  integer :: i = 1
  fact = 1
  do i = 1, n
    fact = fact * i
  end do
end subroutine factorial