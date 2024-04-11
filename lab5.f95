program lab5
  real, allocatable :: A(:,:), b(:)
  integer :: temp = 0, n = 0, m = 0, k = 0, i = 0, j = 0, l = 0

  write(*, *) '[*] Enter matrix size: '
  read(*, *) n
  allocate(A(n , n))
  allocate(b(n**2/4 - 1))
  write(*, *) '[*] Enter matrix: '
  do while(i < n) 
    j = 0
    do while(j < n) 
      read(*, *) A(i, j)
      j = j + 1
    end do
    i = i + 1
  end do
  write(*, *) '[*] Enter m, k (m < k): '
  read(*, *) m, k
  do while (m >= k)
    write(*, *) '[ER] values m, k are incorrect! '
    write(*, *) '[*] Enter m, k (m < k): '
    read(*, *) m, k
  end do
  i = 0
  j = 0
  do while(i < n) 
    j = 0
    do while(j < n) 
      temp = floor(A(i,j))
      if(abs(temp - A(i,j)) == 0 .and. (A(i, j) >= m .and. A(i, j) <= k) .and. i < j .and. j>n-i-1) then
        b(l) = A(i, j)
        l = l + 1
      endif
      j = j + 1
    end do
    i = i + 1
  end do
  write(*, *) b
end program lab5

