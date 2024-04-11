program lab2

integer :: n, temp, i = 0, j = 0, ch = 0, no_zer_size = 0, fake_size = 0
integer, allocatable :: vector(:)

write(*, *) '[*] Put the dimension: '
read(*, *) n
allocate(vector(n))
write(*, *) '[*] Put the vector: '
do while(i<n)
  read(*, *) temp
  vector(i) = temp
  i = i + 1
end do
i = 0
fake_size = n
do while(i<fake_size)
  if(vector(i) == 0) then
    j = i
    do while(j<n)
      vector(j) = vector(j+1)
      vector(j+1) = 0
      j = j + 1 
    end do
  endif
  i = i + 1
end do
i = 0
do while(i<n)
  write(*, *) vector(i)
  i = i + 1
end do

end program lab2