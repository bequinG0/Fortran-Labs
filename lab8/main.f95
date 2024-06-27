module fruit_module
  implicit none
  public Fruit, Apple

  type :: Fruit
  private
    character(len=:), allocatable :: name
    character(len=:), allocatable :: color
    character(len=:), allocatable :: taste
    real :: weight

end module fruit_module

program main
  write(*, *) '1'
end program main
  