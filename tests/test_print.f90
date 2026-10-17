program test_print
   use module_print
   implicit none
   integer(4), allocatable :: i4(:,:)
   integer(8), allocatable :: i8(:,:)
   real(4), allocatable :: r4(:,:)
   real(8), allocatable :: r8(:,:)
   logical(4), allocatable :: l4(:,:)
   integer :: i, j

   ! small matrices: printed in full
   allocate(i4(3,4))
   i4 = reshape([1,-20,300,4000, 5,6,7,8, 9,10,11,12], [3,4])
   call print(i4)

   allocate(i8(2,2))
   i8 = reshape([1_8, -2_8, 3_8, 4_8], [2,2])
   call print(i8)

   allocate(r4(2,3))
   r4 = reshape([1.5, -2.25, 1000.0, 0.001, 3.0, 123456.0], [2,3])
   call print(r4)

   allocate(r8(2,2))
   r8 = reshape([0.1d0, -7.0d0, 2.5d0, 1.0d6], [2,2])
   call print(r8)

   allocate(l4(2,3))
   l4 = reshape([.true., .false., .false., .true., .true., .false.], [2,3])
   call print(l4)

   ! large matrix: rows and columns truncated to corners
   deallocate(i4); allocate(i4(100,100))
   do j = 1, 100
      do i = 1, 100
         i4(i,j) = i*1000 + j
      end do
   end do
   call print(i4)

   deallocate(i8); allocate(i8(5,30))
   i8 = 7_8
   call print(i8)

   deallocate(r8); allocate(r8(10,100))
   r8 = 3.14159265358979d0
   call print(r8)

   ! empty matrix
   deallocate(i4); allocate(i4(0,5))
   call print(i4)

   print '(a)', 'PASS'
end program test_print
