module numpy
   implicit none
   private
   public :: empty, empty_like, zeros_like, ones_like, resize

   interface empty
      module procedure empty_2d_r4, empty_2d_r8, empty_2d_i4, empty_2d_i8, empty_2d_l4
      module procedure empty_1d_r4, empty_1d_r8, empty_1d_i4, empty_1d_i8, empty_1d_l4
   end interface

   interface empty_like
      module procedure empty_like_2d_r4, empty_like_2d_r8, empty_like_2d_i4, empty_like_2d_i8, empty_like_2d_l4
      module procedure empty_like_1d_r4, empty_like_1d_r8, empty_like_1d_i4, empty_like_1d_i8, empty_like_1d_l4
   end interface

   interface zeros_like
      module procedure zeros_like_2d_r4, zeros_like_2d_r8, zeros_like_2d_i4, zeros_like_2d_i8, zeros_like_2d_l4
      module procedure zeros_like_1d_r4, zeros_like_1d_r8, zeros_like_1d_i4, zeros_like_1d_i8, zeros_like_1d_l4
   end interface

   interface ones_like
      module procedure ones_like_2d_r4, ones_like_2d_r8, ones_like_2d_i4, ones_like_2d_i8, ones_like_2d_l4
      module procedure ones_like_1d_r4, ones_like_1d_r8, ones_like_1d_i4, ones_like_1d_i8, ones_like_1d_l4
   end interface

   interface resize
      module procedure resize_2d_r4, resize_2d_r8, resize_2d_i4, resize_2d_i8, resize_2d_l4
      module procedure resize_1d_r4, resize_1d_r8, resize_1d_i4, resize_1d_i8, resize_1d_l4
   end interface

contains

   function empty_2d_r4(m, n, fillvalue) result(a)
      integer, intent(in) :: m, n
      real(4), intent(in) :: fillvalue
      real(4), allocatable :: a(:,:)
      allocate(a(m, n))
      a = fillvalue
   end function empty_2d_r4

   function empty_2d_r8(m, n, fillvalue) result(a)
      integer, intent(in) :: m, n
      real(8), intent(in) :: fillvalue
      real(8), allocatable :: a(:,:)
      allocate(a(m, n))
      a = fillvalue
   end function empty_2d_r8

   function empty_2d_i4(m, n, fillvalue) result(a)
      integer, intent(in) :: m, n
      integer(4), intent(in) :: fillvalue
      integer(4), allocatable :: a(:,:)
      allocate(a(m, n))
      a = fillvalue
   end function empty_2d_i4

   function empty_2d_i8(m, n, fillvalue) result(a)
      integer, intent(in) :: m, n
      integer(8), intent(in) :: fillvalue
      integer(8), allocatable :: a(:,:)
      allocate(a(m, n))
      a = fillvalue
   end function empty_2d_i8

   function empty_2d_l4(m, n, fillvalue) result(a)
      integer, intent(in) :: m, n
      logical(4), intent(in) :: fillvalue
      logical(4), allocatable :: a(:,:)
      allocate(a(m, n))
      a = fillvalue
   end function empty_2d_l4

   function empty_like_2d_r4(prototype) result(a)
      real(4), intent(in) :: prototype(:,:)
      real(4), allocatable :: a(:,:)
      allocate(a(size(prototype, 1), size(prototype, 2)))
   end function empty_like_2d_r4

   function empty_like_2d_r8(prototype) result(a)
      real(8), intent(in) :: prototype(:,:)
      real(8), allocatable :: a(:,:)
      allocate(a(size(prototype, 1), size(prototype, 2)))
   end function empty_like_2d_r8

   function empty_like_2d_i4(prototype) result(a)
      integer(4), intent(in) :: prototype(:,:)
      integer(4), allocatable :: a(:,:)
      allocate(a(size(prototype, 1), size(prototype, 2)))
   end function empty_like_2d_i4

   function empty_like_2d_i8(prototype) result(a)
      integer(8), intent(in) :: prototype(:,:)
      integer(8), allocatable :: a(:,:)
      allocate(a(size(prototype, 1), size(prototype, 2)))
   end function empty_like_2d_i8

   function empty_like_2d_l4(prototype) result(a)
      logical(4), intent(in) :: prototype(:,:)
      logical(4), allocatable :: a(:,:)
      allocate(a(size(prototype, 1), size(prototype, 2)))
   end function empty_like_2d_l4

   function zeros_like_2d_r4(prototype) result(a)
      real(4), intent(in) :: prototype(:,:)
      real(4), allocatable :: a(:,:)
      a = empty(size(prototype, 1), size(prototype, 2), 0.0_4)
   end function zeros_like_2d_r4

   function zeros_like_2d_r8(prototype) result(a)
      real(8), intent(in) :: prototype(:,:)
      real(8), allocatable :: a(:,:)
      a = empty(size(prototype, 1), size(prototype, 2), 0.0_8)
   end function zeros_like_2d_r8

   function zeros_like_2d_i4(prototype) result(a)
      integer(4), intent(in) :: prototype(:,:)
      integer(4), allocatable :: a(:,:)
      a = empty(size(prototype, 1), size(prototype, 2), 0_4)
   end function zeros_like_2d_i4

   function zeros_like_2d_i8(prototype) result(a)
      integer(8), intent(in) :: prototype(:,:)
      integer(8), allocatable :: a(:,:)
      a = empty(size(prototype, 1), size(prototype, 2), 0_8)
   end function zeros_like_2d_i8

   function zeros_like_2d_l4(prototype) result(a)
      logical(4), intent(in) :: prototype(:,:)
      logical(4), allocatable :: a(:,:)
      a = empty(size(prototype, 1), size(prototype, 2), .false._4)
   end function zeros_like_2d_l4

   function ones_like_2d_r4(prototype) result(a)
      real(4), intent(in) :: prototype(:,:)
      real(4), allocatable :: a(:,:)
      a = empty(size(prototype, 1), size(prototype, 2), 1.0_4)
   end function ones_like_2d_r4

   function ones_like_2d_r8(prototype) result(a)
      real(8), intent(in) :: prototype(:,:)
      real(8), allocatable :: a(:,:)
      a = empty(size(prototype, 1), size(prototype, 2), 1.0_8)
   end function ones_like_2d_r8

   function ones_like_2d_i4(prototype) result(a)
      integer(4), intent(in) :: prototype(:,:)
      integer(4), allocatable :: a(:,:)
      a = empty(size(prototype, 1), size(prototype, 2), 1_4)
   end function ones_like_2d_i4

   function ones_like_2d_i8(prototype) result(a)
      integer(8), intent(in) :: prototype(:,:)
      integer(8), allocatable :: a(:,:)
      a = empty(size(prototype, 1), size(prototype, 2), 1_8)
   end function ones_like_2d_i8

   function ones_like_2d_l4(prototype) result(a)
      logical(4), intent(in) :: prototype(:,:)
      logical(4), allocatable :: a(:,:)
      a = empty(size(prototype, 1), size(prototype, 2), .true._4)
   end function ones_like_2d_l4

   function empty_1d_r4(m, fillvalue) result(a)
      integer, intent(in) :: m
      real(4), intent(in) :: fillvalue
      real(4), allocatable :: a(:)
      allocate(a(m))
      a = fillvalue
   end function empty_1d_r4

   function empty_1d_r8(m, fillvalue) result(a)
      integer, intent(in) :: m
      real(8), intent(in) :: fillvalue
      real(8), allocatable :: a(:)
      allocate(a(m))
      a = fillvalue
   end function empty_1d_r8

   function empty_1d_i4(m, fillvalue) result(a)
      integer, intent(in) :: m
      integer(4), intent(in) :: fillvalue
      integer(4), allocatable :: a(:)
      allocate(a(m))
      a = fillvalue
   end function empty_1d_i4

   function empty_1d_i8(m, fillvalue) result(a)
      integer, intent(in) :: m
      integer(8), intent(in) :: fillvalue
      integer(8), allocatable :: a(:)
      allocate(a(m))
      a = fillvalue
   end function empty_1d_i8

   function empty_1d_l4(m, fillvalue) result(a)
      integer, intent(in) :: m
      logical(4), intent(in) :: fillvalue
      logical(4), allocatable :: a(:)
      allocate(a(m))
      a = fillvalue
   end function empty_1d_l4

   function empty_like_1d_r4(prototype) result(a)
      real(4), intent(in) :: prototype(:)
      real(4), allocatable :: a(:)
      allocate(a(size(prototype)))
   end function empty_like_1d_r4

   function empty_like_1d_r8(prototype) result(a)
      real(8), intent(in) :: prototype(:)
      real(8), allocatable :: a(:)
      allocate(a(size(prototype)))
   end function empty_like_1d_r8

   function empty_like_1d_i4(prototype) result(a)
      integer(4), intent(in) :: prototype(:)
      integer(4), allocatable :: a(:)
      allocate(a(size(prototype)))
   end function empty_like_1d_i4

   function empty_like_1d_i8(prototype) result(a)
      integer(8), intent(in) :: prototype(:)
      integer(8), allocatable :: a(:)
      allocate(a(size(prototype)))
   end function empty_like_1d_i8

   function empty_like_1d_l4(prototype) result(a)
      logical(4), intent(in) :: prototype(:)
      logical(4), allocatable :: a(:)
      allocate(a(size(prototype)))
   end function empty_like_1d_l4

   function zeros_like_1d_r4(prototype) result(a)
      real(4), intent(in) :: prototype(:)
      real(4), allocatable :: a(:)
      a = empty(size(prototype), 0.0_4)
   end function zeros_like_1d_r4

   function zeros_like_1d_r8(prototype) result(a)
      real(8), intent(in) :: prototype(:)
      real(8), allocatable :: a(:)
      a = empty(size(prototype), 0.0_8)
   end function zeros_like_1d_r8

   function zeros_like_1d_i4(prototype) result(a)
      integer(4), intent(in) :: prototype(:)
      integer(4), allocatable :: a(:)
      a = empty(size(prototype), 0_4)
   end function zeros_like_1d_i4

   function zeros_like_1d_i8(prototype) result(a)
      integer(8), intent(in) :: prototype(:)
      integer(8), allocatable :: a(:)
      a = empty(size(prototype), 0_8)
   end function zeros_like_1d_i8

   function zeros_like_1d_l4(prototype) result(a)
      logical(4), intent(in) :: prototype(:)
      logical(4), allocatable :: a(:)
      a = empty(size(prototype), .false._4)
   end function zeros_like_1d_l4

   function ones_like_1d_r4(prototype) result(a)
      real(4), intent(in) :: prototype(:)
      real(4), allocatable :: a(:)
      a = empty(size(prototype), 1.0_4)
   end function ones_like_1d_r4

   function ones_like_1d_r8(prototype) result(a)
      real(8), intent(in) :: prototype(:)
      real(8), allocatable :: a(:)
      a = empty(size(prototype), 1.0_8)
   end function ones_like_1d_r8

   function ones_like_1d_i4(prototype) result(a)
      integer(4), intent(in) :: prototype(:)
      integer(4), allocatable :: a(:)
      a = empty(size(prototype), 1_4)
   end function ones_like_1d_i4

   function ones_like_1d_i8(prototype) result(a)
      integer(8), intent(in) :: prototype(:)
      integer(8), allocatable :: a(:)
      a = empty(size(prototype), 1_8)
   end function ones_like_1d_i8

   function ones_like_1d_l4(prototype) result(a)
      logical(4), intent(in) :: prototype(:)
      logical(4), allocatable :: a(:)
      a = empty(size(prototype), .true._4)
   end function ones_like_1d_l4

   function resize_2d_r4(array, m, n, fillvalue) result(a)
      real(4), intent(in) :: array(:,:)
      integer, intent(in) :: m, n
      real(4), intent(in) :: fillvalue
      real(4), allocatable :: a(:,:)
      integer :: ci, cj
      a = empty(m, n, fillvalue)
      ci = min(m, size(array, 1))
      cj = min(n, size(array, 2))
      a(1:ci, 1:cj) = array(1:ci, 1:cj)
   end function resize_2d_r4

   function resize_2d_r8(array, m, n, fillvalue) result(a)
      real(8), intent(in) :: array(:,:)
      integer, intent(in) :: m, n
      real(8), intent(in) :: fillvalue
      real(8), allocatable :: a(:,:)
      integer :: ci, cj
      a = empty(m, n, fillvalue)
      ci = min(m, size(array, 1))
      cj = min(n, size(array, 2))
      a(1:ci, 1:cj) = array(1:ci, 1:cj)
   end function resize_2d_r8

   function resize_2d_i4(array, m, n, fillvalue) result(a)
      integer(4), intent(in) :: array(:,:)
      integer, intent(in) :: m, n
      integer(4), intent(in) :: fillvalue
      integer(4), allocatable :: a(:,:)
      integer :: ci, cj
      a = empty(m, n, fillvalue)
      ci = min(m, size(array, 1))
      cj = min(n, size(array, 2))
      a(1:ci, 1:cj) = array(1:ci, 1:cj)
   end function resize_2d_i4

   function resize_2d_i8(array, m, n, fillvalue) result(a)
      integer(8), intent(in) :: array(:,:)
      integer, intent(in) :: m, n
      integer(8), intent(in) :: fillvalue
      integer(8), allocatable :: a(:,:)
      integer :: ci, cj
      a = empty(m, n, fillvalue)
      ci = min(m, size(array, 1))
      cj = min(n, size(array, 2))
      a(1:ci, 1:cj) = array(1:ci, 1:cj)
   end function resize_2d_i8

   function resize_2d_l4(array, m, n, fillvalue) result(a)
      logical(4), intent(in) :: array(:,:)
      integer, intent(in) :: m, n
      logical(4), intent(in) :: fillvalue
      logical(4), allocatable :: a(:,:)
      integer :: ci, cj
      a = empty(m, n, fillvalue)
      ci = min(m, size(array, 1))
      cj = min(n, size(array, 2))
      a(1:ci, 1:cj) = array(1:ci, 1:cj)
   end function resize_2d_l4

   function resize_1d_r4(array, m, fillvalue) result(a)
      real(4), intent(in) :: array(:)
      integer, intent(in) :: m
      real(4), intent(in) :: fillvalue
      real(4), allocatable :: a(:)
      integer :: c
      a = empty(m, fillvalue)
      c = min(m, size(array))
      a(1:c) = array(1:c)
   end function resize_1d_r4

   function resize_1d_r8(array, m, fillvalue) result(a)
      real(8), intent(in) :: array(:)
      integer, intent(in) :: m
      real(8), intent(in) :: fillvalue
      real(8), allocatable :: a(:)
      integer :: c
      a = empty(m, fillvalue)
      c = min(m, size(array))
      a(1:c) = array(1:c)
   end function resize_1d_r8

   function resize_1d_i4(array, m, fillvalue) result(a)
      integer(4), intent(in) :: array(:)
      integer, intent(in) :: m
      integer(4), intent(in) :: fillvalue
      integer(4), allocatable :: a(:)
      integer :: c
      a = empty(m, fillvalue)
      c = min(m, size(array))
      a(1:c) = array(1:c)
   end function resize_1d_i4

   function resize_1d_i8(array, m, fillvalue) result(a)
      integer(8), intent(in) :: array(:)
      integer, intent(in) :: m
      integer(8), intent(in) :: fillvalue
      integer(8), allocatable :: a(:)
      integer :: c
      a = empty(m, fillvalue)
      c = min(m, size(array))
      a(1:c) = array(1:c)
   end function resize_1d_i8

   function resize_1d_l4(array, m, fillvalue) result(a)
      logical(4), intent(in) :: array(:)
      integer, intent(in) :: m
      logical(4), intent(in) :: fillvalue
      logical(4), allocatable :: a(:)
      integer :: c
      a = empty(m, fillvalue)
      c = min(m, size(array))
      a(1:c) = array(1:c)
   end function resize_1d_l4

end module numpy
