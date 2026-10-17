module module_print
   implicit none
   private
   public :: print

   integer, parameter :: console_width = 80
   integer, parameter :: max_rows = 30
   integer, parameter :: max_cand = 80
   integer, parameter :: cell_len = 32

   interface print
      module procedure print_2d_r4, print_2d_r8, print_2d_i4, print_2d_i8, print_2d_l4
   end interface

contains

   subroutine print_2d_r4(a)
      real(4), intent(in) :: a(:,:)
      character(len=cell_len), allocatable :: s(:,:)
      integer, allocatable :: ri(:), ci(:)
      integer :: i, j

      call select_index(size(a, 1), max_rows, ri)
      call select_index(size(a, 2), max_cand, ci)
      allocate(s(size(ri), size(ci)))
      do j = 1, size(ci)
         do i = 1, size(ri)
            if (ri(i) == 0 .or. ci(j) == 0) then
               s(i, j) = '...'
            else
               write(s(i, j), '(g12.4)') a(ri(i), ci(j))
               s(i, j) = adjustl(s(i, j))
            end if
         end do
      end do
      call emit_matrix(s, size(a, 2))
   end subroutine print_2d_r4

   subroutine print_2d_r8(a)
      real(8), intent(in) :: a(:,:)
      character(len=cell_len), allocatable :: s(:,:)
      integer, allocatable :: ri(:), ci(:)
      integer :: i, j

      call select_index(size(a, 1), max_rows, ri)
      call select_index(size(a, 2), max_cand, ci)
      allocate(s(size(ri), size(ci)))
      do j = 1, size(ci)
         do i = 1, size(ri)
            if (ri(i) == 0 .or. ci(j) == 0) then
               s(i, j) = '...'
            else
               write(s(i, j), '(g14.6)') a(ri(i), ci(j))
               s(i, j) = adjustl(s(i, j))
            end if
         end do
      end do
      call emit_matrix(s, size(a, 2))
   end subroutine print_2d_r8

   subroutine print_2d_i4(a)
      integer(4), intent(in) :: a(:,:)
      character(len=cell_len), allocatable :: s(:,:)
      integer, allocatable :: ri(:), ci(:)
      integer :: i, j

      call select_index(size(a, 1), max_rows, ri)
      call select_index(size(a, 2), max_cand, ci)
      allocate(s(size(ri), size(ci)))
      do j = 1, size(ci)
         do i = 1, size(ri)
            if (ri(i) == 0 .or. ci(j) == 0) then
               s(i, j) = '...'
            else
               write(s(i, j), '(i0)') a(ri(i), ci(j))
            end if
         end do
      end do
      call emit_matrix(s, size(a, 2))
   end subroutine print_2d_i4

   subroutine print_2d_i8(a)
      integer(8), intent(in) :: a(:,:)
      character(len=cell_len), allocatable :: s(:,:)
      integer, allocatable :: ri(:), ci(:)
      integer :: i, j

      call select_index(size(a, 1), max_rows, ri)
      call select_index(size(a, 2), max_cand, ci)
      allocate(s(size(ri), size(ci)))
      do j = 1, size(ci)
         do i = 1, size(ri)
            if (ri(i) == 0 .or. ci(j) == 0) then
               s(i, j) = '...'
            else
               write(s(i, j), '(i0)') a(ri(i), ci(j))
            end if
         end do
      end do
      call emit_matrix(s, size(a, 2))
   end subroutine print_2d_i8

   subroutine print_2d_l4(a)
      logical(4), intent(in) :: a(:,:)
      character(len=cell_len), allocatable :: s(:,:)
      integer, allocatable :: ri(:), ci(:)
      integer :: i, j

      call select_index(size(a, 1), max_rows, ri)
      call select_index(size(a, 2), max_cand, ci)
      allocate(s(size(ri), size(ci)))
      do j = 1, size(ci)
         do i = 1, size(ri)
            if (ri(i) == 0 .or. ci(j) == 0) then
               s(i, j) = '...'
            else
               write(s(i, j), '(l1)') a(ri(i), ci(j))
            end if
         end do
      end do
      call emit_matrix(s, size(a, 2))
   end subroutine print_2d_l4

   subroutine select_index(n, nmax, idx)
      integer, intent(in) :: n, nmax
      integer, allocatable, intent(out) :: idx(:)
      integer :: i, head, tail

      if (n <= nmax) then
         idx = [(i, i = 1, n)]
      else
         head = (nmax - 1) / 2
         tail = nmax - 1 - head
         idx = [(i, i = 1, head), 0, (i, i = n - tail + 1, n)]
      end if
   end subroutine select_index

   subroutine emit_matrix(s, ncols)
      character(len=*), intent(in) :: s(:,:)
      integer, intent(in) :: ncols
      integer, allocatable :: sel(:)
      integer :: i, k, c, w, nfit, nc, hh, tt
      character(len=:), allocatable :: line

      if (size(s) == 0) then
         write(*, '(a)') '[]'
         return
      end if

      nc = size(s, 2)
      w = max(3, maxval(len_trim(s)))
      nfit = max(3, console_width / (w + 1))

      if (ncols <= nfit) then
         sel = [(c, c = 1, nc)]
      else
         hh = (nfit - 1) / 2
         tt = nfit - 1 - hh
         sel = [(c, c = 1, hh), 0, (c, c = nc - tt + 1, nc)]
      end if

      do i = 1, size(s, 1)
         line = ''
         do k = 1, size(sel)
            if (k > 1) line = line // ' '
            if (sel(k) == 0) then
               line = line // repeat(' ', w - 3) // '...'
            else
               line = line // repeat(' ', w - len_trim(s(i, sel(k)))) // trim(s(i, sel(k)))
            end if
         end do
         write(*, '(a)') line
      end do
   end subroutine emit_matrix

end module module_print
