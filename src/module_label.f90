module label
   implicit none
   private
   public :: labeling

   interface labeling
      module procedure labeling_2d_l4, labeling_2d_i4, labeling_2d_r8
   end interface

contains

   function labeling_2d_l4(mask) result(lab)
      logical(4), intent(in) :: mask(:,:)
      integer(4), allocatable :: lab(:,:)
      integer(8), allocatable :: stack(:)
      integer(8) :: top, p, m8
      integer :: m, n, i, j, ci, cj, ni, nj, k, label_id
      integer, parameter :: di(8) = [-1, 1, 0, 0, -1, -1, 1, 1], dj(8) = [0, 0, -1, 1, -1, 1, -1, 1]

      m = size(mask, 1)
      n = size(mask, 2)
      m8 = int(m, 8)
      allocate(lab(m, n), stack(max(1_8, m8 * n)))
      lab = 0
      label_id = 0

      do j = 1, n
         do i = 1, m
            if (.not. mask(i, j) .or. lab(i, j) /= 0) cycle
            label_id = label_id + 1
            lab(i, j) = label_id
            top = 1
            stack(1) = (j - 1) * m8 + i

            do while (top > 0)
               p = stack(top)
               top = top - 1
               ci = int(mod(p - 1, m8)) + 1
               cj = int((p - 1) / m8) + 1

               do k = 1, 8
                  ni = ci + di(k)
                  nj = cj + dj(k)
                  if (ni < 1 .or. ni > m .or. nj < 1 .or. nj > n) cycle
                  if (.not. mask(ni, nj) .or. lab(ni, nj) /= 0) cycle
                  lab(ni, nj) = label_id
                  top = top + 1
                  stack(top) = (nj - 1) * m8 + ni
               end do
            end do
         end do
      end do
   end function labeling_2d_l4

   function labeling_2d_i4(a) result(lab)
      integer(4), intent(in) :: a(:,:)
      integer(4), allocatable :: lab(:,:)
      lab = labeling_2d_l4(a /= 0)
   end function labeling_2d_i4

   function labeling_2d_r8(a) result(lab)
      real(8), intent(in) :: a(:,:)
      integer(4), allocatable :: lab(:,:)
      lab = labeling_2d_l4(a /= 0.0_8)
   end function labeling_2d_r8

end module label
