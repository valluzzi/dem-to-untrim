program test_label
   use label
   use module_print
   implicit none
   integer, parameter :: n = 10
   integer :: i, j, nfail
   integer(4) :: ai(n,n), expected(n,n)
   logical(4) :: mask(n,n)
   real(8) :: ar(n,n)
   integer(4), allocatable :: lab(:,:)

   nfail = 0

   ! vertical stripes: odd columns are foreground, each is its own component
   do j = 1, n
      mask(:,j) = mod(j, 2) == 1
   end do
   ai = merge(1, 0, mask)
   ar = real(ai, 8)
   expected = 0
   do j = 1, n, 2
      expected(:,j) = (j + 1) / 2
   end do

   lab = labeling(mask)
   call expect('logical4 stripes', all(lab == expected), nfail)

   lab = labeling(ai)
   call expect('int4 stripes', all(lab == expected), nfail)

   lab = labeling(ar)
   call expect('real8 stripes', all(lab == expected), nfail)
   print '(a)', 'stripes labeled:'
   call print(lab)

   ! checkerboard: no two foreground cells are 4-adjacent
   do j = 1, n
      do i = 1, n
         ai(i,j) = merge(1, 0, mod(i + j, 2) == 0)
      end do
   end do

   lab = labeling(ai)
   call expect('checkerboard count', count(lab > 0) == n*n/2 .and. maxval(lab) == n*n/2, nfail)
   call expect('checkerboard isolated', all((lab > 0) .eqv. (ai /= 0)), nfail)
   print '(a)', 'checkerboard labeled:'
   call print(lab)

   ! full foreground: one single component
   ai = 1
   lab = labeling(ai)
   call expect('full foreground single label', all(lab == 1), nfail)

   ai = 0
   lab = labeling(ai)
   call expect('empty labeling', all(lab == 0), nfail)

   if (nfail > 0) then
      print *, 'FAILED checks:', nfail
      error stop 1
   end if
   print *, 'PASS'

contains

   subroutine expect(label_text, ok, nfail)
      character(len=*), intent(in) :: label_text
      logical, intent(in) :: ok
      integer, intent(inout) :: nfail
      if (.not. ok) then
         print *, 'FAIL: ', label_text
         nfail = nfail + 1
      end if
   end subroutine expect

end program test_label
