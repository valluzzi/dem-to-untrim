program test_numpy
   use numpy
   implicit none
   integer :: nfail
   real(8), allocatable :: a(:,:)
   real(4), allocatable :: b(:,:)
   integer(4), allocatable :: c(:,:)
   integer(8), allocatable :: d(:,:)
   logical(4), allocatable :: e(:,:)
   real(8), allocatable :: pa(:,:)
   real(4), allocatable :: pb(:,:)
   integer(4), allocatable :: pc(:,:)
   integer(8), allocatable :: pd(:,:)
   logical(4), allocatable :: pe(:,:)
   real(8), allocatable :: a1(:)
   real(4), allocatable :: b1(:)
   integer(4), allocatable :: c1(:)
   integer(8), allocatable :: d1(:)
   logical(4), allocatable :: e1(:)
   real(8), allocatable :: ra(:,:)
   real(4), allocatable :: rb(:,:)
   integer(4), allocatable :: rc(:,:)
   integer(8), allocatable :: rd(:,:)
   logical(4), allocatable :: re(:,:)
   real(8), allocatable :: ra1(:)
   real(4), allocatable :: rb1(:)
   integer(4), allocatable :: rc1(:)
   integer(8), allocatable :: rd1(:)
   logical(4), allocatable :: re1(:)

   nfail = 0

   a = empty(3, 2, 2.5d0)
   call expect('real8 allocated', allocated(a), nfail)
   call expect('real8 shape', all(shape(a) == [3, 2]), nfail)
   call expect('real8 fill', all(a == 2.5d0), nfail)

   b = empty(4, 5, 1.5)
   call expect('real4 allocated', allocated(b), nfail)
   call expect('real4 shape', all(shape(b) == [4, 5]), nfail)
   call expect('real4 fill', all(b == 1.5), nfail)

   c = empty(2, 7, 3_4)
   call expect('int4 allocated', allocated(c), nfail)
   call expect('int4 shape', all(shape(c) == [2, 7]), nfail)
   call expect('int4 fill', all(c == 3_4), nfail)

   d = empty(6, 1, 4_8)
   call expect('int8 allocated', allocated(d), nfail)
   call expect('int8 shape', all(shape(d) == [6, 1]), nfail)
   call expect('int8 fill', all(d == 4_8), nfail)

   e = empty(1, 3, .true.)
   call expect('logical4 allocated', allocated(e), nfail)
   call expect('logical4 shape', all(shape(e) == [1, 3]), nfail)
   call expect('logical4 fill', all(e), nfail)

   pa = empty_like(a)
   call expect('real8 like', allocated(pa) .and. all(shape(pa) == shape(a)), nfail)
   pb = empty_like(b)
   call expect('real4 like', allocated(pb) .and. all(shape(pb) == shape(b)), nfail)
   pc = empty_like(c)
   call expect('int4 like', allocated(pc) .and. all(shape(pc) == shape(c)), nfail)
   pd = empty_like(d)
   call expect('int8 like', allocated(pd) .and. all(shape(pd) == shape(d)), nfail)
   pe = empty_like(e)
   call expect('logical4 like', allocated(pe) .and. all(shape(pe) == shape(e)), nfail)

   call expect('real8 zeros', all(shape(zeros_like(a)) == shape(a)) .and. all(zeros_like(a) == 0.0_8), nfail)
   call expect('real4 zeros', all(shape(zeros_like(b)) == shape(b)) .and. all(zeros_like(b) == 0.0_4), nfail)
   call expect('int4 zeros', all(shape(zeros_like(c)) == shape(c)) .and. all(zeros_like(c) == 0_4), nfail)
   call expect('int8 zeros', all(shape(zeros_like(d)) == shape(d)) .and. all(zeros_like(d) == 0_8), nfail)
   call expect('logical4 zeros', all(shape(zeros_like(e)) == shape(e)) .and. all(.not. zeros_like(e)), nfail)

   call expect('real8 ones', all(shape(ones_like(a)) == shape(a)) .and. all(ones_like(a) == 1.0_8), nfail)
   call expect('real4 ones', all(shape(ones_like(b)) == shape(b)) .and. all(ones_like(b) == 1.0_4), nfail)
   call expect('int4 ones', all(shape(ones_like(c)) == shape(c)) .and. all(ones_like(c) == 1_4), nfail)
   call expect('int8 ones', all(shape(ones_like(d)) == shape(d)) .and. all(ones_like(d) == 1_8), nfail)
   call expect('logical4 ones', all(shape(ones_like(e)) == shape(e)) .and. all(ones_like(e)), nfail)

   a1 = empty(3, 2.5d0)
   call expect('1d real8 empty', allocated(a1) .and. all(shape(a1) == [3]) .and. all(a1 == 2.5d0), nfail)
   b1 = empty(4, 1.5)
   call expect('1d real4 empty', allocated(b1) .and. all(shape(b1) == [4]) .and. all(b1 == 1.5), nfail)
   c1 = empty(2, 3_4)
   call expect('1d int4 empty', allocated(c1) .and. all(shape(c1) == [2]) .and. all(c1 == 3_4), nfail)
   d1 = empty(6, 4_8)
   call expect('1d int8 empty', allocated(d1) .and. all(shape(d1) == [6]) .and. all(d1 == 4_8), nfail)
   e1 = empty(1, .true.)
   call expect('1d logical4 empty', allocated(e1) .and. all(shape(e1) == [1]) .and. all(e1), nfail)

   call expect('1d real8 like', all(shape(empty_like(a1)) == shape(a1)), nfail)
   call expect('1d real4 like', all(shape(empty_like(b1)) == shape(b1)), nfail)
   call expect('1d int4 like', all(shape(empty_like(c1)) == shape(c1)), nfail)
   call expect('1d int8 like', all(shape(empty_like(d1)) == shape(d1)), nfail)
   call expect('1d logical4 like', all(shape(empty_like(e1)) == shape(e1)), nfail)

   call expect('1d real8 zeros', all(shape(zeros_like(a1)) == shape(a1)) .and. all(zeros_like(a1) == 0.0_8), nfail)
   call expect('1d real4 zeros', all(shape(zeros_like(b1)) == shape(b1)) .and. all(zeros_like(b1) == 0.0_4), nfail)
   call expect('1d int4 zeros', all(shape(zeros_like(c1)) == shape(c1)) .and. all(zeros_like(c1) == 0_4), nfail)
   call expect('1d int8 zeros', all(shape(zeros_like(d1)) == shape(d1)) .and. all(zeros_like(d1) == 0_8), nfail)
   call expect('1d logical4 zeros', all(shape(zeros_like(e1)) == shape(e1)) .and. all(.not. zeros_like(e1)), nfail)

   call expect('1d real8 ones', all(shape(ones_like(a1)) == shape(a1)) .and. all(ones_like(a1) == 1.0_8), nfail)
   call expect('1d real4 ones', all(shape(ones_like(b1)) == shape(b1)) .and. all(ones_like(b1) == 1.0_4), nfail)
   call expect('1d int4 ones', all(shape(ones_like(c1)) == shape(c1)) .and. all(ones_like(c1) == 1_4), nfail)
   call expect('1d int8 ones', all(shape(ones_like(d1)) == shape(d1)) .and. all(ones_like(d1) == 1_8), nfail)
   call expect('1d logical4 ones', all(shape(ones_like(e1)) == shape(e1)) .and. all(ones_like(e1)), nfail)

   ra = resize(a, 4, 3, -1.0_8)
   call expect('real8 resize', all(shape(ra) == [4, 3]) .and. all(ra(1:3, 1:2) == 2.5_8) .and. all(ra(4, :) == -1.0_8) .and. all(ra(:, 3) == -1.0_8), nfail)
   rb = resize(b, 2, 6, 0.0_4)
   call expect('real4 resize', all(shape(rb) == [2, 6]) .and. all(rb(:, 1:5) == 1.5_4) .and. all(rb(:, 6) == 0.0_4), nfail)
   rc = resize(c, 3, 2, -1_4)
   call expect('int4 resize', all(shape(rc) == [3, 2]) .and. all(rc(1:2, :) == 3_4) .and. all(rc(3, :) == -1_4), nfail)
   rd = resize(d, 1, 1, 9_8)
   call expect('int8 resize', all(shape(rd) == [1, 1]) .and. all(rd == 4_8), nfail)
   re = resize(e, 2, 4, .false._4)
   call expect('logical4 resize', all(shape(re) == [2, 4]) .and. all(re(1, 1:3)) .and. (.not. re(1, 4)) .and. all(.not. re(2, :)), nfail)

   ra1 = resize(a1, 5, -1.0_8)
   call expect('1d real8 resize', all(shape(ra1) == [5]) .and. all(ra1(1:3) == 2.5_8) .and. all(ra1(4:5) == -1.0_8), nfail)
   rb1 = resize(b1, 2, 0.0_4)
   call expect('1d real4 resize', all(shape(rb1) == [2]) .and. all(rb1 == 1.5_4), nfail)
   rc1 = resize(c1, 4, -1_4)
   call expect('1d int4 resize', all(shape(rc1) == [4]) .and. all(rc1(1:2) == 3_4) .and. all(rc1(3:4) == -1_4), nfail)
   rd1 = resize(d1, 1, 9_8)
   call expect('1d int8 resize', all(shape(rd1) == [1]) .and. all(rd1 == 4_8), nfail)
   re1 = resize(e1, 3, .false._4)
   call expect('1d logical4 resize', all(shape(re1) == [3]) .and. all(re1(1:1)) .and. all(.not. re1(2:3)), nfail)

   if (nfail > 0) then
      print *, 'FAILED checks:', nfail
      error stop 1
   end if
   print *, 'PASS'

contains

   subroutine expect(label, ok, nfail)
      character(len=*), intent(in) :: label
      logical, intent(in) :: ok
      integer, intent(inout) :: nfail
      if (.not. ok) then
         print *, 'FAIL: ', label
         nfail = nfail + 1
      end if
   end subroutine expect

end program test_numpy
