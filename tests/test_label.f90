program test_label
   use label
   use module_asc
   use module_print
   implicit none
   character(len=*), parameter :: fname = 'tests/labels.asc'
   integer, allocatable :: mi(:,:), lab(:,:), flat(:), parent(:)
   logical, allocatable :: seen(:)
   integer :: m, n, nc, i, k, ncomp, nlab, nfail, st

   nfail = 0

   st = readAsc(fname, mi)
   if (st /= 0) then
      print *, 'FAIL: readAsc status ', st
      error stop 1
   end if
   m = size(mi, 1)
   n = size(mi, 2)
   call expect('shape 1024x1024', m == 1024 .and. n == 1024, nfail)

   lab = labeling(mi)
   print '(a)', 'labeled matrix:'
   call print(lab)
   call expect('background stays zero', all((lab /= 0) .eqv. (mi /= 0)), nfail)
   call expect('horizontal neighbours share label', &
        all(lab(1:m-1,:) == lab(2:m,:) .or. mi(1:m-1,:) == 0 .or. mi(2:m,:) == 0), nfail)
   call expect('vertical neighbours share label', &
        all(lab(:,1:n-1) == lab(:,2:n) .or. mi(:,1:n-1) == 0 .or. mi(:,2:n) == 0), nfail)

   ! reference component count from an independent union-find on the same mask
   nc = m*n
   allocate(flat(nc), parent(nc))
   flat = reshape(mi, [nc])
   parent = [(k, k = 1, nc)]
   do k = 1, nc
      if (flat(k) == 0) cycle
      i = mod(k - 1, m) + 1
      if (i < m) then
         if (flat(k + 1) /= 0) call unite(parent, k, k + 1)
      end if
      if (k + m <= nc) then
         if (flat(k + m) /= 0) call unite(parent, k, k + m)
         if (i < m) then
            if (flat(k + m + 1) /= 0) call unite(parent, k, k + m + 1)
         end if
         if (i > 1) then
            if (flat(k + m - 1) /= 0) call unite(parent, k, k + m - 1)
         end if
      end if
   end do
   ncomp = 0
   do k = 1, nc
      if (flat(k) == 0) cycle
      call find(parent, k, i)
      if (i == k) ncomp = ncomp + 1
   end do

   nlab = 0
   if (maxval(lab) == ncomp) then
      allocate(seen(0:ncomp))
      seen = .false.
      seen(pack(lab, mi /= 0)) = .true.
      nlab = count(seen(1:ncomp))
   end if
   call expect('one label per component', nlab == ncomp, nfail)

   print '(a,i0)', 'labels: ', ncomp
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

   subroutine find(parent, x, root)
      integer, intent(inout) :: parent(:)
      integer, intent(in) :: x
      integer, intent(out) :: root
      root = x
      do while (parent(root) /= root)
         parent(root) = parent(parent(root))
         root = parent(root)
      end do
   end subroutine find

   subroutine unite(parent, a, b)
      integer, intent(inout) :: parent(:)
      integer, intent(in) :: a, b
      integer :: ra, rb
      call find(parent, a, ra)
      call find(parent, b, rb)
      if (ra /= rb) parent(max(ra, rb)) = min(ra, rb)
   end subroutine unite

end program test_label
