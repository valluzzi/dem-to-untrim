program test_asc
   use module_asc
   implicit none
   integer, parameter :: rp = 8
   character(len=*), parameter :: fname = 'tests/lidar_V7.asc'
   real(rp), allocatable :: m(:,:)
   real(rp) :: vmin, vmax
   integer :: nrows, ncols, nnodata, status
   integer, allocatable :: mi(:,:)

   status = readAsc(fname, m)
   if (status /= 0) then
      print *, 'FAIL: readAsc status ', status
      stop 1
   end if
   ncols = size(m, 1)
   nrows = size(m, 2)
   nnodata = count(m == -9999.0_rp)
   vmin = minval(m, mask = m /= -9999.0_rp)
   vmax = maxval(m, mask = m /= -9999.0_rp)

   print *, 'file    : ', fname
   print *, 'nrows   : ', nrows
   print *, 'ncols   : ', ncols
   print *, 'nodata  : ', nnodata
   print *, 'min     : ', vmin
   print *, 'max     : ', vmax
   print *, 'first   : ', m(1,nrows), m(2,nrows)
   print *, 'last    : ', m(ncols-1,1), m(ncols,1)

   if (nrows /= 13162 .or. ncols /= 10831) then
      print *, 'FAIL: unexpected shape'
      stop 1
   end if
   status = readAsc(fname, mi)
   if (status /= 0) then
      print *, 'FAIL: integer readAsc status ', status
      stop 1
   end if
   if (any(shape(mi) /= shape(m)) .or. count(mi == -9999) /= nnodata) then
      print *, 'FAIL: integer grid mismatch'
      stop 1
   end if
   print *, 'PASS'
end program test_asc
