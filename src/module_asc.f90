module module_asc
   implicit none
   private
   public :: readAsc
   integer, parameter :: rp = 8
   integer, parameter :: cap = 64*1024*1024   ! read buffer size (bytes)
   integer, parameter :: maxtok = 256         ! longest token guaranteed to fit in the buffer

   type :: reader
      integer :: u
      integer(8) :: fsize, fpos               ! file size and next byte to load (1-based)
      integer :: n = 0, p = 1                 ! unread bytes are buf(p:n)
      character(len=:), allocatable :: buf
   end type

   interface readAsc
      module procedure readAsc_real, readAsc_int
   end interface

contains

   integer function open_grid(r, filename, ncols, nrows, nodata) result(st)
      type(reader), intent(inout) :: r
      character(len=*), intent(in) :: filename
      integer, intent(out) :: ncols, nrows
      real(rp), intent(out) :: nodata
      integer :: ios
      integer(8) :: fsize

      st = 1
      open(newunit=r%u, file=filename, status='old', action='read', &
           access='stream', form='unformatted', iostat=ios)
      if (ios /= 0) then
         print *, 'readAsc: cannot open file ', trim(filename)
         return
      end if
      inquire(unit=r%u, size=fsize)
      r%fsize = fsize
      r%fpos = 1
      allocate(character(len=cap) :: r%buf)
      st = read_header(r, filename, ncols, nrows, nodata)
      if (st /= 0) close(r%u)

   end function open_grid

   integer function read_header(r, filename, ncols, nrows, nodata) result(st)
      type(reader), intent(inout) :: r
      character(len=*), intent(in) :: filename
      integer, intent(out) :: ncols, nrows
      real(rp), intent(out) :: nodata
      character(len=64) :: key, val
      integer :: i, ts
      real(rp) :: x
      logical :: ok

      st = 1
      ncols = 0
      nrows = 0
      nodata = -9999.0_rp
      do i = 1, 6
         call get_token(r, key, ts)
         if (ts == 0) call get_token(r, val, ts)
         if (ts /= 0) then
            print *, 'readAsc: unexpected end of header in ', trim(filename)
            return
         end if
         select case (trim(key))
         case ('NODATA_value', 'nodata_value')
            call to_real(trim(val), nodata, ok)
            if (.not. ok) then
               print *, 'readAsc: invalid NODATA_value in ', trim(filename)
               return
            end if
         case ('ncols', 'nrows')
            call to_real(trim(val), x, ok)
            if (.not. ok) then
               print *, 'readAsc: invalid header value in ', trim(filename)
               return
            end if
            if (trim(key) == 'ncols') then
               ncols = nint(x)
            else
               nrows = nint(x)
            end if
         end select
      end do
      if (ncols <= 0 .or. nrows <= 0) then
         print *, 'readAsc: invalid ncols/nrows in ', trim(filename)
         return
      end if
      st = 0

   end function read_header

   integer function read_row(r, filename, vals) result(st)
      type(reader), intent(inout) :: r
      character(len=*), intent(in) :: filename
      real(rp), intent(out) :: vals(:)
      integer :: j, s, e, ts
      logical :: ok

      st = 1
      do j = 1, size(vals)
         call next_token(r, s, e, ts)
         if (ts /= 0) then
            print *, 'readAsc: unexpected end of data in ', trim(filename)
            return
         end if
         call to_real(r%buf(s:e), vals(j), ok)
         if (.not. ok) then
            print *, 'readAsc: invalid number "', r%buf(s:e), '" in ', trim(filename)
            return
         end if
      end do
      st = 0

   end function read_row

   ! Reads an ESRI ASCII grid (.asc). Returns m(ncols,nrows) in iw orientation: m(:,nrows) is the first (northern) file row.
   integer function readAsc_real(filename, m, load_nodata_as) result(st)
      character(len=*), intent(in) :: filename
      real(rp), allocatable, intent(out) :: m(:,:)
      real(rp), intent(in), optional :: load_nodata_as
      type(reader) :: r
      integer :: ncols, nrows, i
      real(rp) :: nodata

      st = open_grid(r, filename, ncols, nrows, nodata)
      if (st /= 0) return
      allocate(m(ncols, nrows))
      do i = 1, nrows
         st = read_row(r, filename, m(:,nrows-i+1))
         if (st /= 0) exit
         if (present(load_nodata_as)) then
            where (m(:,nrows-i+1) == nodata) m(:,nrows-i+1) = load_nodata_as
         end if
      end do
      close(r%u)

   end function readAsc_real

   integer function readAsc_int(filename, m, load_nodata_as) result(st)
      character(len=*), intent(in) :: filename
      integer, allocatable, intent(out) :: m(:,:)
      integer, intent(in), optional :: load_nodata_as
      type(reader) :: r
      real(rp), allocatable :: row(:)
      integer :: ncols, nrows, i
      real(rp) :: nodata

      st = open_grid(r, filename, ncols, nrows, nodata)
      if (st /= 0) return
      allocate(m(ncols, nrows), row(ncols))
      do i = 1, nrows
         st = read_row(r, filename, row)
         if (st /= 0) exit
         if (present(load_nodata_as)) then
            where (row == nodata) row = real(load_nodata_as, rp)
         end if
         m(:,nrows-i+1) = nint(row)
      end do
      close(r%u)

   end function readAsc_int


   subroutine get_token(r, str, st)
      type(reader), intent(inout) :: r
      character(len=*), intent(out) :: str
      integer, intent(out) :: st
      integer :: s, e
      call next_token(r, s, e, st)
      if (st == 0) str = r%buf(s:e)
   end subroutine get_token

   ! Loads more bytes, keeping the unread tail at the start of the buffer.
   subroutine refill(r)
      type(reader), intent(inout) :: r
      integer :: nkeep, nread
      nkeep = max(0, r%n - r%p + 1)
      if (nkeep > 0 .and. r%p > 1) r%buf(1:nkeep) = r%buf(r%p:r%n)
      r%p = 1
      r%n = nkeep
      nread = int(min(int(cap - nkeep, 8), r%fsize - r%fpos + 1))
      if (nread > 0) then
         read(r%u, pos=r%fpos) r%buf(nkeep+1:nkeep+nread)
         r%fpos = r%fpos + nread
         r%n = nkeep + nread
      end if
   end subroutine refill

   ! Returns buffer indices s:e of the next token; st is 0 on success, 1 at end of file, 2 if token too long.
   subroutine next_token(r, s, e, st)
      type(reader), intent(inout) :: r
      integer, intent(out) :: s, e, st
      integer :: q

      st = 1
      do
         if (r%n - r%p < maxtok .and. r%fpos <= r%fsize) call refill(r)
         do while (r%p <= r%n)
            if (iachar(r%buf(r%p:r%p)) > 32) exit
            r%p = r%p + 1
         end do
         if (r%p <= r%n) then
            if (r%n - r%p >= maxtok .or. r%fpos > r%fsize) exit
         else if (r%fpos > r%fsize) then
            return
         end if
      end do

      s = r%p
      q = s
      do while (q <= r%n)
         if (iachar(r%buf(q:q)) <= 32) exit
         q = q + 1
      end do
      if (q > r%n .and. r%fpos <= r%fsize) then
         print *, 'readAsc: token longer than maxtok'
         st = 2
         return
      end if
      e = q - 1
      r%p = q
      st = 0
   end subroutine next_token

   ! Converts a decimal string. Exact fast path for up to 15 digits and |exponent| <= 22; read(*) otherwise.
   subroutine to_real(str, x, ok)
      character(len=*), intent(in) :: str
      real(rp), intent(out) :: x
      logical, intent(out) :: ok
      integer(8) :: mant
      integer :: i, n, ndig, ex, ev, nexp, ios
      character :: c
      logical :: neg, eneg, dot, fast

      n = len(str)
      i = 1
      neg = .false.
      dot = .false.
      fast = .true.
      mant = 0
      ndig = 0
      ex = 0

      if (n >= 1) then
         if (str(1:1) == '-' .or. str(1:1) == '+') then
            neg = str(1:1) == '-'
            i = 2
         end if
      end if

      do while (i <= n)
         c = str(i:i)
         if (c == '.' .and. .not. dot) then
            dot = .true.
         else if (c >= '0' .and. c <= '9') then
            ndig = ndig + 1
            if (ndig <= 15) then
               mant = mant*10 + (iachar(c) - 48)
            else
               fast = .false.
            end if
            if (dot) ex = ex - 1
         else
            exit
         end if
         i = i + 1
      end do

      if (i <= n) then
         c = str(i:i)
         if (c == 'e' .or. c == 'E' .or. c == 'd' .or. c == 'D') then
            i = i + 1
            eneg = .false.
            if (i <= n) then
               if (str(i:i) == '-' .or. str(i:i) == '+') then
                  eneg = str(i:i) == '-'
                  i = i + 1
               end if
            end if
            ev = 0
            nexp = 0
            do while (i <= n)
               c = str(i:i)
               if (c < '0' .or. c > '9') exit
               if (ev < 10000) ev = ev*10 + (iachar(c) - 48)
               nexp = nexp + 1
               i = i + 1
            end do
            if (nexp == 0 .or. ev >= 10000) fast = .false.
            if (eneg) then
               ex = ex - ev
            else
               ex = ex + ev
            end if
         end if
      end if

      if (i <= n .or. ndig == 0) fast = .false.

      if (fast .and. ex >= -22 .and. ex <= 22) then
         ok = .true.
         x = real(mant, rp)
         if (ex < 0) then
            x = x / 10.0_rp**(-ex)
         else if (ex > 0) then
            x = x * 10.0_rp**ex
         end if
         if (neg) x = -x
      else
         read(str, *, iostat=ios) x
         ok = ios == 0
      end if
   end subroutine to_real

end module module_asc
