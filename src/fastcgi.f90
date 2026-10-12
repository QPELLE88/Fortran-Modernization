!> Minimal binding to the libfcgi `fcgiapp` interface (FCGX_*).
!> Streams are length-based, so output is binary safe and has no line-length limit.
module fastcgi
  use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char, c_null_char, c_null_ptr, c_loc
  use c_strings, only: c_to_fortran
  use strings, only: string_buffer
  implicit none
  private

  public :: fcgi_request, fcgi_accept, fcgi_finish

  type :: fcgi_request
    type(c_ptr) :: in = c_null_ptr
    type(c_ptr) :: out = c_null_ptr
    type(c_ptr) :: err = c_null_ptr
    type(c_ptr) :: envp = c_null_ptr
  contains
    procedure :: param => request_param
    procedure :: write => request_write
    procedure :: log => request_log
    procedure :: read_body => request_read_body
  end type fcgi_request

  interface
    function fcgx_accept_c(in, out, err, envp) bind(c, name='FCGX_Accept') result(rc)
      import :: c_ptr, c_int
      type(c_ptr), intent(out) :: in, out, err, envp
      integer(c_int) :: rc
    end function fcgx_accept_c

    subroutine fcgx_finish_c() bind(c, name='FCGX_Finish')
    end subroutine fcgx_finish_c

    function fcgx_get_param_c(name, envp) bind(c, name='FCGX_GetParam') result(value)
      import :: c_ptr, c_char
      character(kind=c_char), intent(in) :: name(*)
      type(c_ptr), value :: envp
      type(c_ptr) :: value
    end function fcgx_get_param_c

    function fcgx_put_str_c(str, n, stream) bind(c, name='FCGX_PutStr') result(written)
      import :: c_ptr, c_int, c_char
      character(kind=c_char), intent(in) :: str(*)
      integer(c_int), value :: n
      type(c_ptr), value :: stream
      integer(c_int) :: written
    end function fcgx_put_str_c

    function fcgx_get_str_c(str, n, stream) bind(c, name='FCGX_GetStr') result(nread)
      import :: c_ptr, c_int, c_char
      character(kind=c_char), intent(out) :: str(*)
      integer(c_int), value :: n
      type(c_ptr), value :: stream
      integer(c_int) :: nread
    end function fcgx_get_str_c
  end interface

contains

  !> Wait for the next request. Returns .false. when the server should stop.
  logical function fcgi_accept(request)
    type(fcgi_request), intent(inout) :: request

    fcgi_accept = fcgx_accept_c(request%in, request%out, request%err, request%envp) >= 0
  end function fcgi_accept

  subroutine fcgi_finish()
    call fcgx_finish_c()
  end subroutine fcgi_finish

  !> FastCGI parameter (CGI environment variable) for this request, '' if unset.
  function request_param(self, name) result(value)
    class(fcgi_request), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: value

    value = c_to_fortran(fcgx_get_param_c(name // c_null_char, self%envp))
  end function request_param

  subroutine request_write(self, text)
    class(fcgi_request), intent(in) :: self
    character(len=*), intent(in) :: text
    integer(c_int) :: written

    if (len(text) == 0) return
    written = fcgx_put_str_c(text, int(len(text), c_int), self%out)
  end subroutine request_write

  !> Write a line to the FastCGI error stream (ends up in the web server's error log).
  subroutine request_log(self, text)
    class(fcgi_request), intent(in) :: self
    character(len=*), intent(in) :: text
    integer(c_int) :: written

    written = fcgx_put_str_c(text // achar(10), int(len(text) + 1, c_int), self%err)
  end subroutine request_log

  !> Read up to `max_bytes` of the request body. `too_large` is set when more data remains.
  subroutine request_read_body(self, max_bytes, body, too_large)
    class(fcgi_request), intent(in) :: self
    integer, intent(in) :: max_bytes
    character(len=:), allocatable, intent(out) :: body
    logical, intent(out) :: too_large
    integer, parameter :: CHUNK = 4096
    character(len=CHUNK) :: chunk_buf
    character(len=1) :: probe
    type(string_buffer) :: buf
    integer(c_int) :: nread

    call buf%append('')
    too_large = .false.
    do while (buf%length < max_bytes)
      nread = fcgx_get_str_c(chunk_buf, int(min(CHUNK, max_bytes - buf%length), c_int), self%in)
      if (nread <= 0) exit
      call buf%append(chunk_buf(1:nread))
    end do
    if (buf%length >= max_bytes) then
      too_large = fcgx_get_str_c(probe, 1_c_int, self%in) > 0
    end if
    body = buf%to_string()
  end subroutine request_read_body

end module fastcgi
