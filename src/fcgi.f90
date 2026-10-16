!> ISO_C_BINDING interfaces to the FastCGI developer's kit (libfcgi,
!> fcgi_stdio.h).
module fcgi
  use, intrinsic :: iso_c_binding, only: c_int, c_char
  implicit none
  private

  public :: fcgi_accept, fcgi_puts, fcgi_getchar

  interface
    !> Block until the web server hands us the next request; < 0 on shutdown.
    function fcgi_accept() bind(c, name='FCGI_Accept')
      import :: c_int
      integer(c_int) :: fcgi_accept
    end function fcgi_accept

    !> Write a NUL-terminated string followed by a newline to the client.
    function fcgi_puts(str) bind(c, name='FCGI_puts')
      import :: c_int, c_char
      character(kind=c_char), intent(in) :: str(*)
      integer(c_int) :: fcgi_puts
    end function fcgi_puts

    !> Read one byte of the request body; -1 at end of input.
    function fcgi_getchar() bind(c, name='FCGI_getchar')
      import :: c_int
      integer(c_int) :: fcgi_getchar
    end function fcgi_getchar
  end interface

end module fcgi
