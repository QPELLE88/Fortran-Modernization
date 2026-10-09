!> Minimal ISO_C_BINDING interface to libfcgi's stdio layer (fcgi_stdio.h).
!> When the binary is not started under a FastCGI server it behaves as plain CGI.
module fastcgi
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char
  implicit none
  private

  public :: fcgi_accept, fcgi_read_body, fcgi_write

  interface
    integer(c_int) function c_fcgi_accept() bind(C, name='FCGI_Accept')
      import :: c_int
    end function c_fcgi_accept

    integer(c_int) function c_fcgi_getchar() bind(C, name='FCGI_getchar')
      import :: c_int
    end function c_fcgi_getchar

    integer(c_int) function c_fcgi_puts(s) bind(C, name='FCGI_puts')
      import :: c_int, c_char
      character(kind=c_char), intent(in) :: s(*)
    end function c_fcgi_puts
  end interface

contains

  !> Wait for the next request; .false. once the server shuts the connection down.
  logical function fcgi_accept()
    fcgi_accept = c_fcgi_accept() >= 0
  end function fcgi_accept

  !> Read exactly content_length bytes of request body (fewer if the stream ends early).
  subroutine fcgi_read_body(content_length, body)
    integer, intent(in) :: content_length
    character(len=:), allocatable, intent(out) :: body
    integer :: i, ch

    allocate(character(len=max(content_length, 0)) :: body)
    do i = 1, len(body)
      ch = c_fcgi_getchar()
      if (ch < 0) then
        body = body(:i-1)
        return
      end if
      body(i:i) = char(ch)
    end do
  end subroutine fcgi_read_body

  !> Write a complete raw response (headers + body) to the web server.
  subroutine fcgi_write(raw)
    character(len=*), intent(in) :: raw
    integer(c_int) :: rc

    rc = c_fcgi_puts(raw // c_null_char)
  end subroutine fcgi_write

end module fastcgi
