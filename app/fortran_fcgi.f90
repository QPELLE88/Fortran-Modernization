!> Fortran.io FastCGI server.
!>
!> Run behind nginx (or any FastCGI-capable web server), e.g.:
!>     spawn-fcgi -a 127.0.0.1 -p 9000 ./fortran_fcgi
!>
!> Originally based on the Fortran FastCGI example by Ricolindo Carino
!> and Arjen Markus (FLIBS).
program fortran_fcgi
  use, intrinsic :: iso_c_binding, only: c_int, c_null_char
  use fcgi, only: fcgi_accept, fcgi_puts, fcgi_getchar
  use http, only: request_t, response_t, parse_query, status_text
  use fortran_io_app, only: handle_request
  use string_helpers, only: starts_with
  implicit none

  !> Upper bound on accepted application/x-www-form-urlencoded bodies.
  integer, parameter :: MAX_BODY_BYTES = 1024 * 1024
  character(len=*), parameter :: CR = achar(13)

  type(request_t) :: req
  type(response_t) :: res

  do while (fcgi_accept() >= 0)
    call read_request(req)
    call handle_request(req, res)
    call send_response(res)
  end do

contains

  function getenv_str(name) result(value)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: value
    integer :: length, status

    call get_environment_variable(name, length=length, status=status)
    if (status /= 0 .or. length == 0) then
      value = ''
      return
    end if
    allocate (character(len=length) :: value)
    call get_environment_variable(name, value=value)
  end function getenv_str

  subroutine read_request(req)
    type(request_t), intent(out) :: req
    character(len=:), allocatable :: body, length_str
    integer :: content_length, ios, i
    integer(c_int) :: ch

    req%method = getenv_str('REQUEST_METHOD')
    if (len(req%method) == 0) req%method = 'GET'

    req%path = getenv_str('DOCUMENT_URI')
    if (len(req%path) == 0) req%path = '/'

    allocate (req%params(0))
    call parse_query(getenv_str('QUERY_STRING'), req%params)

    if (req%method /= 'POST') return
    if (.not. starts_with(getenv_str('CONTENT_TYPE'), 'application/x-www-form-urlencoded')) return

    length_str = getenv_str('CONTENT_LENGTH')
    if (len(length_str) == 0) return
    read (length_str, *, iostat=ios) content_length
    if (ios /= 0 .or. content_length <= 0) return
    content_length = min(content_length, MAX_BODY_BYTES)

    allocate (character(len=content_length) :: body)
    do i = 1, content_length
      ch = fcgi_getchar()
      if (ch < 0) then
        body = body(:i - 1)
        exit
      end if
      body(i:i) = achar(ch)
    end do
    call parse_query(body, req%params)
  end subroutine read_request

  subroutine send_response(res)
    type(response_t), intent(in) :: res
    integer(c_int) :: rc

    ! FCGI_puts appends '\n', so each header ends in CRLF.
    rc = fcgi_puts('Status: ' // status_text(res%status) // CR // c_null_char)
    rc = fcgi_puts('Content-Type: ' // res%content_type // CR // c_null_char)
    rc = fcgi_puts('X-Content-Type-Options: nosniff' // CR // c_null_char)
    rc = fcgi_puts(CR // c_null_char)
    rc = fcgi_puts(res%body // c_null_char)
  end subroutine send_response

end program fortran_fcgi
