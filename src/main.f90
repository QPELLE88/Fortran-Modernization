!> FastCGI entry point for Fortran.io. Run behind a web server, e.g.
!>   spawn-fcgi -a 127.0.0.1 -p 9000 ./fortran_fcgi
!> Templates and the database are found relative to $FORTRANIO_ROOT (default: cwd).
program fortran_fcgi
  use, intrinsic :: iso_fortran_env, only: error_unit
  use fastcgi, only: fcgi_request, fcgi_accept, fcgi_finish
  use app, only: app_context, http_request, http_response, handle_request, response_headers
  use sqlite_db, only: SQLITE_OK
  implicit none

  !> Largest accepted request body; larger POSTs get 413.
  integer, parameter :: MAX_BODY_BYTES = 64 * 1024

  type(app_context) :: ctx
  type(fcgi_request) :: fcgi
  type(http_request) :: req
  type(http_response) :: res
  character(len=:), allocatable :: root, db_path
  logical :: too_large

  root = env('FORTRANIO_ROOT', '.')
  db_path = env('FORTRANIO_DB', root // '/marsupials.sqlite3')
  ctx%template_dir = root // '/template'
  if (ctx%db%open(db_path) /= SQLITE_OK) then
    write(error_unit, '(a)') 'fortran_fcgi: cannot open ' // db_path // ': ' // ctx%db%errmsg()
    call ctx%db%close()
  end if

  do while (fcgi_accept(fcgi))
    req%method = fcgi%param('REQUEST_METHOD')
    req%path = fcgi%param('DOCUMENT_URI')
    if (len(req%path) == 0) req%path = request_path(fcgi%param('REQUEST_URI'))
    req%query_string = fcgi%param('QUERY_STRING')
    req%content_type = fcgi%param('CONTENT_TYPE')
    req%body = ''
    too_large = content_length(fcgi%param('CONTENT_LENGTH')) > MAX_BODY_BYTES
    if (.not. too_large .and. req%method == 'POST') then
      call fcgi%read_body(MAX_BODY_BYTES, req%body, too_large)
    end if

    if (too_large) then
      res = payload_too_large()
    else
      res = handle_request(ctx, req)
    end if
    if (allocated(res%log)) call fcgi%log('fortran_fcgi: ' // res%log)

    call fcgi%write(response_headers(res))
    if (req%method /= 'HEAD') call fcgi%write(res%body)
  end do

  call ctx%db%close()
  call fcgi_finish()

contains

  function env(name, default) result(value)
    character(len=*), intent(in) :: name, default
    character(len=:), allocatable :: value
    integer :: n, stat

    call get_environment_variable(name, length=n, status=stat)
    if (stat /= 0 .or. n == 0) then
      value = default
    else
      allocate(character(len=n) :: value)
      call get_environment_variable(name, value)
    end if
  end function env

  function request_path(uri) result(path)
    character(len=*), intent(in) :: uri
    character(len=:), allocatable :: path

    if (index(uri, '?') > 0) then
      path = uri(1:index(uri, '?') - 1)
    else
      path = uri
    end if
  end function request_path

  integer function content_length(text)
    character(len=*), intent(in) :: text
    integer :: ios

    content_length = 0
    if (len_trim(text) == 0) return
    if (verify(trim(text), '0123456789') /= 0 .or. len_trim(text) > 9) then
      content_length = huge(0)
      return
    end if
    read(text, *, iostat=ios) content_length
    if (ios /= 0) content_length = huge(0)
  end function content_length

  function payload_too_large() result(r)
    type(http_response) :: r

    r%status = 413
    r%content_type = 'text/plain; charset=utf-8'
    r%extra_headers = ''
    r%body = 'Request body too large' // achar(10)
  end function payload_too_large

end program fortran_fcgi
