!> Fortran.io FastCGI server: accept requests from the web server and dispatch to the app.
program fortran_fcgi
  use fastcgi, only: fcgi_accept, fcgi_read_body, fcgi_write
  use http, only: request_t, response_t, request_from_env, error_response, &
                  serialize_response, content_length_from_env
  use app, only: app_config_t, config_from_env, handle_request
  implicit none

  integer, parameter :: MAX_BODY_BYTES = 64 * 1024

  type(app_config_t) :: cfg
  type(request_t) :: req
  type(response_t) :: resp
  character(len=:), allocatable :: body
  integer :: content_length

  cfg = config_from_env()

  do while (fcgi_accept())
    content_length = content_length_from_env()
    if (content_length < 0) then
      resp = error_response(400, 'Invalid Content-Length.')
    else if (content_length > MAX_BODY_BYTES) then
      resp = error_response(413, 'Request body too large.')
    else
      call fcgi_read_body(content_length, body)
      req = request_from_env(body)
      resp = handle_request(req, cfg)
    end if
    call fcgi_write(serialize_response(resp))
  end do

end program fortran_fcgi
