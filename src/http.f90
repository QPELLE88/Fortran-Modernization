!> CGI-style request parsing and response serialisation.
module http
  use strings, only: url_decode, starts_with, int_to_str, html_escape, CRLF
  implicit none
  private

  public :: param_t, request_t, response_t
  public :: new_request, request_from_env, get_param, parse_form
  public :: html_response, error_response, serialize_response, status_text
  public :: get_env, content_length_from_env

  type :: param_t
    character(len=:), allocatable :: key
    character(len=:), allocatable :: value
  end type param_t

  type :: request_t
    character(len=:), allocatable :: method
    character(len=:), allocatable :: path
    character(len=:), allocatable :: query_string
    type(param_t), allocatable :: params(:)
  end type request_t

  type :: response_t
    integer :: status = 200
    character(len=:), allocatable :: content_type
    character(len=:), allocatable :: body
  end type response_t

contains

  !> Build a request; form-encoded POST bodies are merged after the query-string params.
  function new_request(method, path, query_string, body, content_type) result(req)
    character(len=*), intent(in) :: method, path, query_string
    character(len=*), intent(in), optional :: body, content_type
    type(request_t) :: req
    type(param_t), allocatable :: query_params(:), body_params(:)
    integer :: nq

    req%method = method
    req%path = path
    if (len(req%path) == 0) req%path = '/'
    req%query_string = query_string
    call parse_form(query_string, query_params)
    call parse_form('', body_params)
    if (present(body) .and. present(content_type) .and. method == 'POST') then
      if (starts_with(content_type, 'application/x-www-form-urlencoded')) then
        call parse_form(body, body_params)
      end if
    end if

    nq = size(body_params)
    allocate(req%params(nq + size(query_params)))
    req%params(:nq) = body_params
    req%params(nq+1:) = query_params
  end function new_request

  !> Build a request from the CGI environment that FastCGI exposes for the current request.
  function request_from_env(body) result(req)
    character(len=*), intent(in) :: body
    type(request_t) :: req
    character(len=:), allocatable :: path, uri
    integer :: q

    path = get_env('DOCUMENT_URI')
    if (len(path) == 0) then
      uri = get_env('REQUEST_URI')
      q = index(uri, '?')
      if (q > 0) uri = uri(:q-1)
      path = uri
    end if

    req = new_request(get_env('REQUEST_METHOD'), path, get_env('QUERY_STRING'), &
                      body, get_env('CONTENT_TYPE'))
  end function request_from_env

  !> Split "a=1&b=2" into decoded key/value pairs, preserving order.
  subroutine parse_form(s, params)
    character(len=*), intent(in) :: s
    type(param_t), allocatable, intent(out) :: params(:)
    type(param_t), allocatable :: found(:)
    integer :: start, amp, finish, eq, n
    character(len=:), allocatable :: pair

    allocate(found(count_pairs(s)))
    n = 0
    start = 1
    do while (start <= len(s))
      amp = index(s(start:), '&')
      if (amp == 0) then
        finish = len(s)
      else
        finish = start + amp - 2
      end if
      pair = s(start:finish)
      if (len(pair) > 0) then
        n = n + 1
        eq = index(pair, '=')
        if (eq == 0) then
          found(n)%key = url_decode(pair)
          found(n)%value = ''
        else
          found(n)%key = url_decode(pair(:eq-1))
          found(n)%value = url_decode(pair(eq+1:))
        end if
      end if
      start = finish + 2
    end do
    allocate(params(n))
    params(:) = found(:n)
  end subroutine parse_form

  pure integer function count_pairs(s) result(n)
    character(len=*), intent(in) :: s
    integer :: i

    n = 1
    do i = 1, len(s)
      if (s(i:i) == '&') n = n + 1
    end do
  end function count_pairs

  !> First value for key, or default when absent.
  function get_param(req, key, default) result(value)
    type(request_t), intent(in) :: req
    character(len=*), intent(in) :: key, default
    character(len=:), allocatable :: value
    integer :: i

    value = default
    do i = 1, size(req%params)
      if (req%params(i)%key == key) then
        value = req%params(i)%value
        return
      end if
    end do
  end function get_param

  function html_response(body, status) result(resp)
    character(len=*), intent(in) :: body
    integer, intent(in), optional :: status
    type(response_t) :: resp

    resp%status = 200
    if (present(status)) resp%status = status
    resp%content_type = 'text/html; charset=utf-8'
    resp%body = body
  end function html_response

  function error_response(status, message) result(resp)
    integer, intent(in) :: status
    character(len=*), intent(in) :: message
    type(response_t) :: resp

    resp = html_response('<!DOCTYPE html><html><head><meta charset="utf-8"/><title>' // &
                         int_to_str(status) // ' ' // status_text(status) // &
                         '</title></head><body><h1>' // status_text(status) // &
                         '</h1><p>' // html_escape(message) // '</p></body></html>', status)
  end function error_response

  function serialize_response(resp) result(raw)
    type(response_t), intent(in) :: resp
    character(len=:), allocatable :: raw

    raw = 'Status: ' // int_to_str(resp%status) // ' ' // status_text(resp%status) // CRLF // &
          'Content-Type: ' // resp%content_type // CRLF // &
          'X-Content-Type-Options: nosniff' // CRLF // &
          CRLF // resp%body
  end function serialize_response

  pure function status_text(status) result(text)
    integer, intent(in) :: status
    character(len=:), allocatable :: text

    select case (status)
    case (200)
      text = 'OK'
    case (400)
      text = 'Bad Request'
    case (404)
      text = 'Not Found'
    case (413)
      text = 'Payload Too Large'
    case (414)
      text = 'URI Too Long'
    case (500)
      text = 'Internal Server Error'
    case default
      text = 'Unknown'
    end select
  end function status_text

  !> Value of an environment variable, or '' when unset.
  function get_env(name) result(value)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: value
    integer :: n, stat

    call get_environment_variable(name, length=n, status=stat)
    if (stat /= 0 .or. n == 0) then
      value = ''
      return
    end if
    allocate(character(len=n) :: value)
    call get_environment_variable(name, value=value)
  end function get_env

  !> CONTENT_LENGTH as an integer; 0 when unset, -1 when malformed.
  integer function content_length_from_env() result(n)
    character(len=:), allocatable :: raw
    integer :: ios

    raw = get_env('CONTENT_LENGTH')
    n = 0
    if (len(raw) == 0) return
    if (verify(raw, '0123456789') /= 0 .or. len(raw) > 9) then
      n = -1
      return
    end if
    read (raw, *, iostat=ios) n
    if (ios /= 0) n = -1
  end function content_length_from_env

end module http
