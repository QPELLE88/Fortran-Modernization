!> Minimal HTTP request/response model, independent of the transport
!> (FastCGI, tests, ...).
module http
  use string_helpers, only: int_to_str, param_t => key_value_t
  implicit none
  private

  public :: param_t, request_t, response_t
  public :: url_decode, parse_query, get_param, status_text

  type :: request_t
    character(len=:), allocatable :: method
    character(len=:), allocatable :: path
    type(param_t), allocatable :: params(:)
  end type request_t

  type :: response_t
    integer :: status = 200
    character(len=:), allocatable :: content_type
    character(len=:), allocatable :: body
  end type response_t

contains

  !> Decode an application/x-www-form-urlencoded component
  !> ('+' -> space, %XX -> byte). Malformed escapes are kept verbatim.
  pure function url_decode(str) result(decoded)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: decoded
    integer :: i, code, ierr

    decoded = ''
    i = 1
    do while (i <= len(str))
      select case (str(i:i))
      case ('+')
        decoded = decoded // ' '
      case ('%')
        if (i + 2 <= len(str)) then
          read (str(i + 1:i + 2), '(z2)', iostat=ierr) code
          if (ierr == 0 .and. is_hex(str(i + 1:i + 1)) .and. is_hex(str(i + 2:i + 2))) then
            decoded = decoded // achar(code)
            i = i + 3
            cycle
          end if
        end if
        decoded = decoded // '%'
      case default
        decoded = decoded // str(i:i)
      end select
      i = i + 1
    end do
  end function url_decode

  pure logical function is_hex(c)
    character(len=1), intent(in) :: c

    is_hex = index('0123456789abcdefABCDEF', c) > 0
  end function is_hex

  !> Split "a=1&b=2" into decoded key/value pairs, appending to `params`.
  pure subroutine parse_query(query, params)
    character(len=*), intent(in) :: query
    type(param_t), allocatable, intent(inout) :: params(:)
    integer :: start, amp, eq
    character(len=:), allocatable :: pair

    if (.not. allocated(params)) allocate (params(0))

    start = 1
    do while (start <= len(query))
      amp = index(query(start:), '&')
      if (amp == 0) then
        pair = query(start:)
        start = len(query) + 1
      else
        pair = query(start:start + amp - 2)
        start = start + amp
      end if
      if (len(pair) == 0) cycle

      eq = index(pair, '=')
      if (eq == 0) then
        call append_param(params, url_decode(pair), '')
      else
        call append_param(params, url_decode(pair(:eq - 1)), url_decode(pair(eq + 1:)))
      end if
    end do
  end subroutine parse_query

  pure subroutine append_param(params, key, value)
    type(param_t), allocatable, intent(inout) :: params(:)
    character(len=*), intent(in) :: key, value
    type(param_t), allocatable :: grown(:)
    integer :: n

    n = size(params)
    allocate (grown(n + 1))
    grown(1:n) = params
    grown(n + 1)%key = key
    grown(n + 1)%value = value
    call move_alloc(grown, params)
  end subroutine append_param

  !> First value of parameter `key`, or `default` ('' if absent).
  pure function get_param(req, key, default) result(value)
    type(request_t), intent(in) :: req
    character(len=*), intent(in) :: key
    character(len=*), intent(in), optional :: default
    character(len=:), allocatable :: value
    integer :: i

    if (allocated(req%params)) then
      do i = 1, size(req%params)
        if (req%params(i)%key == key) then
          value = req%params(i)%value
          return
        end if
      end do
    end if

    if (present(default)) then
      value = default
    else
      value = ''
    end if
  end function get_param

  pure function status_text(status) result(text)
    integer, intent(in) :: status
    character(len=:), allocatable :: text

    select case (status)
    case (200)
      text = '200 OK'
    case (400)
      text = '400 Bad Request'
    case (404)
      text = '404 Not Found'
    case (405)
      text = '405 Method Not Allowed'
    case (500)
      text = '500 Internal Server Error'
    case default
      text = int_to_str(status)
    end select
  end function status_text

end module http
