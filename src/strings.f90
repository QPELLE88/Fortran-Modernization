!> Character-string helpers built on deferred-length allocatable strings.
module strings
  implicit none
  private

  public :: string_t
  public :: html_escape, join, url_decode, replace_all, to_lower, starts_with, int_to_str

  character(len=1), parameter, public :: LF = achar(10)
  character(len=1), parameter, public :: CR = achar(13)
  character(len=2), parameter, public :: CRLF = CR // LF

  type :: string_t
    character(len=:), allocatable :: s
  end type string_t

contains

  !> Escape a value for safe inclusion in HTML text or a quoted attribute.
  pure function html_escape(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r, e
    integer :: i, n

    n = 0
    do i = 1, len(s)
      e = escape_char(s(i:i))
      n = n + len(e)
    end do
    allocate(character(len=n) :: r)
    n = 0
    do i = 1, len(s)
      e = escape_char(s(i:i))
      r(n+1:n+len(e)) = e
      n = n + len(e)
    end do
  end function html_escape

  pure function escape_char(c) result(e)
    character(len=1), intent(in) :: c
    character(len=:), allocatable :: e

    select case (c)
    case ('&')
      e = '&amp;'
    case ('<')
      e = '&lt;'
    case ('>')
      e = '&gt;'
    case ('"')
      e = '&quot;'
    case ("'")
      e = '&#39;'
    case default
      if (iachar(c) < 32 .and. c /= achar(9) .and. c /= LF .and. c /= CR) then
        e = '&#xFFFD;'
      else
        e = c
      end if
    end select
  end function escape_char

  !> Concatenate parts in a single allocation.
  pure function join(parts) result(r)
    type(string_t), intent(in) :: parts(:)
    character(len=:), allocatable :: r
    integer :: i, n

    n = 0
    do i = 1, size(parts)
      n = n + len(parts(i)%s)
    end do
    allocate(character(len=n) :: r)
    n = 0
    do i = 1, size(parts)
      r(n+1:n+len(parts(i)%s)) = parts(i)%s
      n = n + len(parts(i)%s)
    end do
  end function join

  !> Decode application/x-www-form-urlencoded text; malformed escapes are kept verbatim.
  pure function url_decode(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    character(len=len(s)) :: buf
    integer :: i, n, hi, lo

    n = 0
    i = 1
    do while (i <= len(s))
      n = n + 1
      buf(n:n) = s(i:i)
      i = i + 1
      if (s(i-1:i-1) == '+') then
        buf(n:n) = ' '
      else if (s(i-1:i-1) == '%' .and. i + 1 <= len(s)) then
        hi = hex_value(s(i:i))
        lo = hex_value(s(i+1:i+1))
        if (hi >= 0 .and. lo >= 0) then
          buf(n:n) = char(hi * 16 + lo)
          i = i + 2
        end if
      end if
    end do
    r = buf(:n)
  end function url_decode

  pure integer function hex_value(c) result(v)
    character(len=1), intent(in) :: c

    select case (c)
    case ('0':'9')
      v = iachar(c) - iachar('0')
    case ('a':'f')
      v = iachar(c) - iachar('a') + 10
    case ('A':'F')
      v = iachar(c) - iachar('A') + 10
    case default
      v = -1
    end select
  end function hex_value

  pure function replace_all(s, old, new) result(r)
    character(len=*), intent(in) :: s, old, new
    character(len=:), allocatable :: r
    integer :: p, k

    if (len(old) == 0) then
      r = s
      return
    end if
    r = ''
    p = 1
    do
      k = index(s(p:), old)
      if (k == 0) exit
      r = r // s(p:p+k-2) // new
      p = p + k - 1 + len(old)
    end do
    r = r // s(p:)
  end function replace_all

  pure function to_lower(s) result(r)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: r
    integer :: i

    r = s
    do i = 1, len(s)
      if (s(i:i) >= 'A' .and. s(i:i) <= 'Z') r(i:i) = achar(iachar(s(i:i)) + 32)
    end do
  end function to_lower

  pure logical function starts_with(s, prefix)
    character(len=*), intent(in) :: s, prefix

    starts_with = .false.
    if (len(s) >= len(prefix)) starts_with = s(1:len(prefix)) == prefix
  end function starts_with

  pure function int_to_str(i) result(r)
    integer, intent(in) :: i
    character(len=:), allocatable :: r
    character(len=24) :: buf

    write (buf, '(i0)') i
    r = trim(buf)
  end function int_to_str

end module strings
