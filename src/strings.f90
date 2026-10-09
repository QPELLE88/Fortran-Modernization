!> Character-string helpers built on deferred-length allocatable strings.
module strings
  implicit none
  private

  public :: string_t
  public :: html_escape, url_decode, replace_all, to_lower, starts_with, int_to_str

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
    character(len=:), allocatable :: r
    integer :: i

    r = ''
    do i = 1, len(s)
      select case (s(i:i))
      case ('&')
        r = r // '&amp;'
      case ('<')
        r = r // '&lt;'
      case ('>')
        r = r // '&gt;'
      case ('"')
        r = r // '&quot;'
      case ("'")
        r = r // '&#39;'
      case default
        if (iachar(s(i:i)) < 32 .and. s(i:i) /= achar(9) .and. s(i:i) /= achar(10) &
            .and. s(i:i) /= achar(13)) then
          r = r // '&#xFFFD;'
        else
          r = r // s(i:i)
        end if
      end select
    end do
  end function html_escape

  !> Decode application/x-www-form-urlencoded text; malformed escapes are kept verbatim.
  pure function url_decode(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: i, hi, lo

    r = ''
    i = 1
    do while (i <= len(s))
      if (s(i:i) == '+') then
        r = r // ' '
        i = i + 1
        cycle
      end if
      if (s(i:i) == '%' .and. i + 2 <= len(s)) then
        hi = hex_value(s(i+1:i+1))
        lo = hex_value(s(i+2:i+2))
        if (hi >= 0 .and. lo >= 0) then
          r = r // char(hi * 16 + lo)
          i = i + 3
          cycle
        end if
      end if
      r = r // s(i:i)
      i = i + 1
    end do
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
