!> String utilities: a growable buffer, HTML escaping, URL decoding and case folding.
module strings
  implicit none
  private

  public :: string_buffer, html_escape, url_decode, to_lower, int_to_str, &
            starts_with, strip_trailing, is_blank

  !> Append-only character buffer with amortised O(1) growth.
  type :: string_buffer
    character(len=:), allocatable :: data
    integer :: length = 0
  contains
    procedure :: append => buffer_append
    procedure :: to_string => buffer_to_string
    procedure :: clear => buffer_clear
  end type string_buffer

contains

  subroutine buffer_append(self, text)
    class(string_buffer), intent(inout) :: self
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: grown
    integer :: needed, capacity

    needed = self%length + len(text)
    if (.not. allocated(self%data)) then
      allocate(character(len=max(256, needed)) :: self%data)
    else if (needed > len(self%data)) then
      capacity = max(2 * len(self%data), needed)
      allocate(character(len=capacity) :: grown)
      grown(1:self%length) = self%data(1:self%length)
      call move_alloc(grown, self%data)
    end if
    self%data(self%length + 1:needed) = text
    self%length = needed
  end subroutine buffer_append

  function buffer_to_string(self) result(text)
    class(string_buffer), intent(in) :: self
    character(len=:), allocatable :: text

    if (allocated(self%data)) then
      text = self%data(1:self%length)
    else
      text = ''
    end if
  end function buffer_to_string

  subroutine buffer_clear(self)
    class(string_buffer), intent(inout) :: self

    self%length = 0
  end subroutine buffer_clear

  !> Escape text for safe use in HTML element content and quoted attribute values.
  !> Control characters other than tab, LF and CR are dropped.
  function html_escape(text) result(escaped)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: escaped
    type(string_buffer) :: buf
    integer :: i, code

    call buf%append('')
    do i = 1, len(text)
      code = iachar(text(i:i))
      select case (code)
      case (iachar('&'))
        call buf%append('&amp;')
      case (iachar('<'))
        call buf%append('&lt;')
      case (iachar('>'))
        call buf%append('&gt;')
      case (iachar('"'))
        call buf%append('&quot;')
      case (iachar("'"))
        call buf%append('&#39;')
      case (9, 10, 13)
        call buf%append(text(i:i))
      case (0:8, 11:12, 14:31, 127)
        continue
      case default
        call buf%append(text(i:i))
      end select
    end do
    escaped = buf%to_string()
  end function html_escape

  !> Decode application/x-www-form-urlencoded text ('+' and %XX escapes).
  !> Malformed escapes are kept literally; decoded NUL bytes are dropped.
  function url_decode(text) result(decoded)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: decoded
    type(string_buffer) :: buf
    integer :: i, hi, lo

    call buf%append('')
    i = 1
    do while (i <= len(text))
      if (text(i:i) == '+') then
        call buf%append(' ')
      else if (text(i:i) == '%' .and. i + 2 <= len(text)) then
        hi = hex_value(text(i + 1:i + 1))
        lo = hex_value(text(i + 2:i + 2))
        if (hi >= 0 .and. lo >= 0) then
          if (hi * 16 + lo /= 0) call buf%append(achar(hi * 16 + lo))
          i = i + 2
        else
          call buf%append('%')
        end if
      else
        call buf%append(text(i:i))
      end if
      i = i + 1
    end do
    decoded = buf%to_string()
  end function url_decode

  pure integer function hex_value(ch)
    character(len=1), intent(in) :: ch

    select case (ch)
    case ('0':'9')
      hex_value = iachar(ch) - iachar('0')
    case ('a':'f')
      hex_value = iachar(ch) - iachar('a') + 10
    case ('A':'F')
      hex_value = iachar(ch) - iachar('A') + 10
    case default
      hex_value = -1
    end select
  end function hex_value

  pure function to_lower(text) result(lowered)
    character(len=*), intent(in) :: text
    character(len=len(text)) :: lowered
    integer :: i

    lowered = text
    do i = 1, len(text)
      if (text(i:i) >= 'A' .and. text(i:i) <= 'Z') then
        lowered(i:i) = achar(iachar(text(i:i)) + 32)
      end if
    end do
  end function to_lower

  pure function int_to_str(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=24) :: tmp

    write(tmp, '(i0)') value
    text = trim(tmp)
  end function int_to_str

  pure logical function starts_with(text, prefix)
    character(len=*), intent(in) :: text, prefix

    starts_with = .false.
    if (len(prefix) <= len(text)) starts_with = text(1:len(prefix)) == prefix
  end function starts_with

  !> Remove trailing spaces, tabs and carriage returns.
  pure function strip_trailing(text) result(stripped)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: stripped
    integer :: n

    n = len(text)
    do while (n > 0)
      if (text(n:n) /= ' ' .and. text(n:n) /= achar(9) .and. text(n:n) /= achar(13)) exit
      n = n - 1
    end do
    stripped = text(1:n)
  end function strip_trailing

  pure logical function is_blank(text)
    character(len=*), intent(in) :: text
    integer :: i

    is_blank = .true.
    do i = 1, len(text)
      select case (iachar(text(i:i)))
      case (9, 10, 13, 32)
        continue
      case default
        is_blank = .false.
        return
      end select
    end do
  end function is_blank

end module strings
