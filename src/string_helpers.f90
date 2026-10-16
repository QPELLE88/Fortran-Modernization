!> General-purpose string utilities built on deferred-length character
!> variables, so callers never have to guess a maximum buffer size.
module string_helpers
  implicit none
  private

  public :: key_value_t
  public :: html_escape
  public :: replace_all
  public :: to_lower
  public :: int_to_str
  public :: starts_with
  public :: read_line
  public :: compact
  public :: string_replace

  character(len=*), parameter, public :: NEW_LINE_CHAR = achar(10)

  type :: key_value_t
    character(len=:), allocatable :: key
    character(len=:), allocatable :: value
  end type key_value_t

contains

  !> Escape the five HTML-significant characters so that untrusted text
  !> is safe inside element content and quoted attribute values.
  pure function html_escape(str) result(escaped)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: escaped
    integer :: i

    escaped = ''
    do i = 1, len(str)
      select case (str(i:i))
      case ('&')
        escaped = escaped // '&amp;'
      case ('<')
        escaped = escaped // '&lt;'
      case ('>')
        escaped = escaped // '&gt;'
      case ('"')
        escaped = escaped // '&quot;'
      case ("'")
        escaped = escaped // '&#39;'
      case default
        escaped = escaped // str(i:i)
      end select
    end do
  end function html_escape

  !> Return a copy of `str` with every non-overlapping occurrence of
  !> `search` replaced by `replacement`.
  pure function replace_all(str, search, replacement) result(out)
    character(len=*), intent(in) :: str, search, replacement
    character(len=:), allocatable :: out
    integer :: p, k

    if (len(search) == 0) then
      out = str
      return
    end if

    out = ''
    p = 1
    do
      k = index(str(p:), search)
      if (k == 0) exit
      out = out // str(p:p + k - 2) // replacement
      p = p + k - 1 + len(search)
    end do
    out = out // str(p:)
  end function replace_all

  pure function to_lower(str) result(lower)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: lower
    integer :: i, code

    do i = 1, len(str)
      code = iachar(str(i:i))
      if (code >= iachar('A') .and. code <= iachar('Z')) then
        lower(i:i) = achar(code + 32)
      else
        lower(i:i) = str(i:i)
      end if
    end do
  end function to_lower

  pure function int_to_str(value) result(str)
    integer, intent(in) :: value
    character(len=:), allocatable :: str
    character(len=32) :: buffer

    write (buffer, '(i0)') value
    str = trim(buffer)
  end function int_to_str

  pure logical function starts_with(str, prefix)
    character(len=*), intent(in) :: str, prefix

    starts_with = .false.
    if (len(prefix) > len(str)) return
    starts_with = str(1:len(prefix)) == prefix
  end function starts_with

  !> Read one record of arbitrary length from a formatted sequential unit.
  subroutine read_line(unit, line, iostat)
    integer, intent(in) :: unit
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: iostat
    character(len=256) :: chunk
    integer :: nread

    line = ''
    do
      read (unit, '(a)', advance='no', size=nread, iostat=iostat) chunk
      line = line // chunk(1:nread)
      if (iostat /= 0) exit
    end do
    if (is_iostat_eor(iostat)) iostat = 0
  end subroutine read_line

  !> Collapse runs of spaces/tabs to a single space, drop control
  !> characters and remove leading blanks (in place).
  subroutine compact(str)
    character(len=*), intent(inout) :: str
    character(len=len(str)) :: outstr
    integer :: i, k
    logical :: last_was_space

    outstr = ' '
    k = 0
    last_was_space = .true.
    do i = 1, len_trim(str)
      select case (iachar(str(i:i)))
      case (9, 32)
        if (.not. last_was_space) then
          k = k + 1
          outstr(k:k) = ' '
        end if
        last_was_space = .true.
      case (33:)
        k = k + 1
        outstr(k:k) = str(i:i)
        last_was_space = .false.
      end select
    end do
    str = outstr
  end subroutine compact

  !> In-place replacement for fixed-length strings (kept for backwards
  !> compatibility; prefer `replace_all`).
  subroutine string_replace(string, substr, replace)
    character(len=*), intent(inout) :: string
    character(len=*), intent(in) :: substr, replace

    string = replace_all(string, substr, replace)
  end subroutine string_replace

end module string_helpers
