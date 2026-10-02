!> String utilities shared by the template engine and the models.
module string_helpers
  implicit none
  private

  public :: compact
  public :: string_insert
  public :: string_delete
  public :: string_replace
  public :: replace_all
  public :: sql_quote

contains

  !> Deletes control characters and removes initial spaces.
  !> Tabs are turned into spaces; runs of spaces are preserved.
  pure subroutine compact(str)
    character(len=*), intent(inout) :: str

    character(len=len(str)) :: outstr
    character(len=1) :: ch
    integer :: i, k

    str = adjustl(str)
    outstr = ' '
    k = 0
    do i = 1, len_trim(str)
      ch = str(i:i)
      select case (iachar(ch))
      case (9, 32)
        k = k + 1
        outstr(k:k) = ' '
      case (33:)
        k = k + 1
        outstr(k:k) = ch
      end select
    end do
    str = adjustl(outstr)
  end subroutine compact

  !> Inserts `second` into the fixed-length `string` at `pos`; the tail is
  !> shifted right and truncated to the length of `string`.
  pure subroutine string_insert(string, pos, second)
    character(len=*), intent(inout) :: string
    integer, intent(in) :: pos
    character(len=*), intent(in) :: second

    integer :: length

    length = len(second)
    string(pos+length:) = string(pos:)
    string(pos:pos+length-1) = second
  end subroutine string_insert

  !> Deletes `length` characters from the fixed-length `string` at `pos`.
  pure subroutine string_delete(string, pos, length)
    character(len=*), intent(inout) :: string
    integer, intent(in) :: pos
    integer, intent(in) :: length

    string(pos:) = string(pos+length:)
  end subroutine string_delete

  !> In-place replacement of every `substr` in the fixed-length `string`.
  pure subroutine string_replace(string, substr, replace)
    character(len=*), intent(inout) :: string
    character(len=*), intent(in) :: substr
    character(len=*), intent(in) :: replace

    integer :: k, p

    if (len(substr) == 0) return
    p = 1
    do while (p <= len(string))
      k = index(string(p:), substr)
      if (k == 0) exit
      call string_delete(string(p:), k, len(substr))
      call string_insert(string(p:), k, replace)
      p = p + k - 1 + len(replace)
    end do
  end subroutine string_replace

  !> Returns `string` with every non-overlapping `substr` replaced by `replace`.
  pure function replace_all(string, substr, replace) result(res)
    character(len=*), intent(in) :: string
    character(len=*), intent(in) :: substr
    character(len=*), intent(in) :: replace
    character(len=:), allocatable :: res

    integer :: k, p

    if (len(substr) == 0) then
      res = string
      return
    end if
    res = ''
    p = 1
    do
      k = index(string(p:), substr)
      if (k == 0) exit
      res = res // string(p:p+k-2) // replace
      p = p + k - 1 + len(substr)
    end do
    res = res // string(p:)
  end function replace_all

  !> Quotes `value` as an SQL string literal.
  pure function sql_quote(value) result(res)
    character(len=*), intent(in) :: value
    character(len=:), allocatable :: res

    res = "'" // replace_all(value, "'", "''") // "'"
  end function sql_quote

end module string_helpers
