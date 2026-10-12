!> Conversion of NUL-terminated C strings to Fortran strings.
module c_strings
  use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_size_t, c_associated, c_f_pointer
  implicit none
  private

  public :: c_to_fortran, c_to_fortran_n

  interface
    pure function c_strlen(s) bind(c, name='strlen') result(n)
      import :: c_ptr, c_size_t
      type(c_ptr), value, intent(in) :: s
      integer(c_size_t) :: n
    end function c_strlen
  end interface

contains

  !> Copy a NUL-terminated C string; a null pointer gives ''.
  function c_to_fortran(ptr) result(text)
    type(c_ptr), intent(in) :: ptr
    character(len=:), allocatable :: text

    if (.not. c_associated(ptr)) then
      text = ''
    else
      text = c_to_fortran_n(ptr, int(c_strlen(ptr)))
    end if
  end function c_to_fortran

  !> Copy exactly `n` bytes starting at `ptr`.
  function c_to_fortran_n(ptr, n) result(text)
    type(c_ptr), intent(in) :: ptr
    integer, intent(in) :: n
    character(len=:), allocatable :: text
    character(kind=c_char), pointer :: chars(:)
    integer :: i

    allocate(character(len=max(n, 0)) :: text)
    if (n <= 0 .or. .not. c_associated(ptr)) return
    call c_f_pointer(ptr, chars, [n])
    do i = 1, n
      text(i:i) = chars(i)
    end do
  end function c_to_fortran_n

end module c_strings
