!> Ordered string key/value list, used for request parameters and template variables.
module kv_list
  use strings, only: url_decode
  implicit none
  private

  public :: kv_pair, string_map, parse_urlencoded

  !> Upper bound on parsed form fields, so a hostile query cannot exhaust memory.
  integer, parameter, public :: MAX_FORM_FIELDS = 256

  type :: kv_pair
    character(len=:), allocatable :: key
    character(len=:), allocatable :: value
  end type kv_pair

  type :: string_map
    type(kv_pair), allocatable :: items(:)
    integer :: count = 0
  contains
    procedure :: set => map_set
    procedure :: add => map_add
    procedure :: get => map_get
    procedure :: has => map_has
  end type string_map

contains

  !> Append a pair, keeping any existing pair with the same key.
  subroutine map_add(self, key, value)
    class(string_map), intent(inout) :: self
    character(len=*), intent(in) :: key, value
    type(kv_pair), allocatable :: grown(:)

    if (.not. allocated(self%items)) then
      allocate(self%items(8))
    else if (self%count == size(self%items)) then
      allocate(grown(2 * size(self%items)))
      grown(1:self%count) = self%items(1:self%count)
      call move_alloc(grown, self%items)
    end if
    self%count = self%count + 1
    self%items(self%count)%key = key
    self%items(self%count)%value = value
  end subroutine map_add

  !> Replace the first pair with this key, or append a new one.
  subroutine map_set(self, key, value)
    class(string_map), intent(inout) :: self
    character(len=*), intent(in) :: key, value
    integer :: i

    do i = 1, self%count
      if (self%items(i)%key == key) then
        self%items(i)%value = value
        return
      end if
    end do
    call self%add(key, value)
  end subroutine map_set

  !> Value of the first pair with this key, or `default` (empty if absent).
  function map_get(self, key, default) result(value)
    class(string_map), intent(in) :: self
    character(len=*), intent(in) :: key
    character(len=*), intent(in), optional :: default
    character(len=:), allocatable :: value
    integer :: i

    do i = 1, self%count
      if (self%items(i)%key == key) then
        value = self%items(i)%value
        return
      end if
    end do
    if (present(default)) then
      value = default
    else
      value = ''
    end if
  end function map_get

  logical function map_has(self, key)
    class(string_map), intent(in) :: self
    character(len=*), intent(in) :: key
    integer :: i

    map_has = .false.
    do i = 1, self%count
      if (self%items(i)%key == key) then
        map_has = .true.
        return
      end if
    end do
  end function map_has

  !> Parse `a=1&b=two` style text into `map`, URL-decoding keys and values.
  subroutine parse_urlencoded(text, map)
    character(len=*), intent(in) :: text
    type(string_map), intent(inout) :: map
    integer :: start, amp, eq
    character(len=:), allocatable :: field

    start = 1
    do while (start <= len(text) .and. map%count < MAX_FORM_FIELDS)
      amp = index(text(start:), '&')
      if (amp == 0) then
        field = text(start:)
        start = len(text) + 1
      else
        field = text(start:start + amp - 2)
        start = start + amp
      end if
      if (len(field) == 0) cycle
      eq = index(field, '=')
      if (eq == 0) then
        call map%add(url_decode(field), '')
      else
        call map%add(url_decode(field(1:eq - 1)), url_decode(field(eq + 1:)))
      end if
    end do
  end subroutine parse_urlencoded

end module kv_list
