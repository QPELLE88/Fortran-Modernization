!> Thin, type-safe SQLite wrapper using the standard ISO_C_BINDING
!> interface to libsqlite3 (no C shim or name-mangling macros needed).
module database
  use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char, c_size_t, c_intptr_t, &
                                         c_null_ptr, c_null_char, c_associated, c_f_pointer
  implicit none
  private

  public :: database_t, statement_t
  public :: SQLITE_OK, SQLITE_ROW, SQLITE_DONE
  public :: SQLITE_OPEN_READONLY, SQLITE_OPEN_READWRITE, SQLITE_OPEN_CREATE

  integer, parameter :: SQLITE_OK = 0
  integer, parameter :: SQLITE_ROW = 100
  integer, parameter :: SQLITE_DONE = 101

  integer, parameter :: SQLITE_OPEN_READONLY = int(z'00000001')
  integer, parameter :: SQLITE_OPEN_READWRITE = int(z'00000002')
  integer, parameter :: SQLITE_OPEN_CREATE = int(z'00000004')

  !> SQLITE_TRANSIENT: ask SQLite to take its own copy of bound text.
  integer(c_intptr_t), parameter :: SQLITE_TRANSIENT = -1_c_intptr_t

  type :: database_t
    type(c_ptr), private :: handle = c_null_ptr
  contains
    procedure :: open => db_open
    procedure :: close => db_close
    procedure :: prepare => db_prepare
    procedure :: errmsg => db_errmsg
    procedure :: is_open => db_is_open
  end type database_t

  type :: statement_t
    type(c_ptr), private :: handle = c_null_ptr
  contains
    procedure :: bind_text => stmt_bind_text
    procedure :: bind_int => stmt_bind_int
    procedure :: step => stmt_step
    procedure :: column_text => stmt_column_text
    procedure :: column_int => stmt_column_int
    procedure :: finalize => stmt_finalize
  end type statement_t

  interface
    function sqlite3_open_v2(filename, ppdb, flags, zvfs) bind(c, name='sqlite3_open_v2')
      import :: c_char, c_ptr, c_int
      character(kind=c_char), intent(in) :: filename(*)
      type(c_ptr), intent(out) :: ppdb
      integer(c_int), value :: flags
      type(c_ptr), value :: zvfs
      integer(c_int) :: sqlite3_open_v2
    end function sqlite3_open_v2

    function sqlite3_close_v2(db) bind(c, name='sqlite3_close_v2')
      import :: c_ptr, c_int
      type(c_ptr), value :: db
      integer(c_int) :: sqlite3_close_v2
    end function sqlite3_close_v2

    function sqlite3_errmsg(db) bind(c, name='sqlite3_errmsg')
      import :: c_ptr
      type(c_ptr), value :: db
      type(c_ptr) :: sqlite3_errmsg
    end function sqlite3_errmsg

    function sqlite3_prepare_v2(db, zsql, nbyte, ppstmt, pztail) bind(c, name='sqlite3_prepare_v2')
      import :: c_ptr, c_int, c_char
      type(c_ptr), value :: db
      character(kind=c_char), intent(in) :: zsql(*)
      integer(c_int), value :: nbyte
      type(c_ptr), intent(out) :: ppstmt
      type(c_ptr), value :: pztail
      integer(c_int) :: sqlite3_prepare_v2
    end function sqlite3_prepare_v2

    function sqlite3_bind_text(stmt, idx, text, nbyte, destructor) bind(c, name='sqlite3_bind_text')
      import :: c_ptr, c_int, c_char, c_intptr_t
      type(c_ptr), value :: stmt
      integer(c_int), value :: idx
      character(kind=c_char), intent(in) :: text(*)
      integer(c_int), value :: nbyte
      integer(c_intptr_t), value :: destructor
      integer(c_int) :: sqlite3_bind_text
    end function sqlite3_bind_text

    function sqlite3_bind_int(stmt, idx, val) bind(c, name='sqlite3_bind_int')
      import :: c_ptr, c_int
      type(c_ptr), value :: stmt
      integer(c_int), value :: idx, val
      integer(c_int) :: sqlite3_bind_int
    end function sqlite3_bind_int

    function sqlite3_step(stmt) bind(c, name='sqlite3_step')
      import :: c_ptr, c_int
      type(c_ptr), value :: stmt
      integer(c_int) :: sqlite3_step
    end function sqlite3_step

    function sqlite3_column_text(stmt, icol) bind(c, name='sqlite3_column_text')
      import :: c_ptr, c_int
      type(c_ptr), value :: stmt
      integer(c_int), value :: icol
      type(c_ptr) :: sqlite3_column_text
    end function sqlite3_column_text

    function sqlite3_column_bytes(stmt, icol) bind(c, name='sqlite3_column_bytes')
      import :: c_ptr, c_int
      type(c_ptr), value :: stmt
      integer(c_int), value :: icol
      integer(c_int) :: sqlite3_column_bytes
    end function sqlite3_column_bytes

    function sqlite3_column_int(stmt, icol) bind(c, name='sqlite3_column_int')
      import :: c_ptr, c_int
      type(c_ptr), value :: stmt
      integer(c_int), value :: icol
      integer(c_int) :: sqlite3_column_int
    end function sqlite3_column_int

    function sqlite3_finalize(stmt) bind(c, name='sqlite3_finalize')
      import :: c_ptr, c_int
      type(c_ptr), value :: stmt
      integer(c_int) :: sqlite3_finalize
    end function sqlite3_finalize

    function c_strlen(str) bind(c, name='strlen')
      import :: c_ptr, c_size_t
      type(c_ptr), value :: str
      integer(c_size_t) :: c_strlen
    end function c_strlen
  end interface

contains

  !> Copy `nbytes` bytes (or up to the NUL terminator if nbytes < 0)
  !> from a C string into a Fortran deferred-length string.
  function c_to_f_string(cstr, nbytes) result(fstr)
    type(c_ptr), intent(in) :: cstr
    integer, intent(in) :: nbytes
    character(len=:), allocatable :: fstr
    character(kind=c_char), pointer :: chars(:)
    integer :: n, i

    if (.not. c_associated(cstr)) then
      fstr = ''
      return
    end if

    n = nbytes
    if (n < 0) n = int(c_strlen(cstr))
    call c_f_pointer(cstr, chars, [n])
    allocate (character(len=n) :: fstr)
    do i = 1, n
      fstr(i:i) = chars(i)
    end do
  end function c_to_f_string

  subroutine db_open(self, path, rc, flags)
    class(database_t), intent(inout) :: self
    character(len=*), intent(in) :: path
    integer, intent(out) :: rc
    integer, intent(in), optional :: flags
    integer(c_int) :: open_flags

    open_flags = SQLITE_OPEN_READONLY
    if (present(flags)) open_flags = flags
    rc = sqlite3_open_v2(path // c_null_char, self%handle, open_flags, c_null_ptr)
  end subroutine db_open

  subroutine db_close(self)
    class(database_t), intent(inout) :: self
    integer(c_int) :: rc

    if (c_associated(self%handle)) rc = sqlite3_close_v2(self%handle)
    self%handle = c_null_ptr
  end subroutine db_close

  logical function db_is_open(self)
    class(database_t), intent(in) :: self

    db_is_open = c_associated(self%handle)
  end function db_is_open

  function db_errmsg(self) result(msg)
    class(database_t), intent(in) :: self
    character(len=:), allocatable :: msg

    if (c_associated(self%handle)) then
      msg = c_to_f_string(sqlite3_errmsg(self%handle), -1)
    else
      msg = 'database is not open'
    end if
  end function db_errmsg

  subroutine db_prepare(self, sql, stmt, rc)
    class(database_t), intent(in) :: self
    character(len=*), intent(in) :: sql
    type(statement_t), intent(out) :: stmt
    integer, intent(out) :: rc

    rc = sqlite3_prepare_v2(self%handle, sql // c_null_char, -1_c_int, stmt%handle, c_null_ptr)
  end subroutine db_prepare

  !> Bind `text` to the 1-based parameter `idx` (?NNN) of the statement.
  integer function stmt_bind_text(self, idx, text) result(rc)
    class(statement_t), intent(inout) :: self
    integer, intent(in) :: idx
    character(len=*), intent(in) :: text

    rc = sqlite3_bind_text(self%handle, idx, text // c_null_char, len(text, kind=c_int), SQLITE_TRANSIENT)
  end function stmt_bind_text

  integer function stmt_bind_int(self, idx, val) result(rc)
    class(statement_t), intent(inout) :: self
    integer, intent(in) :: idx, val

    rc = sqlite3_bind_int(self%handle, idx, val)
  end function stmt_bind_int

  integer function stmt_step(self) result(rc)
    class(statement_t), intent(inout) :: self

    rc = sqlite3_step(self%handle)
  end function stmt_step

  !> Text value of the 0-based result column `icol` of the current row.
  function stmt_column_text(self, icol) result(text)
    class(statement_t), intent(in) :: self
    integer, intent(in) :: icol
    character(len=:), allocatable :: text
    type(c_ptr) :: ptr

    ptr = sqlite3_column_text(self%handle, icol)
    text = c_to_f_string(ptr, int(sqlite3_column_bytes(self%handle, icol)))
  end function stmt_column_text

  integer function stmt_column_int(self, icol) result(val)
    class(statement_t), intent(in) :: self
    integer, intent(in) :: icol

    val = sqlite3_column_int(self%handle, icol)
  end function stmt_column_int

  subroutine stmt_finalize(self)
    class(statement_t), intent(inout) :: self
    integer(c_int) :: rc

    if (c_associated(self%handle)) rc = sqlite3_finalize(self%handle)
    self%handle = c_null_ptr
  end subroutine stmt_finalize

end module database
