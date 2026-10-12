!> Thin binding to libsqlite3: connections, prepared statements and bound parameters.
module sqlite_db
  use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char, c_intptr_t, c_null_ptr, &
                                         c_null_char, c_associated
  use c_strings, only: c_to_fortran, c_to_fortran_n
  implicit none
  private

  public :: database, statement

  integer, parameter, public :: SQLITE_OK = 0, SQLITE_ROW = 100, SQLITE_DONE = 101
  integer, parameter, public :: SQLITE_OPEN_READONLY = 1, SQLITE_OPEN_READWRITE = 2, &
                                SQLITE_OPEN_CREATE = 4
  ! SQLITE_TRANSIENT: sqlite copies bound text before the call returns.
  integer(c_intptr_t), parameter :: SQLITE_TRANSIENT = -1_c_intptr_t

  type :: database
    type(c_ptr) :: handle = c_null_ptr
  contains
    procedure :: open => db_open
    procedure :: close => db_close
    procedure :: is_open => db_is_open
    procedure :: exec => db_exec
    procedure :: prepare => db_prepare
    procedure :: errmsg => db_errmsg
    procedure :: open_statements => db_open_statements
  end type database

  type :: statement
    type(c_ptr) :: handle = c_null_ptr
  contains
    procedure :: bind_text => stmt_bind_text
    procedure :: bind_int => stmt_bind_int
    procedure :: step => stmt_step
    procedure :: column_text => stmt_column_text
    procedure :: finalize => stmt_finalize
  end type statement

  interface
    function sqlite3_open_v2(filename, db, flags, vfs) bind(c, name='sqlite3_open_v2') result(rc)
      import :: c_ptr, c_int, c_char
      character(kind=c_char), intent(in) :: filename(*)
      type(c_ptr), intent(out) :: db
      integer(c_int), value :: flags
      type(c_ptr), value :: vfs
      integer(c_int) :: rc
    end function sqlite3_open_v2

    function sqlite3_close_v2(db) bind(c, name='sqlite3_close_v2') result(rc)
      import :: c_ptr, c_int
      type(c_ptr), value :: db
      integer(c_int) :: rc
    end function sqlite3_close_v2

    function sqlite3_errmsg(db) bind(c, name='sqlite3_errmsg') result(msg)
      import :: c_ptr
      type(c_ptr), value :: db
      type(c_ptr) :: msg
    end function sqlite3_errmsg

    function sqlite3_exec(db, sql, callback, arg, errmsg) bind(c, name='sqlite3_exec') result(rc)
      import :: c_ptr, c_int, c_char
      type(c_ptr), value :: db
      character(kind=c_char), intent(in) :: sql(*)
      type(c_ptr), value :: callback, arg, errmsg
      integer(c_int) :: rc
    end function sqlite3_exec

    function sqlite3_prepare_v2(db, sql, nbyte, stmt, tail) bind(c, name='sqlite3_prepare_v2') result(rc)
      import :: c_ptr, c_int, c_char
      type(c_ptr), value :: db
      character(kind=c_char), intent(in) :: sql(*)
      integer(c_int), value :: nbyte
      type(c_ptr), intent(out) :: stmt
      type(c_ptr), value :: tail
      integer(c_int) :: rc
    end function sqlite3_prepare_v2

    function sqlite3_bind_text(stmt, idx, text, n, destructor) bind(c, name='sqlite3_bind_text') result(rc)
      import :: c_ptr, c_int, c_char, c_intptr_t
      type(c_ptr), value :: stmt
      integer(c_int), value :: idx
      character(kind=c_char), intent(in) :: text(*)
      integer(c_int), value :: n
      integer(c_intptr_t), value :: destructor
      integer(c_int) :: rc
    end function sqlite3_bind_text

    function sqlite3_bind_int(stmt, idx, value) bind(c, name='sqlite3_bind_int') result(rc)
      import :: c_ptr, c_int
      type(c_ptr), value :: stmt
      integer(c_int), value :: idx, value
      integer(c_int) :: rc
    end function sqlite3_bind_int

    function sqlite3_step(stmt) bind(c, name='sqlite3_step') result(rc)
      import :: c_ptr, c_int
      type(c_ptr), value :: stmt
      integer(c_int) :: rc
    end function sqlite3_step

    function sqlite3_column_text(stmt, col) bind(c, name='sqlite3_column_text') result(text)
      import :: c_ptr, c_int
      type(c_ptr), value :: stmt
      integer(c_int), value :: col
      type(c_ptr) :: text
    end function sqlite3_column_text

    function sqlite3_column_bytes(stmt, col) bind(c, name='sqlite3_column_bytes') result(n)
      import :: c_ptr, c_int
      type(c_ptr), value :: stmt
      integer(c_int), value :: col
      integer(c_int) :: n
    end function sqlite3_column_bytes

    function sqlite3_finalize(stmt) bind(c, name='sqlite3_finalize') result(rc)
      import :: c_ptr, c_int
      type(c_ptr), value :: stmt
      integer(c_int) :: rc
    end function sqlite3_finalize

    function sqlite3_next_stmt(db, stmt) bind(c, name='sqlite3_next_stmt') result(next)
      import :: c_ptr
      type(c_ptr), value :: db, stmt
      type(c_ptr) :: next
    end function sqlite3_next_stmt
  end interface

contains

  !> Open `path`; `flags` defaults to read-only. Returns an SQLite result code.
  integer function db_open(self, path, flags) result(rc)
    class(database), intent(inout) :: self
    character(len=*), intent(in) :: path
    integer, intent(in), optional :: flags
    integer(c_int) :: open_flags

    open_flags = SQLITE_OPEN_READONLY
    if (present(flags)) open_flags = flags
    call self%close()
    rc = sqlite3_open_v2(path // c_null_char, self%handle, open_flags, c_null_ptr)
  end function db_open

  subroutine db_close(self)
    class(database), intent(inout) :: self
    integer(c_int) :: rc

    if (c_associated(self%handle)) rc = sqlite3_close_v2(self%handle)
    self%handle = c_null_ptr
  end subroutine db_close

  logical function db_is_open(self)
    class(database), intent(in) :: self

    db_is_open = c_associated(self%handle)
  end function db_is_open

  integer function db_exec(self, sql) result(rc)
    class(database), intent(inout) :: self
    character(len=*), intent(in) :: sql

    rc = sqlite3_exec(self%handle, sql // c_null_char, c_null_ptr, c_null_ptr, c_null_ptr)
  end function db_exec

  integer function db_prepare(self, sql, stmt) result(rc)
    class(database), intent(inout) :: self
    character(len=*), intent(in) :: sql
    type(statement), intent(out) :: stmt

    rc = sqlite3_prepare_v2(self%handle, sql, int(len(sql), c_int), stmt%handle, c_null_ptr)
  end function db_prepare

  function db_errmsg(self) result(msg)
    class(database), intent(in) :: self
    character(len=:), allocatable :: msg

    if (c_associated(self%handle)) then
      msg = c_to_fortran(sqlite3_errmsg(self%handle))
    else
      msg = 'database is not open'
    end if
  end function db_errmsg

  !> Number of prepared statements not yet finalized (should be 0 between requests).
  integer function db_open_statements(self) result(n)
    class(database), intent(in) :: self
    type(c_ptr) :: stmt

    n = 0
    if (.not. c_associated(self%handle)) return
    stmt = sqlite3_next_stmt(self%handle, c_null_ptr)
    do while (c_associated(stmt))
      n = n + 1
      stmt = sqlite3_next_stmt(self%handle, stmt)
    end do
  end function db_open_statements

  integer function stmt_bind_text(self, idx, text) result(rc)
    class(statement), intent(inout) :: self
    integer, intent(in) :: idx
    character(len=*), intent(in) :: text

    rc = sqlite3_bind_text(self%handle, int(idx, c_int), text, int(len(text), c_int), SQLITE_TRANSIENT)
  end function stmt_bind_text

  integer function stmt_bind_int(self, idx, value) result(rc)
    class(statement), intent(inout) :: self
    integer, intent(in) :: idx, value

    rc = sqlite3_bind_int(self%handle, int(idx, c_int), int(value, c_int))
  end function stmt_bind_int

  !> Advance to the next row; returns SQLITE_ROW, SQLITE_DONE or an error code.
  integer function stmt_step(self) result(rc)
    class(statement), intent(inout) :: self

    rc = sqlite3_step(self%handle)
  end function stmt_step

  !> Text of column `col` (0-based) in the current row; NULL gives ''.
  function stmt_column_text(self, col) result(text)
    class(statement), intent(inout) :: self
    integer, intent(in) :: col
    character(len=:), allocatable :: text
    type(c_ptr) :: ptr
    integer :: n

    ptr = sqlite3_column_text(self%handle, int(col, c_int))
    n = sqlite3_column_bytes(self%handle, int(col, c_int))
    text = c_to_fortran_n(ptr, n)
  end function stmt_column_text

  subroutine stmt_finalize(self)
    class(statement), intent(inout) :: self
    integer(c_int) :: rc

    if (c_associated(self%handle)) rc = sqlite3_finalize(self%handle)
    self%handle = c_null_ptr
  end subroutine stmt_finalize

end module sqlite_db
