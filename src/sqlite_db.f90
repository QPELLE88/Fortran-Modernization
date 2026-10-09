!> Thin ISO_C_BINDING wrapper over the SQLite3 C API with prepared statements and bound parameters.
module sqlite_db
  use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_null_ptr, c_null_char, &
                                         c_intptr_t, c_size_t, c_associated, c_f_pointer
  implicit none
  private

  public :: db_t, stmt_t
  public :: db_open_readonly, db_close, db_errmsg
  public :: db_prepare, stmt_bind_text, stmt_bind_int, stmt_step, stmt_column_text, stmt_finalize

  integer(c_int), parameter :: SQLITE_OK = 0
  integer(c_int), parameter :: SQLITE_ROW = 100
  integer(c_int), parameter :: SQLITE_DONE = 101
  integer(c_int), parameter :: SQLITE_OPEN_READONLY = 1
  !> SQLITE_TRANSIENT: SQLite copies bound text before the call returns.
  integer(c_intptr_t), parameter :: SQLITE_TRANSIENT = -1

  type :: db_t
    type(c_ptr) :: handle = c_null_ptr
  end type db_t

  type :: stmt_t
    type(c_ptr) :: handle = c_null_ptr
  end type stmt_t

  interface
    integer(c_int) function c_sqlite3_open_v2(filename, ppdb, flags, zvfs) bind(C, name='sqlite3_open_v2')
      import :: c_int, c_char, c_ptr
      character(kind=c_char), intent(in) :: filename(*)
      type(c_ptr), intent(out) :: ppdb
      integer(c_int), value :: flags
      type(c_ptr), value :: zvfs
    end function c_sqlite3_open_v2

    integer(c_int) function c_sqlite3_close_v2(db) bind(C, name='sqlite3_close_v2')
      import :: c_int, c_ptr
      type(c_ptr), value :: db
    end function c_sqlite3_close_v2

    type(c_ptr) function c_sqlite3_errmsg(db) bind(C, name='sqlite3_errmsg')
      import :: c_ptr
      type(c_ptr), value :: db
    end function c_sqlite3_errmsg

    integer(c_int) function c_sqlite3_prepare_v2(db, sql, nbyte, ppstmt, pztail) bind(C, name='sqlite3_prepare_v2')
      import :: c_int, c_char, c_ptr
      type(c_ptr), value :: db
      character(kind=c_char), intent(in) :: sql(*)
      integer(c_int), value :: nbyte
      type(c_ptr), intent(out) :: ppstmt
      type(c_ptr), value :: pztail
    end function c_sqlite3_prepare_v2

    integer(c_int) function c_sqlite3_bind_text(stmt, idx, text, n, destructor) bind(C, name='sqlite3_bind_text')
      import :: c_int, c_char, c_ptr, c_intptr_t
      type(c_ptr), value :: stmt
      integer(c_int), value :: idx
      character(kind=c_char), intent(in) :: text(*)
      integer(c_int), value :: n
      integer(c_intptr_t), value :: destructor
    end function c_sqlite3_bind_text

    integer(c_int) function c_sqlite3_bind_int(stmt, idx, val) bind(C, name='sqlite3_bind_int')
      import :: c_int, c_ptr
      type(c_ptr), value :: stmt
      integer(c_int), value :: idx, val
    end function c_sqlite3_bind_int

    integer(c_int) function c_sqlite3_step(stmt) bind(C, name='sqlite3_step')
      import :: c_int, c_ptr
      type(c_ptr), value :: stmt
    end function c_sqlite3_step

    type(c_ptr) function c_sqlite3_column_text(stmt, col) bind(C, name='sqlite3_column_text')
      import :: c_int, c_ptr
      type(c_ptr), value :: stmt
      integer(c_int), value :: col
    end function c_sqlite3_column_text

    integer(c_int) function c_sqlite3_column_bytes(stmt, col) bind(C, name='sqlite3_column_bytes')
      import :: c_int, c_ptr
      type(c_ptr), value :: stmt
      integer(c_int), value :: col
    end function c_sqlite3_column_bytes

    integer(c_int) function c_sqlite3_finalize(stmt) bind(C, name='sqlite3_finalize')
      import :: c_int, c_ptr
      type(c_ptr), value :: stmt
    end function c_sqlite3_finalize

    integer(c_size_t) function c_strlen(s) bind(C, name='strlen')
      import :: c_size_t, c_ptr
      type(c_ptr), value :: s
    end function c_strlen
  end interface

contains

  subroutine db_open_readonly(path, db, ok, errmsg)
    character(len=*), intent(in) :: path
    type(db_t), intent(out) :: db
    logical, intent(out) :: ok
    character(len=:), allocatable, intent(out) :: errmsg

    ok = c_sqlite3_open_v2(path // c_null_char, db%handle, SQLITE_OPEN_READONLY, c_null_ptr) == SQLITE_OK
    errmsg = ''
    if (.not. ok) then
      errmsg = db_errmsg(db)
      call db_close(db)
    end if
  end subroutine db_open_readonly

  subroutine db_close(db)
    type(db_t), intent(inout) :: db
    integer(c_int) :: rc

    if (c_associated(db%handle)) rc = c_sqlite3_close_v2(db%handle)
    db%handle = c_null_ptr
  end subroutine db_close

  function db_errmsg(db) result(msg)
    type(db_t), intent(in) :: db
    character(len=:), allocatable :: msg

    if (.not. c_associated(db%handle)) then
      msg = 'out of memory'
      return
    end if
    msg = c_string(c_sqlite3_errmsg(db%handle))
  end function db_errmsg

  subroutine db_prepare(db, sql, stmt, ok)
    type(db_t), intent(in) :: db
    character(len=*), intent(in) :: sql
    type(stmt_t), intent(out) :: stmt
    logical, intent(out) :: ok

    ok = c_sqlite3_prepare_v2(db%handle, sql // c_null_char, -1_c_int, stmt%handle, c_null_ptr) == SQLITE_OK
  end subroutine db_prepare

  !> Bind text to 1-based parameter idx (?1, ?2, ...).
  subroutine stmt_bind_text(stmt, idx, text, ok)
    type(stmt_t), intent(in) :: stmt
    integer, intent(in) :: idx
    character(len=*), intent(in) :: text
    logical, intent(out) :: ok

    ok = c_sqlite3_bind_text(stmt%handle, int(idx, c_int), text // c_null_char, &
                             int(len(text), c_int), SQLITE_TRANSIENT) == SQLITE_OK
  end subroutine stmt_bind_text

  subroutine stmt_bind_int(stmt, idx, val, ok)
    type(stmt_t), intent(in) :: stmt
    integer, intent(in) :: idx, val
    logical, intent(out) :: ok

    ok = c_sqlite3_bind_int(stmt%handle, int(idx, c_int), int(val, c_int)) == SQLITE_OK
  end subroutine stmt_bind_int

  !> Advance the statement; has_row is .true. while a result row is available.
  subroutine stmt_step(stmt, has_row, ok)
    type(stmt_t), intent(in) :: stmt
    logical, intent(out) :: has_row, ok
    integer(c_int) :: rc

    rc = c_sqlite3_step(stmt%handle)
    has_row = rc == SQLITE_ROW
    ok = has_row .or. rc == SQLITE_DONE
  end subroutine stmt_step

  !> Text value of 1-based column col in the current row ('' for NULL).
  function stmt_column_text(stmt, col) result(value)
    type(stmt_t), intent(in) :: stmt
    integer, intent(in) :: col
    character(len=:), allocatable :: value
    type(c_ptr) :: p
    character(kind=c_char), pointer :: chars(:)
    integer :: i, n

    p = c_sqlite3_column_text(stmt%handle, int(col - 1, c_int))
    n = c_sqlite3_column_bytes(stmt%handle, int(col - 1, c_int))
    if (.not. c_associated(p) .or. n <= 0) then
      value = ''
      return
    end if
    allocate(character(len=n) :: value)
    call c_f_pointer(p, chars, [n])
    do i = 1, n
      value(i:i) = chars(i)
    end do
  end function stmt_column_text

  subroutine stmt_finalize(stmt)
    type(stmt_t), intent(inout) :: stmt
    integer(c_int) :: rc

    if (c_associated(stmt%handle)) rc = c_sqlite3_finalize(stmt%handle)
    stmt%handle = c_null_ptr
  end subroutine stmt_finalize

  function c_string(p) result(s)
    type(c_ptr), intent(in) :: p
    character(len=:), allocatable :: s
    character(kind=c_char), pointer :: chars(:)
    integer :: i, n

    if (.not. c_associated(p)) then
      s = ''
      return
    end if
    n = int(c_strlen(p))
    call c_f_pointer(p, chars, [n])
    allocate(character(len=n) :: s)
    do i = 1, n
      s(i:i) = chars(i)
    end do
  end function c_string

end module sqlite_db
