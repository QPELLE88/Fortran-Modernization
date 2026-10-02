!> Sample model: read-only access to the `marsupials` SQLite table.
module marsupial
  use sqlite, only: SQLITE_DATABASE, SQLITE_STATEMENT, SQLITE_COLUMN, SQLITE_CHAR, &
    sqlite3_open, sqlite3_close, sqlite3_column_query, sqlite3_prepare_select, &
    sqlite3_next_row, sqlite3_get_column, sqlite3_finalize
  use string_helpers, only: sql_quote

  implicit none
  private

  public :: marsupial_t
  public :: find_marsupial
  public :: list_marsupials
  public :: MARSUPIAL_DB

  character(len=*), parameter :: MARSUPIAL_DB = 'marsupials.sqlite3'
  character(len=*), parameter :: TABLE = 'marsupials'
  integer, parameter :: FIELD_LEN = 1024

  type :: marsupial_t
    character(len=:), allocatable :: name
    character(len=:), allocatable :: latin_name
    character(len=:), allocatable :: wiki_link
    character(len=:), allocatable :: description
  end type marsupial_t

contains

  !> Finds the first marsupial whose name contains `query` (case-insensitive).
  subroutine find_marsupial(query, found, result)
    character(len=*), intent(in) :: query
    logical, intent(out) :: found
    type(marsupial_t), intent(out) :: result

    type(marsupial_t), allocatable :: rows(:)

    call select_marsupials( &
      'WHERE INSTR(LOWER(name), LOWER(' // sql_quote(trim(query)) // ')) LIMIT 1', rows)
    found = size(rows) > 0
    if (found) result = rows(1)
  end subroutine find_marsupial

  !> Returns every marsupial in the table.
  subroutine list_marsupials(rows)
    type(marsupial_t), allocatable, intent(out) :: rows(:)

    call select_marsupials('', rows)
  end subroutine list_marsupials

  subroutine select_marsupials(clause, rows)
    character(len=*), intent(in) :: clause
    type(marsupial_t), allocatable, intent(out) :: rows(:)

    type(SQLITE_DATABASE) :: db
    type(SQLITE_STATEMENT) :: stmt
    type(SQLITE_COLUMN), pointer :: columns(:)
    type(marsupial_t), allocatable :: grown(:)
    character(len=FIELD_LEN) :: field
    logical :: finished
    integer :: n

    allocate(rows(0))
    call sqlite3_open(MARSUPIAL_DB, db)
    if (db%error /= 0) return

    allocate(columns(4))
    call sqlite3_column_query(columns(1), 'name', SQLITE_CHAR)
    call sqlite3_column_query(columns(2), 'latinName', SQLITE_CHAR)
    call sqlite3_column_query(columns(3), 'wikiLink', SQLITE_CHAR)
    call sqlite3_column_query(columns(4), 'description', SQLITE_CHAR)

    call sqlite3_prepare_select(db, TABLE, columns, stmt, clause)
    if (db%error == 0) then
      n = 0
      do
        call sqlite3_next_row(stmt, columns, finished)
        if (finished) exit

        if (n == size(rows)) then
          allocate(grown(max(4, 2 * n)))
          grown(1:n) = rows(1:n)
          call move_alloc(grown, rows)
        end if
        n = n + 1
        call sqlite3_get_column(columns(1), field)
        rows(n)%name = trim(field)
        call sqlite3_get_column(columns(2), field)
        rows(n)%latin_name = trim(field)
        call sqlite3_get_column(columns(3), field)
        rows(n)%wiki_link = trim(field)
        call sqlite3_get_column(columns(4), field)
        rows(n)%description = trim(field)
      end do
      call sqlite3_finalize(stmt)
      if (n < size(rows)) then
        allocate(grown(n))
        grown(1:n) = rows(1:n)
        call move_alloc(grown, rows)
      end if
    end if

    deallocate(columns)
    call sqlite3_close(db)
  end subroutine select_marsupials

end module marsupial
