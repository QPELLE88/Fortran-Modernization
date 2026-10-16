!> Model for the sample marsupials table:
!>   CREATE TABLE marsupials (name, latinName, wikiLink, description)
module marsupial
  use database, only: database_t, statement_t, SQLITE_OK, SQLITE_ROW
  implicit none
  private

  public :: marsupial_t, find_marsupial, list_marsupials
  public :: DEFAULT_MARSUPIAL_DB

  character(len=*), parameter :: DEFAULT_MARSUPIAL_DB = 'marsupials.sqlite3'
  character(len=*), parameter :: COLUMNS = 'name, latinName, wikiLink, description'

  type :: marsupial_t
    character(len=:), allocatable :: name
    character(len=:), allocatable :: latin_name
    character(len=:), allocatable :: wiki_link
    character(len=:), allocatable :: description
  end type marsupial_t

contains

  !> Find the first marsupial whose name contains `query` (case-insensitive).
  !> `found` is .false. if nothing matches; `ok` is .false. on database errors.
  subroutine find_marsupial(query, found, item, ok, db_path)
    character(len=*), intent(in) :: query
    logical, intent(out) :: found
    type(marsupial_t), intent(out) :: item
    logical, intent(out) :: ok
    character(len=*), intent(in), optional :: db_path
    type(database_t) :: db
    type(statement_t) :: stmt
    integer :: rc

    found = .false.
    ok = .true.
    if (len_trim(query) == 0) return

    call open_db(db, ok, db_path)
    if (.not. ok) return

    call db%prepare('SELECT ' // COLUMNS // ' FROM marsupials ' // &
                    'WHERE INSTR(LOWER(name), LOWER(?1)) > 0 ORDER BY rowid LIMIT 1', stmt, rc)
    if (rc == SQLITE_OK) rc = stmt%bind_text(1, trim(query))
    if (rc == SQLITE_OK) then
      rc = stmt%step()
      if (rc == SQLITE_ROW) then
        found = .true.
        call read_row(stmt, item)
      end if
    else
      ok = .false.
    end if

    call stmt%finalize()
    call db%close()
  end subroutine find_marsupial

  !> Return up to `limit` marsupials in table order.
  subroutine list_marsupials(items, ok, limit, db_path)
    type(marsupial_t), allocatable, intent(out) :: items(:)
    logical, intent(out) :: ok
    integer, intent(in), optional :: limit
    character(len=*), intent(in), optional :: db_path
    type(database_t) :: db
    type(statement_t) :: stmt
    type(marsupial_t) :: row
    integer :: rc, max_rows

    allocate (items(0))
    max_rows = 100
    if (present(limit)) max_rows = limit

    call open_db(db, ok, db_path)
    if (.not. ok) return

    call db%prepare('SELECT ' // COLUMNS // ' FROM marsupials ORDER BY rowid LIMIT ?1', stmt, rc)
    if (rc == SQLITE_OK) rc = stmt%bind_int(1, max_rows)
    if (rc == SQLITE_OK) then
      do while (stmt%step() == SQLITE_ROW)
        call read_row(stmt, row)
        call append_item(items, row)
      end do
    else
      ok = .false.
    end if

    call stmt%finalize()
    call db%close()
  end subroutine list_marsupials

  subroutine append_item(items, item)
    type(marsupial_t), allocatable, intent(inout) :: items(:)
    type(marsupial_t), intent(in) :: item
    type(marsupial_t), allocatable :: grown(:)
    integer :: n

    n = size(items)
    allocate (grown(n + 1))
    grown(1:n) = items
    grown(n + 1) = item
    call move_alloc(grown, items)
  end subroutine append_item

  subroutine open_db(db, ok, db_path)
    type(database_t), intent(inout) :: db
    logical, intent(out) :: ok
    character(len=*), intent(in), optional :: db_path
    integer :: rc

    if (present(db_path)) then
      call db%open(db_path, rc)
    else
      call db%open(DEFAULT_MARSUPIAL_DB, rc)
    end if
    ok = rc == SQLITE_OK
    if (.not. ok) call db%close()
  end subroutine open_db

  subroutine read_row(stmt, item)
    type(statement_t), intent(in) :: stmt
    type(marsupial_t), intent(out) :: item

    item%name = stmt%column_text(0)
    item%latin_name = stmt%column_text(1)
    item%wiki_link = stmt%column_text(2)
    item%description = stmt%column_text(3)
  end subroutine read_row

end module marsupial
