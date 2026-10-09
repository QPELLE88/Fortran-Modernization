!> Model: read-only queries against the marsupials SQLite table.
module marsupials
  use sqlite_db
  implicit none
  private

  public :: marsupial_t, search_marsupials, all_marsupials

  integer, parameter :: MAX_ROWS = 50
  character(len=*), parameter :: SELECT_ALL = &
    'SELECT name, latinName, wikiLink, description FROM marsupials'

  type :: marsupial_t
    character(len=:), allocatable :: name
    character(len=:), allocatable :: latin_name
    character(len=:), allocatable :: wiki_link
    character(len=:), allocatable :: description
  end type marsupial_t

contains

  !> Case-insensitive substring match on name. A blank query matches nothing.
  subroutine search_marsupials(db_path, query, results, ok, errmsg)
    character(len=*), intent(in) :: db_path, query
    type(marsupial_t), allocatable, intent(out) :: results(:)
    logical, intent(out) :: ok
    character(len=:), allocatable, intent(out) :: errmsg

    if (len_trim(adjustl(query)) == 0) then
      allocate(results(0))
      ok = .true.
      errmsg = ''
      return
    end if
    call run_query(db_path, SELECT_ALL // &
                   ' WHERE instr(lower(name), lower(?1)) > 0 ORDER BY rowid LIMIT ?2', &
                   results, ok, errmsg, trim(adjustl(query)))
  end subroutine search_marsupials

  subroutine all_marsupials(db_path, results, ok, errmsg)
    character(len=*), intent(in) :: db_path
    type(marsupial_t), allocatable, intent(out) :: results(:)
    logical, intent(out) :: ok
    character(len=:), allocatable, intent(out) :: errmsg

    call run_query(db_path, SELECT_ALL // ' ORDER BY rowid LIMIT ?2', results, ok, errmsg)
  end subroutine all_marsupials

  !> Open, prepare, bind (?1 = text, ?2 = row limit), collect rows, and always release handles.
  subroutine run_query(db_path, sql, results, ok, errmsg, text)
    character(len=*), intent(in) :: db_path, sql
    type(marsupial_t), allocatable, intent(out) :: results(:)
    logical, intent(out) :: ok
    character(len=:), allocatable, intent(out) :: errmsg
    character(len=*), intent(in), optional :: text
    type(db_t) :: db
    type(stmt_t) :: stmt
    type(marsupial_t) :: rows(MAX_ROWS)
    logical :: has_row
    integer :: n

    allocate(results(0))
    n = 0
    call db_open_readonly(db_path, db, ok, errmsg)
    if (.not. ok) return

    call db_prepare(db, sql, stmt, ok)
    if (ok .and. present(text)) call stmt_bind_text(stmt, 1, text, ok)
    if (ok) call stmt_bind_int(stmt, 2, MAX_ROWS, ok)
    do while (ok .and. n < MAX_ROWS)
      call stmt_step(stmt, has_row, ok)
      if (.not. (ok .and. has_row)) exit
      n = n + 1
      rows(n)%name = stmt_column_text(stmt, 1)
      rows(n)%latin_name = stmt_column_text(stmt, 2)
      rows(n)%wiki_link = stmt_column_text(stmt, 3)
      rows(n)%description = stmt_column_text(stmt, 4)
    end do
    if (.not. ok) errmsg = db_errmsg(db)

    call stmt_finalize(stmt)
    call db_close(db)
    if (ok) results = rows(:n)
  end subroutine run_query

end module marsupials
