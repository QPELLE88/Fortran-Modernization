!> Model: marsupial records stored in the `marsupials` SQLite table.
module marsupials
  use sqlite_db, only: database, statement, SQLITE_OK, SQLITE_ROW, SQLITE_DONE
  use strings, only: starts_with, to_lower
  implicit none
  private

  public :: marsupial, find_marsupials, list_marsupials, safe_wiki_path

  integer, parameter, public :: MAX_SEARCH_RESULTS = 50, MAX_LIST_RESULTS = 1000

  type :: marsupial
    character(len=:), allocatable :: name
    character(len=:), allocatable :: latin_name
    character(len=:), allocatable :: wiki_link
    character(len=:), allocatable :: description
  end type marsupial

  character(len=*), parameter :: SELECT_COLUMNS = &
    'SELECT name, latinName, wikiLink, description FROM marsupials '

contains

  !> Marsupials whose name contains `query` (case-insensitive), in table order.
  !> At most MAX_SEARCH_RESULTS rows; `truncated` is set when more rows match.
  subroutine find_marsupials(db, query, results, ok, truncated)
    type(database), intent(inout) :: db
    character(len=*), intent(in) :: query
    type(marsupial), allocatable, intent(out) :: results(:)
    logical, intent(out) :: ok, truncated

    call run_query(db, SELECT_COLUMNS // &
      'WHERE instr(lower(name), lower(?1)) > 0 ORDER BY rowid LIMIT ?2', &
      query, MAX_SEARCH_RESULTS, results, ok, truncated)
  end subroutine find_marsupials

  !> Marsupials in table order, at most MAX_LIST_RESULTS; `truncated` is set when more exist.
  subroutine list_marsupials(db, results, ok, truncated)
    type(database), intent(inout) :: db
    type(marsupial), allocatable, intent(out) :: results(:)
    logical, intent(out) :: ok, truncated

    call run_query(db, SELECT_COLUMNS // 'ORDER BY rowid LIMIT ?2', '', MAX_LIST_RESULTS, results, ok, truncated)
  end subroutine list_marsupials

  subroutine run_query(db, sql, text_param, limit, results, ok, truncated)
    type(database), intent(inout) :: db
    character(len=*), intent(in) :: sql, text_param
    integer, intent(in) :: limit
    type(marsupial), allocatable, intent(out) :: results(:)
    logical, intent(out) :: ok, truncated
    type(statement) :: stmt
    type(marsupial), allocatable :: grown(:)
    integer :: rc, count

    allocate(results(0))
    ok = .false.
    truncated = .false.
    if (.not. db%is_open()) return
    if (db%prepare(sql, stmt) /= SQLITE_OK) return

    if (index(sql, '?1') > 0) rc = stmt%bind_text(1, text_param)
    rc = stmt%bind_int(2, limit + 1)

    allocate(grown(8))
    count = 0
    do
      rc = stmt%step()
      if (rc /= SQLITE_ROW) exit
      if (count == limit) then
        truncated = .true.
        rc = SQLITE_DONE
        exit
      end if
      if (count == size(grown)) call grow(grown)
      count = count + 1
      grown(count)%name = stmt%column_text(0)
      grown(count)%latin_name = stmt%column_text(1)
      grown(count)%wiki_link = safe_wiki_path(stmt%column_text(2))
      grown(count)%description = stmt%column_text(3)
    end do
    call stmt%finalize()

    ok = rc == SQLITE_DONE
    results = grown(1:count)
  end subroutine run_query

  subroutine grow(items)
    type(marsupial), allocatable, intent(inout) :: items(:)
    type(marsupial), allocatable :: bigger(:)

    allocate(bigger(2 * size(items)))
    bigger(1:size(items)) = items
    call move_alloc(bigger, items)
  end subroutine grow

  !> Links are rendered under https://en.wikipedia.org, so only accept a single
  !> `/wiki/<article>` segment made of URL-safe characters. Anything else is dropped.
  function safe_wiki_path(link) result(path)
    character(len=*), intent(in) :: link
    character(len=:), allocatable :: path
    character(len=*), parameter :: ALLOWED = &
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-.,()%:' // "'"
    character(len=:), allocatable :: article

    path = ''
    if (.not. starts_with(link, '/wiki/')) return
    article = link(7:)
    if (len(article) == 0) return
    if (verify(article, ALLOWED) /= 0) return
    if (article(1:1) == '.' .or. starts_with(to_lower(article), '%2e')) return
    path = link
  end function safe_wiki_path

end module marsupials
