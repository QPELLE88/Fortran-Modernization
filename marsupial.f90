! sample model for Fortran.io and SQLite database
! table should already be created

module marsupial
  use sqlite
  use string_helpers

  implicit none

  type(SQLITE_DATABASE)                       :: db
  type(SQLITE_STATEMENT)                      :: stmt
  type(SQLITE_COLUMN), dimension(:), pointer  :: column => null()
  integer                                     :: i
  logical                                     :: finished
  logical                                     :: dbOpen = .false.

  contains

  ! subroutine insert(name, latinName, wikiLink, description)
    ! columns
    ! character(len=50)             :: name, latinName, wikiLink, description

    ! allocate( column(4) )
    ! call sqlite3_set_column( column(1), name )
    ! call sqlite3_set_column( column(2), latinName )
    ! call sqlite3_set_column( column(3), wikiLink )
    ! call sqlite3_set_column( column(4), description )
    ! call sqlite3_insert( db, 'marsupials', column )
  ! endsubroutine

  subroutine getOneMarsupial(query, name, latinName, wikiLink, description)
    ! columns
    character(len=*)		  :: query
    character(len=50)			:: name, latinName, wikiLink, description

    ! If not found, we want to clear name so the caller knows.
    name = ""

    if (.not. openDatabase()) return
    call allocateColumns()

    call string_replace(query, "'", "''")
    call sqlite3_prepare_select( db, 'marsupials', column, stmt, "WHERE INSTR(LOWER(name), LOWER('" // trim(query) // "')) LIMIT 4")
    if (sqlite3_error(db)) then
      call releaseColumns()
      return
    endif

    i = 1
    do
      call sqlite3_next_row(stmt, column, finished)
      if (finished) exit

      call sqlite3_get_column(column(1), name)
      call sqlite3_get_column(column(2), latinName)
      call sqlite3_get_column(column(3), wikiLink)
      call sqlite3_get_column(column(4), description)
      exit
    end do

    call sqlite3_finalize(stmt)
    call releaseColumns()
  endsubroutine

  subroutine getAllMarsupials(name, latinName, wikiLink, description)
    ! columns
    character(len=50), dimension(8)	:: name, latinName, wikiLink, description

    name = ""

    if (.not. openDatabase()) return
    call allocateColumns()

    call sqlite3_prepare_select( db, 'marsupials', column, stmt, "WHERE 1=1 LIMIT 8")
    if (sqlite3_error(db)) then
      call releaseColumns()
      return
    endif

    i = 1
    do
      call sqlite3_next_row(stmt, column, finished)
      if (finished) exit

      call sqlite3_get_column(column(1), name(i))
      call sqlite3_get_column(column(2), latinName(i))
      call sqlite3_get_column(column(3), wikiLink(i))
      call sqlite3_get_column(column(4), description(i))
      i = i + 1
      if (i > size(name)) exit
    end do

    call sqlite3_finalize(stmt)
    call releaseColumns()
  endsubroutine

  ! Open the database once and reuse the handle for the life of the process.
  logical function openDatabase()
    if (.not. dbOpen) then
      call sqlite3_open('marsupials.sqlite3', db)
      ! On failure the C binding has already closed the handle; retry next request.
      if (.not. sqlite3_error(db)) then
        dbOpen = .true.
      endif
    endif
    openDatabase = dbOpen
  endfunction

  subroutine allocateColumns()
    call releaseColumns()
    allocate( column(4) )
    call sqlite3_column_query( column(1), 'name', SQLITE_CHAR )
    call sqlite3_column_query( column(2), 'latinName', SQLITE_CHAR )
    call sqlite3_column_query( column(3), 'wikiLink', SQLITE_CHAR )
    call sqlite3_column_query( column(4), 'description', SQLITE_CHAR )
  endsubroutine

  subroutine releaseColumns()
    if (associated(column)) deallocate(column)
    nullify(column)
  endsubroutine

  subroutine closeDatabase()
    if (dbOpen) then
      call sqlite3_close(db)
      dbOpen = .false.
    endif
  endsubroutine
endmodule
