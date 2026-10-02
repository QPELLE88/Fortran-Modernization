!> Unit tests. Run from the repository root (`make test`) so that templates
!> and the sample database are found.
program test_suite
  use string_helpers, only: compact, string_replace, replace_all, sql_quote
  use jade, only: template_var, template_var_of, render_jade, jadetemplate
  use marsupial, only: marsupial_t, find_marsupial, list_marsupials
  use cgi_protocol, only: DICT_STRUCT, cgi_store_dict, dict_destroy
  use controller, only: respond

  implicit none

  integer :: failures = 0, checks = 0

  call test_string_helpers()
  call test_jade()
  call test_marsupial()
  call test_controller()

  write(*, '(i0,a,i0,a)') checks - failures, ' of ', checks, ' checks passed'
  if (failures > 0) error stop 1

contains

  subroutine check(ok, name)
    logical, intent(in) :: ok
    character(len=*), intent(in) :: name

    checks = checks + 1
    if (.not. ok) then
      failures = failures + 1
      write(*, '(2a)') 'FAIL: ', name
    end if
  end subroutine check

  subroutine check_equal(actual, expected, name)
    character(len=*), intent(in) :: actual, expected, name

    call check(actual == expected .and. len(actual) == len(expected), name)
    if (actual /= expected .or. len(actual) /= len(expected)) then
      write(*, '(3a)') '  expected: "', expected, '"'
      write(*, '(3a)') '  actual:   "', actual, '"'
    end if
  end subroutine check_equal

  subroutine write_file(path, lines)
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: lines(:)

    integer :: u, i

    open(newunit=u, file=path, status='replace', action='write')
    do i = 1, size(lines)
      write(u, '(a)') trim(lines(i))
    end do
    close(u)
  end subroutine write_file

  subroutine test_string_helpers()
    character(len=20) :: fixed

    call check_equal(replace_all('a-b-c', '-', '+'), 'a+b+c', 'replace_all: every match')
    call check_equal(replace_all('aaa', 'a', 'aa'), 'aaaaaa', 'replace_all: replacement contains match')
    call check_equal(replace_all('abc', 'x', 'y'), 'abc', 'replace_all: no match')
    call check_equal(replace_all('abc', '', 'y'), 'abc', 'replace_all: empty pattern')
    call check_equal(replace_all('#{a}#{a}', '#{a}', ''), '', 'replace_all: to empty')
    call check_equal(sql_quote("o'brien"), "'o''brien'", 'sql_quote: doubles quotes')

    fixed = 'a.b.c'
    call string_replace(fixed, '.', ' ')
    call check_equal(trim(fixed), 'a b c', 'string_replace: in place')

    fixed = '   h1  title'
    call compact(fixed)
    call check_equal(trim(fixed), 'h1  title', 'compact: strips leading blanks')
  end subroutine test_string_helpers

  subroutine test_jade()
    character(len=:), allocatable :: html
    character(len=*), parameter :: nested = 'build/test_nested.jade'

    call write_file(nested, [character(len=40) :: &
      'ul.list', &
      '  li#first one', &
      '  li two', &
      'p after'])
    call render_jade(nested, html=html)
    call check_equal(html, &
      '<ul id="" class=" list"><li id="first" class="">one</li><li id="" class="">two' // &
      '</li></ul><p id="" class="">after</p>', &
      'render_jade: nesting, siblings and closing every tag at EOF')

    call render_jade('template/does-not-exist.jade', html=html)
    call check_equal(html, '<!-- template not found: template/does-not-exist.jade -->', &
      'render_jade: missing template')

    call render_jade('template/result.jade', html=html)
    call check(index(html, '#{name}') > 0, 'render_jade: keeps placeholders')
    call check(index(html, '</p></div></div>') > 0, 'render_jade: result.jade is balanced')
  end subroutine test_jade

  subroutine test_marsupial()
    type(marsupial_t) :: m
    type(marsupial_t), allocatable :: rows(:)
    logical :: found

    call find_marsupial('KOALA', found, m)
    call check(found, 'find_marsupial: case-insensitive match')
    if (found) then
      call check_equal(m%name, 'koala', 'find_marsupial: name')
      call check_equal(m%latin_name, 'Phascolarctos cinereus', 'find_marsupial: latin name')
      call check_equal(m%wiki_link, '/wiki/Koala', 'find_marsupial: wiki link')
    end if

    call find_marsupial('rock wall', found, m)
    call check(found .and. m%name == 'allied rock wallaby', 'find_marsupial: substring match')

    call find_marsupial('xyz', found, m)
    call check(.not. found, 'find_marsupial: no match')

    call find_marsupial("o'brien", found, m)
    call check(.not. found, 'find_marsupial: quotes are escaped')

    call list_marsupials(rows)
    call check(size(rows) == 5, 'list_marsupials: returns every row')
    if (size(rows) == 5) call check_equal(rows(5)%name, 'wombat', 'list_marsupials: last row')
  end subroutine test_marsupial

  function render(query) result(page)
    character(len=*), intent(in) :: query
    character(len=:), allocatable :: page

    type(DICT_STRUCT), pointer :: dict
    character(len=2000) :: line
    logical :: stopped
    integer :: u, io

    dict => null()
    call cgi_store_dict(dict, query)
    open(newunit=u, status='scratch')
    call respond(dict, u, stopped)
    call check(.not. stopped, 'respond: keeps serving')
    rewind(u)
    page = ''
    do
      read(u, '(a)', iostat=io) line
      if (io /= 0) exit
      if (line(1:8) /= '%REMARK%') page = page // trim(line) // new_line('a')
    end do
    close(u)
    call dict_destroy(dict)
  end function render

  subroutine test_controller()
    character(len=:), allocatable :: page

    page = render('DOCUMENT_URI=/')
    call check(index(page, '<title>FORTRAN.io</title>') > 0, 'GET /: layout')
    call check(index(page, 'Setup Guide') > 0, 'GET /: index template')

    page = render('DOCUMENT_URI=/search&q=kang')
    call check(index(page, 'Macropus rufus') > 0, 'GET /search?q=kang: result')

    page = render('DOCUMENT_URI=/search&q=xyz')
    call check(index(page, 'No results in this database') > 0, 'GET /search?q=xyz: empty state')

    page = render('DOCUMENT_URI=/all')
    call check(index(page, 'koala') > 0 .and. index(page, 'wombat') > 0, 'GET /all: every row')

    page = render('DOCUMENT_URI=/missing')
    call check(index(page, 'Page not found!') > 0, 'GET /missing: not found')
  end subroutine test_controller

end program test_suite
