!> Unit tests for Fortran.io. Run from the repository root: `make test`.
program test_suite
  use string_helpers, only: key_value_t, html_escape, replace_all, to_lower, int_to_str
  use http, only: request_t, response_t, url_decode, parse_query, get_param
  use jade, only: jade_render, jade_render_file, interpolate
  use marsupial, only: marsupial_t, find_marsupial, list_marsupials
  use fortran_io_app, only: handle_request
  implicit none

  integer :: failures = 0, checks = 0

  call test_string_helpers()
  call test_url_decoding()
  call test_jade()
  call test_marsupial_model()
  call test_routes()

  write (*, '(i0, a, i0, a)') checks - failures, ' / ', checks, ' checks passed'
  if (failures > 0) error stop 1

contains

  subroutine check(condition, name)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: name

    checks = checks + 1
    if (.not. condition) then
      failures = failures + 1
      write (*, '(a)') 'FAIL: ' // name
    end if
  end subroutine check

  subroutine check_equal(actual, expected, name)
    character(len=*), intent(in) :: actual, expected, name

    call check(actual == expected .and. len(actual) == len(expected), name)
    if (actual /= expected .or. len(actual) /= len(expected)) then
      write (*, '(a)') '  expected: [' // expected // ']'
      write (*, '(a)') '  actual:   [' // actual // ']'
    end if
  end subroutine check_equal

  logical function contains_text(haystack, needle)
    character(len=*), intent(in) :: haystack, needle

    contains_text = index(haystack, needle) > 0
  end function contains_text

  integer function count_text(haystack, needle) result(n)
    character(len=*), intent(in) :: haystack, needle
    integer :: p, k

    n = 0
    p = 1
    do
      k = index(haystack(p:), needle)
      if (k == 0) exit
      n = n + 1
      p = p + k - 1 + len(needle)
    end do
  end function count_text

  subroutine test_string_helpers()
    call check_equal(html_escape('<a href="x">Tom & Jerry''s</a>'), &
                     '&lt;a href=&quot;x&quot;&gt;Tom &amp; Jerry&#39;s&lt;/a&gt;', 'html_escape')
    call check_equal(replace_all('a-b-c', '-', '--'), 'a--b--c', 'replace_all grows')
    call check_equal(replace_all('aaa', 'aa', 'b'), 'ba', 'replace_all non-overlapping')
    call check_equal(to_lower('KoAlA'), 'koala', 'to_lower')
    call check_equal(int_to_str(-42), '-42', 'int_to_str')
  end subroutine test_string_helpers

  subroutine test_url_decoding()
    type(request_t) :: req

    call check_equal(url_decode('sugar+glider%21'), 'sugar glider!', 'url_decode')
    call check_equal(url_decode('100%'), '100%', 'url_decode trailing percent')
    call check_equal(url_decode('%zz%4'), '%zz%4', 'url_decode malformed escapes')

    allocate (req%params(0))
    call parse_query('q=koala&empty=&flag&&x=a%3Db', req%params)
    call check(size(req%params) == 4, 'parse_query count')
    call check_equal(get_param(req, 'q'), 'koala', 'get_param q')
    call check_equal(get_param(req, 'empty', 'dflt'), '', 'get_param empty value')
    call check_equal(get_param(req, 'x'), 'a=b', 'get_param decoded')
    call check_equal(get_param(req, 'missing', 'dflt'), 'dflt', 'get_param default')
  end subroutine test_url_decoding

  subroutine test_jade()
    character(len=:), allocatable :: html
    type(key_value_t) :: vars(2)
    logical :: ok
    character(len=*), parameter :: NL = new_line('a')

    call check_equal(jade_render('.a.b#main(data-x="1") hi'), &
                     '<div id="main" class="a b" data-x="1">hi</div>', 'jade class/id/attrs')
    call check_equal(jade_render('ul' // NL // '  li one' // NL // '  li two' // NL // 'p end'), &
                     '<ul>' // NL // '<li>one</li>' // NL // '<li>two</li></ul>' // NL // '<p>end</p>', &
                     'jade nesting closes all levels')
    call check_equal(jade_render('input(type="text"name="q")'), '<input type="text" name="q">', &
                     'jade void element, unseparated attrs')
    call check_equal(jade_render('button(class="btn"class="btn-default") Go'), &
                     '<button class="btn btn-default">Go</button>', 'jade merges class attributes')
    call check_equal(jade_render('a.link(href="#top") up'), '<a class="link" href="#top">up</a>', &
                     'jade hash inside attribute')
    call check_equal(jade_render('p' // NL // '  | raw text'), '<p>' // NL // 'raw text</p>', 'jade pipe text')

    vars(1)%key = 'name'
    vars(1)%value = '<b>"x"</b>'
    vars(2)%key = 'html'
    vars(2)%value = '<i>ok</i>'
    call check_equal(interpolate('#{name}|!{html}|#{nope}', vars), &
                     '&lt;b&gt;&quot;x&quot;&lt;/b&gt;|<i>ok</i>|', 'interpolate escapes')
    vars(1)%value = '#{html}'
    call check_equal(interpolate('#{name}', vars), '#{html}', 'interpolate does not rescan values')

    call jade_render_file('template/index.jade', html, ok)
    call check(ok, 'render index.jade')
    call check(count_text(html, '<div') == count_text(html, '</div>'), 'index.jade divs balanced')
    call check(.not. contains_text(html, 'id=""'), 'index.jade has no empty id attrs')

    call jade_render_file('template/does-not-exist.jade', html, ok)
    call check(.not. ok, 'missing template reports failure')
  end subroutine test_jade

  subroutine test_marsupial_model()
    type(marsupial_t) :: item
    type(marsupial_t), allocatable :: items(:)
    logical :: found, ok

    call find_marsupial('KOALA', found, item, ok)
    call check(ok .and. found, 'find koala (case-insensitive)')
    if (found) then
      call check_equal(item%latin_name, 'Phascolarctos cinereus', 'koala latin name')
      call check_equal(item%wiki_link, '/wiki/Koala', 'koala wiki link')
    end if

    call find_marsupial('wal', found, item, ok)
    call check(found, 'find by substring')
    if (found) call check_equal(item%name, 'allied rock wallaby', 'substring match')

    call find_marsupial("' OR 1=1 --", found, item, ok)
    call check(ok .and. .not. found, 'SQL injection attempt is treated as data')

    call find_marsupial('%', found, item, ok)
    call check(ok .and. .not. found, 'LIKE wildcards are not interpreted')

    call find_marsupial('   ', found, item, ok)
    call check(ok .and. .not. found, 'blank query finds nothing')

    call list_marsupials(items, ok)
    call check(ok .and. size(items) == 5, 'list returns all 5 rows')
    if (size(items) == 5) then
      call check(len(items(5)%description) > 50, 'long descriptions are not truncated')
    end if

    call list_marsupials(items, ok, limit=2)
    call check(size(items) == 2, 'list honours limit')

    call find_marsupial('koala', found, item, ok, db_path='no/such/file.sqlite3')
    call check(.not. ok, 'missing database reports failure')
  end subroutine test_marsupial_model

  subroutine test_routes()
    type(response_t) :: res

    call request('/', '', res)
    call check(res%status == 200, '/ is 200')
    call check(contains_text(res%body, 'FORTRAN.io') .and. contains_text(res%body, '</html>'), '/ renders')

    call request('/search', 'q=sugar', res)
    call check(res%status == 200, '/search is 200')
    call check(contains_text(res%body, 'Petaurus breviceps'), '/search finds sugar glider')

    call request('/search', 'q=%3Cscript%3E', res)
    call check(contains_text(res%body, 'No results for &quot;&lt;script&gt;&quot;'), '/search escapes query')
    call check(.not. contains_text(res%body, '<script>'), '/search does not reflect raw HTML')

    call request('/all', '', res)
    call check(res%status == 200, '/all is 200')
    call check(count_text(res%body, 'en.wikipedia.org/wiki/') == 5, '/all lists every marsupial')
    call check(count_text(res%body, '<div') == count_text(res%body, '</div>'), '/all divs balanced')

    call request('/healthz', '', res)
    call check(res%status == 200 .and. res%body == 'ok', '/healthz')

    call request('/nope', '', res)
    call check(res%status == 404, 'unknown path is 404')
  end subroutine test_routes

  subroutine request(path, query, res)
    character(len=*), intent(in) :: path, query
    type(response_t), intent(out) :: res
    type(request_t) :: req

    req%method = 'GET'
    req%path = path
    allocate (req%params(0))
    call parse_query(query, req%params)
    call handle_request(req, res)
  end subroutine request

end program test_suite
