!> Unit tests for Fortran.io. Run from the repository root with `make test`.
program test_suite
  use strings, only: string_buffer, html_escape, url_decode, to_lower, strip_trailing
  use kv_list, only: string_map, parse_urlencoded, MAX_FORM_FIELDS
  use sqlite_db, only: database, SQLITE_OK, SQLITE_OPEN_READWRITE, SQLITE_OPEN_CREATE
  use jade, only: render_source, render_template
  use marsupials, only: marsupial, find_marsupials, list_marsupials, safe_wiki_path, &
    MAX_SEARCH_RESULTS, MAX_LIST_RESULTS
  use app, only: app_context, http_request, http_response, handle_request, response_headers
  implicit none

  character(len=1), parameter :: LF = achar(10)
  integer :: passed = 0, failed = 0

  call test_strings()
  call test_form_parsing()
  call test_templates()
  call test_model()
  call test_controller()

  print '(a,i0,a,i0,a)', 'test_suite: ', passed, ' passed, ', failed, ' failed'
  if (failed > 0) error stop 1

contains

  subroutine check(condition, name)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: name

    if (condition) then
      passed = passed + 1
    else
      failed = failed + 1
      print '(a)', 'FAIL: ' // name
    end if
  end subroutine check

  subroutine check_equal(actual, expected, name)
    character(len=*), intent(in) :: actual, expected, name

    call check(actual == expected .and. len(actual) == len(expected), name)
    if (actual /= expected .or. len(actual) /= len(expected)) then
      print '(a)', '  expected: [' // expected // ']'
      print '(a)', '  actual:   [' // actual // ']'
    end if
  end subroutine check_equal

  logical function contains_text(haystack, needle)
    character(len=*), intent(in) :: haystack, needle

    contains_text = index(haystack, needle) > 0
  end function contains_text

  subroutine test_strings()
    type(string_buffer) :: buf
    integer :: i

    call check_equal(html_escape('<a href="x">''&'), '&lt;a href=&quot;x&quot;&gt;&#39;&amp;', 'html_escape specials')
    call check_equal(html_escape('ok' // achar(0) // achar(7) // 'x'), 'okx', 'html_escape drops control chars')
    call check_equal(url_decode('a+b%20c%3D%zz%4'), 'a b c=%zz%4', 'url_decode escapes and malformed input')
    call check_equal(url_decode('x%00y%0Az'), 'xy' // LF // 'z', 'url_decode drops NUL')
    call check_equal(to_lower('KoAla 42'), 'koala 42', 'to_lower')
    call check_equal(strip_trailing('abc ' // achar(13) // achar(9)), 'abc', 'strip_trailing')

    do i = 1, 20000
      call buf%append('0123456789')
    end do
    call check(buf%length == 200000 .and. buf%to_string() == repeat('0123456789', 20000), 'string_buffer growth')
  end subroutine test_strings

  subroutine test_form_parsing()
    type(string_map) :: params, many
    character(len=:), allocatable :: long_query
    integer :: i

    call parse_urlencoded('q=koala&empty=&flag&&q=second&name=sugar+glider', params)
    call check_equal(params%get('q'), 'koala', 'first value wins')
    call check_equal(params%get('empty'), '', 'empty value')
    call check(params%has('flag'), 'key without value')
    call check_equal(params%get('name'), 'sugar glider', 'plus decodes to space')
    call check_equal(params%get('missing', 'dflt'), 'dflt', 'default for missing key')

    long_query = ''
    do i = 1, 1000
      long_query = long_query // 'k=v&'
    end do
    call parse_urlencoded(long_query, many)
    call check(many%count == MAX_FORM_FIELDS, 'field count is capped')

    call parse_urlencoded('q=' // repeat('a', 10000), many)
    call check(.true., 'long values parse without overflow')
  end subroutine test_form_parsing

  subroutine test_templates()
    type(string_map) :: vars, none
    character(len=:), allocatable :: html, source
    logical :: ok
    integer :: i

    call check_equal(render_source('h1 Hello', none), '<h1>Hello</h1>' // LF, 'single element')
    call check_equal(render_source('ul' // LF // '  li one' // LF // '  li two', none), &
      '<ul>' // LF // '  <li>one</li>' // LF // '  <li>two</li>' // LF // '</ul>' // LF, 'nested siblings')
    call check_equal(render_source('.a.b#c(href="#x", class="d") text', none), &
      '<div id="c" class="a b d" href="#x">text</div>' // LF, 'selector, attributes and class merge')
    call check_equal(render_source('a.btn(href="/")', none), '<a class="btn" href="/"></a>' // LF, 'tag with class')
    call check_equal(render_source('input(name="q"placeholder="eg&nbsp;koala")', none), &
      '<input name="q" placeholder="eg&nbsp;koala">' // LF, 'void element, unseparated attrs, entity kept')
    call check_equal(render_source('(x="1", style=''color: "red"'') more', none), &
      '<div x="1" style="color: &quot;red&quot;">more</div>' // LF, 'implicit div and quote escaping')
    call check_equal(render_source('p' // LF // '  | raw text', none), &
      '<p>' // LF // '  raw text' // LF // '</p>' // LF, 'pipe text line')
    call check_equal(render_source('div' // achar(13) // LF // achar(13) // LF // '  p x' // achar(13) // LF, none), &
      '<div>' // LF // '  <p>x</p>' // LF // '</div>' // LF, 'CRLF and blank lines')

    call vars%set('name', '<script>alert("x")</script>')
    call vars%set('link', '/wiki/A" onmouseover="x')
    html = render_source('a(href="https://en.wikipedia.org#{link}") #{name} #{unknown}', vars)
    call check(.not. contains_text(html, '<script>'), 'text variables are escaped')
    call check(contains_text(html, '&lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt;'), 'escaped text present')
    call check(contains_text(html, 'href="https://en.wikipedia.org/wiki/A&quot; onmouseover=&quot;x"'), &
      'attribute variables are escaped')

    do i = 1, 30
      call vars%set('v' // achar(iachar('a') + mod(i, 26)) // achar(iachar('0') + i / 10), 'x')
    end do
    call vars%set('last', 'reached')
    call check_equal(render_source('p #{last}', vars), '<p>reached</p>' // LF, 'more than ten variables')

    source = 'p ' // repeat('long ', 2000)
    html = render_source(source, none)
    call check(len(html) == len('<p></p>' // LF) + 10000 - 1, 'lines longer than 1000 characters')

    call render_template('template/index.jade', none, html, ok)
    call check(ok .and. contains_text(html, '<h1>FORTRAN.io'), 'index.jade renders')
    call check(.not. contains_text(html, 'id=""'), 'no empty id attributes')
    call render_template('template/test.jade', none, html, ok)
    call check(ok .and. contains_text(html, '<a id="id" class="class" href="#x">'), 'test.jade renders')
    call render_template('template/missing.jade', none, html, ok)
    call check(.not. ok, 'missing template reports failure')
  end subroutine test_templates

  subroutine make_fixture_db(path)
    character(len=*), intent(in) :: path
    type(database) :: db
    integer :: rc

    open(newunit=rc, file=path, status='replace')
    close(rc, status='delete')
    rc = db%open(path, SQLITE_OPEN_READWRITE + SQLITE_OPEN_CREATE)
    call check(rc == SQLITE_OK, 'create fixture database')
    rc = db%exec('CREATE TABLE marsupials (name CHAR(50), latinName CHAR(50), wikiLink CHAR(50), ' // &
                 'description CHAR(50));' // &
                 "INSERT INTO marsupials VALUES ('koala', 'Phascolarctos cinereus', '/wiki/Koala', 'furry bear');" // &
                 "INSERT INTO marsupials VALUES ('Red Kangaroo', 'Macropus rufus', '/wiki/Red_kangaroo', 'big');" // &
                 "INSERT INTO marsupials VALUES ('quokka''s cousin', 'X y', 'javascript:alert(1)', 'tricky');" // &
                 "INSERT INTO marsupials VALUES ('wombat', 'Vombatus ursinus', '/wiki/Wombat', NULL);")
    call check(rc == SQLITE_OK, 'seed fixture database')
    call db%close()
  end subroutine make_fixture_db

  subroutine test_model()
    character(len=*), parameter :: path = 'build/test/fixture.sqlite3'
    type(database) :: db
    type(marsupial), allocatable :: found(:)
    logical :: ok, truncated
    integer :: i

    call make_fixture_db(path)
    call check(db%open(path) == SQLITE_OK, 'open fixture read-only')

    call find_marsupials(db, 'koala', found, ok, truncated)
    call check(ok .and. size(found) == 1, 'exact search')
    if (size(found) == 1) call check_equal(found(1)%latin_name, 'Phascolarctos cinereus', 'search row content')
    call find_marsupials(db, 'KANG', found, ok, truncated)
    call check(ok .and. size(found) == 1, 'case-insensitive search')
    call find_marsupials(db, 'o', found, ok, truncated)
    call check(ok .and. size(found) == 4, 'search returns every match')
    call find_marsupials(db, "' OR 1=1 --", found, ok, truncated)
    call check(ok .and. size(found) == 0, 'SQL injection is inert')
    call find_marsupials(db, "quokka's", found, ok, truncated)
    call check(ok .and. size(found) == 1, 'quotes are matched literally')
    if (size(found) == 1) call check_equal(found(1)%wiki_link, '', 'non-wiki links are dropped')
    call find_marsupials(db, '%', found, ok, truncated)
    call check(ok .and. size(found) == 0, 'percent is not a wildcard')

    call list_marsupials(db, found, ok, truncated)
    call check(ok .and. size(found) == 4 .and. .not. truncated, 'list returns every row')
    if (size(found) == 4) then
      call check_equal(found(4)%name, 'wombat', 'last row is listed')
      call check_equal(found(4)%description, '', 'NULL column reads as empty')
    end if

    do i = 1, 2000
      call find_marsupials(db, 'wom', found, ok, truncated)
    end do
    call check(ok .and. db%open_statements() == 0, 'statements are finalized')
    call db%close()

    call find_marsupials(db, 'koala', found, ok, truncated)
    call check(.not. ok .and. size(found) == 0, 'closed database reports failure')

    call test_truncation()
    call test_wiki_paths()
  end subroutine test_model

  subroutine test_truncation()
    character(len=*), parameter :: path = 'build/test/bulk.sqlite3'
    type(database) :: db
    type(marsupial), allocatable :: found(:)
    logical :: ok, truncated
    integer :: rc

    open(newunit=rc, file=path, status='replace')
    close(rc, status='delete')
    call check(db%open(path, SQLITE_OPEN_READWRITE + SQLITE_OPEN_CREATE) == SQLITE_OK, 'create bulk database')
    rc = db%exec('CREATE TABLE marsupials (name, latinName, wikiLink, description);' // &
                 'WITH RECURSIVE n(x) AS (SELECT 1 UNION ALL SELECT x + 1 FROM n WHERE x < ' // &
                 '1001) INSERT INTO marsupials SELECT ''bulk'' || x, '''', '''', '''' FROM n;')
    call check(rc == SQLITE_OK, 'seed bulk database')

    call list_marsupials(db, found, ok, truncated)
    call check(ok .and. size(found) == MAX_LIST_RESULTS .and. truncated, 'list over the cap is flagged')
    call find_marsupials(db, 'bulk', found, ok, truncated)
    call check(ok .and. size(found) == MAX_SEARCH_RESULTS .and. truncated, 'search over the cap is flagged')
    call find_marsupials(db, 'bulk1000', found, ok, truncated)
    call check(ok .and. size(found) == 1 .and. .not. truncated, 'search under the cap is not flagged')
    call check(db%open_statements() == 0, 'truncated queries finalize their statements')
    call db%close()
  end subroutine test_truncation

  subroutine test_wiki_paths()
    call check_equal(safe_wiki_path('/wiki/Koala'), '/wiki/Koala', 'plain article path kept')
    call check_equal(safe_wiki_path("/wiki/Leadbeater%27s_possum"), '/wiki/Leadbeater%27s_possum', &
                     'percent-encoded article kept')
    call check_equal(safe_wiki_path('/wiki/AC/DC'), '/wiki/AC/DC', 'slash inside a title kept')
    call check_equal(safe_wiki_path('/wiki/.hack'), '/wiki/.hack', 'leading dot in a title kept')
    call check_equal(safe_wiki_path('/wiki/'), '', 'empty article dropped')
    call check_equal(safe_wiki_path('/wiki/a//b'), '', 'empty segment dropped')
    call check_equal(safe_wiki_path('/wiki/a/..'), '', 'trailing dot segment dropped')
    call check_equal(safe_wiki_path('/wiki/a/.%2E/b'), '', 'mixed encoded dot segment dropped')
    call check_equal(safe_wiki_path('/wiki/../w/index.php'), '', 'dot segments dropped')
    call check_equal(safe_wiki_path('/wiki/%2E%2E/x'), '', 'encoded dot segments dropped')
    call check_equal(safe_wiki_path('/wiki/&#x2F;&#x2F;evil.example'), '', 'HTML entities dropped')
    call check_equal(safe_wiki_path('/wiki/x"onmouseover=alert(1)'), '', 'quotes dropped')
    call check_equal(safe_wiki_path('/wiki/a\\b'), '', 'backslashes dropped')
    call check_equal(safe_wiki_path('//evil.example/wiki/x'), '', 'protocol-relative dropped')
  end subroutine test_wiki_paths

  function request(method, path, query, body, content_type) result(req)
    character(len=*), intent(in) :: method, path, query
    character(len=*), intent(in), optional :: body, content_type
    type(http_request) :: req

    req%method = method
    req%path = path
    req%query_string = query
    req%body = ''
    req%content_type = ''
    if (present(body)) req%body = body
    if (present(content_type)) req%content_type = content_type
  end function request

  subroutine test_controller()
    type(app_context) :: ctx, broken
    type(http_response) :: res

    ctx%template_dir = 'template'
    call check(ctx%db%open('marsupials.sqlite3') == SQLITE_OK, 'open bundled database')

    res = handle_request(ctx, request('GET', '/', ''))
    call check(res%status == 200 .and. contains_text(res%body, 'finally, a Fortran Web Framework'), 'home page')
    call check(contains_text(response_headers(res), 'Status: 200 OK' // achar(13) // LF), 'status header')

    res = handle_request(ctx, request('GET', '/search', 'q=koala'))
    call check(res%status == 200 .and. contains_text(res%body, 'Phascolarctos cinereus'), 'search finds koala')
    call check(contains_text(res%body, 'value="koala"'), 'search box keeps the query')

    res = handle_request(ctx, request('GET', '/search', 'q='))
    call check(res%status == 200 .and. .not. contains_text(res%body, 'Phascolarctos'), 'blank search lists nothing')

    res = handle_request(ctx, request('GET', '/search', 'q=%3Cscript%3Ealert(1)%3C%2Fscript%3E'))
    call check(.not. contains_text(res%body, '<script>'), 'reflected query is escaped')
    call check(contains_text(res%body, '&lt;script&gt;'), 'escaped query is shown')

    res = handle_request(ctx, request('GET', '/search', 'q=' // repeat('k', 5000)))
    call check(res%status == 200 .and. contains_text(res%body, 'No marsupials match'), 'very long query')

    res = handle_request(ctx, request('POST', '/search', '', 'q=kangaroo', 'application/x-www-form-urlencoded'))
    call check(contains_text(res%body, 'Macropus rufus'), 'POST form search')

    res = handle_request(ctx, request('GET', '/all', ''))
    call check(res%status == 200 .and. contains_text(res%body, 'Vombatus ursinus'), '/all includes the last row')

    res = handle_request(ctx, request('GET', '/nope', ''))
    call check(res%status == 404 .and. contains_text(res%body, 'Page not found!'), '404 page')

    res = handle_request(ctx, request('DELETE', '/', ''))
    call check(res%status == 405 .and. contains_text(res%extra_headers, 'Allow:'), '405 for other methods')

    broken%template_dir = 'no-such-dir'
    res = handle_request(broken, request('GET', '/', ''))
    call check(res%status == 500 .and. allocated(res%log), 'missing template is a 500 with a log line')
    call check(.not. contains_text(res%body, 'no-such-dir'), '500 page hides internals')
    res = handle_request(broken, request('GET', '/all', ''))
    call check(res%status == 500, 'closed database is a 500')

    call ctx%db%close()
  end subroutine test_controller

end program test_suite
