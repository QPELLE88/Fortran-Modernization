!> Unit tests. Run from the repository root (`make test`) so template/ and marsupials.sqlite3 resolve.
program test_suite
  use strings
  use http
  use jade
  use marsupials
  use app
  implicit none

  integer :: failures = 0, checks = 0

  call test_strings()
  call test_forms()
  call test_jade()
  call test_model()
  call test_routes()

  write (*, '(i0, a, i0, a)') checks, ' checks, ', failures, ' failures'
  if (failures > 0) error stop 1

contains

  subroutine check(cond, label)
    logical, intent(in) :: cond
    character(len=*), intent(in) :: label

    checks = checks + 1
    if (.not. cond) then
      failures = failures + 1
      write (*, '(a)') 'FAIL: ' // label
    end if
  end subroutine check

  subroutine check_eq(actual, expected, label)
    character(len=*), intent(in) :: actual, expected, label

    call check(actual == expected .and. len(actual) == len(expected), label)
    if (actual /= expected .or. len(actual) /= len(expected)) then
      write (*, '(a)') '  expected: [' // expected // ']'
      write (*, '(a)') '  actual:   [' // actual // ']'
    end if
  end subroutine check_eq

  logical function contains_str(haystack, needle)
    character(len=*), intent(in) :: haystack, needle

    contains_str = index(haystack, needle) > 0
  end function contains_str

  subroutine test_strings()
    call check_eq(html_escape('<a href="x">Tom & Jerry'' s</a>'), &
                  '&lt;a href=&quot;x&quot;&gt;Tom &amp; Jerry&#39; s&lt;/a&gt;', 'html_escape')
    call check_eq(html_escape(''), '', 'html_escape empty')
    call check_eq(url_decode('sugar+glider%21'), 'sugar glider!', 'url_decode plus and hex')
    call check_eq(url_decode('100%'), '100%', 'url_decode trailing percent')
    call check_eq(url_decode('%zz%4'), '%zz%4', 'url_decode malformed escapes kept')
    call check_eq(replace_all('a.b.c', '.', ' '), 'a b c', 'replace_all')
    call check_eq(replace_all("it's", "'", "''"), "it''s", 'replace_all growing')
    call check_eq(to_lower('KoAla'), 'koala', 'to_lower')
    call check(starts_with('/wiki/Koala', '/wiki/'), 'starts_with true')
    call check(.not. starts_with('/w', '/wiki/'), 'starts_with shorter')
    call check_eq(int_to_str(-42), '-42', 'int_to_str')
  end subroutine test_strings

  subroutine test_forms()
    type(request_t) :: req
    character(len=:), allocatable :: long_query

    req = new_request('GET', '/search', 'q=sugar+glider&x=1&&flag&q=second')
    call check(size(req%params) == 4, 'parse_form skips empty pairs')
    call check_eq(get_param(req, 'q', ''), 'sugar glider', 'get_param first value wins')
    call check_eq(get_param(req, 'flag', 'missing'), '', 'get_param key without value')
    call check_eq(get_param(req, 'nope', 'dflt'), 'dflt', 'get_param default')

    req = new_request('GET', '', '')
    call check_eq(req%path, '/', 'empty path defaults to /')
    call check(size(req%params) == 0, 'empty query string')

    req = new_request('POST', '/search', 'a=1', 'q=wombat', 'application/x-www-form-urlencoded')
    call check_eq(get_param(req, 'q', ''), 'wombat', 'form POST body merged')
    req = new_request('POST', '/search', '', 'q=wombat', 'text/plain')
    call check_eq(get_param(req, 'q', 'none'), 'none', 'non-form POST body ignored')

    long_query = 'q=' // repeat('k', 5000)
    req = new_request('GET', '/search', long_query)
    call check(len(get_param(req, 'q', '')) == 5000, 'query strings are not truncated')
  end subroutine test_forms

  subroutine test_jade()
    character(len=:), allocatable :: html
    type(template_var_t), allocatable :: no_vars(:)

    allocate(no_vars(0))
    html = render_jade('.row' // LF // '  .col-sm-12' // LF // '    h3 Hi', no_vars)
    call check_eq(html, '<div class="row">' // LF // '  <div class="col-sm-12">' // LF // &
                  '    <h3>Hi</h3>' // LF // '  </div>' // LF // '</div>' // LF, 'nesting')

    html = render_jade('a#home.btn.btn-primary(href="/", title="x") Go', no_vars)
    call check_eq(html, '<a id="home" class="btn btn-primary" href="/" title="x">Go</a>' // LF, &
                  'id, classes and attrs')

    html = render_jade('button(class="btn"type="submit") Send', no_vars)
    call check_eq(html, '<button class="btn" type="submit">Send</button>' // LF, 'attrs without separator')

    html = render_jade('.x(a="(#.)") t', no_vars)
    call check_eq(html, '<div class="x" a="(#.)">t</div>' // LF, 'specials inside quoted attrs')

    html = render_jade('form' // LF // '  input(name="q")' // LF // '  hr' // LF // 'p after', no_vars)
    call check_eq(html, '<form>' // LF // '  <input name="q">' // LF // '  <hr>' // LF // '</form>' // &
                  LF // '<p>after</p>' // LF, 'void elements are not closed')

    html = render_jade('ul' // CRLF // CRLF // '  li one' // CRLF // '  li two' // CRLF, no_vars)
    call check_eq(html, '<ul>' // LF // '  <li>one</li>' // LF // '  <li>two</li>' // LF // '</ul>' // LF, &
                  'CRLF and blank lines')

    html = render_jade('p' // LF // '  | plain text', no_vars)
    call check_eq(html, '<p>' // LF // '  plain text' // LF // '</p>' // LF, 'piped text')

    html = render_jade('a(href="https://en.wikipedia.org#{link}") #{name} #{missing}', &
                       [tvar('name', '<script>alert("x")</script>'), tvar('link', '/wiki/"onmouseover')])
    call check_eq(html, '<a href="https://en.wikipedia.org/wiki/&quot;onmouseover">' // &
                  '&lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt; </a>' // LF, 'interpolation is escaped')

    html = render_jade('p #{unterminated', no_vars)
    call check_eq(html, '<p>#{unterminated</p>' // LF, 'unterminated placeholder kept')
  end subroutine test_jade

  subroutine test_model()
    type(marsupial_t), allocatable :: rows(:)
    character(len=:), allocatable :: errmsg
    logical :: ok

    call search_marsupials('marsupials.sqlite3', 'koala', rows, ok, errmsg)
    call check(ok .and. size(rows) == 1, 'search koala')
    if (size(rows) == 1) then
      call check_eq(rows(1)%latin_name, 'Phascolarctos cinereus', 'koala latin name')
      call check_eq(rows(1)%wiki_link, '/wiki/Koala', 'koala wiki link')
    end if

    call search_marsupials('marsupials.sqlite3', '  KANGA ', rows, ok, errmsg)
    call check(ok .and. size(rows) == 1, 'search is trimmed and case-insensitive')

    call search_marsupials('marsupials.sqlite3', 'a', rows, ok, errmsg)
    call check(ok .and. size(rows) == 5, 'search returns every match')

    call search_marsupials('marsupials.sqlite3', "' OR 1=1 --", rows, ok, errmsg)
    call check(ok .and. size(rows) == 0, 'quotes are bound, not injected')

    call search_marsupials('marsupials.sqlite3', '%', rows, ok, errmsg)
    call check(ok .and. size(rows) == 0, 'LIKE wildcards are literal')

    call search_marsupials('marsupials.sqlite3', '   ', rows, ok, errmsg)
    call check(ok .and. size(rows) == 0, 'blank search matches nothing')

    call all_marsupials('marsupials.sqlite3', rows, ok, errmsg)
    call check(ok .and. size(rows) == 5, 'all marsupials')
    if (size(rows) == 5) call check_eq(rows(5)%name, 'wombat', 'all ordered by rowid')

    call all_marsupials('does-not-exist.sqlite3', rows, ok, errmsg)
    call check(.not. ok .and. len(errmsg) > 0 .and. size(rows) == 0, 'missing database reports error')
  end subroutine test_model

  subroutine test_routes()
    type(app_config_t) :: cfg
    type(response_t) :: resp

    cfg%template_dir = 'template'
    cfg%db_path = 'marsupials.sqlite3'

    resp = handle_request(new_request('GET', '/', ''), cfg)
    call check(resp%status == 200 .and. contains_str(resp%body, 'finally, a Fortran Web Framework'), 'GET /')
    call check(contains_str(resp%body, 'Setup Guide'), 'GET / keeps long lines intact')

    resp = handle_request(new_request('GET', '/search', 'q=sugar'), cfg)
    call check(resp%status == 200 .and. contains_str(resp%body, 'Petaurus breviceps'), 'GET /search?q=sugar')
    call check(contains_str(resp%body, 'href="https://en.wikipedia.org/wiki/Sugar_glider"'), 'result link')

    resp = handle_request(new_request('GET', '/search', 'q=%3Cimg+src%3Dx%3E'), cfg)
    call check(contains_str(resp%body, '&ldquo;&lt;img src=x&gt;&rdquo;'), 'search echo is escaped')
    call check(.not. contains_str(resp%body, '<img src=x>'), 'no raw markup from query')

    resp = handle_request(new_request('GET', '/search', ''), cfg)
    call check(contains_str(resp%body, 'Type a marsupial name') .and. &
               .not. contains_str(resp%body, 'Phascolarctos'), 'empty search shows prompt')

    resp = handle_request(new_request('GET', '/all', ''), cfg)
    call check(resp%status == 200 .and. contains_str(resp%body, 'Vombatus ursinus'), 'GET /all lists every row')

    resp = handle_request(new_request('GET', '/test', ''), cfg)
    call check(resp%status == 200 .and. contains_str(resp%body, '<a id="id" href="/">id and link</a>'), 'GET /test')

    resp = handle_request(new_request('GET', '/missing', ''), cfg)
    call check(resp%status == 404, 'unknown route is 404')
    call check(starts_with(serialize_response(resp), 'Status: 404 Not Found' // CRLF), 'status header')

    cfg%template_dir = 'no-such-dir'
    resp = handle_request(new_request('GET', '/', ''), cfg)
    call check(resp%status == 500, 'missing template is 500')

    call check_eq(safe_wiki_path('/wiki/Allied_rock-wallaby'), '/wiki/Allied_rock-wallaby', 'wiki path kept')
    call check_eq(safe_wiki_path('@evil.example/x'), '', 'off-site link dropped')
    call check_eq(safe_wiki_path('/wiki/../w/index.php'), '', 'dot segments dropped')
    call check_eq(safe_wiki_path('/wiki/"x'), '', 'quotes dropped')
  end subroutine test_routes

end program test_suite
