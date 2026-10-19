! unit tests for jadetemplate escaping; run with `make test`
program test_jade
  use jade
  use string_helpers
  implicit none

  integer :: failures = 0
  character(len=*), parameter :: RESULT = 'template/result.jade', EDGE = 'test/edge.jade'

  call check(RESULT, '/wiki/Koala', 'koala', 'a b', 'x', &
    '<a href="https://en.wikipedia.org/wiki/Koala" id="" class="">koala</a>')
  call check(RESULT, '/wiki/AC/DC', 'x', 'x', 'x', 'href="https://en.wikipedia.org/wiki/AC/DC"')
  call check(RESULT, '/wiki/Koala', '<script>alert(1)</script>', 'x', 'x', &
    '>&lt;script&gt;alert(1)&lt;/script&gt;</a>')
  call check(RESULT, '/wiki/Koala', 'x', '" onmouseover="alert(1)', 'x', &
    '>&quot; onmouseover=&quot;alert(1)</em>')
  call check(RESULT, '@evil.example/', 'x', 'x', 'x', 'href="https://en.wikipedia.org/@evil.example/"')
  call check(RESULT, '.evil.example/', 'x', 'x', 'x', 'href="https://en.wikipedia.org/.evil.example/"')
  call check(RESULT, '/x" onclick="alert(1)', 'x', 'x', 'x', &
    'href="https://en.wikipedia.org/x%22%20onclick=%22alert(1)"')
  call check(RESULT, '/a\b<c>', 'x', 'x', 'x', 'href="https://en.wikipedia.org/a%5Cb%3Cc%3E"')
  call check(RESULT, '/wiki/Koala', '#{description}', 'x', 'SECRET', '>#{description}</a>')
  call check(RESULT, '/wiki/Koala', 'Tom & Jerry''s', 'x', 'x', '>Tom &amp; Jerry&#39;s</a>')
  call check_url('', '//evil.example/x', '/evil.example/x')
  call check_url('', 'javascript:alert(1)', '/javascript:alert(1)')
  call check_url('/wiki/', 'Koala', 'Koala')
  call check_url('/', '//evil.example/x', 'evil.example/x')
  call check_url('/', '/\evil.example', '%5Cevil.example')
  call check_url('/search?q=', 'Koala', 'Koala')
  call check_url('/search?q=', 'a&b=c#d', 'a%26b%3Dc%23d')
  call check_url(html_unescape('https://en.wikipedia.org&#64;'), 'x', '/x')
  call check_url(html_unescape('https://en.wikipedia.org&#x40;'), '//x', '//x')
  if (html_unescape('a&amp;b&#64;&#x2F;&nope;') /= 'a&b@/&nope;') then
    failures = failures + 1
    print '(a)', 'FAIL: html_unescape'
  endif

  call check(EDGE, 'x', 'Koala', 'x', 'x', 'Use #{ before Koala')
  call check(EDGE, 'a\b', 'x', 'x', 'x', 'href="/find?q=>a%5Cb"')
  call check(EDGE, 'x', 'x', '//evil.example', 'x', 'title="//evil.example"')
  call check(EDGE, 'x', 'x', 'x', 'Koala', 'href="/search?q=Koala"')
  call check(EDGE, 'x', 'x', 'x', 'a&b=c', 'href="/search?q=a%26b%3Dc"')

  if (failures > 0) then
    print '(i0,a)', failures, ' test(s) failed'
    error stop 1
  endif
  print '(a)', 'all jade tests passed'

contains

  subroutine check(template, wikiLink, name, latinName, description, expected)
    character(len=*), intent(in) :: template, wikiLink, name, latinName, description, expected
    character(len=50), dimension(10,2) :: pagevars
    character(len=10000) :: templatefile
    character(len=10000) :: line
    integer :: unitNo

    pagevars = ''
    pagevars(1,1) = 'name'
    pagevars(2,1) = 'latinName'
    pagevars(3,1) = 'wikiLink'
    pagevars(4,1) = 'description'
    pagevars(1,2) = name
    pagevars(2,2) = latinName
    pagevars(3,2) = wikiLink
    pagevars(4,2) = description

    templatefile = template
    open(newunit=unitNo, status='scratch')
    call jadetemplate(templatefile, unitNo, pagevars)
    rewind(unitNo)
    read(unitNo, '(a)') line
    close(unitNo)

    if (index(line, expected) == 0) then
      failures = failures + 1
      print '(a)', 'FAIL: expected to find: ' // expected
      print '(a)', '      in: ' // trim(line)
    endif
  end subroutine

  subroutine check_url(prefix, value, expected)
    character(len=*), intent(in) :: prefix, value, expected
    character(len=:), allocatable :: got

    got = url_path_escape(prefix, value)
    if (got /= expected) then
      failures = failures + 1
      print '(a)', 'FAIL: url_path_escape("' // prefix // '", "' // value // '") = ' // got
    endif
  end subroutine
end program
