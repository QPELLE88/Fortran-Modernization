! Checks that jadetemplate escapes #{key} values for their HTML context.
! Run with: make test
program test_jade
  use jade
  implicit none

  integer :: failures = 0

  call check('a(href="https://en.wikipedia.org#{v}") x', '/wiki/Koala', &
    '<a href="https://en.wikipedia.org/wiki/Koala" id="" class="">x</a>')
  call check('p #{v}', '<script>alert("x")</script>', &
    '<p id="" class="">&lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt;</p>')
  call check('p #{v} #{w}', '#{w}', '<p id="" class="">#{w} &amp;</p>')
  call check('a(title="#{v}") x', '" onmouseover="alert(1)', &
    '<a title="&quot; onmouseover=&quot;alert(1)" id="" class="">x</a>')
  call check("a(title='#{v}') x", "' onclick='x", &
    "<a title='&#39; onclick=&#39;x' id="""" class="""">x</a>")
  call check('a(title=#{v}) x', 'a onclick=x', &
    '<a title=a&#32;onclick&#61;x id="" class="">x</a>')
  call check('a(href="#{v}") x', 'javascript:alert(1)', &
    '<a href="#" id="" class="">x</a>')
  call check('a(href="#{v}") x', ' JaVa'//achar(9)//'Script:alert(1)', &
    '<a href="#" id="" class="">x</a>')
  call check('a(href="#{v}") x', 'https://example.com/a?b=1&c=2', &
    '<a href="https://example.com/a?b=1&amp;c=2" id="" class="">x</a>')
  call check('a(href="https://en.wikipedia.org#{v}") x', '.evil.com', &
    '<a href="https://en.wikipedia.org/.evil.com" id="" class="">x</a>')
  call check('a(href="https://en.wikipedia.org#{v}") x', '@evil.com/"x', &
    '<a href="https://en.wikipedia.org/@evil.com/%22x" id="" class="">x</a>')
  call check('a(href="/#{v}") x', '/evil.com', &
    '<a href="/%2Fevil.com" id="" class="">x</a>')
  call check('a(href="/search?q=#{v}") x', 'a&b=c d', &
    '<a href="/search?q=a%26b%3Dc%20d" id="" class="">x</a>')
  call check('a(href="https://en.wikipedia.org/wiki/#{v}") x', 'Tasmanian_devil', &
    '<a href="https://en.wikipedia.org/wiki/Tasmanian_devil" id="" class="">x</a>')
  call check('script #{v}', 'alert(1)', '<script id="" class=""></script>')
  call check('p #{missing}', 'x', '<p id="" class="">#{missing}</p>')

  if (failures > 0) then
    print '(I0,A)', failures, ' jade test(s) failed'
    error stop 1
  endif
  print '(A)', 'all jade tests passed'

contains

  subroutine check(source, value, expected)
    character(len=*), intent(in) :: source, value, expected
    character(len=10000)         :: templatefile
    character(len=50), dimension(10,2) :: pagevars
    character(len=20000)         :: actual
    integer                      :: src, out

    open(newunit=src, file='test_jade.tmp.jade', status='replace')
    write(src, '(a)') source
    close(src)

    pagevars = ''
    pagevars(1,1) = 'v'
    pagevars(1,2) = value
    pagevars(2,1) = 'w'
    pagevars(2,2) = '&'

    templatefile = 'test_jade.tmp.jade'
    open(newunit=out, status='scratch')
    call jadetemplate(templatefile, out, pagevars)
    rewind(out)
    read(out, '(a)') actual
    close(out)
    open(newunit=src, file='test_jade.tmp.jade')
    close(src, status='delete')

    if (trim(actual) /= expected) then
      failures = failures + 1
      print '(A)', 'FAIL: ' // source
      print '(A)', '  value:    ' // value
      print '(A)', '  expected: ' // expected
      print '(A)', '  actual:   ' // trim(actual)
    endif
  endsubroutine
endprogram
