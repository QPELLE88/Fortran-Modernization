module string_helpers
  implicit none

  contains

  subroutine compact(str)
    ! Converts multiple spaces and tabs to single spaces; deletes control characters;
    ! removes initial spaces.

    character(len=*):: str
    character(len=1):: ch
    character(len=len_trim(str)):: outstr
    integer::isp,k,ich,lenstr,i
    str=adjustl(str)
    lenstr=len_trim(str)
    outstr=' '
    isp=0
    k=0
    do i=1,lenstr
      ch=str(i:i)
      ich=iachar(ch)
      select case(ich)
        case(9,32)     ! space or tab character
          if(isp==0) then
            k=k+1
            outstr(k:k)=' '
          end if
          ! isp=1
        case(33:)      ! not a space, quote, or control character
          k=k+1
          outstr(k:k)=ch
          isp=0
      end select
    end do
    str=adjustl(outstr)
  end subroutine compact

  subroutine string_insert( string, pos, second )
      character(len=*), intent(inout) :: string
      integer, intent(in)             :: pos
      character(len=*), intent(in)    :: second

      integer                         :: length

      length = len( second )
      string(pos+length:)      = string(pos:)
      string(pos:pos+length-1) = second

  end subroutine string_insert

  subroutine string_delete( string, pos, length )
      character(len=*), intent(inout) :: string
      integer, intent(in)             :: pos
      integer, intent(in)             :: length

      string(pos:)             = string(pos+length:)

  end subroutine string_delete

  subroutine string_replace( string, substr, replace )
      character(len=*), intent(inout) :: string
      character(len=*), intent(in)    :: substr
      character(len=*), intent(in)    :: replace

      integer                         :: k, p

      p = 1
      do
        k = index( string(p:), substr )
        if ( k > 0 ) then
          call string_delete( string(p:), k, len(substr) )
          call string_insert( string(p:), k, replace )
          p = p + k - 1 + len(replace)
        else
          exit
        endif
      enddo
  end subroutine string_replace

  function to_lower(str) result(lower)
    character(len=*), intent(in) :: str
    character(len=len(str))      :: lower
    integer                      :: i, c

    lower = str
    do i = 1, len(str)
      c = iachar(str(i:i))
      if (c >= iachar('A') .and. c <= iachar('Z')) lower(i:i) = achar(c + 32)
    enddo
  end function to_lower

  ! escapes & < > " ' for HTML text and quoted attribute values
  function html_escape(str) result(escaped)
    character(len=*), intent(in)  :: str
    character(len=:), allocatable :: escaped
    integer                       :: i

    escaped = ''
    do i = 1, len(str)
      select case (str(i:i))
        case ('&')
          escaped = escaped // '&amp;'
        case ('<')
          escaped = escaped // '&lt;'
        case ('>')
          escaped = escaped // '&gt;'
        case ('"')
          escaped = escaped // '&quot;'
        case ("'")
          escaped = escaped // '&#39;'
        case default
          escaped = escaped // str(i:i)
      end select
    enddo
  end function html_escape

  ! escapes everything but a few URL-safe ASCII characters, for unquoted
  ! attribute values (where a space, quote, '=' or '`' would end the value)
  function html_escape_unquoted(str) result(escaped)
    character(len=*), intent(in)  :: str
    character(len=:), allocatable :: escaped
    character(len=*), parameter   :: RAW = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' // &
      '0123456789-._~/:?#@!$()*+,;%[]'
    character(len=12)             :: entity
    integer                       :: i, c

    escaped = ''
    do i = 1, len(str)
      c = iachar(str(i:i))
      if (index(RAW, str(i:i)) > 0 .or. c >= 128) then
        escaped = escaped // str(i:i)
      else
        write(entity, '(a,i0,a)') '&#', c, ';'
        escaped = escaped // trim(entity)
      endif
    enddo
  end function html_escape_unquoted

  ! decodes &amp; &lt; &gt; &quot; &apos; and ASCII numeric character
  ! references, so a URL prefix can be read the way a browser reads it
  function html_unescape(str) result(decoded)
    character(len=*), intent(in)  :: str
    character(len=:), allocatable :: decoded, ref
    integer                       :: i, semi, code

    decoded = ''
    i = 1
    do while (i <= len(str))
      semi = 0
      if (str(i:i) == '&') semi = index(str(i:min(len(str), i + 9)), ';')
      if (semi > 2) then
        ref = to_lower(str(i + 1:i + semi - 2))
        code = -1
        select case (ref)
          case ('amp')
            code = iachar('&')
          case ('lt')
            code = iachar('<')
          case ('gt')
            code = iachar('>')
          case ('quot')
            code = iachar('"')
          case ('apos')
            code = iachar("'")
          case default
            if (len(ref) > 2 .and. ref(1:2) == '#x') then
              code = parse_digits(ref(3:), 16)
            else if (len(ref) > 1 .and. ref(1:1) == '#') then
              code = parse_digits(ref(2:), 10)
            endif
        end select
        if (code >= 0 .and. code < 128) then
          decoded = decoded // achar(code)
          i = i + semi
          cycle
        endif
      endif
      decoded = decoded // str(i:i)
      i = i + 1
    enddo
  end function html_unescape

  ! value of a short run of decimal or hex digits, -1 if it has any other character
  integer function parse_digits(digits, base)
    character(len=*), intent(in) :: digits
    integer, intent(in)          :: base
    character(len=*), parameter  :: HEXDIGITS = '0123456789abcdef'
    integer                      :: i, d

    parse_digits = -1
    if (len(digits) == 0 .or. len(digits) > 6) return
    parse_digits = 0
    do i = 1, len(digits)
      d = index(HEXDIGITS(1:base), digits(i:i)) - 1
      if (d < 0) then
        parse_digits = -1
        return
      endif
      parse_digits = parse_digits * base + d
    enddo
  end function parse_digits

  ! Makes value safe to append to the (HTML-decoded) start of a URL, prefix,
  ! without changing the scheme or host the template already chose:
  !  - value starts the URL ("" or "/"): it becomes a same-origin path
  !    (one leading '/', never '//')
  !  - value follows a host ("https://h"): it gets a leading '/'
  !  - value follows a path, query or fragment: it is kept as is, and in a
  !    query '&', '=', '+', ';' and '#' are encoded so it stays one parameter
  ! Every byte outside the RFC 3986 set is percent-encoded, including '\',
  ! which browsers treat as '/'.
  function url_path_escape(prefix, value) result(escaped)
    character(len=*), intent(in)  :: prefix, value
    character(len=:), allocatable :: escaped, path, p, rest, safe
    character(len=*), parameter   :: UNRESERVED = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' // &
      "0123456789-._~/:@!$'()*,%"
    character(len=*), parameter   :: HEX = '0123456789ABCDEF'
    integer                       :: i, c, j

    p = prefix
    do i = 1, len(p)
      if (p(i:i) == '\') p(i:i) = '/'
    enddo
    j = scheme_length(p)
    rest = p(j + 1:)

    path = value
    safe = UNRESERVED // '?#&=+;[]'
    if (verify(rest, '/') == 0 .and. j == 0 .and. len(rest) <= 1) then
      do while (len(path) > 0)
        if (path(1:1) /= '/') exit
        path = path(2:)
      enddo
      if (len(rest) == 0) path = '/' // path
    else if (len(rest) > 2) then
      if (rest(1:2) == '//' .and. verify(rest(3:), '/') /= 0 .and. scan(rest(3:), '/?#') == 0) then
        if (len(path) == 0) then
          path = '/'
        else if (path(1:1) /= '/') then
          path = '/' // path
        endif
      endif
    endif
    if (index(p, '?') > 0 .and. index(p, '#') == 0) safe = UNRESERVED // '?[]'

    escaped = ''
    do i = 1, len(path)
      if (index(safe, path(i:i)) > 0) then
        escaped = escaped // path(i:i)
      else
        c = iachar(path(i:i))
        escaped = escaped // '%' // HEX(c / 16 + 1:c / 16 + 1) // HEX(mod(c, 16) + 1:mod(c, 16) + 1)
      endif
    enddo
  end function url_path_escape

  ! length of a leading "scheme:" in url, 0 if none
  integer function scheme_length(url)
    character(len=*), intent(in) :: url
    integer                      :: colon

    scheme_length = 0
    colon = index(url, ':')
    if (colon < 2) return
    if (scan(url(1:1), 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz') == 0) return
    if (verify(url(1:colon - 1), 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+.-') /= 0) return
    scheme_length = colon
  end function scheme_length
endmodule
