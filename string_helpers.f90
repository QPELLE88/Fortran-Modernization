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

  ! Turns value into a URL path that cannot change the scheme or host of
  ! prefix // value: it must start with '/' (added when missing, unless prefix
  ! already ends in '/'), cannot start with '//' when it begins the URL, and
  ! every byte outside the RFC 3986 path/query/fragment set is percent-encoded
  ! (including '\', which browsers treat as '/').
  function url_path_escape(prefix, value) result(escaped)
    character(len=*), intent(in)  :: prefix, value
    character(len=:), allocatable :: escaped, path
    character(len=*), parameter   :: SAFE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' // &
      "0123456789-._~/?#[]@!$&'()*+,;=:%"
    character(len=*), parameter   :: HEX = '0123456789ABCDEF'
    integer                       :: i, c, n

    n = len_trim(prefix)
    path = value
    if (n == 0) then
      do while (len(path) >= 2)
        if (path(1:2) /= '//') exit
        path = path(2:)
      enddo
      if (len(path) == 0) then
        path = '/'
      else if (path(1:1) /= '/') then
        path = '/' // path
      endif
    else if (prefix(n:n) /= '/') then
      if (len(path) == 0) then
        path = '/'
      else if (path(1:1) /= '/') then
        path = '/' // path
      endif
    endif

    escaped = ''
    do i = 1, len(path)
      if (index(SAFE, path(i:i)) > 0) then
        escaped = escaped // path(i:i)
      else
        c = iachar(path(i:i))
        escaped = escaped // '%' // HEX(c / 16 + 1:c / 16 + 1) // HEX(mod(c, 16) + 1:mod(c, 16) + 1)
      endif
    enddo
  end function url_path_escape
endmodule
