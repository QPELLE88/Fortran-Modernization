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

      integer                      :: i

      lower = str
      do i = 1, len(str)
        if (str(i:i) >= 'A' .and. str(i:i) <= 'Z') then
          lower(i:i) = achar(iachar(str(i:i)) + 32)
        endif
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

  ! encodes every ASCII non-alphanumeric character as a numeric entity,
  ! for values placed in unquoted attributes or inside a tag
  function html_escape_strict(str) result(escaped)
      character(len=*), intent(in)  :: str
      character(len=:), allocatable :: escaped

      character(len=3)              :: code
      integer                       :: i

      escaped = ''
      do i = 1, len(str)
        select case (str(i:i))
          case ('A':'Z', 'a':'z', '0':'9', achar(128):achar(255))
            ! non-ASCII (UTF-8) bytes cannot end an unquoted value
            escaped = escaped // str(i:i)
          case default
            write(code, '(I0)') iachar(str(i:i))
            escaped = escaped // '&#' // trim(code) // ';'
        end select
      enddo
  end function html_escape_strict

  ! decodes the entities html_escape and html_escape_strict produce, plus
  ! other numeric references (&#NN; and &#xHH;) below 256
  function html_unescape(str) result(decoded)
      character(len=*), intent(in)  :: str
      character(len=:), allocatable :: decoded

      integer                       :: i, semi, code, io

      decoded = ''
      i = 1
      do while (i <= len(str))
        semi = 0
        if (str(i:i) == '&') semi = index(str(i:min(len(str), i + 7)), ';')
        if (semi > 2) then
          semi = i + semi - 1
          code = -1
          select case (str(i + 1:semi - 1))
            case ('amp')
              code = 38
            case ('lt')
              code = 60
            case ('gt')
              code = 62
            case ('quot')
              code = 34
            case ('apos')
              code = 39
            case default
              if (str(i + 1:i + 1) == '#' .and. semi - i > 2) then
                if (str(i + 2:i + 2) == 'x' .or. str(i + 2:i + 2) == 'X') then
                  read(str(i + 3:semi - 1), '(Z8)', iostat=io) code
                else
                  read(str(i + 2:semi - 1), *, iostat=io) code
                endif
                if (io /= 0) code = -1
              endif
          end select
          if (code >= 0 .and. code < 256) then
            decoded = decoded // achar(code)
            i = semi + 1
            cycle
          endif
        endif
        decoded = decoded // str(i:i)
        i = i + 1
      enddo
  end function html_unescape

  ! percent-encodes characters outside allowed; with keepEscapes,
  ! well-formed %XX sequences are passed through unchanged
  function percent_encode(str, allowed, keepEscapes) result(encoded)
      character(len=*), intent(in)  :: str, allowed
      logical, intent(in)           :: keepEscapes
      character(len=:), allocatable :: encoded

      character(len=*), parameter   :: UNRESERVED = &
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~'
      character(len=2)              :: hex
      integer                       :: i

      encoded = ''
      do i = 1, len(str)
        if (index(UNRESERVED, str(i:i)) > 0 .or. index(allowed, str(i:i)) > 0) then
          encoded = encoded // str(i:i)
          cycle
        endif
        if (keepEscapes .and. str(i:i) == '%' .and. i + 2 <= len(str)) then
          if (verify(str(i+1:i+2), '0123456789ABCDEFabcdef') == 0) then
            encoded = encoded // '%'
            cycle
          endif
        endif
        write(hex, '(Z2.2)') iachar(str(i:i))
        encoded = encoded // '%' // hex
      enddo
  end function percent_encode
endmodule
