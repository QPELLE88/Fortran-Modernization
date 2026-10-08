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

  ! percent-encodes everything except RFC 3986 unreserved characters,
  ! so the result is safe to use as a single URL path segment
  function url_encode(str) result(encoded)
      character(len=*), intent(in)  :: str
      character(len=:), allocatable :: encoded

      character(len=2)              :: hex
      integer                       :: i

      encoded = ''
      do i = 1, len(str)
        select case (str(i:i))
          case ('A':'Z', 'a':'z', '0':'9', '-', '.', '_', '~')
            encoded = encoded // str(i:i)
          case default
            write(hex, '(Z2.2)') iachar(str(i:i))
            encoded = encoded // '%' // hex
        end select
      enddo
  end function url_encode
endmodule
