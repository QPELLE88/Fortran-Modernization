! cgi_protocol.f90 --
!     Module for interfacing with a web server via the CGI protocol
!     (GET, POST, ...)
!
!     TODO:
!     - find out how to deal with text area data
!     - find out how to deal with uploaded files
!     - merge environment variables into the cgi_get routines
!     - implement the SCGI protocol
!     - implement delayed responses (also via customisable template)
!
module cgi_protocol
    implicit none

    integer, parameter :: DICT_KEY_LENGTH    = 80
    integer, parameter :: DICT_VALUE_LENGTH  = 200
    integer, parameter :: DICT_BUFFER_LENGTH = DICT_KEY_LENGTH + DICT_VALUE_LENGTH + 1

    integer, parameter :: output_no_header    = 0
    integer, parameter :: output_html         = 1
    integer, parameter :: output_text         = 2
    integer, parameter, private :: output_html_delayed = 3  ! Not implemented yet!
    integer, parameter, private :: output_text_delayed = 4

    type DICT_DATA
        character(len=DICT_VALUE_LENGTH) :: value
    end type DICT_DATA

    interface cgi_get
        module procedure cgi_get_string
        module procedure cgi_get_integer
        module procedure cgi_get_real
    end interface

    type(DICT_DATA), parameter :: dict_null = dict_data('')

    integer, private, save     :: method    = -1
    integer, private, save     :: luout_cgi = -1
    logical, private, save     :: header_written

    integer, parameter, private :: SESSION_ID_BYTES  = 16  ! 128 bits
    integer, parameter, private :: SESSION_ID_LENGTH = 2 * SESSION_ID_BYTES
    character(len=16), parameter, private :: hex_digits = '0123456789abcdef'
    character(len=DICT_VALUE_LENGTH), private, save :: session_dir = ' '

    private :: cgi_session_is_issued
    private :: cgi_revoke_session
    private :: cgi_random_session_id
    private :: cgi_free_unit

!
! Body of source code for storing and retrieving the data
! (also contains the CONTAINS clause)
!
include 'dictionary.f90'

! cgi_begin --
!     Determine the type of interaction with the server and retrieve the data
!
! Arguments:
!     html           Whether the output will be HTML or plain text
!     dict           Dictionary holding the information from the server (output)
!     luout          LU-number for writing the file (output!)
!
! Note:
!     This routine determines the way the server passes on the data
!     by looking for clues in the environment variables QUERY_LENGTH
!     and QUERY_STRING
!
!
! TODO:
!     Support for two-pass run (if the computation takes a longer
!     than a few seconds)
!
subroutine cgi_begin( html, dict, luout )
    integer, intent(in)               :: html
    type(DICT_STRUCT), pointer        :: dict
    integer, intent(out)              :: luout

    integer                           :: length
    integer                           :: status
    logical                           :: opend
    character(len=DICT_BUFFER_LENGTH) :: string
    character(len=1)                  :: ch

    !
    ! Clean up, if necessary
    !
    if ( associated(dict) ) then
        call dict_destroy( dict )
    endif

    header_written = .false.

    !
    ! Determine which input method
    !
    call get_environment_variable( "QUERY_STRING", length=length, status=status )
    if ( status == 0 ) then
        call cgi_get_method( dict, length )
        method = 1
    else
        call get_environment_variable( "CONTENT_LENGTH", value=string, status=status )
        if ( status == 0 ) then
            read( string, * ) length
            call cgi_post_method( dict, length )
            method = 1
        else
            read( *, '(A)', advance = 'no' ) ch
            if ( ch == '%' ) then
                !
                ! TODO: better method for determining length
                call cgi_dustmote_method( dict )
                method = 2
            elseif ( index( '1234567890', ch ) > 0 ) then
   !            call cgi_simple_cgi( dict )
                method = -1
            else
                method = -1
            endif
        endif
    endif

    !
    ! If we did not get the correct information, just blow the
    ! whole thing off
    !
    if ( method == -1 ) then
        call cgi_error( "CGI protocol not recognised or not implemented" )
    endif

    !
    ! What LU-number for the output
    ! method 1: write directly to standard output (assumed to be at unit 6)
    ! method 2: write to a file first (cgiout)
    !
    if ( method == 1 ) then
        luout = 6
    endif
    if ( method == 2 ) then
        do luout = 10,99
            inquire( luout, opened = opend )
            if ( .not. opend ) then
                exit
            endif
        enddo
        open( luout, file = 'cgiout' )
    endif
    luout_cgi = luout

    !
    ! Write the header lines
    !
    select case ( html )
        case ( output_html, output_text )
            call cgi_header( html )
        case( output_html_delayed, output_text_delayed )
            ! TODO
        case( output_no_header )
            ! Writing the header is delayed, because the type is not known yet
        case default
            call cgi_error( "Programming error: wrong value for parameter 'html' in CGI_BEGIN" )
    end select

end subroutine cgi_begin

! cgi_header --
!     Write the CGI header information
!
! Arguments:
!     type           Type of header
!
subroutine cgi_header( type )
    integer, intent(in) :: type

    header_written = .true.

    select case ( type )
        case ( output_html, output_html_delayed )
            write( luout_cgi, '(a)' ) 'Content-Type: text/html;charset=iso8859-1'
            write( luout_cgi, '(a)' ) ''
        case( output_text, output_text_delayed )
            write( luout_cgi, '(a)' ) 'Content-Type: text/plain;charset=iso8859-1'
            write( luout_cgi, '(a)' ) ''
        case( output_no_header )
            call cgi_error( "Programming error: value 'output_no_header' not allowed in CGI_HEADER" )
        case default
            call cgi_error( "Programming error: wrong value for parameter 'type' in CGI_HEADER" )
    end select
end subroutine cgi_header

! cgi_get_method --
!     Get the information via the environment variable QUERY_STRING
!
! Arguments:
!     dict           Dictionary holding the information from the server (output)
!     length         Total length of the input
!
subroutine cgi_get_method( dict, length )
    type(DICT_STRUCT), pointer :: dict
    integer, intent(in)        :: length

    character(len=length)      :: buffer

    call get_environment_variable( "QUERY_STRING", value=buffer )
    call cgi_store_dict( dict, buffer )

end subroutine cgi_get_method

! cgi_post_method --
!     Get the information via standard input
!
! Arguments:
!     dict           Dictionary holding the information from the server (output)
!     length         Total length of the input
!
subroutine cgi_post_method( dict, length )
    type(DICT_STRUCT), pointer :: dict
    integer, intent(in)        :: length

    character(len=length)      :: buffer

    read( *, '(a)', advance='no' ) buffer
    call cgi_store_dict( dict, buffer )

end subroutine cgi_post_method

! cgi_dustmote_method --
!     Get the information line by line
!
! Arguments:
!     dict           Dictionary holding the information from the server (output)
!
!
subroutine cgi_dustmote_method( dict )
    type(DICT_STRUCT), pointer :: dict

    type(DICT_DATA)            :: data
    character(len=DICT_KEY_LENGTH)    :: key
    character(len=DICT_BUFFER_LENGTH) :: input
    integer                    :: k
    integer                    :: lu
    integer                    :: ierr
    logical                    :: opend

    read( *, '(a)' ) input ! Skip the remainder of the first line
    read( *, '(a)' ) input

    do while ( input /= '%END%' )
        call cgi_store_dict( dict, input )

        read( *, '(a)', iostat=ierr ) input
        if ( ierr /= 0 ) then
            exit
        endif
    enddo

end subroutine cgi_dustmote_method

! cgi_store_dict --
!     Store the information in the dictionary
!
! Arguments:
!     dict           Dictionary holding all information
!     string         Complete string received from CGI server
!
subroutine cgi_store_dict( dict, string )
    type(DICT_STRUCT), pointer      :: dict
    character(len=*), intent(in)    :: string

    character(len=DICT_KEY_LENGTH)  :: key
    character(len=len(string))      :: buffer
    type(DICT_DATA)                 :: data

    integer                         :: k
    integer                         :: keq

    buffer = string

    do
        k = index( buffer, '&' )
        if ( k .le. 0 ) then
            if ( buffer == ' ' ) then
                exit
            else
                k = len(buffer) + 1   ! Remaining piece
            endif
        endif

        !
        ! Store the string
        !
        keq = index( buffer(1:k-1), '=' )
        if ( keq > 0 ) then
            key = buffer(1:keq-1)
            call cgi_decode_string( key )
            data%value = buffer(keq+1:k-1)
            call cgi_decode_string( data%value )

            if ( .not. associated( dict ) ) then
                call dict_create( dict, key, data )
            else
                call dict_add_key( dict, key, data )
            endif
        endif

        if ( k < len(buffer) ) then
            buffer = buffer(k+1:)
        else
            buffer = ' '
        endif

    enddo
end subroutine cgi_store_dict

! cgi_decode_string --
!     Decode the string (replace + and %xx)
!
! Arguments:
!     dict           Dictionary holding all information
!     string         Complete string received from CGI server
!
subroutine cgi_decode_string( string )
    character(len=*), intent(inout) :: string

    integer :: k
    integer :: ch
    integer :: n
    integer :: p
    integer :: ierr

    !
    ! First the +'s
    !
    do
        k = index( string, '+' )
        if ( k .le. 0 ) exit

        string(k:k) = ' '
    enddo

    !
    ! Now %xx
    !
    n = len(string)
    p = 1
    do
        k = index( string(p:), '%' ) + p - 1
        if ( k .lt. p .or. k+2 .gt. n) exit

        read( string(k+1:k+2), '(z2)', iostat=ierr ) ch
        if ( ierr /= 0 ) then
            p = k + 1
            cycle
        endif
        string(k:) = achar(ch) // string(k+3:)
        n = n - 2
        p = k + 1
    enddo
end subroutine cgi_decode_string

! cgi_end --
!     Indicate to the server that we are done
! Arguments:
!     None
! Note:
!     This is simply done by writing a file cgiready,
!     if method 2 is used. Stop in all cases
!
subroutine cgi_end

    integer :: lu
    logical :: opend

    if ( method == 2 ) then
        do lu = 10,99
            inquire( lu, opened=opend )
            if ( .not. opend ) then
                open( lu, file = "cgiready" )
                close( lu )
                exit
            endif
        enddo
    endif

    stop

end subroutine cgi_end

! cgi_error --
!     Report a fatal error
! Arguments:
!     msg               Message to be printed
!     template          Template file to be used (optional)
!
subroutine cgi_error( msg, template )
    character(len=*), intent(in)           :: msg
    character(len=*), intent(in), optional :: template

    character(len=200)                     :: text
    integer :: k
    integer :: ierr
    integer :: lu
    logical :: opend
    logical :: exists

    exists = .false.
    if ( present(template) ) then
        inquire( file = template, exist = exists )
    endif

    if ( .not. header_written ) then
        write( luout_cgi, '(a)' ) 'Content-Type: text/html;charset=iso8859-1'
        write( luout_cgi, '(a)' ) ''
    endif

    if ( exists ) then
        do lu = 10,99
            inquire( lu, opened = opend )
            if ( .not. opend ) then
                exit
            endif
        enddo
        open( lu, file = template )

        do
            read( lu, '(a)', iostat=ierr ) text
            if ( ierr /= 0 ) exit

            k = index( text, 'MSG' )
            if ( k > 0 ) then
                write( luout_cgi, '(3a)' ) text(1:k-1), trim(msg), text(k+3:)
            else
                write( luout_cgi, '(a)' ) text
            endif
        enddo
        close( lu )
    else
        write( luout_cgi, * ) '<html>'
        write( luout_cgi, * ) '<head><title>Severe error</title></head>'
        write( luout_cgi, * ) '<body>'
        write( luout_cgi, * ) '<b>', trim(msg), '</b>'
        write( luout_cgi, * ) '</body></html>'
    endif

    call cgi_end

end subroutine cgi_error

! cgi_session_directory --
!     Set the directory in which issued session IDs are recorded
! Arguments:
!     dir           Directory name (must exist and be readable and writable
!                   only by the CGI application, e.g. mode 0700: the issued
!                   IDs are the file names). An empty string, or a name
!                   longer than DICT_VALUE_LENGTH, disables sessions
!
subroutine cgi_session_directory( dir )
    character(len=*), intent(in) :: dir

    if ( len_trim(dir) > len(session_dir) ) then
        session_dir = ' '
    else
        session_dir = dir
    endif

end subroutine cgi_session_directory

! cgi_get_session --
!     Get the value of the "sessionid" variable
! Arguments:
!     dict          Dictionary with values
!     value         Value of the session ID (character(len=32) or longer)
! Note:
!     The session ID can be used to uniquely identify the
!     connection with the user. But it should be passed into the
!     HTML output as a hidden variable (see the documentation
!     for more information)
!
!     A client-supplied "sessionid" is only accepted if it was issued
!     earlier by cgi_new_session, i.e. it is a 32-character lowercase
!     hex string with a matching file in the directory set via
!     cgi_session_directory. Otherwise a new ID is issued.
!
!     Limits: the helper only issues and recognises IDs. It does not
!     expire them, bind them to a user, or authenticate anyone, and an
!     ID obtained by an attacker can be planted in a victim's form.
!     Call cgi_new_session after any change of privilege (e.g. login).
!
!     If no session directory is set, /dev/urandom cannot be read, the
!     ID cannot be recorded, or value is too short, value is set to
!     blanks and any "sessionid" in the dictionary is blanked.
!
subroutine cgi_get_session( dict, value )
    type(DICT_STRUCT), pointer :: dict
    character(len=*)           :: value

    type(DICT_DATA)            :: data

    if ( associated(dict) ) then
        if ( dict_has_key( dict, "sessionid" ) ) then
            data = dict_get_key( dict, "sessionid" )
            if ( cgi_session_is_issued( data%value ) .and. &
                 len(value) >= SESSION_ID_LENGTH ) then
                value = data%value(1:SESSION_ID_LENGTH)
                return
            endif
        endif
    endif

    call cgi_new_session( dict, value )

end subroutine cgi_get_session

! cgi_new_session --
!     Issue a new session ID, replacing any "sessionid" in the dictionary
!     (an issued ID found there is revoked once the new ID is recorded;
!     if no new ID can be issued, the old one stays valid)
! Arguments:
!     dict          Dictionary with values
!     value         New session ID (blank on failure, see cgi_get_session)
!
subroutine cgi_new_session( dict, value )
    type(DICT_STRUCT), pointer :: dict
    character(len=*)           :: value

    character(len=SESSION_ID_LENGTH) :: id
    character(len=DICT_VALUE_LENGTH) :: old_id
    type(DICT_DATA)                  :: data
    logical                          :: ok
    integer                          :: lu
    integer                          :: ierr

    value = ' '
    old_id = ' '
    if ( associated(dict) ) then
        if ( dict_has_key( dict, "sessionid" ) ) then
            data = dict_get_key( dict, "sessionid" )
            old_id = data%value
            data%value = ' '
            call dict_add_key( dict, "sessionid", data )
        endif
    endif

    if ( len(value) < SESSION_ID_LENGTH .or. len_trim(session_dir) == 0 ) then
        return
    endif

    call cgi_random_session_id( id, ok )
    if ( .not. ok ) then
        return
    endif

    lu = cgi_free_unit()
    if ( lu < 0 ) then
        return
    endif
    open( lu, file = trim(session_dir) // '/' // id, status = 'new', &
        action = 'write', iostat = ierr )
    if ( ierr /= 0 ) then
        return
    endif
    close( lu )

    call cgi_revoke_session( old_id )

    value = id
    data%value = id
    if ( associated(dict) ) then
        call dict_add_key( dict, "sessionid", data )
    else
        call dict_create( dict, "sessionid", data )
    endif

end subroutine cgi_new_session

! cgi_revoke_session --
!     Remove the record of an issued session ID, so it is no longer accepted
! Arguments:
!     id            Session ID to revoke (ignored if it was not issued)
!
subroutine cgi_revoke_session( id )
    character(len=*), intent(in) :: id

    integer :: lu
    integer :: ierr

    if ( .not. cgi_session_is_issued( id ) ) then
        return
    endif

    lu = cgi_free_unit()
    if ( lu < 0 ) then
        return
    endif
    open( lu, file = trim(session_dir) // '/' // id(1:SESSION_ID_LENGTH), &
        status = 'old', iostat = ierr )
    if ( ierr == 0 ) then
        close( lu, status = 'delete' )
    endif

end subroutine cgi_revoke_session

! cgi_session_is_issued --
!     Check that a session ID is well-formed and was issued by this server
! Arguments:
!     id            Session ID to check
!
logical function cgi_session_is_issued( id )
    character(len=*), intent(in) :: id

    cgi_session_is_issued = .false.

    if ( len_trim(session_dir) == 0 ) then
        return
    endif
    if ( len_trim(id) /= SESSION_ID_LENGTH ) then
        return
    endif
    if ( verify( id(1:SESSION_ID_LENGTH), hex_digits ) /= 0 ) then
        return
    endif

    inquire( file = trim(session_dir) // '/' // id(1:SESSION_ID_LENGTH), &
        exist = cgi_session_is_issued )

end function cgi_session_is_issued

! cgi_random_session_id --
!     Generate a hex-encoded 128-bit session ID from /dev/urandom
! Arguments:
!     id            Generated ID
!     ok            Whether the random bytes could be read
!
subroutine cgi_random_session_id( id, ok )
    character(len=SESSION_ID_LENGTH), intent(out) :: id
    logical, intent(out)                          :: ok

    character(len=SESSION_ID_BYTES) :: bytes
    integer                         :: lu
    integer                         :: ierr
    integer                         :: i
    integer                         :: b

    id = ' '
    ok = .false.

    lu = cgi_free_unit()
    if ( lu < 0 ) then
        return
    endif
    open( lu, file = '/dev/urandom', access = 'stream', form = 'unformatted', &
        action = 'read', status = 'old', iostat = ierr )
    if ( ierr /= 0 ) then
        return
    endif
    read( lu, iostat = ierr ) bytes
    close( lu )
    if ( ierr /= 0 ) then
        return
    endif

    do i = 1,SESSION_ID_BYTES
        b = iand( ichar(bytes(i:i)), 255 )
        id(2*i-1:2*i-1) = hex_digits(b/16+1:b/16+1)
        id(2*i:2*i)     = hex_digits(mod(b,16)+1:mod(b,16)+1)
    enddo
    ok = .true.

end subroutine cgi_random_session_id

! cgi_free_unit --
!     Return an unused LU-number between 10 and 99, or -1
!
integer function cgi_free_unit()

    integer :: lu
    logical :: opend

    cgi_free_unit = -1
    do lu = 10,99
        inquire( lu, opened = opend )
        if ( .not. opend ) then
            cgi_free_unit = lu
            exit
        endif
    enddo

end function cgi_free_unit

! cgi_get_* --
!     Get the value of variables
! Arguments:
!     dict          Dictionary with values
!     varname       Name of the variable to retrieve
!     value         Value of the variable
! Note:
!     If the variable does not exist, then the value
!     is not changed! (Use dict_has_key() to check the
!     existence)
!
subroutine cgi_get_string( dict, varname, value )
    type(DICT_STRUCT), pointer :: dict
    character(len=*)           :: varname
    character(len=*)           :: value

    type(DICT_DATA)            :: data

    if ( dict_has_key( dict, varname ) ) then
        data = dict_get_key( dict, varname )
        value = data%value
    endif

end subroutine cgi_get_string

subroutine cgi_get_integer( dict, varname, value )
    type(DICT_STRUCT), pointer :: dict
    character(len=*)           :: varname
    integer                    :: value

    type(DICT_DATA)            :: data
    integer                    :: ierr
    integer                    :: new_value

    if ( dict_has_key( dict, varname ) ) then
        data = dict_get_key( dict, varname )
        read( data%value, *, iostat=ierr ) new_value
        if ( ierr == 0 ) then
            value = new_value
        endif
    endif

end subroutine cgi_get_integer

subroutine cgi_get_real( dict, varname, value )
    type(DICT_STRUCT), pointer :: dict
    character(len=*)           :: varname
    real                       :: value

    type(DICT_DATA)            :: data
    integer                    :: ierr
    real                       :: new_value

    if ( dict_has_key( dict, varname ) ) then
        data = dict_get_key( dict, varname )
        read( data%value, *, iostat=ierr ) new_value
        if ( ierr == 0 ) then
            value = new_value
        endif
    endif

end subroutine cgi_get_real

subroutine cgi_get_logical( dict, varname, value )
    type(DICT_STRUCT), pointer :: dict
    character(len=*)           :: varname
    logical                    :: value

    type(DICT_DATA)            :: data
    integer                    :: ierr
    integer                    :: new_value

    if ( dict_has_key( dict, varname ) ) then
        data = dict_get_key( dict, varname )
        read( data%value, *, iostat=ierr ) new_value
        if ( ierr == 0 ) then
            value = (new_value == 1)
        endif
    endif

end subroutine cgi_get_logical

end module cgi_protocol
