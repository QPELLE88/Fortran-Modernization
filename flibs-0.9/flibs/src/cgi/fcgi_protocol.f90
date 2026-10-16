!
! fcgi_protocol.f90 --
!
!     By Ricolindo.Carino@gmail.com
!
!     Module for interfacing a Fortran program with a web server (nginx) via the FastCGI protocol
!
!     Requires the cgi_protocol module from FLIBS
!
!     Provides the following:
!
!         function fcgip_accept_environment_variables(), bound to libfcgi/FCGI_Accept()
!            - waits for FCGI environment variables from the webserver
!
!         function fcgip_get_char(), bound to libfcgi/FCGI_getchar()
!            - reads a character code from POSTed data (negative at EOF)
!
!         function fcgip_put_string(), bound to libfcgi/FCGI_puts()
!            - copies a line of text to the webserver
!
!         subroutine fcgip_make_dictionary()
!            - breaks up QUERY_STRING or POSTed data into a dictionary
!
!         subroutine fcgip_put_file ()
!            - copies a file line by line to the webserver
!
!         function fcgip_request_rejected()
!            - true if fcgip_make_dictionary() refused the request (e.g. POST body too large);
!              fcgip_put_file() then sends the error status instead of the file
!
!     User must supply the routine to interpret the dictionary and compose a
!         response to QUERY_STRING or POSTed data
!

module fcgi_protocol

    use cgi_protocol

    implicit none

    ! FCGI library (libfcgi) routines
    interface

        ! The function to wait for FastCGI environment variables from the webserver
        function fcgip_accept_environment_variables () bind(C, NAME='FCGI_Accept')
            use ISO_C_BINDING
            implicit none
            integer(C_INT) :: fcgip_accept_environment_variables
        end function fcgip_accept_environment_variables

        ! The function to retrieve a character from POSTed data (returns EOF, i.e. < 0, at end of data)
        function fcgip_get_char () bind(C, NAME='FCGI_getchar')
            use ISO_C_BINDING
            implicit none
            integer(C_INT) :: fcgip_get_char
        end function fcgip_get_char

        ! The function to copy a null-terminated string to the webserver
        function fcgip_put_string (s) bind(C, NAME='FCGI_puts')
            use ISO_C_BINDING
            implicit none
            integer(C_INT) :: fcgip_put_string
            character(C_CHAR), dimension(*) :: s
        end function fcgip_put_string

    end interface

    ! public abbreviations
    character(len=3), parameter                :: AFORMAT = '(a)'
    character(len=2), parameter                :: CRLF = achar(13)//achar(10)
    character(len=1), parameter                :: NUL = achar(0)

    ! private objects
    ! MAX_CONTENT_LENGTH must be enough for "long" QUERY_STRING or POSTed content
    integer, parameter, private                :: MAX_CONTENT_LENGTH = 1024
    character(len=MAX_CONTENT_LENGTH), private :: content
    integer, private                           :: iStat
    ! HTTP status of a request refused by fcgip_make_dictionary(), blank if accepted
    character(len=40), private                 :: rejectStatus = ' '

contains

    subroutine fcgip_make_dictionary( dict, unitNo )
        ! Retrieve FastCGI environment variables into dictionary 'dict'
        ! Invoked after FCGI_Accept()/fcgip_accept_environment_variable() has completed
        ! Write debugging information to file unit number 'unitNo', which must already be open
        ! Debugging information should begin with %REMARK%, so as not to be sent
        !     to the webserver, see fcgi_put_file()

        type(DICT_STRUCT), pointer        :: dict
        integer, intent(in)               :: unitNo

        integer                           :: i
        integer                           :: iLen
        integer                           :: iChar
        integer                           :: nRead

        ! write to the beginning of file unitNo
        rewind (unitNo)
        rejectStatus = ' '

        ! Clean up dictionary ?
        if ( associated(dict) ) then
            call dict_destroy( dict )
            write(unitNo, AFORMAT) '%REMARK% cleaned dictionary...'
        else
            write(unitNo, AFORMAT) '%REMARK% dictionary NOT associated()...'
        endif

        ! add the requested script ('/' if none) to dictionary
        call get_environment_variable('DOCUMENT_URI', content)
        iLen = len_trim(content)
        if ( iLen > 0 ) then
            content = 'DOCUMENT_URI='//content
        else ! default is /, to ensure dictionary is not empty
            content = 'DOCUMENT_URI=/'
        endif
        iLen = len_trim(content)
        call cgi_store_dict( dict, content(:iLen) )
        write(unitNo, AFORMAT) '%REMARK% added to dictionary: '//content(:iLen)

        ! QUERY_STRING (request method was GET) ?
        call get_environment_variable( "QUERY_STRING", value=content, length=iLen, status=iStat )
        if ( iStat == 0 ) then
            write(unitNo, AFORMAT) '%REMARK% QUERY_STRING='//trim(content)
            if ( iLen > 0 ) then
                    call cgi_store_dict( dict, content(:iLen) )
                    write(unitNo, AFORMAT) '%REMARK% added to dictionary: QUERY_STRING='//content(:iLen)
            end if
        endif

        ! anything in CONTENT_LENGTH (request method was POST) ?
        ! The body length is client-controlled: it must be a plain non-negative integer
        ! no larger than MAX_CONTENT_LENGTH, or the body would not fit in 'content'
        call get_environment_variable( "CONTENT_LENGTH", value=content, status=iStat )
        if ( iStat == -1 ) then
            call fcgip_reject_request( unitNo, '413 Payload Too Large' )
        else if ( iStat == 0 .and. len_trim(content) > 0 ) then
            write(unitNo, AFORMAT) '%REMARK% CONTENT_LENGTH='//trim(content)
            content = adjustl(content)
            if ( verify(trim(content), '0123456789') /= 0 ) then
                call fcgip_reject_request( unitNo, '400 Bad Request' )
            else
                read( content, *, iostat=iStat ) iLen
                if ( iStat /= 0 ) then ! only digits, so the value overflowed
                    call fcgip_reject_request( unitNo, '413 Payload Too Large' )
                else if ( iLen > MAX_CONTENT_LENGTH ) then
                    call fcgip_reject_request( unitNo, '413 Payload Too Large' )
                else
                    nRead = 0
                    do i=1,iLen
                        iChar = fcgip_get_char()
                        if ( iChar < 0 ) exit ! EOF before CONTENT_LENGTH bytes
                        content( i:i ) = achar(iChar)
                        nRead = i
                    end do
                    content( nRead+1: ) = ' '
                    if ( nRead > 0 ) then
                        call cgi_store_dict( dict, content(:nRead) )
                        write(unitNo, AFORMAT) '%REMARK% added to dictionary: CONTENT='//content(:nRead)
                    end if
                end if
            end if
        endif

        ! for other environment variables, see <nginx directory>/conf/fastcgi_params

        write(unitNo, AFORMAT) '%REMARK% completed dictionary...'

    end subroutine fcgip_make_dictionary


    subroutine fcgip_reject_request( unitNo, status )
        ! Mark the current request as refused with HTTP status 'status'
        integer, intent(in)               :: unitNo
        character(len=*), intent(in)      :: status

        rejectStatus = status
        write(unitNo, AFORMAT) '%REMARK% rejected request: '//trim(status)

    end subroutine fcgip_reject_request


    logical function fcgip_request_rejected()
        ! True if fcgip_make_dictionary() refused the current request
        fcgip_request_rejected = rejectStatus /= ' '
    end function fcgip_request_rejected



    subroutine fcgip_put_file ( unitNo, mimetype )
        ! Copy file 'unitNo' line by line to the webserver via FCGI_puts()
        !     except for lines beginning with %REMARK%
        ! File must already exist, expected to contain the response to some query

        integer, intent(in)                    :: unitNo
        character(len=*), intent(in), optional :: mimetype

        character(len=80)                      :: mimetype_

        mimetype_ = 'text/html'
        if ( present(mimetype) ) then
            mimetype_ = mimetype
        endif

        ! flush any pending writes
        flush(unitNo)

        ! refused request: send only the error status
        if ( rejectStatus /= ' ' ) then
            iStat = fcgip_put_string ('Status: '//trim(rejectStatus)//CRLF// &
                'Content-type: text/plain'//CRLF//NUL)
            iStat = fcgip_put_string (trim(rejectStatus)//NUL)
            return
        endif

        ! let the server know what type of data
        iStat = fcgip_put_string ('Content-type: '//trim(mimetype_)//CRLF//NUL)

        ! copy line by line to webserver, except those starting with %REMARK%
        rewind(unitNo)
        do while (.true.)
            read(unitNo, AFORMAT, iostat=iStat) content
            if (iStat < 0) exit ! no more lines
            if (content(:8) == '%REMARK%') cycle
            iStat = fcgip_put_string (trim(content)//NUL) ! FCGI_puts expects NULL terminated strings
        end do

    end subroutine fcgip_put_file


end module fcgi_protocol
