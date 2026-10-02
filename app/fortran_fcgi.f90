!> Fortran FastCGI server.
!> Based on Fortran FastCGI by Ricolindo.Carino@gmail.com and arjen.markus895@gmail.com
!>
!> Requires the FLIBS modules cgi_protocol and fcgi_protocol and libfcgi.
!> See README.md for setup instructions.
program fortran_fcgi
  use fcgi_protocol, only: DICT_STRUCT, fcgip_accept_environment_variables, &
    fcgip_make_dictionary, fcgip_put_file
  use controller, only: respond

  implicit none

  type(DICT_STRUCT), pointer :: dict => null()
  logical :: stopped
  integer :: unit, status

  ! responses are buffered in a scratch file; for debugging use
  ! open(newunit=unit, file='fcgiout', status='replace') to keep %REMARK% lines
  open(newunit=unit, status='scratch')

  do while (fcgip_accept_environment_variables() >= 0)
    call fcgip_make_dictionary(dict, unit)
    call respond(dict, unit, stopped)
    call fcgip_put_file(unit, 'text/html')
    if (stopped) exit
  end do

  close(unit)

  ! the webserver reports an error once this process terminates
  status = fcgip_accept_environment_variables()
end program fortran_fcgi
