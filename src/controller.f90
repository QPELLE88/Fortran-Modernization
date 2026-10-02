!> Routes a FastCGI request to a page and writes the HTML response.
module controller
  use fcgi_protocol, only: DICT_STRUCT, cgi_get
  use jade, only: template_var, template_var_of, jadefile, jadetemplate
  use marsupial, only: marsupial_t, find_marsupial, list_marsupials

  implicit none
  private

  public :: respond

  integer, parameter :: URI_LEN = 80
  integer, parameter :: QUERY_LEN = 80

contains

  !> Writes the response for the request described by `dict` to `unit`.
  !> Lines starting with `%REMARK%` are debug output and are not sent.
  subroutine respond(dict, unit, stopped)
    type(DICT_STRUCT), pointer :: dict
    integer, intent(in) :: unit
    logical, intent(out) :: stopped

    character(len=URI_LEN) :: scriptName

    stopped = .false.

    write(unit, '(a)') &
      '%REMARK% respond() started ...', &
      '<!DOCTYPE html>', &
      '<html>', &
      '<head>', &
      '<meta charset="utf-8"/>', &
      '<meta name="viewport" content="width=device-width, initial-scale=1"/>', &
      '<title>FORTRAN.io</title>', &
      '<link rel="stylesheet" type="text/css" href="/static/bootstrap.min.css"/>', &
      '</head>', &
      '<body>'

    scriptName = '/'
    call cgi_get(dict, 'DOCUMENT_URI', scriptName)

    select case (trim(scriptName))
    case ('/')
      call jadefile('template/index.jade', unit)
    case ('/test')
      call jadefile('template/test.jade', unit)
    case ('/search')
      call search_page(dict, unit)
    case ('/all')
      call all_page(unit)
    case default
      write(unit, '(a)') 'Page not found!'
    end select

    write(unit, '(a)') &
      '</body>', &
      '</html>', &
      '%REMARK% respond() completed ...'
  end subroutine respond

  subroutine search_page(dict, unit)
    type(DICT_STRUCT), pointer :: dict
    integer, intent(in) :: unit

    character(len=QUERY_LEN) :: query
    type(marsupial_t) :: match
    logical :: found

    ! tags which contain multiple templates are written by the controller
    write(unit, '(a)') '<div class="container">'
    call jadefile('template/search.jade', unit)

    query = ''
    call cgi_get(dict, 'q', query)
    call find_marsupial(query, found, match)
    if (found) then
      call jadetemplate('template/result.jade', unit, marsupial_vars(match))
    else
      write(unit, '(a)') '<p>No results in this database :-(</p>'
    end if

    write(unit, '(a)') '</div>'
  end subroutine search_page

  subroutine all_page(unit)
    integer, intent(in) :: unit

    type(marsupial_t), allocatable :: rows(:)
    integer :: i

    write(unit, '(a)') '<div class="container">'
    call jadefile('template/search.jade', unit)

    call list_marsupials(rows)
    do i = 1, size(rows)
      call jadetemplate('template/result.jade', unit, marsupial_vars(rows(i)))
    end do

    write(unit, '(a)') '</div>'
  end subroutine all_page

  pure function marsupial_vars(m) result(vars)
    type(marsupial_t), intent(in) :: m
    type(template_var) :: vars(4)

    vars(1) = template_var_of('name', m%name)
    vars(2) = template_var_of('latinName', m%latin_name)
    vars(3) = template_var_of('wikiLink', m%wiki_link)
    vars(4) = template_var_of('description', m%description)
  end function marsupial_vars

end module controller
