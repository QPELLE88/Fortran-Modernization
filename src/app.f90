!> Fortran.io controller: maps request paths to views.
module fortran_io_app
  use http, only: request_t, response_t, get_param
  use jade, only: key_value_t, jade_render_file
  use marsupial, only: marsupial_t, find_marsupial, list_marsupials
  use string_helpers, only: html_escape
  implicit none
  private

  public :: handle_request

  character(len=*), parameter :: TEMPLATE_DIR = 'template/'
  integer, parameter :: MAX_LISTED = 50

contains

  subroutine handle_request(req, res)
    type(request_t), intent(in) :: req
    type(response_t), intent(out) :: res
    character(len=:), allocatable :: path, content
    logical :: ok

    res%status = 200
    res%content_type = 'text/html; charset=utf-8'

    path = '/'
    if (allocated(req%path)) then
      if (len(req%path) > 0) path = req%path
    end if

    select case (path)
    case ('/')
      call jade_render_file(TEMPLATE_DIR // 'index.jade', content, ok)
    case ('/test')
      call jade_render_file(TEMPLATE_DIR // 'test.jade', content, ok)
    case ('/search')
      call search_page(get_param(req, 'q'), content, ok)
    case ('/all')
      call all_page(content, ok)
    case ('/healthz')
      res%content_type = 'text/plain; charset=utf-8'
      res%body = 'ok'
      return
    case default
      res%status = 404
      content = '<div class="container"><h1>Page not found!</h1>' // &
                '<p><a href="/">Back to FORTRAN.io</a></p></div>'
      ok = .true.
    end select

    if (.not. ok) then
      res%status = 500
      content = '<div class="container"><h1>Internal server error</h1></div>'
    end if

    res%body = layout(content)
  end subroutine handle_request

  subroutine search_page(query, content, ok)
    character(len=*), intent(in) :: query
    character(len=:), allocatable, intent(out) :: content
    logical, intent(out) :: ok
    character(len=:), allocatable :: header, result
    type(marsupial_t) :: item
    logical :: found

    call jade_render_file(TEMPLATE_DIR // 'search.jade', header, ok)
    if (.not. ok) return

    call find_marsupial(query, found, item, ok)
    if (.not. ok) return

    if (found) then
      call render_result(item, result, ok)
      if (.not. ok) return
    else
      result = '<p>No results for &quot;' // html_escape(query) // &
               '&quot; in this database :-(</p>'
    end if

    content = '<div class="container">' // header // result // '</div>'
  end subroutine search_page

  subroutine all_page(content, ok)
    character(len=:), allocatable, intent(out) :: content
    logical, intent(out) :: ok
    character(len=:), allocatable :: header, result
    type(marsupial_t), allocatable :: items(:)
    integer :: i

    call jade_render_file(TEMPLATE_DIR // 'search.jade', header, ok)
    if (.not. ok) return

    call list_marsupials(items, ok, limit=MAX_LISTED)
    if (.not. ok) return

    content = '<div class="container">' // header
    do i = 1, size(items)
      call render_result(items(i), result, ok)
      if (.not. ok) return
      content = content // result
    end do
    content = content // '</div>'
  end subroutine all_page

  subroutine render_result(item, html, ok)
    type(marsupial_t), intent(in) :: item
    character(len=:), allocatable, intent(out) :: html
    logical, intent(out) :: ok
    type(key_value_t) :: vars(4)

    vars(1)%key = 'name'
    vars(1)%value = item%name
    vars(2)%key = 'latinName'
    vars(2)%value = item%latin_name
    vars(3)%key = 'wikiLink'
    vars(3)%value = item%wiki_link
    vars(4)%key = 'description'
    vars(4)%value = item%description

    call jade_render_file(TEMPLATE_DIR // 'result.jade', html, ok, vars=vars)
  end subroutine render_result

  pure function layout(content) result(html)
    character(len=*), intent(in) :: content
    character(len=:), allocatable :: html

    html = '<!DOCTYPE html>' // new_line('a') // &
           '<html lang="en">' // new_line('a') // &
           '<head>' // new_line('a') // &
           '<meta charset="utf-8"/>' // new_line('a') // &
           '<meta name="viewport" content="width=device-width, initial-scale=1"/>' // new_line('a') // &
           '<title>FORTRAN.io</title>' // new_line('a') // &
           '<link rel="stylesheet" type="text/css" href="/static/bootstrap.min.css"/>' // new_line('a') // &
           '</head>' // new_line('a') // &
           '<body>' // new_line('a') // &
           content // new_line('a') // &
           '</body>' // new_line('a') // &
           '</html>'
  end function layout

end module fortran_io_app
