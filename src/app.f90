!> Controller: maps a request to a response. Independent of FastCGI so it can be unit tested.
module app
  use strings, only: html_escape, starts_with
  use http, only: request_t, response_t, get_param, get_env, html_response, error_response
  use jade, only: template_var_t, tvar, load_template, render_jade, render_template_file
  use marsupials, only: marsupial_t, search_marsupials, all_marsupials
  implicit none
  private

  public :: app_config_t, config_from_env, handle_request, safe_wiki_path

  type :: app_config_t
    character(len=:), allocatable :: template_dir
    character(len=:), allocatable :: db_path
  end type app_config_t

  character(len=*), parameter :: WIKI_PATH_CHARS = &
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/_-.,()%'

contains

  !> FORTRAN_IO_TEMPLATE_DIR and FORTRAN_IO_DB override the defaults (relative to the cwd).
  function config_from_env() result(cfg)
    type(app_config_t) :: cfg

    cfg%template_dir = get_env('FORTRAN_IO_TEMPLATE_DIR')
    if (len(cfg%template_dir) == 0) cfg%template_dir = 'template'
    cfg%db_path = get_env('FORTRAN_IO_DB')
    if (len(cfg%db_path) == 0) cfg%db_path = 'marsupials.sqlite3'
  end function config_from_env

  function handle_request(req, cfg) result(resp)
    type(request_t), intent(in) :: req
    type(app_config_t), intent(in) :: cfg
    type(response_t) :: resp

    select case (req%path)
    case ('/')
      resp = static_page(cfg, 'index.jade')
    case ('/test')
      resp = static_page(cfg, 'test.jade')
    case ('/search')
      resp = search_page(cfg, get_param(req, 'q', ''))
    case ('/all')
      resp = all_page(cfg)
    case default
      resp = html_response(layout('<p>Page not found!</p>'), 404)
    end select
  end function handle_request

  function static_page(cfg, name) result(resp)
    type(app_config_t), intent(in) :: cfg
    character(len=*), intent(in) :: name
    type(response_t) :: resp
    character(len=:), allocatable :: html, errmsg
    type(template_var_t), allocatable :: no_vars(:)
    logical :: ok

    allocate(no_vars(0))
    call render_template_file(cfg%template_dir // '/' // name, no_vars, html, ok, errmsg)
    if (.not. ok) then
      resp = error_response(500, errmsg)
      return
    end if
    resp = html_response(layout(html))
  end function static_page

  function search_page(cfg, query) result(resp)
    type(app_config_t), intent(in) :: cfg
    character(len=*), intent(in) :: query
    type(response_t) :: resp
    type(marsupial_t), allocatable :: rows(:)
    character(len=:), allocatable :: errmsg, empty_message
    logical :: ok

    call search_marsupials(cfg%db_path, query, rows, ok, errmsg)
    if (.not. ok) then
      resp = error_response(500, errmsg)
      return
    end if
    if (len_trim(query) == 0) then
      empty_message = '<p>Type a marsupial name to search.</p>'
    else
      empty_message = '<p>No results for &ldquo;' // html_escape(trim(query)) // &
                      '&rdquo; in this database :-(</p>'
    end if
    resp = results_page(cfg, rows, empty_message)
  end function search_page

  function all_page(cfg) result(resp)
    type(app_config_t), intent(in) :: cfg
    type(response_t) :: resp
    type(marsupial_t), allocatable :: rows(:)
    character(len=:), allocatable :: errmsg
    logical :: ok

    call all_marsupials(cfg%db_path, rows, ok, errmsg)
    if (.not. ok) then
      resp = error_response(500, errmsg)
      return
    end if
    resp = results_page(cfg, rows, '<p>No results in this database :-(</p>')
  end function all_page

  function results_page(cfg, rows, empty_message) result(resp)
    type(app_config_t), intent(in) :: cfg
    type(marsupial_t), intent(in) :: rows(:)
    character(len=*), intent(in) :: empty_message
    type(response_t) :: resp
    character(len=:), allocatable :: header, row_template, html, errmsg
    type(template_var_t), allocatable :: no_vars(:)
    logical :: ok
    integer :: i

    allocate(no_vars(0))
    call render_template_file(cfg%template_dir // '/search.jade', no_vars, header, ok, errmsg)
    if (ok) call load_template(cfg%template_dir // '/result.jade', row_template, ok, errmsg)
    if (.not. ok) then
      resp = error_response(500, errmsg)
      return
    end if

    html = '<div class="container">' // new_line('a') // header
    if (size(rows) == 0) html = html // empty_message // new_line('a')
    do i = 1, size(rows)
      html = html // render_jade(row_template, [ &
        tvar('name', rows(i)%name), &
        tvar('latinName', rows(i)%latin_name), &
        tvar('wikiLink', safe_wiki_path(rows(i)%wiki_link)), &
        tvar('description', rows(i)%description)])
    end do
    html = html // '</div>'
    resp = html_response(layout(html))
  end function results_page

  !> Keep only /wiki/... article paths so a row cannot redirect the link off Wikipedia.
  pure function safe_wiki_path(link) result(path)
    character(len=*), intent(in) :: link
    character(len=:), allocatable :: path

    path = ''
    if (.not. starts_with(link, '/wiki/') .or. len(link) <= 6) return
    if (verify(link, WIKI_PATH_CHARS) /= 0 .or. index(link, '..') /= 0) return
    path = link
  end function safe_wiki_path

  pure function layout(content) result(page)
    character(len=*), intent(in) :: content
    character(len=:), allocatable :: page
    character(len=1), parameter :: NL = new_line('a')

    page = '<!DOCTYPE html>' // NL // &
           '<html lang="en">' // NL // &
           '<head>' // NL // &
           '<meta charset="utf-8"/>' // NL // &
           '<meta name="viewport" content="width=device-width, initial-scale=1"/>' // NL // &
           '<title>FORTRAN.io</title>' // NL // &
           '<link rel="stylesheet" type="text/css" href="/static/bootstrap.min.css"/>' // NL // &
           '</head>' // NL // &
           '<body>' // NL // &
           content // &
           '</body>' // NL // &
           '</html>'
  end function layout

end module app
