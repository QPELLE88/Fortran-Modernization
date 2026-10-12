!> Controller: routes a request to a page and renders it.
!> Has no FastCGI dependency, so it can be unit tested directly.
module app
  use sqlite_db, only: database
  use kv_list, only: string_map, parse_urlencoded
  use strings, only: string_buffer, html_escape, int_to_str, starts_with, is_blank, to_lower
  use jade, only: load_template, render_source
  use marsupials, only: marsupial, find_marsupials, list_marsupials
  implicit none
  private

  public :: app_context, http_request, http_response, handle_request, response_headers

  character(len=2), parameter :: CRLF = achar(13) // achar(10)

  type :: app_context
    type(database) :: db
    character(len=:), allocatable :: template_dir
  end type app_context

  type :: http_request
    character(len=:), allocatable :: method
    character(len=:), allocatable :: path
    character(len=:), allocatable :: query_string
    character(len=:), allocatable :: content_type
    character(len=:), allocatable :: body
  end type http_request

  type :: http_response
    integer :: status = 200
    character(len=:), allocatable :: content_type
    character(len=:), allocatable :: extra_headers
    character(len=:), allocatable :: body
    !> Diagnostic for the server error log; never shown to the client.
    character(len=:), allocatable :: log
  end type http_response

contains

  function handle_request(ctx, req) result(res)
    type(app_context), intent(inout) :: ctx
    type(http_request), intent(in) :: req
    type(http_response) :: res
    type(string_map) :: params
    type(string_buffer) :: page
    logical :: ok

    res%content_type = 'text/html; charset=utf-8'
    res%extra_headers = ''

    select case (req%method)
    case ('GET', 'HEAD', 'POST')
      continue
    case default
      res%status = 405
      res%extra_headers = 'Allow: GET, HEAD, POST' // CRLF
      res%body = error_page(res%status)
      return
    end select

    if (req%method == 'POST' .and. &
        starts_with(to_lower(req%content_type), 'application/x-www-form-urlencoded')) then
      call parse_urlencoded(req%body, params)
    end if
    call parse_urlencoded(req%query_string, params)

    call page%append(page_head())
    select case (req%path)
    case ('/')
      call append_template(ctx, page, 'index.jade', ok, res)
    case ('/test')
      call append_template(ctx, page, 'test.jade', ok, res)
    case ('/search')
      call search_page(ctx, page, params%get('q'), ok, res)
    case ('/all')
      call all_page(ctx, page, ok, res)
    case default
      res%status = 404
      res%body = error_page(res%status)
      return
    end select

    if (.not. ok) then
      res%status = 500
      res%body = error_page(res%status)
      return
    end if
    call page%append(page_tail())
    res%body = page%to_string()
  end function handle_request

  subroutine search_page(ctx, page, query, ok, res)
    type(app_context), intent(inout) :: ctx
    type(string_buffer), intent(inout) :: page
    character(len=*), intent(in) :: query
    logical, intent(out) :: ok
    type(http_response), intent(inout) :: res
    type(marsupial), allocatable :: found(:)
    type(string_map) :: vars

    call page%append('<div class="container">' // achar(10))
    call vars%set('q', query)
    call append_template(ctx, page, 'search.jade', ok, res, vars)
    if (.not. ok) return

    if (is_blank(query)) then
      call page%append('<p>Type a marsupial name to search.</p>' // achar(10))
    else
      call find_marsupials(ctx%db, trim(adjustl(query)), found, ok)
      if (.not. ok) then
        res%log = 'search query failed: ' // ctx%db%errmsg()
        return
      end if
      call append_results(ctx, page, found, query, ok, res)
      if (.not. ok) return
    end if
    call page%append('</div>' // achar(10))
  end subroutine search_page

  subroutine all_page(ctx, page, ok, res)
    type(app_context), intent(inout) :: ctx
    type(string_buffer), intent(inout) :: page
    logical, intent(out) :: ok
    type(http_response), intent(inout) :: res
    type(marsupial), allocatable :: found(:)
    type(string_map) :: vars

    call page%append('<div class="container">' // achar(10))
    call vars%set('q', '')
    call append_template(ctx, page, 'search.jade', ok, res, vars)
    if (.not. ok) return

    call list_marsupials(ctx%db, found, ok)
    if (.not. ok) then
      res%log = 'list query failed: ' // ctx%db%errmsg()
      return
    end if
    call append_results(ctx, page, found, '', ok, res)
    if (.not. ok) return
    call page%append('</div>' // achar(10))
  end subroutine all_page

  subroutine append_results(ctx, page, found, query, ok, res)
    type(app_context), intent(inout) :: ctx
    type(string_buffer), intent(inout) :: page
    type(marsupial), intent(in) :: found(:)
    character(len=*), intent(in) :: query
    logical, intent(out) :: ok
    type(http_response), intent(inout) :: res
    character(len=:), allocatable :: source
    type(string_map) :: vars
    integer :: i

    ok = .true.
    if (size(found) == 0) then
      if (is_blank(query)) then
        call page%append('<p>No results in this database :-(</p>' // achar(10))
      else
        call page%append('<p>No marsupials match &ldquo;' // html_escape(query) // &
                         '&rdquo; in this database :-(</p>' // achar(10))
      end if
      return
    end if

    call load_template(ctx%template_dir // '/result.jade', source, ok)
    if (.not. ok) then
      res%log = 'cannot read template ' // ctx%template_dir // '/result.jade'
      return
    end if
    do i = 1, size(found)
      call vars%set('name', found(i)%name)
      call vars%set('latinName', found(i)%latin_name)
      call vars%set('wikiLink', found(i)%wiki_link)
      call vars%set('description', found(i)%description)
      call page%append(render_source(source, vars))
    end do
  end subroutine append_results

  subroutine append_template(ctx, page, name, ok, res, vars)
    type(app_context), intent(in) :: ctx
    type(string_buffer), intent(inout) :: page
    character(len=*), intent(in) :: name
    logical, intent(out) :: ok
    type(http_response), intent(inout) :: res
    type(string_map), intent(in), optional :: vars
    type(string_map) :: no_vars
    character(len=:), allocatable :: source

    call load_template(ctx%template_dir // '/' // name, source, ok)
    if (.not. ok) then
      res%log = 'cannot read template ' // ctx%template_dir // '/' // name
      return
    end if
    if (present(vars)) then
      call page%append(render_source(source, vars))
    else
      call page%append(render_source(source, no_vars))
    end if
  end subroutine append_template

  function page_head() result(html)
    character(len=:), allocatable :: html

    html = '<!DOCTYPE html>' // achar(10) // &
           '<html lang="en">' // achar(10) // &
           '<head>' // achar(10) // &
           '<meta charset="utf-8">' // achar(10) // &
           '<meta name="viewport" content="width=device-width, initial-scale=1">' // achar(10) // &
           '<title>FORTRAN.io</title>' // achar(10) // &
           '<link rel="stylesheet" type="text/css" href="/static/bootstrap.min.css">' // achar(10) // &
           '</head>' // achar(10) // &
           '<body>' // achar(10)
  end function page_head

  function page_tail() result(html)
    character(len=:), allocatable :: html

    html = '</body>' // achar(10) // '</html>' // achar(10)
  end function page_tail

  function error_page(status) result(html)
    integer, intent(in) :: status
    character(len=:), allocatable :: html

    html = page_head() // '<div class="container">' // achar(10) // &
           '<h1>' // int_to_str(status) // ' ' // reason_phrase(status) // '</h1>' // achar(10)
    if (status == 404) html = html // '<p>Page not found!</p>' // achar(10)
    html = html // '<p><a href="/">Back to FORTRAN.io</a></p>' // achar(10) // &
           '</div>' // achar(10) // page_tail()
  end function error_page

  function reason_phrase(status) result(phrase)
    integer, intent(in) :: status
    character(len=:), allocatable :: phrase

    select case (status)
    case (200)
      phrase = 'OK'
    case (404)
      phrase = 'Not Found'
    case (405)
      phrase = 'Method Not Allowed'
    case (413)
      phrase = 'Payload Too Large'
    case default
      phrase = 'Internal Server Error'
    end select
  end function reason_phrase

  !> CGI response header block for `res` (terminated by a blank line).
  function response_headers(res) result(headers)
    type(http_response), intent(in) :: res
    character(len=:), allocatable :: headers

    headers = 'Status: ' // int_to_str(res%status) // ' ' // reason_phrase(res%status) // CRLF // &
              'Content-Type: ' // res%content_type // CRLF // &
              'Content-Length: ' // int_to_str(len(res%body)) // CRLF // &
              'X-Content-Type-Options: nosniff' // CRLF // &
              res%extra_headers // CRLF
  end function response_headers

end module app
