!> A small Jade/Pug-style template renderer.
!>
!> Each non-blank line is `tag#id.class(attrs) text`; indentation nests elements.
!> Template text is trusted markup; `#{key}` placeholders are replaced with HTML-escaped values.
module jade
  use strings, only: string_t, html_escape, LF, CR
  implicit none
  private

  public :: template_var_t, tvar, load_template, render_jade, render_template_file

  type :: template_var_t
    character(len=:), allocatable :: key
    character(len=:), allocatable :: value
  end type template_var_t

  character(len=*), parameter :: NAME_CHARS = &
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_:'

contains

  pure function tvar(key, value) result(v)
    character(len=*), intent(in) :: key, value
    type(template_var_t) :: v

    v%key = key
    v%value = value
  end function tvar

  subroutine load_template(path, source, ok, errmsg)
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: source
    logical, intent(out) :: ok
    character(len=:), allocatable, intent(out) :: errmsg
    integer :: unit, ios, nbytes

    source = ''
    errmsg = ''
    open (newunit=unit, file=path, access='stream', form='unformatted', &
          status='old', action='read', iostat=ios)
    ok = ios == 0
    if (.not. ok) then
      errmsg = 'cannot open template ' // path
      return
    end if
    inquire (unit=unit, size=nbytes)
    if (nbytes > 0) then
      deallocate(source)
      allocate(character(len=nbytes) :: source)
      read (unit, iostat=ios) source
      ok = ios == 0
      if (.not. ok) errmsg = 'cannot read template ' // path
    end if
    close (unit)
  end subroutine load_template

  subroutine render_template_file(path, vars, html, ok, errmsg)
    character(len=*), intent(in) :: path
    type(template_var_t), intent(in) :: vars(:)
    character(len=:), allocatable, intent(out) :: html
    logical, intent(out) :: ok
    character(len=:), allocatable, intent(out) :: errmsg
    character(len=:), allocatable :: source

    html = ''
    call load_template(path, source, ok, errmsg)
    if (ok) html = render_jade(source, vars)
  end subroutine render_template_file

  function render_jade(source, vars) result(html)
    character(len=*), intent(in) :: source
    type(template_var_t), intent(in) :: vars(:)
    character(len=:), allocatable :: html
    character(len=:), allocatable :: line, content, tag, id, classes, attrs, text, open_tag
    type(string_t), allocatable :: open_tags(:)
    integer, allocatable :: indents(:)
    logical, allocatable :: has_children(:)
    integer :: pos, nl, indent, depth

    html = ''
    open_tag = ''
    depth = 0
    allocate(open_tags(0), indents(0), has_children(0))
    pos = 1
    do while (pos <= len(source))
      nl = index(source(pos:), LF)
      if (nl == 0) then
        line = source(pos:)
        pos = len(source) + 1
      else
        line = source(pos:pos+nl-2)
        pos = pos + nl
      end if
      if (len(line) > 0) then
        if (line(len(line):) == CR) line = line(:len(line)-1)
      end if
      if (verify(line, ' ' // achar(9)) == 0) cycle

      indent = verify(line, ' ' // achar(9)) - 1
      content = trim(line(indent+1:))

      do while (depth > 0)
        if (indents(depth) < indent) exit
        call close_top()
      end do
      if (depth > 0) has_children(depth) = .true.

      call parse_line(content, tag, id, classes, attrs, text)
      if (tag == '|') then
        html = html // pad(depth) // interpolate(text, vars) // LF
        cycle
      end if

      open_tag = '<' // tag
      if (len(id) > 0) open_tag = open_tag // ' id="' // id // '"'
      if (len(classes) > 0) open_tag = open_tag // ' class="' // classes // '"'
      if (len(attrs) > 0) open_tag = open_tag // ' ' // interpolate(attrs, vars)
      html = html // pad(depth) // open_tag // '>' // interpolate(text, vars) // LF

      if (.not. is_void(tag)) then
        open_tags = [open_tags, string_t(tag)]
        indents = [indents, indent]
        has_children = [has_children, .false.]
        depth = depth + 1
      end if
    end do

    do while (depth > 0)
      call close_top()
    end do

  contains

    subroutine close_top()
      if (has_children(depth)) then
        html = html // pad(depth - 1) // '</' // open_tags(depth)%s // '>' // LF
      else
        html = html(:len(html)-1) // '</' // open_tags(depth)%s // '>' // LF
      end if
      depth = depth - 1
      open_tags = open_tags(:depth)
      indents = indents(:depth)
      has_children = has_children(:depth)
    end subroutine close_top

  end function render_jade

  !> Split `tag#id.cls1.cls2(attrs) text` into its parts. Missing tag names default to div.
  pure subroutine parse_line(content, tag, id, classes, attrs, text)
    character(len=*), intent(in) :: content
    character(len=:), allocatable, intent(out) :: tag, id, classes, attrs, text
    integer :: i, j, n

    id = ''
    classes = ''
    attrs = ''
    text = ''
    n = len(content)

    if (content(1:1) == '|') then
      tag = '|'
      if (n > 1) text = content(2:)
      if (len(text) > 0) then
        if (text(1:1) == ' ') text = text(2:)
      end if
      return
    end if

    j = name_end(content, 1)
    if (j > 1) then
      tag = content(:j-1)
    else
      tag = 'div'
    end if

    i = j
    do while (i <= n)
      select case (content(i:i))
      case ('.', '#')
        j = name_end(content, i + 1)
        if (content(i:i) == '#') then
          id = content(i+1:j-1)
        else if (len(classes) == 0) then
          classes = content(i+1:j-1)
        else
          classes = classes // ' ' // content(i+1:j-1)
        end if
        i = j
      case ('(')
        j = closing_paren(content, i)
        attrs = normalize_attrs(content(i+1:j-1))
        i = j + 1
      case default
        exit
      end select
    end do

    if (i <= n) then
      text = content(i:)
      if (text(1:1) == ' ') text = text(2:)
    end if
  end subroutine parse_line

  !> Index just past the run of name characters starting at `start`.
  pure integer function name_end(s, start) result(j)
    character(len=*), intent(in) :: s
    integer, intent(in) :: start

    j = start
    do while (j <= len(s))
      if (index(NAME_CHARS, s(j:j)) == 0) exit
      j = j + 1
    end do
  end function name_end

  !> Index of the ')' matching the '(' at `open`, ignoring parens inside quotes.
  pure integer function closing_paren(s, open) result(j)
    character(len=*), intent(in) :: s
    integer, intent(in) :: open
    character(len=1) :: quote

    quote = ' '
    do j = open + 1, len(s)
      if (quote /= ' ') then
        if (s(j:j) == quote) quote = ' '
      else if (s(j:j) == '"' .or. s(j:j) == "'") then
        quote = s(j:j)
      else if (s(j:j) == ')') then
        return
      end if
    end do
    j = len(s) + 1
  end function closing_paren

  !> Turn `a="1", b="2"c="3"` into `a="1" b="2" c="3"`.
  pure function normalize_attrs(raw) result(out)
    character(len=*), intent(in) :: raw
    character(len=:), allocatable :: out
    character(len=1) :: quote, c
    integer :: i

    out = ''
    quote = ' '
    do i = 1, len(raw)
      c = raw(i:i)
      if (quote /= ' ') then
        out = out // c
        if (c == quote) then
          quote = ' '
          if (i < len(raw)) then
            if (index(' ,', raw(i+1:i+1)) == 0) out = out // ' '
          end if
        end if
      else if (c == '"' .or. c == "'") then
        quote = c
        out = out // c
      else if (c == ',' .or. c == ' ') then
        if (len(out) > 0) then
          if (out(len(out):) /= ' ') out = out // ' '
        end if
      else
        out = out // c
      end if
    end do
    out = trim(out)
  end function normalize_attrs

  !> Replace #{key} with the escaped value of key; unknown keys render as ''.
  pure function interpolate(s, vars) result(r)
    character(len=*), intent(in) :: s
    type(template_var_t), intent(in) :: vars(:)
    character(len=:), allocatable :: r
    integer :: p, k, e, i
    character(len=:), allocatable :: key

    r = ''
    p = 1
    do
      k = index(s(p:), '#{')
      if (k == 0) exit
      e = index(s(p+k+1:), '}')
      if (e == 0) exit
      key = trim(adjustl(s(p+k+1:p+k+e-1)))
      r = r // s(p:p+k-2)
      do i = 1, size(vars)
        if (vars(i)%key == key) then
          r = r // html_escape(vars(i)%value)
          exit
        end if
      end do
      p = p + k + e + 1
    end do
    r = r // s(p:)
  end function interpolate

  pure logical function is_void(tag)
    character(len=*), intent(in) :: tag

    select case (tag)
    case ('area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'link', 'meta', &
          'source', 'track', 'wbr')
      is_void = .true.
    case default
      is_void = .false.
    end select
  end function is_void

  pure function pad(depth) result(s)
    integer, intent(in) :: depth
    character(len=:), allocatable :: s

    s = repeat('  ', max(depth, 0))
  end function pad

end module jade
