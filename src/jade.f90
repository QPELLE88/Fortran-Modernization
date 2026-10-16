!> A small Jade/Pug-style template renderer.
!>
!> Supported syntax (one element per line, nesting by indentation):
!>
!>     tag.class1.class2#id(attr="value", other='x') text content
!>     .class            -> <div class="class">
!>     #id               -> <div id="id">
!>     | raw text line
!>     #{key}            -> value of `key`, HTML-escaped
!>     !{key}            -> value of `key`, inserted verbatim (trusted HTML)
!>
!> Template text itself is trusted; only interpolated values are escaped.
module jade
  use string_helpers, only: key_value_t, html_escape, read_line, NEW_LINE_CHAR
  implicit none
  private

  public :: key_value_t
  public :: jade_render, jade_render_file, interpolate

  type :: open_tag_t
    integer :: indent
    character(len=:), allocatable :: name
  end type open_tag_t

  character(len=*), parameter :: VOID_ELEMENTS(*) = [character(len=6) :: &
    'area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'link', 'meta', &
    'source', 'track', 'wbr']

contains

  !> Render the template file at `path`, optionally interpolating `vars`.
  subroutine jade_render_file(path, html, ok, vars)
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: html
    logical, intent(out) :: ok
    type(key_value_t), intent(in), optional :: vars(:)
    character(len=:), allocatable :: source, line
    integer :: unit, ios

    html = ''
    open (newunit=unit, file=path, status='old', action='read', iostat=ios)
    ok = ios == 0
    if (.not. ok) return

    source = ''
    do
      call read_line(unit, line, ios)
      if (ios /= 0) exit
      source = source // line // NEW_LINE_CHAR
    end do
    close (unit)
    ok = is_iostat_end(ios)
    if (.not. ok) return

    if (present(vars)) then
      html = jade_render(source, vars)
    else
      html = jade_render(source)
    end if
  end subroutine jade_render_file

  !> Render newline-separated Jade `source` to HTML.
  function jade_render(source, vars) result(html)
    character(len=*), intent(in) :: source
    type(key_value_t), intent(in), optional :: vars(:)
    character(len=:), allocatable :: html
    type(open_tag_t), allocatable :: stack(:)
    character(len=:), allocatable :: line, tag, open_markup, content
    integer :: start, nl, indent
    logical :: first, is_text

    allocate (stack(0))
    html = ''
    first = .true.
    start = 1
    do while (start <= len(source))
      nl = index(source(start:), NEW_LINE_CHAR)
      if (nl == 0) then
        line = source(start:)
        start = len(source) + 1
      else
        line = source(start:start + nl - 2)
        start = start + nl
      end if
      line = expand_tabs(line)
      if (len_trim(line) == 0) cycle

      indent = verify(line, ' ') - 1
      line = line(indent + 1:len_trim(line))

      call close_tags(stack, indent, html)

      is_text = line(1:1) == '|'
      if (is_text) then
        content = adjustl(line(2:))
        open_markup = ''
        tag = ''
      else
        call parse_element(line, tag, open_markup, content)
      end if

      if (.not. first) html = html // NEW_LINE_CHAR
      first = .false.
      html = html // open_markup // trim(content)

      if (.not. is_text .and. .not. is_void(tag)) call push_tag(stack, indent, tag)
    end do
    call close_tags(stack, -1, html)

    if (present(vars)) html = interpolate(html, vars)
  end function jade_render

  !> Close every open element nested at `indent` or deeper.
  subroutine close_tags(stack, indent, html)
    type(open_tag_t), allocatable, intent(inout) :: stack(:)
    integer, intent(in) :: indent
    character(len=:), allocatable, intent(inout) :: html
    integer :: depth

    type(open_tag_t), allocatable :: kept(:)

    depth = size(stack)
    do while (depth > 0)
      if (stack(depth)%indent < indent) exit
      html = html // '</' // stack(depth)%name // '>'
      depth = depth - 1
    end do
    if (depth == size(stack)) return
    allocate (kept(depth))
    kept(:) = stack(1:depth)
    call move_alloc(kept, stack)
  end subroutine close_tags

  subroutine push_tag(stack, indent, name)
    type(open_tag_t), allocatable, intent(inout) :: stack(:)
    integer, intent(in) :: indent
    character(len=*), intent(in) :: name
    type(open_tag_t), allocatable :: grown(:)
    integer :: n

    n = size(stack)
    allocate (grown(n + 1))
    grown(1:n) = stack
    grown(n + 1)%indent = indent
    grown(n + 1)%name = name
    call move_alloc(grown, stack)
  end subroutine push_tag

  !> Parse "tag.cls#id(attrs) content" into the opening markup and content.
  subroutine parse_element(line, tag, open_markup, content)
    character(len=*), intent(in) :: line
    character(len=:), allocatable, intent(out) :: tag, open_markup, content
    character(len=:), allocatable :: classes, elem_id, attrs
    integer :: pos, n, stop_at

    n = len(line)
    pos = scan_name(line, 1)
    tag = line(1:pos - 1)
    if (len(tag) == 0) tag = 'div'

    classes = ''
    elem_id = ''
    attrs = ''
    do while (pos <= n)
      select case (line(pos:pos))
      case ('.')
        stop_at = scan_name(line, pos + 1)
        call append_class(classes, line(pos + 1:stop_at - 1))
        pos = stop_at
      case ('#')
        if (pos < n) then
          if (line(pos + 1:pos + 1) == '{') exit
        end if
        stop_at = scan_name(line, pos + 1)
        elem_id = line(pos + 1:stop_at - 1)
        pos = stop_at
      case ('(')
        stop_at = matching_paren(line, pos)
        call parse_attributes(line(pos + 1:stop_at - 1), classes, elem_id, attrs)
        pos = stop_at + 1
      case default
        exit
      end select
    end do

    if (pos <= n) then
      content = adjustl(line(pos:))
    else
      content = ''
    end if

    open_markup = '<' // tag
    if (len(elem_id) > 0) open_markup = open_markup // ' id="' // elem_id // '"'
    if (len(classes) > 0) open_markup = open_markup // ' class="' // classes // '"'
    open_markup = open_markup // attrs // '>'
  end subroutine parse_element

  !> Parse `key="value", key2='v' flag` attribute lists. Separators
  !> (commas/whitespace) between attributes are optional.
  subroutine parse_attributes(text, classes, elem_id, attrs)
    character(len=*), intent(in) :: text
    character(len=:), allocatable, intent(inout) :: classes, elem_id, attrs
    character(len=:), allocatable :: key, value
    character(len=1) :: quote
    integer :: pos, n, start, close_at
    logical :: has_value

    n = len(text)
    pos = 1
    do
      do while (pos <= n)
        if (index(' ,', text(pos:pos)) == 0) exit
        pos = pos + 1
      end do
      if (pos > n) exit

      start = pos
      do while (pos <= n)
        if (index(' ,=', text(pos:pos)) > 0) exit
        pos = pos + 1
      end do
      key = text(start:pos - 1)

      has_value = .false.
      value = ''
      if (pos <= n) then
        if (text(pos:pos) == '=') then
          has_value = .true.
          pos = pos + 1
          if (pos <= n) then
            if (text(pos:pos) == '"' .or. text(pos:pos) == "'") then
              quote = text(pos:pos)
              close_at = index(text(pos + 1:), quote)
              if (close_at == 0) close_at = n - pos + 1
              value = text(pos + 1:pos + close_at - 1)
              pos = pos + close_at + 1
            else
              start = pos
              do while (pos <= n)
                if (index(' ,', text(pos:pos)) > 0) exit
                pos = pos + 1
              end do
              value = text(start:pos - 1)
            end if
          end if
        end if
      end if

      if (key == 'class') then
        call append_class(classes, value)
      else if (key == 'id') then
        elem_id = value
      else if (has_value) then
        if (index(value, '"') > 0) then
          attrs = attrs // ' ' // key // "='" // value // "'"
        else
          attrs = attrs // ' ' // key // '="' // value // '"'
        end if
      else
        attrs = attrs // ' ' // key
      end if
    end do
  end subroutine parse_attributes

  !> Replace `#{key}` with the escaped value and `!{key}` with the raw
  !> value of `key`. Unknown keys render as empty strings. Substituted
  !> values are never re-scanned.
  pure function interpolate(text, vars) result(out)
    character(len=*), intent(in) :: text
    type(key_value_t), intent(in) :: vars(:)
    character(len=:), allocatable :: out
    integer :: pos, k, close_at, i
    logical :: escape
    character(len=:), allocatable :: key

    out = ''
    pos = 1
    do
      k = scan_marker(text(pos:))
      if (k == 0) exit
      k = pos + k - 1
      close_at = index(text(k + 2:), '}')
      if (close_at == 0) exit

      escape = text(k:k) == '#'
      key = text(k + 2:k + close_at)
      out = out // text(pos:k - 1)
      do i = 1, size(vars)
        if (vars(i)%key == key) then
          if (escape) then
            out = out // html_escape(vars(i)%value)
          else
            out = out // vars(i)%value
          end if
          exit
        end if
      end do
      pos = k + close_at + 2
    end do
    out = out // text(pos:)
  end function interpolate

  !> Position of the first "#{" or "!{" in `text`, or 0.
  pure integer function scan_marker(text) result(k)
    character(len=*), intent(in) :: text
    integer :: h, b

    h = index(text, '#{')
    b = index(text, '!{')
    if (h == 0) then
      k = b
    else if (b == 0) then
      k = h
    else
      k = min(h, b)
    end if
  end function scan_marker

  !> Index just past an identifier starting at `start`.
  pure integer function scan_name(line, start) result(pos)
    character(len=*), intent(in) :: line
    integer, intent(in) :: start

    pos = start
    do while (pos <= len(line))
      if (index('.#( ', line(pos:pos)) > 0) exit
      pos = pos + 1
    end do
  end function scan_name

  !> Index of the ')' matching the '(' at `open_at`, ignoring quoted text.
  pure integer function matching_paren(line, open_at) result(pos)
    character(len=*), intent(in) :: line
    integer, intent(in) :: open_at
    character(len=1) :: quote

    quote = ' '
    pos = open_at + 1
    do while (pos <= len(line))
      if (quote /= ' ') then
        if (line(pos:pos) == quote) quote = ' '
      else if (line(pos:pos) == '"' .or. line(pos:pos) == "'") then
        quote = line(pos:pos)
      else if (line(pos:pos) == ')') then
        return
      end if
      pos = pos + 1
    end do
  end function matching_paren

  pure subroutine append_class(classes, name)
    character(len=:), allocatable, intent(inout) :: classes
    character(len=*), intent(in) :: name

    if (len_trim(name) == 0) return
    if (len(classes) > 0) then
      classes = classes // ' ' // trim(adjustl(name))
    else
      classes = trim(adjustl(name))
    end if
  end subroutine append_class

  pure logical function is_void(tag)
    character(len=*), intent(in) :: tag

    is_void = any(VOID_ELEMENTS == tag)
  end function is_void

  pure function expand_tabs(line) result(out)
    character(len=*), intent(in) :: line
    character(len=:), allocatable :: out
    integer :: i

    out = line
    do i = 1, len(out)
      if (out(i:i) == achar(9)) out(i:i) = ' '
    end do
  end function expand_tabs

end module jade
