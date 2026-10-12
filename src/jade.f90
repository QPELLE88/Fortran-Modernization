!> Jade/Pug-style indentation templates rendered to HTML.
!>
!> Each non-blank line is either an element, written as
!>   tag.class1.class2#id(attr="value", other='value') inner text
!> (the tag defaults to `div`), or a text line starting with `|`.
!> Children are indented under their parent. `#{name}` inside text or attribute values
!> is replaced with the HTML-escaped value of template variable `name`; literal template
!> text is trusted and emitted as written.
module jade
  use strings, only: string_buffer, html_escape, strip_trailing, is_blank
  use kv_list, only: string_map
  implicit none
  private

  public :: load_template, render_source, render_template

  character(len=1), parameter :: LF = achar(10)

  type :: open_element
    character(len=:), allocatable :: tag
    integer :: indent = 0
  end type open_element

  type :: html_writer
    type(string_buffer) :: out
    type(open_element), allocatable :: stack(:)
    integer :: depth = 0
    logical :: line_open = .false.
  end type html_writer

contains

  !> Read a whole template file into `source`.
  subroutine load_template(path, source, ok)
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: source
    logical, intent(out) :: ok
    integer :: unit, ios, nbytes

    ok = .false.
    source = ''
    open(newunit=unit, file=path, access='stream', form='unformatted', status='old', &
         action='read', iostat=ios)
    if (ios /= 0) return
    inquire(unit=unit, size=nbytes)
    if (nbytes > 0) then
      deallocate(source)
      allocate(character(len=nbytes) :: source)
      read(unit, iostat=ios) source
    end if
    close(unit)
    ok = ios == 0
  end subroutine load_template

  !> Load and render the template at `path`.
  subroutine render_template(path, vars, html, ok)
    character(len=*), intent(in) :: path
    type(string_map), intent(in) :: vars
    character(len=:), allocatable, intent(out) :: html
    logical, intent(out) :: ok
    character(len=:), allocatable :: source

    call load_template(path, source, ok)
    if (ok) then
      html = render_source(source, vars)
    else
      html = ''
    end if
  end subroutine render_template

  !> Render template text (LF or CRLF line endings) to HTML.
  function render_source(source, vars) result(html)
    character(len=*), intent(in) :: source
    type(string_map), intent(in) :: vars
    character(len=:), allocatable :: html
    type(html_writer) :: w
    character(len=:), allocatable :: line
    integer :: start, nl, indent

    allocate(w%stack(16))
    call w%out%append('')
    start = 1
    do while (start <= len(source))
      nl = index(source(start:), LF)
      if (nl == 0) then
        line = strip_trailing(source(start:))
        start = len(source) + 1
      else
        line = strip_trailing(source(start:start + nl - 2))
        start = start + nl
      end if
      if (is_blank(line)) cycle

      indent = verify(line, ' ' // achar(9)) - 1
      call close_to(w, indent)
      call render_line(w, line(indent + 1:), indent, vars)
    end do
    call close_to(w, -1)
    html = w%out%to_string()
  end function render_source

  subroutine render_line(w, content, indent, vars)
    type(html_writer), intent(inout) :: w
    character(len=*), intent(in) :: content
    integer, intent(in) :: indent
    type(string_map), intent(in) :: vars
    character(len=:), allocatable :: tag, elem_id, classes, text, markup
    type(string_map) :: attrs
    logical :: is_text
    integer :: i

    call parse_line(content, tag, elem_id, classes, attrs, text, is_text)
    if (is_text) then
      call write_text(w, interpolate(text, vars, .false.))
      return
    end if

    markup = '<' // tag
    if (len(elem_id) > 0) markup = markup // ' id="' // interpolate(elem_id, vars, .true.) // '"'
    if (len(classes) > 0) markup = markup // ' class="' // interpolate(classes, vars, .true.) // '"'
    do i = 1, attrs%count
      markup = markup // ' ' // attrs%items(i)%key // '="' // &
               interpolate(attrs%items(i)%value, vars, .true.) // '"'
    end do
    markup = markup // '>' // interpolate(text, vars, .false.)

    if (is_void_element(tag)) then
      call write_open(w, markup)
      call w%out%append(LF)
      w%line_open = .false.
    else
      call write_open(w, markup)
      call push(w, tag, indent)
    end if
  end subroutine render_line

  !> Split a line into tag, id, classes, attributes and inner text.
  subroutine parse_line(content, tag, elem_id, classes, attrs, text, is_text)
    character(len=*), intent(in) :: content
    character(len=:), allocatable, intent(out) :: tag, elem_id, classes, text
    type(string_map), intent(out) :: attrs
    logical, intent(out) :: is_text
    integer :: pos, n, start
    character(len=1) :: marker

    n = len(content)
    elem_id = ''
    classes = ''
    text = ''
    tag = ''
    is_text = .false.

    if (content(1:1) == '|') then
      is_text = .true.
      text = content(2:)
      if (len(text) > 0) then
        if (text(1:1) == ' ') text = text(2:)
      end if
      return
    end if

    pos = 1
    do while (pos <= n)
      if (.not. is_name_char(content(pos:pos))) exit
      pos = pos + 1
    end do
    tag = content(1:pos - 1)

    do while (pos <= n)
      marker = content(pos:pos)
      if (marker /= '.' .and. marker /= '#') exit
      start = pos + 1
      do while (start <= n)
        if (.not. is_name_char(content(start:start))) exit
        start = start + 1
      end do
      if (start == pos + 1) exit
      if (marker == '.') then
        call append_class(classes, content(pos + 1:start - 1))
      else
        elem_id = content(pos + 1:start - 1)
      end if
      pos = start
    end do

    if (pos <= n) then
      if (content(pos:pos) == '(') call parse_attributes(content, pos, attrs, elem_id, classes)
    end if

    if (pos == 1) then
      is_text = .true.
      text = content
      return
    end if
    if (pos <= n) then
      if (content(pos:pos) == ' ') pos = pos + 1
      text = content(pos:)
    end if
    if (len(tag) == 0) tag = 'div'
  end subroutine parse_line

  !> Parse `(name="value", name2='v' flag)` starting at `pos`; leaves `pos` after `)`.
  subroutine parse_attributes(content, pos, attrs, elem_id, classes)
    character(len=*), intent(in) :: content
    integer, intent(inout) :: pos
    type(string_map), intent(inout) :: attrs
    character(len=:), allocatable, intent(inout) :: elem_id, classes
    character(len=:), allocatable :: key, value
    character(len=1) :: quote
    integer :: n, start

    n = len(content)
    pos = pos + 1
    do while (pos <= n)
      if (content(pos:pos) == ')') then
        pos = pos + 1
        return
      end if
      if (content(pos:pos) == ' ' .or. content(pos:pos) == ',' .or. content(pos:pos) == achar(9)) then
        pos = pos + 1
        cycle
      end if

      start = pos
      do while (pos <= n)
        if (index(' =,)' // achar(9), content(pos:pos)) > 0) exit
        pos = pos + 1
      end do
      if (pos == start) then
        pos = pos + 1
        cycle
      end if
      key = content(start:pos - 1)
      value = ''

      if (pos <= n) then
        if (content(pos:pos) == '=') then
          pos = pos + 1
          if (pos <= n) then
            quote = content(pos:pos)
            if (quote == '"' .or. quote == "'") then
              start = pos + 1
              pos = start
              do while (pos <= n)
                if (content(pos:pos) == quote) exit
                pos = pos + 1
              end do
              value = content(start:min(pos, n + 1) - 1)
              pos = pos + 1
            else
              start = pos
              do while (pos <= n)
                if (index(' ,)' // achar(9), content(pos:pos)) > 0) exit
                pos = pos + 1
              end do
              value = content(start:pos - 1)
            end if
          end if
        end if
      end if

      if (.not. is_attr_name(key)) cycle
      if (key == 'class') then
        call append_class(classes, value)
      else if (key == 'id' .and. len(elem_id) == 0) then
        elem_id = value
      else
        call attrs%add(key, value)
      end if
    end do
  end subroutine parse_attributes

  subroutine append_class(classes, name)
    character(len=:), allocatable, intent(inout) :: classes
    character(len=*), intent(in) :: name

    if (len(name) == 0) return
    if (len(classes) == 0) then
      classes = name
    else
      classes = classes // ' ' // name
    end if
  end subroutine append_class

  !> Replace `#{name}` with escaped variable values. Literal text is kept as written,
  !> except that `"` is escaped inside attribute values.
  function interpolate(text, vars, in_attribute) result(out)
    character(len=*), intent(in) :: text
    type(string_map), intent(in) :: vars
    logical, intent(in) :: in_attribute
    character(len=:), allocatable :: out
    type(string_buffer) :: buf
    integer :: pos, open_at, close_at

    call buf%append('')
    pos = 1
    do while (pos <= len(text))
      open_at = index(text(pos:), '#{')
      if (open_at == 0) then
        call append_literal(buf, text(pos:), in_attribute)
        exit
      end if
      open_at = pos + open_at - 1
      close_at = index(text(open_at + 2:), '}')
      if (close_at == 0) then
        call append_literal(buf, text(pos:), in_attribute)
        exit
      end if
      close_at = open_at + 2 + close_at - 1
      call append_literal(buf, text(pos:open_at - 1), in_attribute)
      call buf%append(html_escape(vars%get(text(open_at + 2:close_at - 1))))
      pos = close_at + 1
    end do
    out = buf%to_string()
  end function interpolate

  subroutine append_literal(buf, text, in_attribute)
    type(string_buffer), intent(inout) :: buf
    character(len=*), intent(in) :: text
    logical, intent(in) :: in_attribute
    integer :: i

    if (.not. in_attribute) then
      call buf%append(text)
      return
    end if
    do i = 1, len(text)
      if (text(i:i) == '"') then
        call buf%append('&quot;')
      else
        call buf%append(text(i:i))
      end if
    end do
  end subroutine append_literal

  subroutine write_open(w, markup)
    type(html_writer), intent(inout) :: w
    character(len=*), intent(in) :: markup

    if (w%line_open) call w%out%append(LF)
    call w%out%append(repeat(' ', 2 * w%depth) // markup)
    w%line_open = .true.
  end subroutine write_open

  subroutine write_text(w, text)
    type(html_writer), intent(inout) :: w
    character(len=*), intent(in) :: text

    if (w%line_open) call w%out%append(LF)
    call w%out%append(repeat(' ', 2 * w%depth) // text // LF)
    w%line_open = .false.
  end subroutine write_text

  subroutine push(w, tag, indent)
    type(html_writer), intent(inout) :: w
    character(len=*), intent(in) :: tag
    integer, intent(in) :: indent
    type(open_element), allocatable :: grown(:)

    if (w%depth == size(w%stack)) then
      allocate(grown(2 * size(w%stack)))
      grown(1:w%depth) = w%stack(1:w%depth)
      call move_alloc(grown, w%stack)
    end if
    w%depth = w%depth + 1
    w%stack(w%depth)%tag = tag
    w%stack(w%depth)%indent = indent
  end subroutine push

  !> Close every open element indented at or deeper than `indent`.
  subroutine close_to(w, indent)
    type(html_writer), intent(inout) :: w
    integer, intent(in) :: indent

    do while (w%depth > 0)
      if (w%stack(w%depth)%indent < indent) exit
      if (w%line_open) then
        call w%out%append('</' // w%stack(w%depth)%tag // '>' // LF)
        w%line_open = .false.
      else
        call w%out%append(repeat(' ', 2 * (w%depth - 1)) // '</' // w%stack(w%depth)%tag // '>' // LF)
      end if
      w%depth = w%depth - 1
    end do
  end subroutine close_to

  pure logical function is_name_char(ch)
    character(len=1), intent(in) :: ch

    select case (ch)
    case ('a':'z', 'A':'Z', '0':'9', '_', '-', ':')
      is_name_char = .true.
    case default
      is_name_char = .false.
    end select
  end function is_name_char

  pure logical function is_attr_name(name)
    character(len=*), intent(in) :: name
    integer :: i

    is_attr_name = len(name) > 0
    do i = 1, len(name)
      if (.not. (is_name_char(name(i:i)) .or. name(i:i) == '.' .or. name(i:i) == '@')) then
        is_attr_name = .false.
        return
      end if
    end do
  end function is_attr_name

  pure logical function is_void_element(tag)
    character(len=*), intent(in) :: tag

    select case (tag)
    case ('area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'link', 'meta', &
          'source', 'track', 'wbr')
      is_void_element = .true.
    case default
      is_void_element = .false.
    end select
  end function is_void_element

end module jade
