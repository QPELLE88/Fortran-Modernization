!> Minimal Jade/HAML-like template engine.
!>
!> Each template line is `tag[.class][#id][(attrs)] inner text`; nesting is
!> expressed through indentation. `#{key}` placeholders are substituted by
!> `jadetemplate`.
module jade
  use string_helpers, only: compact, string_replace, replace_all, html_escape

  implicit none
  private

  public :: template_var
  public :: template_var_of
  public :: jadefile
  public :: jadetemplate
  public :: render_jade

  integer, parameter :: LINE_LEN = 1000
  integer, parameter :: TOKEN_LEN = 256
  integer, parameter :: MAX_DEPTH = 30

  !> A `#{key}` -> value substitution for `jadetemplate`.
  type :: template_var
    character(len=:), allocatable :: key
    character(len=:), allocatable :: value
  end type template_var

contains

  !> Builds a `template_var` (component-wise, which avoids gfortran
  !> structure-constructor bugs with deferred-length components).
  pure function template_var_of(key, value) result(var)
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    type(template_var) :: var

    var%key = key
    var%value = value
  end function template_var_of

  !> Renders the template at `path` to `unit`, one output line per input line.
  subroutine jadefile(path, unit)
    character(len=*), intent(in) :: path
    integer, intent(in) :: unit

    call render_jade(path, unit=unit)
  end subroutine jadefile

  !> Renders the template at `path` as a single line to `unit`, substituting
  !> every `#{key}` from `vars` with its HTML-escaped value.
  subroutine jadetemplate(path, unit, vars)
    character(len=*), intent(in) :: path
    integer, intent(in) :: unit
    type(template_var), intent(in) :: vars(:)

    character(len=:), allocatable :: html
    integer :: i

    call render_jade(path, html=html)
    do i = 1, size(vars)
      html = replace_all(html, '#{' // vars(i)%key // '}', html_escape(vars(i)%value))
    end do
    write(unit, '(a)') html
  end subroutine jadetemplate

  !> Renders the template at `path` either line by line to `unit` or, when
  !> `html` is present, concatenated into `html`.
  subroutine render_jade(path, html, unit)
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out), optional :: html
    integer, intent(in), optional :: unit

    character(len=LINE_LEN) :: inputLine, outputLine, innerContent
    character(len=LINE_LEN) :: spaceless
    character(len=TOKEN_LEN) :: tag, closeTag, className, elemID
    character(len=TOKEN_LEN) :: tagLevels(0:MAX_DEPTH)
    integer :: spaceLevels(0:MAX_DEPTH)
    integer :: templater, io, spaceCount, lastSpaceCount, lastIndent

    if (present(html)) html = ''

    open(newunit=templater, file=path, status='old', action='read', iostat=io)
    if (io /= 0) then
      call emit('<!-- template not found: ' // path // ' -->')
      return
    end if

    tagLevels = ''
    spaceLevels = 0
    lastSpaceCount = -1
    lastIndent = 0
    do
      read(templater, '(a)', iostat=io) inputLine
      if (io /= 0) exit
      if (len_trim(inputLine) == 0) cycle

      spaceless = trim(inputLine)
      call compact(spaceless)
      spaceless = trim(spaceless) // '   '
      spaceCount = index(inputLine, trim(spaceless))
      className = ''
      elemID = ''
      innerContent = spaceless(index(spaceless, ' ') + 1:)

      if (spaceless(1:1) == '.') then
        ! starts with a class definition
        tag = 'div'
        className = spaceless(2:index(spaceless, ' ') - 1)
        if (index(className, '(') > 0) then
          tag = trim(tag) // className(index(className, '('):index(className, ')'))
          className = className(1:index(className, '#') - 1)
        end if
        if (index(className, '#') > 0) then
          className = className(1:index(className, '#') - 1)
          elemID = spaceless(index(spaceless, '#'):index(spaceless, ' '))
        end if
      else if (spaceless(1:1) == '#') then
        ! starts with an ID definition
        tag = 'div'
        elemID = spaceless(2:index(spaceless, ' ') - 1)
        if (index(elemID, '(') > 0) then
          tag = trim(tag) // elemID(index(elemID, '('):index(elemID, ')'))
          elemID = elemID(1:index(elemID, '#') - 1)
        end if
        if (index(elemID, '.') > 0) then
          elemID = elemID(1:index(elemID, '.') - 1)
          className = spaceless(index(spaceless, '.'):index(spaceless, ' '))
        end if
      else if (spaceless(1:1) == '(') then
        ! starts with div attributes
        if (index(spaceless, ')') > index(spaceless, ' ')) then
          tag = 'div' // spaceless(1:index(spaceless, ')'))
        else
          tag = 'div' // spaceless(1:index(spaceless, ' ') - 1)
        end if
      else
        ! custom tag
        tag = spaceless(1:index(spaceless, ' ') - 1)
        if (index(tag, '.') > 0 .and. index(tag, '.') < index(tag, '(')) then
          tag = tag(1:index(tag, '.') - 1)
          className = spaceless(index(spaceless, '.'):index(spaceless, '(') - 1)
        end if
        if (index(tag, '#') > 0 .and. index(tag, '#') < index(tag, '(')) then
          tag = tag(1:index(tag, '#') - 1)
          elemID = spaceless(index(spaceless, '#'):index(spaceless, '(') - 1)
        end if
        if (index(tag, '(') == 0) then
          if (index(tag, '#') > 0) then
            elemID = tag(index(tag, '#'):index(tag, ' '))
            tag = tag(1:index(tag, '#') - 1)
          end if
          if (index(tag, '.') > 0) then
            className = tag(index(tag, '.'):index(tag, ' '))
            tag = tag(1:index(tag, '.') - 1)
          end if
        end if
      end if

      ! handle multiple classes
      call string_replace(className, '.', ' ')
      ! never leave a # in the ID
      call string_replace(elemID, '#', '')

      if (index(tag, '(') > 0) then
        call string_replace(tag, '(', ' ')
        call string_replace(tag, ')', ' ')
        call string_replace(tag, ',', ' ')
      end if

      ! determine close tag, ahead of time
      closeTag = tag
      if (index(closeTag, ' ') > 0) closeTag = closeTag(1:index(closeTag, ' '))
      if (index(closeTag, '#') > 0) closeTag = closeTag(1:index(closeTag, '#') - 1)
      if (index(closeTag, '.') > 0) closeTag = closeTag(1:index(closeTag, '.'))

      outputLine = ''

      if (lastSpaceCount < spaceCount) then
        ! went up a level
        call push(closeTag, spaceCount)
      else if (lastSpaceCount == spaceCount) then
        ! same level; previous tag is closed; replace with this tag
        outputLine = closing(tagLevels(lastIndent))
        tagLevels(lastIndent) = closeTag
      else
        ! went down at least one level
        do while (lastIndent > 0)
          if (spaceLevels(lastIndent) < spaceCount) exit
          outputLine = trim(outputLine) // closing(tagLevels(lastIndent))
          lastIndent = lastIndent - 1
        end do
        call push(closeTag, spaceCount)
      end if

      outputLine = trim(outputLine) // '<' // &
        trim(tag) // &
        ' id="' // trim(elemID) // '"' // &
        ' class="' // trim(className) // '"' // &
        '>' // trim(innerContent)

      lastSpaceCount = spaceCount

      if (len_trim(closeTag) > 0) call emit(outputLine)
    end do
    close(templater)

    ! close every tag that is still open
    outputLine = ''
    do while (lastIndent > 0)
      outputLine = trim(outputLine) // closing(tagLevels(lastIndent))
      lastIndent = lastIndent - 1
    end do
    if (len_trim(outputLine) > 0) call emit(outputLine)

  contains

    subroutine push(name, indent)
      character(len=*), intent(in) :: name
      integer, intent(in) :: indent

      if (lastIndent >= MAX_DEPTH) error stop 'jade: template nested too deeply'
      lastIndent = lastIndent + 1
      tagLevels(lastIndent) = name
      spaceLevels(lastIndent) = indent
    end subroutine push

    pure function closing(name) result(res)
      character(len=*), intent(in) :: name
      character(len=:), allocatable :: res

      if (len_trim(name) == 0) then
        res = ''
      else
        res = '</' // trim(name) // '>'
      end if
    end function closing

    subroutine emit(line)
      character(len=*), intent(in) :: line

      if (present(html)) then
        html = html // trim(line)
      else if (present(unit)) then
        write(unit, '(a)') trim(line)
      end if
    end subroutine emit

  end subroutine render_jade

end module jade
