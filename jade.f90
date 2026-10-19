module jade
  use string_helpers

  implicit none

  integer, parameter, private :: CTX_TEXT = 0, CTX_QUOTED = 1, CTX_UNQUOTED = 2

  contains

  ! Renders templatefile, then fills placeholders in a single pass so that
  ! substituted values are never expanded again:
  !   #{key}  value is HTML-escaped for where it lands (text, quoted or
  !           unquoted attribute); inside a URL attribute (href, src, ...) it
  !           is also kept on the template's host and percent-encoded
  !   !{key}  value is written raw; only use for trusted markup
  ! Unknown keys and unclosed openers are left as they are.
  subroutine jadetemplate(templatefile, unitNo, replacements)
    character(len=10000)         :: templatefile
    integer                      :: unitNo, i, k, n, p, start, keyEnd, ctx
    character(len=*), dimension(10,2)    :: replacements
    character(len=3)   :: AFORMAT = '(a)'
    character(len=:), allocatable :: rendered, piece, value, urlPrefix
    logical            :: isUrl

    call jadefile(templatefile, 0)

    rendered = ''
    n = len_trim(templatefile)
    p = 1
    do while (p <= n)
      start = placeholder_index(templatefile(p:n), .false.)
      if (start == 0) exit
      start = p + start - 1
      keyEnd = index(templatefile(start + 2:n), '}')
      if (keyEnd == 0) exit
      keyEnd = start + 1 + keyEnd
      ! an unclosed opener before this one is literal text
      k = placeholder_index(templatefile(start + 2:keyEnd - 1), .true.)
      if (k > 0) start = start + 1 + k

      piece = templatefile(p:start - 1)
      rendered = rendered // piece

      i = find_key(replacements, templatefile(start + 2:keyEnd - 1))
      if (i == 0) then
        piece = templatefile(start:keyEnd)
      else if (templatefile(start:start) == '!') then
        piece = trim(replacements(i, 2))
      else
        call attribute_context(rendered, ctx, isUrl, urlPrefix)
        if (isUrl) then
          value = url_path_escape(html_unescape(urlPrefix), trim(replacements(i, 2)))
        else
          value = trim(replacements(i, 2))
        endif
        if (ctx == CTX_UNQUOTED) then
          piece = html_escape_unquoted(value)
        else
          piece = html_escape(value)
        endif
      endif
      rendered = rendered // piece
      p = keyEnd + 1
    enddo
    if (p <= n) then
      piece = templatefile(p:n)
      rendered = rendered // piece
    endif
    write(unitNo, AFORMAT) rendered
  endsubroutine

  ! position of the first (or last) "#{" or "!{" in str, 0 if none
  integer function placeholder_index(str, last)
    character(len=*), intent(in) :: str
    logical, intent(in)          :: last
    integer                      :: escaped, raw

    escaped = index(str, '#{', back=last)
    raw = index(str, '!{', back=last)
    if (escaped == 0 .or. raw == 0) then
      placeholder_index = max(escaped, raw)
    else if (last) then
      placeholder_index = max(escaped, raw)
    else
      placeholder_index = min(escaped, raw)
    endif
  endfunction

  integer function find_key(replacements, key)
    character(len=*), dimension(:,:), intent(in) :: replacements
    character(len=*), intent(in)                 :: key
    integer                                      :: i

    find_key = 0
    if (len_trim(key) == 0) return
    do i = 1, size(replacements, 1)
      if (replacements(i, 1) == '') exit
      if (replacements(i, 1) == key) then
        find_key = i
        return
      endif
    enddo
  endfunction

  ! Scans html (the output so far) and reports where the next character lands:
  ! ctx is CTX_TEXT, CTX_QUOTED (inside a quoted attribute value) or
  ! CTX_UNQUOTED (unquoted value, or inside a tag outside any value); isUrl
  ! and prefix (the value written so far) describe a URL attribute.
  ! Earlier substitutions are escaped, so they cannot fake '<', '>' or quotes.
  subroutine attribute_context(html, ctx, isUrl, prefix)
    character(len=*), intent(in)               :: html
    integer, intent(out)                       :: ctx
    logical, intent(out)                       :: isUrl
    character(len=:), allocatable, intent(out) :: prefix

    integer, parameter :: S_TEXT = 0, S_TAG = 1, S_AFTER_EQ = 2, S_QUOTED = 3, S_UNQUOTED = 4
    character(len=*), parameter :: SPACES = ' ' // achar(9) // achar(10) // achar(12) // achar(13)
    character(len=:), allocatable :: attrName
    character(len=1)   :: c, quote
    integer            :: i, k, n, state, nameStart, valueStart

    n = len(html)
    state = S_TEXT
    attrName = ''
    quote = '"'
    nameStart = 0
    valueStart = 1
    i = 1
    do while (i <= n)
      c = html(i:i)
      select case (state)
        case (S_TEXT)
          if (c == '<' .and. i < n) then
            if (index(html(i:), '<!--') == 1) then
              k = index(html(i + 4:), '-->')
              if (k == 0) exit
              i = i + 4 + k + 2
              cycle
            endif
            if (is_tag_start(html(i + 1:i + 1))) then
              state = S_TAG
              nameStart = 0
            endif
          endif
        case (S_TAG)
          if (c == '>') then
            state = S_TEXT
          else if (c == '=') then
            if (nameStart > 0) attrName = to_lower(html(nameStart:i - 1))
            nameStart = 0
            state = S_AFTER_EQ
          else if (index(SPACES, c) > 0 .or. c == '/') then
            if (nameStart > 0) attrName = to_lower(html(nameStart:i - 1))
            nameStart = 0
          else if (nameStart == 0) then
            nameStart = i
          endif
        case (S_AFTER_EQ)
          if (c == '"' .or. c == "'") then
            quote = c
            valueStart = i + 1
            state = S_QUOTED
          else if (c == '>') then
            state = S_TEXT
          else if (index(SPACES, c) == 0) then
            valueStart = i
            state = S_UNQUOTED
          endif
        case (S_QUOTED)
          if (c == quote) state = S_TAG
        case (S_UNQUOTED)
          if (c == '>') then
            state = S_TEXT
          else if (index(SPACES, c) > 0) then
            state = S_TAG
          endif
      end select
      i = i + 1
    enddo

    ctx = CTX_TEXT
    isUrl = .false.
    prefix = ''
    select case (state)
      case (S_TAG)
        ctx = CTX_UNQUOTED
      case (S_AFTER_EQ)
        ctx = CTX_UNQUOTED
        isUrl = is_url_attribute(attrName)
      case (S_QUOTED, S_UNQUOTED)
        ctx = CTX_QUOTED
        if (state == S_UNQUOTED) ctx = CTX_UNQUOTED
        isUrl = is_url_attribute(attrName)
        prefix = html(valueStart:n)
    end select
  endsubroutine

  logical function is_tag_start(c)
    character(len=1), intent(in) :: c

    is_tag_start = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. c == '/' .or. c == '!'
  endfunction

  logical function is_url_attribute(name)
    character(len=*), intent(in) :: name

    select case (name)
      case ('href', 'src', 'action', 'formaction', 'cite', 'poster', 'data', 'background', 'xlink:href')
        is_url_attribute = .true.
      case default
        is_url_attribute = .false.
    end select
  endfunction

  subroutine jadefile(templatefile, unitNo)
    character(len=10000)  :: templatefile
    character(len=80)  :: spaceless, tag, closeTag, className, elemID
    character(len=1000)   :: inputLine, outputLine, innerContent
    integer            :: templater, io, spaceCount, unitNo, lastSpaceCount, lastIndent
    integer, dimension (0:30) :: spaceLevels
    character(len=80), dimension (0:30) :: tagLevels
    character(len=3)   :: AFORMAT = '(a)'

    open(newunit=templater, file=templatefile)
    templatefile = ''
    lastSpaceCount = -1
    lastIndent = 0
    do
      read(templater, '(A)', IOSTAT=io) inputLine
      if (io < 0) then
        outputLine = '</' // &
          trim(tagLevels(lastIndent)) // &
          '>'
        if (unitNo == 0) then
          templatefile = trim(templatefile) // outputLine
        else
          write(unitNo, AFORMAT) outputLine
        endif
        exit
      endif

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
        className = spaceless(2: index(spaceless, ' ') - 1)
        if (index(className, '(') > 0) then
          tag = trim(tag) // className( index(className, '(') : index(className, ')'))
          className = className(1 : index(className, '#') - 1)
        endif
        if (index(className, '#') > 0) then
          className = className(1 : index(className, '#') - 1)
          elemID = spaceless(index(spaceless, '#') : index(spaceless, ' '))
        endif
      else
        ! starts with an ID definition
        if (spaceless(1:1) == '#') then
          tag = 'div'
          elemID = spaceless(2: index(spaceless, ' ') - 1)
          if (index(elemID, '(') > 0) then
            tag = trim(tag) // elemID( index(elemID, '(') : index(elemID, ')'))
            elemID = elemID(1 : index(elemID, '#') - 1)
          endif
          if (index(elemID, '.') > 0) then
            elemID = elemID(1 : index(elemID, '.') - 1)
            className = spaceless(index(spaceless, '.') : index(spaceless, ' '))
          endif
        else
          if (spaceless(1:1) == '(') then
            ! starts with a div attributes
            if (index(spaceless, ')') > index(spaceless, ' ')) then
              tag = 'div' // spaceless(1: index(spaceless, ')'))
            else
              tag = 'div' // spaceless(1: index(spaceless, ' ') - 1)
            endif
          else
            ! custom tag
            tag = spaceless(1: index(spaceless, ' ') - 1)
            if (index(tag, '.') > 0 .and. index(tag, '.') < index(tag, '(')) then
              tag = tag(1: index(tag, '.') - 1)
              className = spaceless(index(spaceless, '.') : index(spaceless, '(') - 1)
            endif
            if (index(tag, '#') > 0 .and. index(tag, '#') < index(tag, '(')) then
              tag = tag(1: index(tag, '#') - 1)
              elemID = spaceless(index(spaceless, '#') : index(spaceless, '(') - 1)
            endif
            if (index(tag, '(') == 0) then
              if (index(tag, '#') > 0) then
                elemID = tag(index(tag, '#') : index(tag, ' '))
                tag = tag(1: index(tag, '#') - 1)
              endif
              if (index(tag, '.') > 0) then
                className = tag(index(tag, '.') : index(tag, ' '))
                tag = tag(1: index(tag, '.') - 1)
              endif
            endif
          endif
        endif
      endif

      ! handle multiple classes
      call string_replace(className, '.', ' ')
      ! just make sure I don't leave a # in the ID
      call string_replace(elemID, '#', '')

      if (index(tag, '(') > 0) then
        call string_replace(tag, '(', ' ')
        call string_replace(tag, ')', ' ')
        call string_replace(tag, ',', ' ')
      endif

      ! determine close tag, ahead of time
      closeTag = tag
      if (index(closeTag, ' ') > 0) then
        closeTag = closeTag(1 : index(closeTag, ' '))
      endif
      if (index(closeTag, '#') > 0) then
        closeTag = closeTag(1 : index(closeTag, '#') - 1)
      endif
      if (index(closeTag, '.') > 0) then
        closeTag = closeTag(1 : index(closeTag, '.'))
      endif

      outputLine = ''

      if (lastSpaceCount < spaceCount) then
        ! went up a level
        lastIndent = lastIndent + 1
        spaceLevels(lastIndent) = spaceCount
        tagLevels(lastIndent) = closeTag
      else
        if (lastSpaceCount == spaceCount) then
          ! same level; previous tag is closed; replace with this tag
          outputLine = trim(outputLine) // &
            '</' // &
            trim(tagLevels(lastIndent)) // &
            '>'
          tagLevels(lastIndent) = closeTag
        else
          ! went down at least one level
          do
            if (spaceLevels(lastIndent) >= spaceCount) then
              ! closed or sibling tag
              outputLine = trim(outputLine) // &
                '</' // &
                trim(tagLevels(lastIndent)) // &
                '>'
              lastIndent = lastIndent - 1
           else
              ! bottomed out; open this tag
              lastIndent = lastIndent + 1
              tagLevels(lastIndent) = closeTag
              spaceLevels(lastIndent) = spaceCount
              exit
            endif
          end do
        endif
      endif

      outputLine = trim(outputLine) // '<' // &
        trim(tag) // &
        ' id="' // trim(elemID) // '"' // &
        ' class="' // trim(className) // '"'

      !if (index(tag, 'img') == 1 .or. index(tag, 'link') == 1) then
        ! single closing tag
      !  outputLine = trim(outputLine) // &
      !    '/>'
      !else
        ! complete opening tag and inner content
      outputLine = trim(outputLine) // &
        '>' // &
        trim(innerContent)

      !endif

      lastSpaceCount = spaceCount

      if (.not. trim(closeTag) == '') then
        if (unitNo == 0) then
          templatefile = trim(templatefile) // outputLine
        else
          write(unitNo, AFORMAT) outputLine
        endif
      endif
    end do
    close(templater)
  endsubroutine
endmodule
