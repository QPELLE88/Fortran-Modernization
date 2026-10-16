module jade
  use string_helpers

  implicit none

  private :: find_key, escape_for_context, placeholder_context, &
    is_url_attribute, url_escape, has_unsafe_scheme

  integer, parameter, private :: CTX_TEXT = 0, CTX_ATTR = 1, &
    CTX_ATTR_UNQUOTED = 2, CTX_RAWTEXT = 3

  contains

  ! Renders a Jade template, then fills each #{key} with the matching value
  ! from replacements(:,2), escaped for where it lands in the HTML (text,
  ! attribute, URL attribute). Values are inserted in a single pass, so a
  ! value containing #{...} is never expanded. Unknown keys are left as is.
  subroutine jadetemplate(templatefile, unitNo, replacements)
    character(len=10000)         :: templatefile
    integer                      :: unitNo, i, n, p, start, keyEnd
    character(len=*), dimension(10,2)    :: replacements
    character(len=3)   :: AFORMAT = '(a)'
    character(len=:), allocatable :: rendered, piece

    call jadefile(templatefile, 0)

    rendered = ''
    n = len_trim(templatefile)
    p = 1
    do while (p <= n)
      start = index(templatefile(p:n), '#{')
      if (start == 0) exit
      start = p + start - 1
      keyEnd = index(templatefile(start + 2:n), '}')
      if (keyEnd == 0) exit
      keyEnd = start + 1 + keyEnd

      piece = templatefile(p:start - 1)
      rendered = rendered // piece

      i = find_key(replacements, templatefile(start + 2:keyEnd - 1))
      if (i > 0) then
        piece = escape_for_context(rendered, trim(replacements(i, 2)))
      else
        piece = templatefile(start:keyEnd)
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

  ! escapes value for the HTML context at the end of html
  function escape_for_context(html, value) result(escaped)
    character(len=*), intent(in)  :: html, value
    character(len=:), allocatable :: escaped, attrValue, encoded
    integer                       :: ctx
    logical                       :: isUrl

    call placeholder_context(html, ctx, isUrl, attrValue)
    select case (ctx)
      case (CTX_TEXT)
        escaped = html_escape(value)
      case (CTX_ATTR)
        if (isUrl) then
          encoded = url_escape(attrValue, value)
          escaped = html_escape(encoded)
        else
          escaped = html_escape(value)
        endif
      case (CTX_ATTR_UNQUOTED)
        if (isUrl) then
          encoded = url_escape(attrValue, value)
          escaped = html_escape_strict(encoded)
        else
          escaped = html_escape_strict(value)
        endif
      case default
        ! inside <script>, <style> or a comment: no safe encoding here
        escaped = ''
    end select
  endfunction

  ! scans html (output so far) to find where the next character would land
  subroutine placeholder_context(html, ctx, isUrl, attrValue)
    character(len=*), intent(in)               :: html
    integer, intent(out)                       :: ctx
    logical, intent(out)                       :: isUrl
    character(len=:), allocatable, intent(out) :: attrValue

    integer, parameter :: S_TEXT = 0, S_TAGNAME = 1, S_BEFORE_ATTR = 2, &
      S_ATTRNAME = 3, S_AFTER_EQ = 4, S_QUOTED = 5, S_UNQUOTED = 6
    character(len=*), parameter :: SPACES = ' ' // achar(9) // achar(10) // &
      achar(12) // achar(13)
    character(len=:), allocatable :: lower, tagName, attrName
    character(len=1)   :: c, quote
    integer            :: i, k, n, state, tagStart, nameStart, valueStart
    logical            :: tagClosed

    lower = to_lower(html)
    n = len(html)
    state = S_TEXT
    tagName = ''
    attrName = ''
    attrValue = ''
    quote = '"'
    tagStart = 1
    nameStart = 1
    valueStart = 1
    isUrl = .false.
    ctx = CTX_RAWTEXT

    i = 1
    do while (i <= n)
      c = html(i:i)
      tagClosed = .false.
      select case (state)
        case (S_TEXT)
          if (c == '<') then
            if (i + 3 <= n) then
              if (html(i:i + 3) == '<!--') then
                k = index(html(i + 4:), '-->')
                if (k == 0) return
                i = i + 4 + k + 2
                cycle
              endif
            endif
            state = S_TAGNAME
            tagStart = i + 1
          endif
        case (S_TAGNAME)
          if (index(SPACES, c) > 0 .or. c == '>') then
            tagName = lower(tagStart:i - 1)
            state = S_BEFORE_ATTR
            tagClosed = (c == '>')
          endif
        case (S_BEFORE_ATTR)
          if (c == '>') then
            tagClosed = .true.
          elseif (c == '=') then
            state = S_AFTER_EQ
          elseif (index(SPACES, c) == 0 .and. c /= '/') then
            nameStart = i
            state = S_ATTRNAME
          endif
        case (S_ATTRNAME)
          if (c == '=' .or. c == '>' .or. index(SPACES, c) > 0) then
            attrName = lower(nameStart:i - 1)
            state = S_BEFORE_ATTR
            if (c == '=') state = S_AFTER_EQ
            tagClosed = (c == '>')
          endif
        case (S_AFTER_EQ)
          if (c == '"' .or. c == "'") then
            quote = c
            valueStart = i + 1
            state = S_QUOTED
          elseif (c == '>') then
            tagClosed = .true.
          elseif (index(SPACES, c) == 0) then
            valueStart = i
            state = S_UNQUOTED
          endif
        case (S_QUOTED)
          if (c == quote) state = S_BEFORE_ATTR
        case (S_UNQUOTED)
          if (c == '>') then
            tagClosed = .true.
          elseif (index(SPACES, c) > 0) then
            state = S_BEFORE_ATTR
          endif
      end select

      if (tagClosed) then
        state = S_TEXT
        if (tagName == 'script' .or. tagName == 'style') then
          k = index(lower(i + 1:), '</' // tagName)
          if (k == 0) return
          i = i + k
          cycle
        endif
      endif
      i = i + 1
    enddo

    select case (state)
      case (S_TEXT)
        ctx = CTX_TEXT
      case (S_QUOTED)
        ctx = CTX_ATTR
        attrValue = html(valueStart:n)
        isUrl = is_url_attribute(attrName)
      case (S_UNQUOTED)
        ctx = CTX_ATTR_UNQUOTED
        attrValue = html(valueStart:n)
        isUrl = is_url_attribute(attrName)
      case (S_AFTER_EQ)
        ctx = CTX_ATTR_UNQUOTED
        isUrl = is_url_attribute(attrName)
      case default
        ! tag or attribute name position
        ctx = CTX_ATTR_UNQUOTED
    end select
  endsubroutine

  logical function is_url_attribute(name)
    character(len=*), intent(in) :: name

    select case (name)
      case ('href', 'src', 'action', 'formaction', 'poster', 'cite', &
            'background', 'data', 'codebase', 'longdesc', 'usemap', &
            'ping', 'manifest', 'xlink:href')
        is_url_attribute = .true.
      case default
        is_url_attribute = .false.
    end select
  endfunction

  ! percent-encodes value for a URL attribute whose text so far is prefix,
  ! so the value cannot pick the scheme or (after a fixed host) the host
  function url_escape(prefix, value) result(encoded)
    character(len=*), intent(in)  :: prefix, value
    character(len=:), allocatable :: encoded, pre
    character(len=*), parameter   :: RESERVED = ":/?#[]@!$&'()*+,;="
    integer                       :: k

    pre = trim(adjustl(prefix))
    if (len(pre) == 0) then
      if (has_unsafe_scheme(value)) then
        encoded = '#'
      else
        encoded = percent_encode(value, RESERVED, .true.)
      endif
    elseif (scan(pre, '?#') > 0) then
      encoded = percent_encode(value, '', .false.)
    else
      encoded = percent_encode(value, RESERVED, .true.)
      ! prefix ends inside the host ("https://example.com"): force a path
      k = index(pre, '//')
      if (k > 0) then
        if (len(pre) > k + 1) then
          if (scan(pre(k + 2:), '/') == 0 .and. index(encoded, '/') /= 1) then
            encoded = '/' // encoded
          endif
        endif
      endif
      ! "/" or "scheme:" followed by "/..." would start a new host
      if ((pre == '/' .or. pre(len(pre):) == ':') .and. index(encoded, '/') == 1) then
        encoded = '%2F' // encoded(2:)
      endif
    endif
  endfunction

  ! true when value starts with a scheme other than http, https or mailto;
  ! browsers ignore whitespace and control characters inside the scheme
  logical function has_unsafe_scheme(value)
    character(len=*), intent(in)  :: value
    character(len=:), allocatable :: stripped, scheme
    integer                       :: i, colon

    stripped = ''
    do i = 1, len(value)
      if (iachar(value(i:i)) > 32 .and. iachar(value(i:i)) /= 127) then
        stripped = stripped // value(i:i)
      endif
    enddo

    has_unsafe_scheme = .false.
    colon = index(stripped, ':')
    if (colon == 0) return
    if (scan(stripped(1:colon - 1), '/?#') > 0) return
    scheme = to_lower(stripped(1:colon - 1))
    has_unsafe_scheme = .not. (scheme == 'http' .or. scheme == 'https' .or. &
      scheme == 'mailto')
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
