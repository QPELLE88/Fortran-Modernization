module jade
  use string_helpers

  implicit none

  contains

  ! Renders templatefile, then fills placeholders in a single pass so that
  ! substituted values are never expanded again:
  !   #{key}  value is HTML-escaped; inside a URL attribute (href, src, ...)
  !           it is also forced to a same-host path and percent-encoded
  !   !{key}  value is written raw; only use for trusted markup
  ! Unknown keys are left as they are.
  subroutine jadetemplate(templatefile, unitNo, replacements)
    character(len=10000)         :: templatefile
    integer                      :: unitNo, i, n, p, start, keyEnd
    character(len=*), dimension(10,2)    :: replacements
    character(len=3)   :: AFORMAT = '(a)'
    character(len=:), allocatable :: rendered, piece, urlPrefix
    logical            :: raw

    call jadefile(templatefile, 0)

    rendered = ''
    n = len_trim(templatefile)
    p = 1
    do while (p <= n)
      start = next_placeholder(templatefile(p:n))
      if (start == 0) exit
      start = p + start - 1
      keyEnd = index(templatefile(start + 2:n), '}')
      if (keyEnd == 0) exit
      keyEnd = start + 1 + keyEnd

      piece = templatefile(p:start - 1)
      rendered = rendered // piece
      raw = templatefile(start:start) == '!'

      i = find_key(replacements, templatefile(start + 2:keyEnd - 1))
      if (i == 0) then
        piece = templatefile(start:keyEnd)
      else if (raw) then
        piece = trim(replacements(i, 2))
      else if (in_url_attribute(rendered, urlPrefix)) then
        piece = html_escape(url_path_escape(urlPrefix, trim(replacements(i, 2))))
      else
        piece = html_escape(trim(replacements(i, 2)))
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

  ! position of the first "#{" or "!{" in str, 0 if none
  integer function next_placeholder(str)
    character(len=*), intent(in) :: str
    integer                      :: escaped, raw

    escaped = index(str, '#{')
    raw = index(str, '!{')
    if (escaped == 0) then
      next_placeholder = raw
    else if (raw == 0) then
      next_placeholder = escaped
    else
      next_placeholder = min(escaped, raw)
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

  ! True when the end of html is inside a double-quoted URL attribute value
  ! of an open tag; prefix is the part of that value written so far.
  ! Earlier substitutions are escaped, so they cannot fake '<', '>' or '"'.
  logical function in_url_attribute(html, prefix)
    character(len=*), intent(in)               :: html
    character(len=:), allocatable, intent(out) :: prefix
    character(len=:), allocatable              :: tagText, attrName
    integer                                    :: lt, q, nameEnd, nameStart, i, quotes

    in_url_attribute = .false.
    prefix = ''
    lt = index(html, '<', back=.true.)
    if (lt == 0 .or. index(html, '>', back=.true.) > lt) return
    tagText = html(lt + 1:)

    quotes = 0
    do i = 1, len(tagText)
      if (tagText(i:i) == '"') quotes = quotes + 1
    enddo
    if (mod(quotes, 2) == 0) return

    q = index(tagText, '"', back=.true.)
    nameEnd = q - 1
    do while (nameEnd > 0)
      if (tagText(nameEnd:nameEnd) /= ' ') exit
      nameEnd = nameEnd - 1
    enddo
    if (nameEnd < 2) return
    if (tagText(nameEnd:nameEnd) /= '=') return
    nameEnd = nameEnd - 1
    do while (nameEnd > 0)
      if (tagText(nameEnd:nameEnd) /= ' ') exit
      nameEnd = nameEnd - 1
    enddo
    nameStart = nameEnd
    do while (nameStart > 1)
      if (tagText(nameStart - 1:nameStart - 1) == ' ') exit
      nameStart = nameStart - 1
    enddo
    if (nameEnd < nameStart) return
    attrName = to_lower(tagText(nameStart:nameEnd))

    select case (attrName)
      case ('href', 'src', 'action', 'formaction', 'cite', 'poster', 'data', 'background')
        in_url_attribute = .true.
        prefix = tagText(q + 1:)
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
