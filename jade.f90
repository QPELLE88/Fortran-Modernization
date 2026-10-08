module jade
  use string_helpers

  implicit none

  contains

  ! Renders a Jade template and fills in placeholders from replacements(:,1) keys.
  ! #{key} inserts the HTML-escaped value; !{key} inserts it raw (trusted HTML only).
  ! Substitution is a single pass, so inserted values are never re-scanned.
  subroutine jadetemplate(templatefile, unitNo, replacements)
    character(len=10000)         :: templatefile
    integer                      :: unitNo, i, n, p, start, keyEnd, escapedAt, rawAt
    character(len=*), dimension(10,2)    :: replacements
    character(len=3)   :: AFORMAT = '(a)'
    character(len=:), allocatable :: rendered, key
    logical            :: found

    call jadefile(templatefile, 0)

    rendered = ''
    n = len_trim(templatefile)
    p = 1
    do while (p <= n)
      escapedAt = index(templatefile(p:n), '#{')
      rawAt = index(templatefile(p:n), '!{')
      if (escapedAt == 0 .or. (rawAt > 0 .and. rawAt < escapedAt)) then
        start = rawAt
      else
        start = escapedAt
      endif
      if (start == 0) exit
      start = p + start - 1

      keyEnd = index(templatefile(start + 2:n), '}')
      if (keyEnd == 0) exit
      keyEnd = start + keyEnd

      rendered = rendered // templatefile(p:start - 1)
      key = templatefile(start + 2:keyEnd)

      found = .false.
      i = 1
      do
        if ((i > 10) .or. (replacements(i,1) == '')) then
          exit
        endif
        if (trim(replacements(i, 1)) == key) then
          found = .true.
          if (templatefile(start:start) == '!') then
            rendered = rendered // trim(replacements(i, 2))
          else
            rendered = rendered // html_escape(trim(replacements(i, 2)))
          endif
          exit
        endif
        i = i + 1
      enddo
      if (.not. found) then
        rendered = rendered // templatefile(start:keyEnd + 1)
      endif

      p = keyEnd + 2
    enddo
    if (p <= n) then
      rendered = rendered // templatefile(p:n)
    endif
    write(unitNo, AFORMAT) rendered
  endsubroutine

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
