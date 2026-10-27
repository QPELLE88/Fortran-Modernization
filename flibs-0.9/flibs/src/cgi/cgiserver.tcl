# cgiserver.tcl --
#     A tiny CGI/HTTP server useful for simple applications
#     Derived from the tiny CGI server - http://wiki.tcl.tk/16867
#     This was derived from "DustMotePlus"
#     The server supports a small fraction of the CGI/HTTP standard only
#
#     Purpose:
#     - Present HTML files
#     - Run programs via a simple protocol (no need to know
#       much about CGI)
#
#     Usage:
#         tclsh cgiserver.tcl ?-cfg configuration-file?
#
#     Features:
#     - You can show static HTML pages (requested URL ends in .html)
#     - You can run Tcl scripts (requested URL ends in .tcl) that
#       print the result on standard output and then finish
#     - You can run programs (URL ends in .exe) that use the simple
#       protocol exemplified in "cgi.f90" (see also below)
#
#     Security:
#     - The path of a URL is percent-decoded and must resolve (symbolic
#       links included) to a regular file below the root directory.
#       ".." segments, backslashes, colons (drive letters, NTFS streams)
#       and control characters are refused outright (403).
#     - Tcl scripts and programs (.tcl/.exe) are only run from the
#       cgi-bin directory below the root; elsewhere they are refused.
#       Files in the cgi-bin directory are never sent as static content.
#     - There is no authentication, so by default the server only
#       listens on 127.0.0.1.
#
#     The configuration file can specify:
#     - root      The root directory of the application (default: startup directory)
#     - default   The start-up page (default: index.html)
#     - port      Port to be used (default: 8015)
#     - address   Address to listen on (default: 127.0.0.1)
#     - cgibin    Directory, relative to the root, holding the scripts
#                 and programs that may be run (default: cgi-bin)
#     - encoding  The encoding of the data sent to the client (default: iso8859-1)
#
#     What configuration file:
#     - If the configuration file is not specified on the command-line
#       via "-cfg name", then a file cgiserver.cfg is sought in the
#       current directory.
#     - If it does not exist, the internal defaults are used
#
#     Protocol:
#     The protocol implemented for executable programs (URL: *.exe)
#     is very simple:
#     - The program is started and fed the key-value pairs from the
#       request (on standard input). To dinstinguish requests, lines
#       with %BEGIN% and %END%
#       are added.
#     - The program must write its results to a file "cgiout" and on
#       completion of that file, create a file "cgiready".
#     - The server program takes care of all the rest.
#     The reason for using an external file and not standard output
#     is that there is no buffering problem then and the end of the
#     processing is very clear-cut. That way the program can run in
#     a loop waiting for new requests.
#
#     Known limitations:
#     - The protocol is very simple, it does not cope well with text
#       from a textarea control
#     - No support for multiple instances of the same program at the
#       moment (this requires directory and process management)
#       This means among other things, that the output may get
#       confused if more than process is running at the same time
#     - No checking of the incoming connections, you can do this
#       however by redefining the "answer" procedure in the
#       configuration file
#     - On Windows, if you start this script with wish or a similar
#       shell, the server socket will not be closed properly when
#       terminating the server.
#
#     Ideas for improvements:
#     - Run Tcl scripts in their own interpreter, instead of a separate
#       process?
#     - Add some logging facilities?
#     - Present a graphical screen if Tk is available?
#
#     Note:
#     This server is not intended to become a full-fledged HTTP/CGI
#     server system a la Apache or AOLserver. Only the simplest
#     processing options are implemented.
#

# defaults --
#
set root      [pwd]
set default   index.html
set port      8015
set encoding  iso8859-1
set address   127.0.0.1
set cgibin    cgi-bin

# bgerror --
#     Handle background errors (echo to the screen and to the client)
# Arguments:
#     msg       Error message
# Result:
#     None
#
proc bgerror msg {
    global clientSock
    puts stdout "bgerror: $msg\n$::errorInfo"
    puts $clientSock "HTTP/1.0 200 OK"
    puts $clientSock "Content-Type: text/plain;charset=$::encoding\n"
    puts $clientSock "Processing error: $msg\n$::errorInfo"
    close $clientSock
}

# answer --
#     Accept incoming connections
# Arguments:
#     socketChannel     Channel for the incoming connection
#     host2             Host of the connection
#     port2             Port of the connection
# Result:
#     None
# Note:
#     Right now any connection is accepted
#
proc answer {socketChannel host2 port2} {
  fileevent $socketChannel readable [list serve $socketChannel]
}

# percentDecode --
#     Decode the %XX escapes (UTF-8) in the path of a URL
# Arguments:
#     text      Encoded text
# Result:
#     Decoded text
#
proc percentDecode {text} {
    set bytes ""
    set start 0
    foreach {match hex} [regexp -all -inline -indices {%([0-9A-Fa-f]{2})} $text] {
        append bytes [encoding convertto utf-8 \
            [string range $text $start [expr {[lindex $match 0]-1}]]]
        append bytes [binary format H2 [string range $text {*}$hex]]
        set start [expr {[lindex $match 1]+1}]
    }
    append bytes [encoding convertto utf-8 [string range $text $start end]]
    return [encoding convertfrom utf-8 $bytes]
}

# realPath --
#     Absolute path with symbolic links resolved in every component
#     (file normalize leaves the last component alone)
# Arguments:
#     path      Path to resolve
# Result:
#     Resolved path
#
proc realPath {path} {
    file dirname [file normalize [file join $path __dummy__]]
}

# isBelow --
#     Check that a path lies strictly below a directory
# Arguments:
#     path      Resolved path
#     dir       Resolved directory
# Result:
#     1 if so, 0 otherwise
#
proc isBelow {path dir} {
    if {$::tcl_platform(platform) eq "windows"} {
        set path [string tolower $path]
        set dir  [string tolower $dir]
    }
    set p [file split $path]
    set d [file split $dir]
    set n [llength $d]
    expr {[llength $p] > $n && [lrange $p 0 [expr {$n-1}]] eq $d}
}

# resolveUrlPath --
#     Map the path of a URL onto a file below the root directory
# Arguments:
#     tail      Path part of the URL (starts with /)
# Result:
#     List of the resolved file name and the action (static, tcl or
#     exe), or "" if the request is not allowed
#
proc resolveUrlPath {tail} {
    set path [percentDecode $tail]
    if {[string match */ $path]} {
        append path $::default
    }
    if {[regexp {[\x00-\x1f\x7f\\:]} $path]} {
        return ""
    }

    set parts {}
    foreach part [split $path /] {
        if {$part eq "" || $part eq "."} {
            continue
        }
        if {$part eq ".." || [string match ~* $part]} {
            return ""
        }
        lappend parts $part
    }
    if {[llength $parts] == 0} {
        return ""
    }

    set root [realPath $::root]
    set name [realPath [file join $root {*}$parts]]
    if {![isBelow $name $root]} {
        return ""
    }

    #
    # A symbolic link may not turn a static file into a script/program
    # or the other way around
    #
    set requested [file extension [lindex $parts end]]
    set target    [file extension $name]
    if {$requested ne $target && ($requested in {.tcl .exe} || $target in {.tcl .exe})} {
        return ""
    }

    #
    # The cgi-bin directory must lie within the root; if it does not,
    # nothing is executed
    #
    set cgibin [realPath [file join $root $::cgibin]]
    set incgibin 0
    if {$cgibin eq $root || [isBelow $cgibin $root]} {
        set incgibin [isBelow $name $cgibin]
    }

    switch -- $target {
        .tcl    {set action tcl}
        .exe    {set action exe}
        default {set action static}
    }
    if {$action ne "static"} {
        if {!$incgibin} {
            return ""
        }
    } elseif {($incgibin && $cgibin ne $root) || [string tolower $target] in {.tcl .exe}} {
        #
        # Never send the contents of the cgi-bin directory, nor files
        # that merely differ in case from a script/program (case-insensitive
        # file systems)
        #
        return ""
    }
    return [list $name $action]
}

# forbidden --
#     Refuse the request
# Arguments:
#     sock      Channel for the socket
# Result:
#     None
#
proc forbidden {sock} {
    puts $sock "HTTP/1.0 403 Forbidden"
    puts $sock "Content-Type: text/html;charset=$::encoding\n"
    puts $sock "<html><head><title>Forbidden</title></head>"
    puts $sock "<body><center>"
    puts $sock "Access to the URL you requested is not allowed."
    puts $sock "</center></body></html>"
    close $sock
}

# serve --
#     Handle the incoming request
# Arguments:
#     sock      Channel for the socket
# Result:
#     None
# Side effects:
#     Sends data to the client, may start external programs
# Note:
#     Currently, the external programs are not kept alive
#
proc serve sock {
    global clientSock
    set clientSock $sock ;# For background errors
    fconfigure $sock -blocking 0

    #
    # Get the input from the GET request
    #
    gets $sock line
    puts "Received: $line"

    if { [fblocked $sock] } {
        puts "Socket appears to be blocked - no processing"
        return
    }

    #
    # No more input needed, so construct the proper URL
    # and identify the action to be taken
    #
    fileevent $sock readable ""
    set tail /
    regexp {(/[^ ?]*)(\?[^ ]*)?} $line -> tail args
    set resolved [resolveUrlPath $tail]
    set args [string range $args 1 end]
    if {$resolved eq ""} {
        puts "URL $tail refused"
        forbidden $sock
        return
    }
    lassign $resolved name action

    #
    # The URL must be an existing file:
    # - A Tcl script
    # - An executable (hm, how to deal with platforms that do not use
    #   the extension .exe?)
    # - An HTML file (anything else, actually)
    #
    set exe 0
    set perm r
    if {[file isfile $name] && [file readable $name]} {
        puts $sock "HTTP/1.0 200 OK"
        if {$action eq "tcl"} {
            set ::env(QUERY_STRING) [string range $args 1 end]
            set name [list |tclsh $name]
        } elseif {$action eq "exe"} {
            set exe  1
            set perm w+
            set name [list |$name]
        } else {
           puts $sock "Content-Type: text/html;charset=$::encoding\n"
        }
        set inchan [open $name $perm]
        if { $exe } {
            puts "Executing program $name ..."
            sendHttpData $inchan $args
            waitForProgram
            close $inchan
            set inchan [open "cgiout"]
        }

        #
        # Copy the output to the client
        #
        fconfigure $inchan -translation binary
        fconfigure $sock   -translation binary
        fcopy $inchan $sock -command [list done $inchan $sock]
    } else {
        puts "URL/File $name not found!"
        puts $sock "HTTP/1.0 404 Not found\n"
        puts $sock "<html><head><title>No such URL</title></head>"
        puts $sock "<body><center>"
        puts $sock "The URL you requested does not exist on this site."
        puts $sock "</center></body></html>"
        close $sock
    }
}

# done --
#     Callback for the fcopy command - close the channels
# Arguments:
#     file         Incoming channel
#     sock         Outgoing channel (the client)
#     bytes        Number of bytes that was sent
#     msg          Any message from the fcopy command
proc done {file sock bytes {msg {}}} {
    close $file
    close $sock
}

# sendHttpData --
#     Send key/value pairs to the CGI process
# Arguments:
#     inchan       Channel of incoming process
#     httpdata     HTTP data sent by the client
# Result:
#     None
# Notes:
#     The CGI process receives the data on stdin
#     The data start with the line %BEGIN% and
#     and end with %END%, so that there can be
#     no ambiguity over them
#
proc sendHttpData {inchan httpdata} {
    file delete -force cgiout
    file delete -force cgiready

    puts $inchan %BEGIN%
    foreach keyvalue [split $httpdata "&"] {
        puts $inchan $keyvalue
    }
    puts $inchan %END%
    flush $inchan
}

# waitForProgram --
#     Wait for the program to finish its job
# Arguments:
#     None
# Result:
#     None
#
proc waitForProgram {} {
    checkFile cgiready
    vwait continue
}
proc checkFile {name} {
    if { [file exists $name] } {
        set ::continue 1
    } else {
        after 20 [list checkFile $name]
    }
}

# main --
#     Get the thing started
#

set cfg [lsearch $argv "-cfg"]
if { $cfg >= 0 } {
    incr cfg
    set cfgfile [lindex $argv $cfg]
} else {
    set cfgfile "cgiserver.cfg"
}

if { [file readable $cfgfile] } {
    puts "Using configuration file $cfgfile"
    source $cfgfile
}   else {
    puts "Using default configuration"
}

#
# Check the default URL and start the server
#
if { ! [file readable [file join $root $default]] } {
    puts "Error: could not find startup file/URL -  [file join $root $default]"
    puts "Stopping server"
} else {
    socket -server answer -myaddr $address $port
    puts "Server ready..."
    vwait forever
}
