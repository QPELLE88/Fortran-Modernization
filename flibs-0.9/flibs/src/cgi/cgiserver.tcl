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
#     TODO: use cgi-bin/* instead of the extension
#
#     The configuration file can specify:
#     - root      The root directory of the application (default: startup directory)
#     - default   The start-up page (default: index.html)
#     - port      Port to be used (default: 80)
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
#       moment (this requires directory and process management).
#       Because the protocol uses the fixed files "cgiout" and
#       "cgiready", requests for programs are queued and handled
#       strictly one at a time. A program that does not finish within
#       "exeTimeout" seconds is killed so that it cannot block the queue.
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
    if {[string match */ $tail]} {append tail $::default}
    set name [string map {%20 " "} $::root$tail]
    set args [string range $args 1 end]

    #
    # The URL must be an existing file:
    # - A Tcl script
    # - An executable (hm, how to deal with platforms that do not use
    #   the extension .exe?)
    # - An HTML file (anything else, actually)
    #
    set exe 0
    set perm r
    if {[file readable $name]} {
        puts $sock "HTTP/1.0 200 OK"
        if {[file extension $name] eq ".tcl"} {
            set ::env(QUERY_STRING) [string range $args 1 end]
            set name [list |tclsh $name]
        } elseif {[file extension $name] eq ".exe"} {
            set exe  1
            set perm w+
            set name [list |$name]
        } else {
           puts $sock "Content-Type: text/html;charset=$::encoding\n"
        }
        if { $exe } {
            queueProgram $sock $name $args
            return
        }
        set inchan [open $name $perm]

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

# Queue of pending requests for programs (*.exe) and a flag that
# is set while one of them is being handled. The protocol uses the
# fixed files "cgiout" and "cgiready", so only one program request
# may be in progress at any time. exeTimeout is the maximum time (in
# seconds) a program may take before it is killed.
#
set exeQueue   {}
set exeBusy    0
set exeTimeout 60

# queueProgram --
#     Register a request for a program and start it if none is running
# Arguments:
#     sock         Channel for the socket
#     name         Command to open as a pipe
#     httpdata     HTTP data sent by the client
# Result:
#     None
#
proc queueProgram {sock name httpdata} {
    lappend ::exeQueue [list $sock $name $httpdata]
    runNextProgram
}

# runNextProgram --
#     Start the next queued program request, if no other one is active
# Arguments:
#     None
# Result:
#     None
#
proc runNextProgram {} {
    if { $::exeBusy || [llength $::exeQueue] == 0 } {
        return
    }
    set ::exeBusy  1
    set request    [lindex $::exeQueue 0]
    set ::exeQueue [lrange $::exeQueue 1 end]
    lassign $request sock name httpdata

    if { [catch {startProgram $sock $name $httpdata} msg] } {
        programError $sock $msg
    }
}

# startProgram --
#     Remove the previous output files, start the program and send it
#     the request data
# Arguments:
#     sock         Channel for the socket
#     name         Command to open as a pipe
#     httpdata     HTTP data sent by the client
# Result:
#     None
# Note:
#     Where possible the program is started in its own process group
#     (via setsid), so that stopProgram can also kill any processes
#     it started
#
proc startProgram {sock name httpdata} {
    file delete -force cgiout
    file delete -force cgiready

    if { $::tcl_platform(platform) eq "unix" && [auto_execok setsid] ne "" } {
        set name [list |setsid [string range [lindex $name 0] 1 end]]
    }

    puts "Executing program $name ..."
    set inchan [open $name w+]
    if { [catch {sendHttpData $inchan $httpdata} msg] } {
        stopProgram $inchan
        error $msg
    }
    set deadline [expr {[clock milliseconds] + 1000 * $::exeTimeout}]
    waitForProgram $sock $inchan $deadline
}

# waitForProgram --
#     Poll for the file "cgiready" without nesting the event loop, then
#     send the program's output to its own client
# Arguments:
#     sock         Channel for the socket
#     inchan       Channel of the program
#     deadline     Time (in ms) after which the program is abandoned
# Result:
#     None
# Note:
#     Once "cgiready" exists, "cgiout" is opened, the program (and its
#     process group) is killed and both files are removed before the
#     next request is started. The open channel is then streamed to
#     the client, so neither a slow client nor a lingering process can
#     affect another request
#
proc waitForProgram {sock inchan deadline} {
    set released 0
    if { [catch {
        if { [file exists cgiready] } {
            set outfile [open "cgiout"]
            fconfigure $outfile -translation binary
            stopProgram $inchan
            if { [file exists cgiout] } {
                # The open file could not be removed (e.g. on Windows)
                set data [read $outfile]
                close $outfile
                unset outfile
                file delete -force cgiout
            }
            set released 1
            releaseProgram
            if { [info exists data] } {
                sendProgramOutput $sock $data
            } else {
                fconfigure $sock -translation binary
                fcopy $outfile $sock -command [list done $outfile $sock]
            }
        } elseif { [clock milliseconds] > $deadline } {
            stopProgram $inchan
            error "program did not finish within $::exeTimeout seconds"
        } else {
            after 20 [list waitForProgram $sock $inchan $deadline]
        }
    } msg] } {
        if { [info exists outfile] } {
            catch {close $outfile}
        }
        if { ! $released } {
            stopProgram $inchan
            programError $sock $msg
        } else {
            puts "Error sending program output: $msg"
            catch {close $sock}
        }
    }
}

# stopProgram --
#     Kill a program (and its process group) and remove the files
#     "cgiout" and "cgiready", so that it cannot write them while a
#     later request is running
# Arguments:
#     inchan       Channel of the program
# Result:
#     None
#
proc stopProgram {inchan} {
    catch {
        foreach p [pid $inchan] {
            catch {exec kill -KILL -- -$p}
            catch {exec kill -KILL $p}
        }
    }
    catch {
        fconfigure $inchan -blocking 0
        close $inchan
    }
    catch {file delete -force cgiout}
    catch {file delete -force cgiready}
}

# sendProgramOutput --
#     Send the output of a program to its client in the background
# Arguments:
#     sock         Channel for the socket
#     data         Output of the program
# Result:
#     None
#
proc sendProgramOutput {sock data} {
    fconfigure $sock -translation binary -blocking 0
    puts -nonewline $sock $data
    close $sock
}

# programError --
#     Report a failed program request to its client (details are
#     only logged) and start the next request
# Arguments:
#     sock         Channel for the socket
#     msg          Error message
# Result:
#     None
#
proc programError {sock msg} {
    puts "Error running program: $msg"
    catch {
        fconfigure $sock -translation binary
        puts -nonewline $sock "Content-Type: text/plain;charset=$::encoding\n\n"
        puts -nonewline $sock "Processing error: could not run program\n"
        close $sock
    }
    releaseProgram
}

# releaseProgram --
#     Mark the active program request as finished and start the next one
# Arguments:
#     None
# Result:
#     None
#
proc releaseProgram {} {
    set ::exeBusy 0
    after idle runNextProgram
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
    puts $inchan %BEGIN%
    foreach keyvalue [split $httpdata "&"] {
        puts $inchan $keyvalue
    }
    puts $inchan %END%
    flush $inchan
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
    puts "Using configuration file $cgfile"
    source $cgifile
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
    socket -server answer $port
    puts "Server ready..."
    vwait forever
}
