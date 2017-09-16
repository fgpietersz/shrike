#=Dandelion webserver
#Light weight webserver, using non-blocking sockets. It has just enough functionality to handle GET, POST and HEAD requests.

#GPL licensed. See end of file for copyright notice.

#Request is parsed, checked and passed to handler. The simple static_handler and simple_handler are provided as an example. simple_handler is more intersting, as it shows various information, but reveals more about your server than you probably want exposed on the net.

#The handler mostly receives the same as CGI variables in $headers dict. A major exception is that REQUEST_URI has had query string and fragment removed and been url decoded. Some other things are missing. Please see the output of simple_handler to see what is there.

#The handler should include a "Connection: Close" header as keep alive is not supported. This is done by try_file and the example handler, and looking up the beginning of the response in ::dandelion::first_line is sufficient to ensure this.

#=Features
#*Serves static and dynamic pages.
#*Can handle multiple simultaneous clients
#*Can run multiple servers on different ports in a single process.
#Limitations:
#* Not well tested, although should be acceptable for non-critical sites.
#* Limited testing of performance, which is not expected to be sufficient for busy sites.
#* Implements only a subset of HTTP/1.1. Important omissions below:
#* No keep alive (and therefore no pipelining as well.This is allowed by the standard as long as Connection: Close is sent with each response. 
#*Absolut URLs in requests are not accepted.
#*Chunked requests are not accepted.
#* 100 Continue not sent (may be implemented by handler)
#* If-Modified-Since and If-Unmodified-Since preconditions are ignored for static files. (may be implemented by handler)
# For a site that where reliability really matters, or that has high traffic, this is probably not the right solution.


#=To do
#* Handle absolute requests
#* SSL (tls extension)
#* Improve deny response detail and formatting, and add custom error messages.
#* Test static_handler example and add redirects for directories without "/"
#* Content size and date headers for static files.
#* Host header and absolute request uri.
#*Consider implmenting more responses: 100, 504,....
#*Put more info into headers dict: DOCUMENT_ROOT, REMOTE_ADDR etc.
#*Error logging to file, console or none
#*Add more filters
#*Timeout connections that take too long. [after {close $socket) and undo all [chan configure] should do it, but how to deal with removing the event when the socket closes?.
#*Implement keep-alive. Needs new API with socket closed only by server, not app, and content length header is sent.
#*Pipelining. Should work once keep-alive is implemented.
#*More request types: TRACE and OPTIONS.

#=Usage

#Call ::dandelion::init with name value list of configs, then enter Tcl event loop. If-Modified-Since:  or If-Unmodified-Since: headersSee default configs below for more explanation.

#Example:
#::dandelion::init port 8080 limit 10000 doc_root ~/mypics
#vwait forever

#This will start a server on port 8080 with a request size limit of 10,000 bytes, and serve the contents of the mypics directory in your home directory

#::dandelion::init doc_root ~/public_html
#::dandelion::init doc_root /var/www handler simple_handler port 8081 static_fail 0
#vwait forever

#This will start one server on the default port (8080) to serve the contents of public_html in your home directory.

#It will start another on port 8081 that will serve files from /var/ww, or show the details of a failed request.

#==Filters

#===Socket filter
#Pass ::dandelion::init a filter_sock setting list of lambdas to run, each of which receives the socket handle as the arg.. IP is easilly pulled from the socket handle with [chan configure $sock -peername]. If the lambda returns 1, the response will be terminated (it may be necessary to call [deny] or similar to return some data.
#===Header filter
#For re-directs. List of lambdas in filter_req is iterated over. Each gets a dict of headers and a dict of settings as args, and returns a list of a dict of headers and a dict of settings [list $headers $settings].
#This is the place to do redirects etc.

#work around tcl bug # 2116053
namespace eval tcl::mathfunc namespace export min max

package provide dandelion 1.0
namespace import ::tcl::mathop::*
namespace import ::tcl::mathfunc::*

namespace eval dandelion {

#=Initialise namespace variables

#NB Check whether changes in error codes need to be made to wsinter as well.
#Dict of http error codes
#Check what is actually needed!
variable errors [dict create \
        100 Continue \
        200 OK \
        204 {No Content} \
        400 {Bad Request} \
        404 {Not Found} \
        405 {Method Not Allowed} \
        411 {Length Required} \
        413 {Request Entity Too Large} \
        500 {Internal Server Error} \
        501 {Not Implemented} \
        503 {Service Unavailable} \
        504 {Service Temporarily Unavailable}]

#Dict of actual first lines or response
variable first_line [dict create]
dict for {k v} $errors {
        dict set first_line $k "HTTP/1.1 $k [dict get $errors $k]\nConnection: Close"
}

#Just to avoid overwriting global
variable f
#Get mime types
variable mime [dict create {*}[read [set f [open [file join [file dirname [info script]] mime.txt] r]]] ]

#Dict of ports in use
#If a port is in use by an http server then the port number will be a key and the value will be a dict {sock server-socket root document-root ......}
variable ports [dict create]
#==Default config
#*default: default file to server for directories
#*limit: request size limit in bytes (must be raised for post to be practical)
#* addr: IP address to bind to: 0 for all
#* check_dir: check if requested remains in doc_root (i.e. do not follow symlinks to directories). NB does follow symlinks to files)
#* doc_root: root directory of site
#* port: port on which to open server
#* handler: proc to pass parsed request to. See default for example.
#* static: try serving a static file first (only if the file extension has a mime type).
#* static_fail: what to do if static file cannot be found. 1 to return 404, 0 to fall back to handler
variable config [dict create \
        limit 1024 \
        addr 0 \
        check_dir 0 \
        doc_root [file join ~ public_html] \
        port 8080 \
        handler static_handler \
        static 1 \
        static_fail 1 \
        filter_sock {} \
        filter_req {}]

#Just for pattern matching
variable pattern_sp [subst -nocommands -novariables {[ \t]*}]

#=Procs

#==init
#Start http server
#Calling init on the same port again will close server socket and open new one.
#$args should be name value list to override defaults.
proc init {args} {
        variable ports
        variable config
        #$args overrides config. {*}$config {*}$args does not work.
        set settings [dict create {*}[concat $config $args]]
        dict with settings {
                set doc_root [file normalize $doc_root]
                if {[dict exists $ports $port]} {stop $port}
                set opts [list -server ::dandelion::respond]
                if {$addr != 0} {lappend opts -myaddr $addr}
                lappend opts $port
        }
        dict set settings server_sock [socket {*}$opts]
        #puts "server on [dict get $settings port]"
        dict set ports $port $settings
}

#==stop
#Stop server, remove info from dict.
proc stop {port} {
        variable ports
        close [dict get [dict get $ports $port] server_sock]
        dict unset ports $port
}

#==respond
#Respond to request: Configure socket and call [get_initial] to start reading in data.
#args: socket, client address, client port.
proc respond {sock caddr cport} {
        variable ports
        #Get the server port and use it to get the setting from the dict. Very neat behaviour on the part of [chan configure]/[fconfigure] as we do not need the original file handle.
        set settings [dict get $ports [lindex [chan configure $sock -sockname] 2]]
        chan configure $sock -blocking 0 -buffersize [min [dict get $settings limit] 1000000]
        #filter by socket data: ip is easilly pulled from the scoket name with [chan configure $sock -peername]. A true return value from the lambda expression called will terminate the response.
        foreach i [dict get $settings filter_sock] {
                if {[apply $i $sock]} {return}
        }
        chan event $sock readable [list ::dandelion::get_initial $sock $settings {}]
        return
}

#==get_initial
#Get the initial line of the requests.
proc get_initial {sock settings {line {}}} {
        #Check request size against limit.
        if {([chan pending input $sock]+[string bytelength $line])>[dict get $settings limit]} {
                deny $sock 413 $settings
                return
        }
        #Try to get the line.
        append line [gets $sock]
        #If we got a partial line, complete when more is available.
        if {[chan blocked $sock]} {
                chan event $sock [list ::dandelion::get_initial $sock $settings $line]
                return
        }
        set size [string bytelength $line]
        set line [split [string trim $line]]
        #Should only be three elements in the inital line.
        if {[llength $line] > 3} {
                deny $sock 400 $settings
                return
        }
        #Reject request for files if request uri is absolute. This should be extended to proper http 1.1 support.
        set uri [lindex $line 1]
        if {[string index $uri 0] ne {/}} {
                deny $sock 400 $settings
                return
        }
        #Strip fragment.
        if {[string first # $uri] != -1} {
                set uri [string range $uri 0 [string first # $uri]]
        }
        #Separate query string.
        lassign [split $uri ?] path query
        if {[string first {./} $uri] != -1} {
                deny $sock 404 $settings
                return
        }
        chan event $sock readable [list ::dandelion::get_headers $sock $size [dict create REQUEST_METHOD [lindex $line 0] REQUEST_URI $uri SCRIPT_NAME [decode $path] QUERY_STRING $query SERVER_PROTOCOL [lindex $line 2]] $settings {}]
        return
}

# A helper procedure to read from a socket but also check for an error
proc get_sock_line { sock line } {
        upvar $line line_out
    if { [catch { set num_bytes [gets $sock line_out]}] } {
        #puts "Error reading socket.  Closing."
        return -1
    }
    return $num_bytes
}

#==get_headers
#Get headers a line at a time. Once complete either pass to try_file or handler, or read body.
#Args: socket bytes-data-read dict-of-headers settings partial-line-if-any
proc get_headers {sock size headers settings {partial {}}} {
        variable pattern_sp
        #If what is in the buffer, and what has already been processed, exceeds the limit, send back a 413
        if {([chan pending input $sock] + $size) > [dict get $settings limit]} {
                deny $sock 413 $settings
                return
        }
        while {[get_sock_line $sock line] >= 0} {
                #Deal with partial line: Store data in event handler. Increase size by data read from buffer
                #Increase size by data read from buffer. Add two bytes for discarded cr-lf newline.
                set size [+ $size [string bytelength $line] 2]
                if { "$partial$line" eq {}} {
                        #headers done, adjust to match CGI, then get body (if any!)
                        foreach i [list CONTENT_TYPE CONTENT_LENGTH] {
                                if [dict exists $headers HTTP_$i] {
                                        dict set headers $i [dict get  $headers HTTP_$i]
                                }
                        }
                        foreach i [dict get $settings filter_req] {
                                lassign [apply $i $headers $settings] headers settings
                                if {[dict size $settings] == 0} {return}
                        }
                        #If the request is a GET or HEAD, respond immediately. If a POST get the body. If none of these, reply not implemented.
                        switch [dict get $headers REQUEST_METHOD] {
                                GET -
                                DELETE -
                                HEAD {
                                        if {[dict get $settings static] && [try_file $sock $headers $settings]} {return}
                                        [dict get $settings handler] $sock $headers $settings {}
                                        return
                                }
                                PUT -
                                POST {
                                        #check length within limit
                                        if {[dict exists $headers CONTENT_LENGTH]} {
                                                if {([dict get $headers CONTENT_LENGTH] + $size)  > [dict get $settings limit]} {
                                                        deny $sock 413 $settings
                                                        return
                                                }
                                        } else {
                                                #insist on content length for post
                                                deny $sock 411 $settings
                                                return
                                        }
                                        #Change encoding to match content type
                                        if {[dict exists $headers CONTENT_TYPE]} {
                                                if {[string match multipart/* [dict get $headers CONTENT_TYPE]]} {
                                                        chan configure $sock -encoding binary -translation binary
                                                }
                                        }
                                        ::dandelion::get_body $sock $size $headers $settings {}
                                        return
                                }
                                default {
                                        deny $sock 501 $settings
                                        return
                                }
                        }
                }
                #Check for continuation line. Inefficient but not common case
                if {[string match $pattern_sp $line]} {
                        dict append headers [lindex [dict keys $headers] end] " $line"
                        continue
                }
                #Increase size by data read from buffer. 2 bytes for discarded cr-lf newline.
                set size [+ $size [string bytelength $partial$line] 2]
                #Parse header, add to dict. dashes to underscores like CGI, combine multiple values. Combine multiple values.
                set index [string first : $line]
                set key HTTP_[string map {- _} [string toupper [string trim [string range $line 0 [- $index 1]]]]]
                if {[dict exists $headers $key]} {
                        dict append headers $key ,[string trim [string range $line [+ $index 1] end]]
                } else {
                        dict set headers $key [string trim [string range $line [+ $index 1] end]]
                }
        }
        if {[chan blocked $sock]} {
                chan event $sock readable [list ::dandelion::get_headers $sock [+ $size [string bytelength $partial]] $headers $settings $partial$line]
                return
        }
        #if it is not blocking and headers have not finished and there is nothing to read, something is wrong with the request
    #puts "Something is wrong.  headers dict is:"
    #puts "$headers"
        #deny $sock 400 $settings
        catch { close $sock }
        return
}

#==get_body
#Read request body, try to serve static file, if that fails adjust headers and pass to handler.
#Args: socket, size read so far, dict of headers, server settings.
proc get_body {sock size headers settings {body {}}} {
        if {([chan pending input $sock] + $size + [string bytelength $body]) > [dict get $settings limit]} {
                deny $sock 413 $settings
                return
        }
        set data [read $sock]
        if {[string bytelength $data] == 0 && [chan blocked $sock]} {
                chan event $sock readable [list ::dandelion::get_body $sock $size $headers $settings $body]
                return
        } else {
                append body $data
        }
        #nothing left to read, so try returning file, if cannot adjust headers to CGI var names and pass to handler
        if {[chan configure $sock -encoding] eq {binary}} {
                set blength [string length $body]
        } else {
                set blength [string bytelength $body]
        }
        if {[dict exists $headers CONTENT_LENGTH] && ($blength < [dict get $headers CONTENT_LENGTH])} {
                chan event $sock readable [list ::dandelion::get_body $sock $size $headers $settings $body]
                return
        }
        [dict get $settings handler] $sock $headers $settings $body
        return
}

#==try_file
#Try to return a file. If file exists, return file and close socket. If file does not exist, then pass to handler to respond dynamically. returns 1 if it responds, 0 otherwise.
proc try_file {sock headers settings} {
        #get mime type. If it does not have a mime type do not serve and go to handler
        variable mime
        set ext [file extension [dict get $headers SCRIPT_NAME]]
        if {[dict exists $mime $ext]} {
                set mime_type [dict get $mime $ext]
        } else {
                return 0
        }
        #normalised request path
        set path [file normalize [file join [dict get $settings doc_root] [string trimleft [dict get $headers SCRIPT_NAME] /]]]
        #ensure within doc_root
        if {[dict get $settings check_dir] && (![string equal [string range $path 0 [- [string length [dict get $settings doc_root]] 1]] [dict get $settings doc_root]])} {
                deny $sock 404 $settings
                #return 1 to say response sent
                return 1
        }
        #open file, if error, return 0 so handler is used. Return 404 or fallback to handler depending on settings.
        if {[catch {file stat $path attribs}]} {
                if {[dict get $settings static_fail]} {
                        deny $sock 404 $settings
                        return 1
                } else {
                        #add back looking for default file for directory here. Check if directory and call directory handler?
                        return 0
                }
        }
        switch [dict get $headers REQUEST_METHOD] {
                GET {
                        send_head $sock $mime_type
                        set fd [open $path r]
                        # FG ++
                        # Don't check for mime_type - just always return files exactly as they exist on the server
                        #if {![string match text/* $mime_type]} {
                                chan configure $sock -encoding binary -translation binary
                                chan configure $fd -encoding binary -translation binary
                        #}
                        chan copy $fd $sock -command [list ::dandelion::file_done $fd $sock]
                }
                HEAD {
                        #just send the header
                        send_head $sock $mime_type
                        close $sock
                }
                default {
                        deny $sock 405 $settings
                        close $sock
                }
        }
        return 1
}

#==send_head
#send headers for a file sent by try_file
#connection close is required to make it clear keep alive and pipelining are not supported.
proc send_head {sock mime {cache_control "public, max-age=31536000"}} {
        upvar attribs attribs
        variable first_line
        puts $sock [dict get $first_line 200]
        puts $sock "Content-Type: $mime"
        puts $sock [clock format [clock seconds] -format {Date: %a, %d %b %Y %T GMT} -timezone :UTC]
        puts $sock [clock format $attribs(mtime) -format {Last-Modified: %a, %d %b %Y %T GMT} -timezone :UTC]
        puts $sock "Cache-Control: $cache_control"
        puts $sock "Content-Length: $attribs(size)\n"
}

#==file_done
#Helper proc to allow try_file to close channels after file is done.
#Needs extending with error handler?
proc file_done {fd sock bytes {error {}}} {
        close $fd
        close $sock
}

#==decode
#This decodes data in www-url-encoded format. It is a bit ugly to include it, but doing so removes the only external dependency.
proc decode {str} {
    # rewrite "+" back to space
    # protect \ from quoting another '\'
    set str [string map [list + { } "\\" "\\\\"] $str]

    # convert %HH to \uxxx, process the escapes, and convert from utf-8
    return [encoding convertfrom utf-8 [subst -novar -nocommand [regsub -all -- {%([A-Fa-f0-9][A-Fa-f0-9])} $str {\\u00\1}]]]
}


#==deny
#Refuse access for whatever reason: e.g. large request that might be DOS
#args: sock: socket, code: http response code, settings: server settings.
proc deny {sock code settings} {
        #expand to be able to return custom error page from file
        variable first_line
        variable errors
        puts $sock [dict get $first_line $code]
        puts $sock "Content-Type: text/html\n"
        set msg [dict get $errors $code]
        puts $sock "<html><head><title>$msg</title></head></html><h1>$msg</h1>"
        close $sock 
}

#=Examples and tests

#==static_handler
#Just serve static files, provided as a safe default.
####needs testing
proc static_handler {sock headers settings body} {
        dict set headers SCRIPT_NAME [file join [dict get $headers SCRIPT_NAME] index.html]
        if {[try_file $sock $headers $settings]} {return}
        deny $sock 404 $settings
}

#==simple_handler
#Simple example of how a handler works.
proc simple_handler {sock headers settings body} {
        variable first_line
        puts $sock [dict get $first_line 404]\n
        puts $sock {<html><head><title>Show request</title></head><body><h1>Headers</h1><table>}
        dict for {name value} $headers {
                puts $sock "<tr><td>$name</td><td>$value</td></tr>"
        }
        puts $sock {</table><h1>Settings</h1><table>}
        dict for {name value} $settings {
                puts $sock "<tr><td>$name</td><td>$value</td></tr>"
        }
        puts $sock {</table><h1>Body</h1><table>}
        puts $sock $body
        puts $sock {</body></html>}
        catch { close $sock }
        return
}

#End namespace
}

#==Copyright
#Copyright Graeme Pietersz 2008
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 2 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>
