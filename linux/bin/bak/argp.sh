#!/usr/bin/env bash
# https://gitlab.com/wef/dotfiles/-/blob/master/bin/argp.sh

TIME_STAMP="20230410.133538"

# Copyright (C) 2019-2021 Bob Hepple < bob dot hepple at gmail dot com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

# Certainly would not work on plain old sh, csh, ksh ... .

ARGP_argp_sh_usage() {
    cat <<"EOF"
Usage: argp.sh or argp

A wrapper for getopt(1) which simulates argp(3) for bash. Normally
other scripts pipe their option descriptions in here for automatic
help and man-page production and for generating the code for
getopt(1). Requires bash-3+. See also argp(1) which is a binary (and
much faster) version of this.

# https://gitlab.com/wef/dotfiles/-/blob/master/bin/argp.sh
# http://bhepple.com/doku/doku.php?id=unixscripts:2-argp.sh

It buys you:

o all the goodness of getopt
o define options in one central place together with descriptions using a
  simple flat file format or XML.
o fewer 'magic' and duplicated values in your code
o automatic consistency between the getopt calling parameters, the case
  statement processing the user's options and the help/man pages
o less spaghetti in your own code
o easier to maintain
o help-page and man-page printing - all from the same data sources
o checking of option consistency at runtime
o range checking of option values at runtime
o pretty easy to use
o portable to OS's without long option support - the help page
  adapts too

###############################################################################

Usage: argp.sh / argp reads its configuration from stdin (fd0) (see
below for details) and outputs the following:

* --help output on stdout fd1
* errors on stderr fd2
* a list of commands on fd3

The calling program can then 'eval' that list of commands in order to
set environment variables as set by the user in command line options.
Also, any remaining arguments after the options are removed are set to
$1 $2 ...

eg. if the user typed 'myprog --foobar --barfoo=three a b c' then after
'eval'ing the fd3 output of argp.sh / argp you would have
FOOBAR="set"
BARFOO="three"
$1 $2 ... = a b c

Usually, the calling program would invoke argp.sh / argp like this:

    exec 4>&1
    eval "$(echo "$ARGS" | argp.sh "$@" 3>&1 1>&4 || echo exit $? )"
    exec 4>&-

Here's a bit of an explanation in case you're interested:
    exec 4>&1  # make fd4 a copy of fd1 stdout eg /dev/pts/0, the terminal
    eval $(
        # inside this sub-shell fd1 is now a pipe
        echo "$ARGS" |

        # 3>&1: connect fd3 to fd1 (the pipe) so that anything printed
        # on fd3 is fed to the pipe (and eventually eval'd).
        #
        # 1>&4: fd1 is connected to fd4 ie /dev/pts/0 so that anything
        # that argp.sh / argp prints on stdout (eg the --help output) ends up
        # on the terminal:

        argp.sh "$@" 3>&1 1>&4 || echo exit $?
    )
    exec 4>&- # close fd4

###############################################################################

It's easier to look at a running script but here's the manual:

###############################################################################
# this is a comment.

# The following line defines our program name. We might also write
# this as ARGP_PROG=$(basename "$0"):
ARGP_PROG=my-script

# '--quiet' is a built-in option, but we normally delete it if 'verbose' is
# supported.
ARGP_DELETE=quiet

# this defines the program's version string to be displayed by the -V option:
ARGP_VERSION=1.6

# this string is added as a prefix to all option names on output, eg
if you need to use an option that clashes with a system environment
parameter eg --home:
ARGP_PREFIX=FOO_

# if this is set then multiple invocations of a string or array option will
# be concatenated using this separator eg '-s s -s b' will give 'a:b'
ARGP_OPTION_SEP=:

# Here are the options that this program will accept:
########       ##### ###     #### #####   ###########
# name         sname arg     type range   description
########       ##### ###     #### #####   ###########

name=default : 'name' must be present and unique. 'default' must be given but
               may be ''

sname        : the single-letter short name of the option (use '' if a short
               name is not needed)

arg          : the name of the argument to be used in --help (not for
               booleans)

type         : b (boolean), i (integer), d (double), s[:] (string),
               a[:] (array) or u (url). Default is boolean if arg is
               not given, otherwise string. If a boolean is
               initialised to '0', then it is incremented every time
               it is given. If a boolean is initialised to anything
               else then it flips between 'default' and 'range' every
               time it is given. If 's:' or 'a:' then ':' overrides
               ARGP_OPTION_SEP for this option. Any syntactically
               clean string can be used instead of ':' ie not ' or ".

range        : for numeric options: min:max eg 1.2:4.5 or min-max eg 1-3
               for string options: an extended regexp
               for array options: a space separated list of alternates
               in quotes "..." or '...'
               for boolean options: the value to assign the option when set

desc         : leave empty for hidden options. Long descriptions can
               span lines by putting '\' at the end of the line and by
               terminated the description with a '.' in the first
               column.

###############################################################################
Examples:

# This is the simplest possible option definition. It specifies a
# hidden option (--hidden) which does not appear in the help or man
# pages. It defaults to being a simple flag (boolean) with a default
# value of '' and 'set' if --hidden is on the command line.
HIDDEN=

# a boolean option with a short name and a numeric default which is
# incremented every time the --bool, -b option is given:
BOOL='0'       b     ''      b    ''      description of this option

# this is a (boolean) flag which gets set to the string 'foobar' when
# --flag, -f is on the command line otherwise it is set to 'barfoo':
FLAG='barfoo' f    ''      b    'foobar'  a flag

# here is an integer value option which must sit in a given range:
INT=1         i     units   i    '1:3'  an integer.

# here is an array value ie just a string which must take one of
# the values in the 'range':
ARRAY=a        a     widgets a    'a b'  an array

# this option is a simple string which will be checked against a regex(7):
STRING=''      s     string  s    '^foo.*bar$|^$'  a string.

# a double value which is checked against a range:
DOUBLE=1       d     miles   d    0.1:1.3 a double.

# this is a URL which will be checked against the URL regex and has a
# default value
URL='http://www.foobar.com'     u     url     u    ''      a url.

# delete this one as we want to use the 'q' option for something else:
ARGP_DELETE=quiet

# this uses the same short letter 'q' as the --quiet option which was
# deleted above
QUAINT=''      q     ''      s    ''      a quaint description

# here we define the non-option arguments that the command takes:
ARGP_ARGS= [--] [args]
ARGP_SHORT=This is a short description for the first line of the man page.
ARGP_USAGE=
This is a longer description.
Spring the flangen dump. Spring the flingen dump. Spring the flangen
dump. Spring the flingen dump.

###############################################################################

XML can also be instead of the above (provided xmlstarlet is available):

<?xml version="1.0" encoding="UTF-8"?>
<argp>
  <prog>fs</prog>
  <args>[--] [pattern]</args>
  <short>Search for filenames in sub-directories.</short>
  <delete>quiet</delete>
  <version>1.2</version>
  <prefix>FOO_</prefix>
  <usage>Presently this is just a shorthand for:

find . -follow \$EXCLUDE -type $TYPE -name '*pattern*' -print 2>/dev/null |sort
  </usage>
  <option name="EXCLUDE" sname="x" type="s" arg="directory">exclude directory</option>
  <option name="DIR"     sname="d" type="b" default="f" range="d">search for a directory rather than a file</option>
  <option name="SECRET" type="b"/>
</argp>

Note that the attribute 'name' is required - the others are all
optional. Also the option value should be on a single line (the
program usage can be on multiple lines).

###############################################################################

Note that --verbose, --help, --quiet and --version options will be
added automatically. Also, a hidden option '--print-man-page' is
provided to print a skeleton man(1) page which can be used as the
starting point for a full man page. Another hidden option
'--print-xml' prints out the XML equivalent of the argp input.

If POSIXLY_CORRECT is set, then option parsing will end on the first
non-option argument (eg like ssh(1)).

###############################################################################

Here is a sample of the output when the command is called with --help:

Usage: my-script [OPTION...] [--] [args]
This is a longer description. Spring the flangen dump. Spring the
flingen dump. Spring the flangen dump. Spring the flingen dump."

Options:

  -a, --array=<widgets>      an array Must be of type 'a'. Must be in the range
                             'a b'.
  -b, --bool                 description of this option
  -d, --double=<miles>       a double. Must be of type 'd'. Must be in the
                             range '0.1-1.3'.
  -f, --flag                 a flag
  -i, --int=<units>          an integer. Must be of type 'i'. Must be in the
                             range '1-3'.
  -q, --quaint               a quaint description Must fit the regex ''.
  -s, --string=<string>      a string. Must fit the regex '^foo.*bar$|^$'.
  -u, --url=<url>            a url. Must fit the regex
'^(nfs|http|https|ftp|file)://[[:alnum:]_.-]*[^[:space:]]*$'.
  -v, --verbose              be verbose
  -h, --help                 print this help message
  -V, --version              print version and exit

EOF
}

argp_sh_version() {
    echo "$GARGP_VERSION"
}

print_array() {
    local i
    let n=0
    for i in "$@"; do printf %s " [$n]='$i'"; let n+=1; done
}

debug_args() {
    {
        printf %s "${FUNCNAME[1]} "
        print_array "$@"
        echo
    } >&2
}

abend() {
    STAT=$1; shift
    echo "$ARGP_PROG: $*" | fmt -w "$GARGP_RMARGIN" >&2
    echo "exit $STAT;" >&3
    exit "$STAT"
}

# first param (name) can be an env or an option name
add_opt() {
    local name default desc sopt arg lopt type range
    name=$(convert_to_env_name "$1")
    lopt=$(convert_to_option_name "$1") # long name of the option
    default=${2:-}
    sopt="${3:-}" # short option letter - optional
    arg="${4:-}" # argument label - optional
    type="${5:-}" # type of the argument - optional
    range="${6:-}" # range for the argument - optional
    desc="${7:-}" # essential except for 'silent/secret' options
    local allowed_chars='^[a-zA-Z0-9_][a-zA-Z0-9_]*$'
    local opt

    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    [[ "$name" ]] || abend 1 "$ARGP_PROG: argp.sh: add_opt requires a name"

    [[ "$name" =~ $allowed_chars ]] || {
        abend 1 "argp.sh: apt_opt: name (\"$name\") must obey the regexp $allowed_chars"
    }

    export ARGP_ARG_"$name"="$arg"
    export ARGP_DEFAULT_"$name"="$default"
    
    # this would boom! if [[ "$name" == "name" ]] ... but since all
    # option names are up-shifted, using lowercase variable names
    # keeps us safe.
    export "$name"="$default"

    # check it's not already in use
    for opt in ${ARGP_OPTION_LIST:-}; do
        if [[ "$name" == "$opt" ]]; then
            abend 1 "$ARGP_PROG: argp.sh: ${FUNCNAME[0]}: option name \"$name\" is already in use"
        fi
        # check that the (short) option letter is not already in use:
        if [[ "$sopt" ]]; then
            if [[ "$sopt" == "$(get_opt_letter "$opt")" ]]; then
                abend 1 "$ARGP_PROG: argp.sh: ${FUNCNAME[0]}: short option \"$sopt\" is already in use by $opt"
            fi
        fi
    done

    if [[ "$sopt" ]]; then
        [[ ${#sopt} -ne 1 ]] && {
            abend 1 "$ARGP_PROG: argp.sh: ${FUNCNAME[0]}: short option \"$sopt\" for option $name is not a single character"
        }
        export ARGP_SOPT_"$name"="$sopt"
    fi
    export ARGP_LOPT_"$name"="$lopt"

    if [[ "$desc" ]]; then
        export ARGP_DESC_OPT_"$name"="$desc"
    fi

    # shellcheck disable=SC2018
    # shellcheck disable=SC2019
    type=$( echo "$type"| tr 'A-Z' 'a-z' )
    # use a while loop just for the 'break':
    while true; do
        case "$type" in
            b)
                break
                ;;
            i)
                [[ "$range" ]] || break
                echo "$range" | grep -Eq "$GARGP_INT_RANGE_REGEX" && break
                ;;
            r|f|d)
                [[ "$range" ]] || break
                echo "$range" | grep -Eq "$GARGP_FLOAT_RANGE_REGEX" && break
                ;;
            s*)
                [[ "$range" ]] || break
                # just test the regex:
                echo "" | grep -Eq "$range"
                [[ $? -eq 2 ]] || break
                ;;
            a*)
                [[ "$range" ]] && break
                ;;
            u*)
                local sep=${type:1:1}
                type=s$sep
                range="$GARGP_URL_REGEX"
                break
                ;;
            "")
                type=s
                break
                ;;
        esac
        abend 1 "$ARGP_PROG: argp.sh: ${FUNCNAME[0]}: bad argument type ('$type') or range ('$range') for option '$name'."
    done
    export ARGP_TYPE_"$name"="$type"
    export ARGP_RANGE_"$name"="$range"
    export ARGP_WAS_SET_"$name"=""

    ARGP_OPTION_LIST="${ARGP_OPTION_LIST:-} $name"
}

get_opt_letter() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    local name="$1"
    local l="ARGP_SOPT_$name"
    printf %s  "${!l:-}"
    [[ "$ARGP_DEBUG" ]] && echo "returning ${!l:-}" >&2
}

get_opt_string() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    local name="$1"
    local l="ARGP_LOPT_$name"
    printf %s  "${!l:-}"
}

get_opt_arg() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    local name="$1"
    local l="ARGP_ARG_$name"
    printf %s "${!l:-}"
}

# convert an environment name (eg ARGP_DEBUG) to a long option name (eg argp-debug)
convert_to_option_name() {
    echo "$1" | tr '[:upper:]_' '[:lower:]-'
}

convert_to_env_name() {
    echo "$1" | tr '[:lower:]-' '[:upper:]_'
}

get_opt_type() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    local name="$1"
    local t="ARGP_TYPE_$name"

    printf %s "${!t:-}"
}

get_opt_range() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    local name="$1"
    local r="ARGP_RANGE_$name"

    printf %s "${!r:-}"
}

get_opt_was_set() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    local name="$1"
    local r="ARGP_WAS_SET_$name"

    printf %s "${!r:-}"
}

get_opt_default() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    local name="$1"
    local d=ARGP_DEFAULT_$name
    local default="${!d}"
    printf %s "${default:-}"
}

get_opt_raw_desc() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    local name="$1"
    local l="ARGP_DESC_OPT_$name"
    printf %s "${!l:-}"
}

get_opt_desc() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    local name
    name="$1"
    local l
    l=$( get_opt_raw_desc "$name" )
    [[ -z "$l" ]] && return 0 # hidden option

    printf %s "$l"
    local type
    type=$(get_opt_type "$name")
    local range
    range=$(get_opt_range "$name")

    default=$(get_opt_default "$name")
    [[ "$default" ]] && echo -n " Default is '$default'."

    if  [[ "$type" && "$type" != "b" && "$type" != "h" ]]; then
        if [[ "$type" != s* || "$range" ]]; then
            if [[ "$type" != s* ]]; then
                echo -n " Must be of type '$type'."
            fi
            if [[ "$range" ]]; then
                case "$type" in
                    s*)
                        echo -n " Must fit the regex '$range'."
                        ;;
                    *)
                        echo -n " Must be in the range '$range'."
                        ;;
                esac
            fi
        fi
    fi
}

# allow them to specify the long option name or the environment parameter
del_opt() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"

    local opt_name pre env_name
    for opt_name in $@; do
        env_name=$(convert_to_env_name "$opt_name")
        opt_name=$(convert_to_option_name "$opt_name")
        for pre in ARGP_SOPT_ ARGP_LOPT_ ARGP_DESC_OPT_ ARGP_ARG_  ARGP_TYPE_ ARGP_RANGE_ ARGP_WAS_SET_; do
            local n=$pre$env_name
            [[ "${!n:-}" ]] && unset "$pre$env_name"
        done
        local opt_list opt
        opt_list=""
        for opt in ${ARGP_OPTION_LIST:-}; do
            [[ "$opt" == "$env_name" ]] || opt_list="$opt_list $opt"
        done
        ARGP_OPTION_LIST="$opt_list"

        [[ "$opt_name" == "$GARGP_HELP_loption" ]] && {
            GARGP_HELP_option=
            GARGP_HELP_loption=
        }
        [[ "$opt_name" == "$GARGP_VERSION_loption" ]] && {
            GARGP_VERSION_option=
            GARGP_VERSION_loption=
        }
        [[ "$opt_name" == "$GARGP_PRINTMAN_loption" ]] && {
            GARGP_PRINTMAN_loption=
        }
        [[ "$opt_name" == "$GARGP_PRINTXML_loption" ]] && {
            GARGP_PRINTXML_loption=
        }
    done
}

# prints only short options that take no parameter
print_short_flags() {
    local name desc l a flags=""

    for name in ${ARGP_OPTION_LIST:-}; do
        [[ "$name" == $( convert_to_env_name "$GARGP_ENDOPTS_loption") ]] && continue
        desc=$(get_opt_desc "$name")
        [[ "$desc" ]] || continue
        l=$(get_opt_letter "$name")
        [[ "$l" ]] || continue
        a=$(get_opt_arg "$name")
        [[ "$a" ]] && continue
        flags="$flags$l"
    done
    printf %s "$flags"
}

# prints only long options that take no parameter
print_long_flags() {
    local name desc l a
    local flags=""
    local space=""

    for name in ${ARGP_OPTION_LIST:-}; do
        desc=$(get_opt_desc "$name")
        [[ "$desc" ]] || continue
        l=$(get_opt_string "$name")
        [[ "$l" == "$GARGP_ENDOPTS_loption" ]] && continue
        [[ "$l" ]] || continue
        a=$(get_opt_arg "$name")
        [[ "$a" ]] && continue
        printf -- "$space--%s" "$l"
        space=" "
    done
    printf %s "$flags"
}

# prints short and long options that take a parameter
print_man_all_args() {
    local name fmt
    for name in ${ARGP_OPTION_LIST:-}; do
        local desc
        desc=$(get_opt_desc "$name")
        [[ "$desc" ]] || continue
        local arg
        arg=$(get_opt_arg "$name")
        [[ "$arg" ]] || continue

        local sopt
        sopt=$(get_opt_letter "$name")
        local lopt
        lopt=$(get_opt_string "$name")

        echo -n " ["
        [[ "$sopt" ]] && printf -- '\\fB\-%s\\fP' "$sopt"
        [[ "$sopt" && "$lopt" ]] && echo -n ","
        printf -- '\\fB\-\-%s\\fP=\\fI%s\\fP' "$lopt" "$arg"
        echo -n "]"
    done
}

# prints short and long options that take a parameter - only used for
# default usage message
print_all_args() {
    local name
    for name in ${ARGP_OPTION_LIST:-}; do
        local desc
        desc=$(get_opt_desc "$name")
        [[ "$desc" ]] || continue
        local arg
        arg=$(get_opt_arg "$name")
        [[ "$arg" ]] || continue

        local sopt
        sopt=$(get_opt_letter "$name")
        local lopt
        lopt=$(get_opt_string "$name")

        echo -n " ["
        [[ "$sopt" ]] && printf -- '-%s' "$sopt"
        [[ "$sopt" && "$lopt" ]] && echo -n ","
        [[ "$lopt" ]] && printf -- "--%s=%s" $lopt $arg
        echo -n "]"
    done
}

# print the help line for all options
print_all_opts() {
    local name
    for name in ${ARGP_OPTION_LIST:-}; do
        print_opt "$name"
    done
}

# print the option line for a man page
print_man_opt() {
    local name="$1"
    local l desc sopt lopt arg

    desc=$(get_opt_desc "$name")
    [[ "$desc" ]] || return 0
    sopt=$(get_opt_letter "$name")
    lopt=$(get_opt_string "$name")
    arg=$(get_opt_arg "$name")

    echo ".TP"
    echo -n ".B "
    # NB 'echo -n "-E"' swallows the -E!! and it has no --
    [[ "$sopt" ]] && printf -- '\\fB\-%s\\fP' "$sopt"
    if [[ "$GARGP_LONG_GETOPT" ]]; then
        if [[ "$lopt" != "$GARGP_ENDOPTS_loption" ]]; then
            [[ "$sopt" && "$lopt" ]] && echo -n ", "
            [[ "$lopt" ]] && printf -- '\\fB\-\-%s\\fP' "$lopt"
        fi
    fi
    [[ "$arg" ]] && echo -n " \\fI$arg\\fR"
    echo
    echo "$desc"
}

# Create a skeleton man page - uses these parameters if defined:
# USAGE
# ARGP_ARGS
# SHORT_DESC
print_man_page() {
    local flags
    flags=$(print_short_flags)
    [[ "$flags" ]] && flags='
.RB "[" \-'$flags' "]"'
    local lflags
    lflags=$(print_long_flags | sed 's/-/\\-/g')
    [[ "$lflags" ]] && lflags='
[
.B '$lflags'
]'
    local args
    args=$( print_man_all_args )

    # shellcheck disable=SC2018
    # shellcheck disable=SC2019
    cat <<EOF
.TH $( echo "$ARGP_PROG"|tr 'a-z' 'A-Z' ) 1 \" -*- nroff -*-
.SH NAME
$ARGP_PROG \- $SHORT_DESC
.SH SYNOPSIS
.hy 0
.na
.B $ARGP_PROG$flags$lflags
$args
${ARGP_ARGS:-}
.ad b
.hy 0
.SH DESCRIPTION
.nf
${USAGE:-}
.P
.SH OPTIONS
EOF

    local name
    for name in ${ARGP_OPTION_LIST:-}; do
        print_man_opt "$name"
    done

    cat <<EOF
.SH "EXIT STATUS"
.SH "ENVIRONMENT"
.SH "FILES"
.SH "EXAMPLES"
.SH "NOTES"
.SH "BUGS"
.SH "SEE ALSO"
.SH "AUTHOR"
Written by Foo Bar <foobar@foobar.org>
.P
.RB http://foobar.foobar.org/foobar
.SH "COPYRIGHT"
Copyright (c) 2012 Foo Bar
.br
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
.P
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
.P
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
EOF
}

escape() {
    sed 's/</\&lt;/g; s/>/\&gt;/g'
}

print_xml() {
    local name

    echo '<?xml version="1.0" encoding="UTF-8"?>'
    echo "<argp><prog>$ARGP_PROG</prog>"
    echo "<args>$ARGP_ARGS</args>"
    echo "<short>$SHORT_DESC</short>"
    echo "<version>$GARGP_VERSION</version>"
    echo "<usage>$( echo "$USAGE" | escape )</usage>"
    for name in ${ARGP_OPTION_LIST:-}; do
        case $(convert_to_option_name "$name") in
            $GARGP_PRINTXML_loption| \
            $GARGP_PRINTMAN_loption| \
            $GARGP_HELP_loption| \
            $GARGP_ENDOPTS_loption)
                continue;
        esac
        echo "<option name='$name' sname='$(get_opt_letter "$name")' type='$(get_opt_type "$name")' arg='$(get_opt_arg "$name")' default='$(get_opt_default "$name")' range='$(get_opt_range "$name")'>$(get_opt_raw_desc "$name")</option>"
    done
    echo "</argp>"
}

add_std_opts() {
    get_opt_name -"$GARGP_HELP_option" >/dev/null && GARGP_HELP_option=
    get_opt_name --"$GARGP_HELP_loption"  >/dev/null && GARGP_HELP_loption=
    if [[ "$GARGP_HELP_option$GARGP_HELP_loption" ]]; then
        add_opt "$GARGP_HELP_loption" "" "$GARGP_HELP_option" "" b "" "print this help and exit"
    fi

    get_opt_name -"$GARGP_VERSION_option"  >/dev/null && GARGP_VERSION_option=
    get_opt_name --"$GARGP_VERSION_loption"  >/dev/null && GARGP_VERSION_loption=
    if [[ "$GARGP_VERSION_option$GARGP_VERSION_loption" ]]; then
        add_opt "$GARGP_VERSION_loption" "" "$GARGP_VERSION_option" "" b "" "print version and exit"
    fi

    get_opt_name -"$GARGP_VERBOSE_option" >/dev/null  && GARGP_VERBOSE_option=
    get_opt_name --"$GARGP_VERBOSE_loption" >/dev/null  && GARGP_VERBOSE_loption=
    if [[ "$GARGP_VERBOSE_option$GARGP_VERBOSE_loption" ]]; then
        add_opt "$GARGP_VERBOSE_loption" "${VERBOSE:-}" "$GARGP_VERBOSE_option" "" b "" "do it verbosely"
    fi

    get_opt_name -"$GARGP_QUIET_option" >/dev/null  && GARGP_QUIET_option=
    get_opt_name --"$GARGP_QUIET_loption"  >/dev/null && GARGP_QUIET_loption=
    if [[ "$GARGP_QUIET_option$GARGP_QUIET_loption" ]]; then
        add_opt "$GARGP_QUIET_loption" "${QUIET:-}" "$GARGP_QUIET_option" "" b "" "do it quietly"
    fi

    add_opt "$GARGP_PRINTMAN_loption"
    add_opt "$GARGP_PRINTXML_loption"
    add_opt "$GARGP_ENDOPTS_loption" "" "-" "" b "" "explicitly ends the options"
}

print_opt() {
    local name="$1"
    local l n desc sopt lopt arg

    desc=$(get_opt_desc "$name")
    [[ "$desc" ]] || return 0
    sopt=$(get_opt_letter "$name")
    lopt=$(get_opt_string "$name")
    arg=$(get_opt_arg "$name")

    local line=""
    for (( n=0 ; ${#line} < GARGP_SHORT_OPT_COL ; n++ )) ; do
        line="$line "
    done
    [[ "$sopt" ]] && line="${line}-$sopt"
    if [[ "$GARGP_LONG_GETOPT" && "$lopt" != "$GARGP_ENDOPTS_loption" ]]; then
        [[ "$sopt" && "$lopt" ]] && line="$line, "
        if [[ "$lopt" ]]; then
            for (( n=0 ; ${#line} < GARGP_LONG_OPT_COL ; n++ )); do
                line="$line "
            done
            [[ "$lopt" ]] && line="${line}--$lopt"
            [[ "$lopt" && "$arg" ]] && line="$line="
        fi
    fi
    [[ "$arg" ]] && line="${line}$arg"

    line="$line "
    while (( ${#line} < GARGP_OPT_DOC_COL - 1)) ; do line="$line " ; done
    # NB 'echo "-E"' swallows the -E!! and it has no -- so use printf
    printf -- "%s" "$line"

    local first="FIRST_yes"
    if (( ${#line} >= GARGP_OPT_DOC_COL )); then
        echo
        first=""
    fi
    local width=$(( GARGP_RMARGIN - GARGP_OPT_DOC_COL ))
    if ! type fmt &> /dev/null || [[ "$width" -lt 10 ]]; then
        printf -- "%s\n" "$desc"
        return 0
    fi

    export ARGP_INDENT=""
    while (( ${#ARGP_INDENT} < GARGP_OPT_DOC_COL - 1)); do
        ARGP_INDENT="$ARGP_INDENT "
    done
    echo "$desc" | fmt -w "$width" -s |
    while read -r l; do
        [[ "$first" ]] || printf %s "$ARGP_INDENT"
        first=""
        printf -- "%s\n" "$l"
    done
    unset ARGP_INDENT
}

# honour GNU ARGP_HELP_FMT parameter
load_help_fmt() {
    [[ "${ARGP_HELP_FMT:-}" ]] || return 0
    OFS="$IFS"
    IFS=','
    # shellcheck disable=SC2086
    set -- $ARGP_HELP_FMT
    IFS="$OFS"
    while [[ "$1" ]]; do
        case "$1" in
            short-opt-col*)
                GARGP_SHORT_OPT_COL=$(echo "$1"|cut -d'=' -f 2)
                shift
                ;;
            long-opt-col*)
                GARGP_LONG_OPT_COL=$(echo "$1"|cut -d'=' -f 2)
                shift
                ;;
            opt-doc-col*)
                GARGP_OPT_DOC_COL=$(echo "$1"|cut -d'=' -f 2)
                shift
                ;;
            rmargin*)
                GARGP_RMARGIN=$(echo "$1"|cut -d'=' -f 2)
                shift
                ;;
            *)
                shift
                ;;
        esac
    done
}

default_usage() {
    local flags
    flags=$( print_short_flags )
    [[ "$flags" ]] && flags="[-$flags]"
    local lflags
    lflags=$( print_long_flags )
    [[ "$lflags" ]] && lflags=" [$lflags]"
    local args
    args=$( print_all_args )
    local fmt
    fmt="fmt -w $GARGP_RMARGIN -s"
    type fmt &> /dev/null || fmt=cat
    echo -e "Usage: $ARGP_PROG $flags$lflags $args ${ARGP_ARGS:-}" |$fmt
    echo
    echo "${USAGE:-}"
    echo
    echo "Options:"
    echo
    print_all_opts
}

range_check_entry() {
    local name="$1"
    local value="$2"
    local type="$3"
    local range="$4"

    [[ "$ARGP_DEBUG" ]] && echo "${FUNCNAME[0]}: VALUE='$value' TYPE='$type' RANGE='$range'" >&2
    # just using 'while' for the sake of the 'break':
    while [[ "$type" ]]; do
        case "$type" in
            i)
                [[ "$value" =~ $GARGP_INT_REGEX ]] || break
                [[ "$range" ]] && {
                    [[ "$range" =~ $GARGP_INT_RANGE_REGEX ]] || break
                    lower=${BASH_REMATCH[1]}
                    upper=${BASH_REMATCH[2]}
                    [[ "$lower" && "$value" -lt "$lower" ]] && break
                    [[ "$upper" && "$value" -gt "$upper" ]] && break
                }
                return 0
                ;;
            r|f|d)
                [[ "$value" =~ $GARGP_FLOAT_REGEX ]] || break
                [[ "$range" ]] && {
                    [[ "$range" =~ $GARGP_FLOAT_RANGE_REGEX ]] || break
                    lower=${BASH_REMATCH[1]}
                    upper=${BASH_REMATCH[3]}
                    [[ "$lower" ]] && {
                        awk "BEGIN {if ($value < $lower) {exit 1} else {exit 0}}" || break
                    }
                    [[ "$upper" ]] && {
                        awk "BEGIN {if ($value > $upper) {exit 1} else {exit 0}}" || break
                    }
                }
                return 0
                ;;
            s*)
                [[ "$range" ]] && {
                    [[ "$value" =~ $range ]] || break
                }
                return 0
                ;;
            a*)
                local val
                sep=${type:1:1}
                [[ "$sep" ]] && IFS=$sep
                for val in $range; do
                    [[ "$val" == "$value" ]] && IFS="$GARGP_DEFAULT_IFS" && return 0
                done
                IFS="$GARGP_DEFAULT_IFS"
                break
                ;;
        esac
    done

    msg="$ARGP_PROG: value '$value' given for option '$name'"
    if [[ "$type" != s* && "$type" != a* ]]; then
        msg+=" must be of type '$type'"
        [[ "$range" ]] && msg+=" and"
    fi
    [[ "$range" ]] && {
        case "$type" in
            s*)
                msg+=" must fit the regex '$range'"
                ;;
            a*)
                msg+=" must be one of these values: '$range'"
                ;;
            r|f|d|i)
                msg+=" must be in the range '$range'"
                ;;
        esac
    }
    abend 1 "$msg"
}

get_opt_name() {
    # returns the name for an option letter or word
    local opt="$1" # an option eg -c or --foobar
    local name
    for name in ${ARGP_OPTION_LIST:-}; do
        local argp_l=ARGP_LOPT_$name
        local argp_s=ARGP_SOPT_$name
        if  [[ "--${!argp_l:-}" = "$opt" ]] || \
            [[ "-${!argp_s:-}" = "$opt" ]]; then
            echo "${name}"
            return 0
        fi
    done
    return 1
}

process_opts() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    local shift_num=0 option name type range was_set
    while true; do
        option="${1:-}"
        if  [[ "$GARGP_HELP_option"  &&  "-$GARGP_HELP_option"  == "$option" ]] ||
            [[ "$GARGP_HELP_loption" && "--$GARGP_HELP_loption" == "$option" ]]; then
            usage 2>/dev/null || default_usage
            echo "exit 0;" >&3
            exit 0
        fi

        if  [[ "$GARGP_VERSION_option"  &&  "-$GARGP_VERSION_option"  == "$option" ]] ||
            [[ "$GARGP_VERSION_loption" && "--$GARGP_VERSION_loption" == "$option" ]]; then
            echo "echo $ARGP_PROG: version: '$ARGP_VERSION'; exit 0;" >&3
            exit 0
        fi

        if [[ "$GARGP_PRINTMAN_loption" && --$GARGP_PRINTMAN_loption == "$option" ]]; then
            print_man_page
            echo "exit 0;" >&3
            exit 0
        fi

        if [[ "$GARGP_PRINTXML_loption" && --$GARGP_PRINTXML_loption == "$option" ]]; then
            print_xml
            echo "exit 0;" >&3
            exit 0
        fi

        ((shift_num++))
        shift

        [[ "$option" == "--" ]] && break

        # here is where all the user options get done:
        name=$(get_opt_name "$option")
        [[ "$name" ]] || {
            abend 1 "$ARGP_PROG: argp.sh: no name for option \"$option\""
        }

        type=$(get_opt_type "$name")
        [[ "$type" ]] || {
            abend 1 "$ARGP_PROG: argp.sh: no type for option \"$option\""
        }
        range=$(get_opt_range "$name")

        was_set=$(get_opt_was_set "$name")

        [[ "$ARGP_DEBUG" ]] &&
            echo "process_opts: option='$option' name='$name' type='$type' range='$range' was_set='$was_set' value='${!name}'"
        case $type in
            b)
                if [[ "$range" ]]; then
                    if [[ -z "${!name}" ]]; then
                        export "$name"="$range"
                    else
                        export "$name="
                    fi
                else
                    if [[ "${!name}" =~ ^[0-9]+$ ]]; then
                        export "$name"=$(( name + 1 ))
                    else
                        if [[ -z "${!name}" ]]; then
                            export "$name=set"
                        else
                            export "$name="
                        fi
                    fi
                fi
                ;;
            *)
                local value="$1"
                [[ "$range" ]] &&
                range_check_entry "$name" "$value" "$type" "$range"
                case $type in
                    a*|s*)
                        local SEP=${type:1:1}
                        [[ "$SEP" ]] || SEP="$ARGP_OPTION_SEP"
                        if [[ "$was_set" && "$SEP" && "${!name}" ]]; then
                            export "$name=${!name}${SEP}$value"
                        else
                            export "$name=$value"
                        fi
                        ;;
                    *)
                        export "$name"="$value"
                        ;;
                esac
                ((shift_num++))
                shift
                set +x
                ;;
        esac
        export ARGP_WAS_SET_$name="yes"

    done
    return $shift_num
}


output_values() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"
    local name value was_set
    for name in $ARGP_OPTION_LIST; do
        value="${!name}"
        was_set=$(get_opt_was_set "$name")
        [[ "$was_set" ]] || {
            value=$(get_opt_default "$name")
        }
        value="${value//\\/\\\\}"
        value="${value//\'/\\\'}"
        echo -n "export $ARGP_PREFIX$name=\$'$value'; "
    done

    echo -n "set -- "
    for value in "$@"; do
        value="${value//\\/\\\\}"
        value="${value//\'/\\\'}"
        echo -n " \$'$value'"
    done
    echo
}

call_getopt() {
    [[ "$ARGP_DEBUG" ]] && debug_args "$@"

    local short_options=""
    local short_options_arg=""
    local long_options=""
    local long_options_arg=""
    local stop_early=""
    local opt temp name long arg

    for name in $ARGP_OPTION_LIST; do
        opt=$(get_opt_letter "$name")
        arg=$(get_opt_arg "$name")
        long=$(get_opt_string "$name")
        [[ "$opt" == '-' ]] && continue
        if [[ "$opt" ]]; then
            if [[ "$arg" ]]; then
                short_options_arg+="$opt:"
            else
                short_options+="$opt"
            fi
        fi

        if [[ "$arg" ]]; then
            [[ "$long_options_arg" ]] && long_options_arg+=","
            long_options_arg+="$long:"
        else
            [[ "$long_options" ]] && long_options+=","
            long_options+="$long"
        fi
    done

    [[ "$STOP_ON_FIRST_NON_OPT" ]] && stop_early="+"
    if [[ "${GARGP_LONG_GETOPT:-''}" ]]; then
        local short_args=""
        local long_args=""
        [[ "$short_options$short_options_arg" ]] && short_args="-o $stop_early$short_options$short_options_arg"
        [[ "$long_options" ]] && long_args="--long $long_options"
        [[ "$long_options_arg" ]] && long_args="$long_args --long $long_options_arg"
        [[ "$ARGP_DEBUG" ]] && echo "call_getopt: set -- \$(getopt $short_args $long_args -n $ARGP_PROG -- $*)" >&2
        # shellcheck disable=SC2086
        temp=$(getopt $short_args $long_args -n "$ARGP_PROG" -- "$@") || abend $? "getopt failure"
    else
        [[ "$ARGP_DEBUG" ]] && echo "call_getopt: set -- \$(getopt $short_options$short_options_arg $*)" >&2
        # shellcheck disable=SC2086
        temp=$(getopt $short_options$short_options_arg "$@") || abend $? "getopt failure"
    fi

    eval set -- "$temp"

    [[ "$ARGP_DEBUG" ]] && debug_args "$@"

    return_args_from_getopt=( "$@" )
}

sort_option_names_by_key()
{
    [[ "$ARGP_DEBUG" ]] && echo "sort_option_names_by_key: before: $ARGP_OPTION_LIST" >&2
    local new_option_list name key
    local tmp
    tmp=$( mktemp )
    # shellcheck disable=SC2064
    trap "rm -f $tmp" return

    for name in ${ARGP_OPTION_LIST:-}; do
        key=$(get_opt_letter "$name")
        [[ "$key" ]] || key="~" # ie collate last
        [[ "$key" == - ]] && key="~" # ie collate last
        echo "$key $name"
    done | sort --ignore-case > "$tmp"

    while read -r key name; do
        new_option_list+="$name "
    done < "$tmp"

    ARGP_OPTION_LIST="$new_option_list"
    [[ "$ARGP_DEBUG" ]] && echo "sort_option_names_by_key: after: $new_option_list" >&2
}

initialise() {
    GARGP_VERSION="$TIME_STAMP"
    GARGP_DEFAULT_IFS=$' \t\n'
    IFS="$GARGP_DEFAULT_IFS"
    STOP_ON_FIRST_NON_OPT=${POSIXLY_CORRECT:-}
    unset POSIXLY_CORRECT

    ARGP_OPTION_LIST=""
    ARGP_OPTION_SEP=
    GARGP_LONG_GETOPT=""
    # decide if this getopt supports long options:
    {
        getopt --test &>/dev/null; ARGP_STAT=$?
    } || :
    [[ $ARGP_STAT -eq 4 ]] && GARGP_LONG_GETOPT="GARGP_LONG_GETOPT_yes"

    GARGP_HELP_loption="help"
    GARGP_HELP_option="h"
    GARGP_VERBOSE_loption="verbose"
    GARGP_VERBOSE_option="v"
    VERBOSE=${VERBOSE:-}
    GARGP_QUIET_option="q"
    GARGP_QUIET_loption="quiet"
    QUIET=${QUIET:-}
    GARGP_VERSION_loption="version"
    GARGP_VERSION_option="V"
    GARGP_PRINTMAN_loption="print-man-page"
    GARGP_PRINTXML_loption="print-xml"
    GARGP_ENDOPTS_loption="end-all-options"

    GARGP_SHORT_OPT_COL=2
    GARGP_LONG_OPT_COL=6
    GARGP_OPT_DOC_COL=29

    GARGP_INT_REGEX="[+-]*[[:digit:]]+"
    GARGP_INT_RANGE_REGEX="($GARGP_INT_REGEX)*[-:]($GARGP_INT_REGEX)*"
    GARGP_FLOAT_REGEX="[+-]*[[:digit:]]+(\\.[[:digit:]]+)*"
    GARGP_FLOAT_RANGE_REGEX="($GARGP_FLOAT_REGEX)[-:]($GARGP_FLOAT_REGEX)"
    # FIXME: this needs a few tweaks:
    GARGP_URL_REGEX="(nfs|http|https|ftp|file)://[[:alnum:]_.-]*[^[:space:]]*"

    # cron jobs have TERM=dumb and tput throws errors:
    _GPG_COLUMNS=$( [[ "$TERM" && "$TERM" != "dumb" ]] && tput cols || echo 80)
    GARGP_RMARGIN=$_GPG_COLUMNS

    load_help_fmt
    (( GARGP_RMARGIN > _GPG_COLUMNS )) && GARGP_RMARGIN=$_GPG_COLUMNS

    # we're being called directly from the commandline (possibly in error
    # but maybe the guy is just curious):
    tty -s && {
        ARGP_VERSION=$GARGP_VERSION
        add_std_opts
        ARGP_PROG=${0##*/} # == basename
        ARGP_argp_sh_usage
        exit 0
    }
}

read_xml_format() {
    type xmlstarlet &>/dev/null || abend 1 "Please install xmlstarlet"
    echo "ARGP_DELETE=verbose quiet version"
    xmlstarlet sel --text -t -m '/argp' \
        -v "concat('ARGP_DELETE=', delete)" -n \
        -v "concat('ARGP_VERSION=',version)" -n \
        -v "concat('ARGP_PROG=', prog)" -n \
        -v "concat('ARGP_PREFIX=', prefix)" -n \
        -t -m '/argp/option' \
        -v "concat(@name, \"='\", @default, \"' '\", @sname, \"' '\", @arg, \"' '\", @type, \"' '\", @range, \"' \", self::option)" -n \
        -t -m '/argp' \
        -v "concat('ARGP_ARGS=', args)" -n \
        -v "concat('ARGP_SHORT=', short)" -n \
        -v "concat('ARGP_USAGE=', usage)" -n
}

read_config() {
    # note that we can't use process substitution:
    # foobar < <( barfoo )
    # as POSIXLY_CORRECT disables it! So we'll use a temp file for the xml.
    local tmp
    tmp=$( mktemp )
    # shellcheck disable=SC2064
    trap "rm -f $tmp" EXIT

    local file_type line
    # shellcheck disable=SC2162
    while read line; do
        [[ "$file_type" ]] || {
            file_type="flat"
            [[ "$line" == "<?xml"* ]] && {
                file_type="xml"
                {
                    echo "$line"
                    cat
                } | read_xml_format > "$tmp"
                exec < "$tmp"
                continue
            }
        }

        [[ "$ARGP_DEBUG" ]] && echo "read: $line" >&2
        case "$line" in
            "ARGP_PROG="*)
                ARGP_PROG="${line#ARGP_PROG=}"
                ;;
            "ARGP_DELETE="*)
                del_opt "${line#ARGP_DELETE=}"
                ;;
            "ARGP_ARGS="*)
                ARGP_ARGS="${line#ARGP_ARGS=}"
                ;;
            "ARGP_SHORT="*)
                SHORT_DESC="${line#ARGP_SHORT=}"
                ;;
            "ARGP_VERSION="*)
                ARGP_VERSION="${line#ARGP_VERSION=}"
                ;;
            "ARGP_OPTION_SEP="*)
                ARGP_OPTION_SEP="${line#ARGP_OPTION_SEP=}"
                ;;
            "ARGP_PREFIX="*)
                ARGP_PREFIX="${line#ARGP_PREFIX=}"
                ;;
            "ARGP_USAGE="*)
                if [[ "${line#ARGP_USAGE=}" ]]; then
                    USAGE="${line#ARGP_USAGE=} "$'\n'
                else
                    USAGE=
                fi
                USAGE+=$(cat)
                break
                ;;
            [A-Za-z]*=*)
                local name regex default sname arg type range desc var v
                name="${line%%=*}"
                line="${line#$name=}"
                name=$(convert_to_env_name "$name")
                # initial value could contain spaces, quotes, anything -
                # but I don't think we need to support escaped quotes:
                regex="^[[:space:]]*('[^']*'|[^[:space:]]+)[[:space:]]*(.*)"
                for var in default sname arg type range; do
                    [[ "$line" =~ $regex ]] || break
                    v="${BASH_REMATCH[1]}"
                    v="${v%\'}"
                    v="${v#\'}"
                    local "$var"="$v"
                    line="${BASH_REMATCH[2]}"
                done
                desc="$line"
                while [[ "$desc" == *\\ ]]; do
                    desc="${desc%\\}"
                    read line
                    desc+="$line"
                    [[ "$ARGP_DEBUG" ]] && echo "read for DESC: $line" >&2
                done
                add_opt "$name" "$default" "$sname" "$arg" "$type" "$range" "$desc"
                ;;
            *) # includes comments
                ;;
        esac
    done
}

main() {
    add_std_opts

    read_config

    sort_option_names_by_key

    call_getopt "$@"

    [[ "$ARGP_DEBUG" ]] && debug_args "$@"

    process_opts "${return_args_from_getopt[@]}"
    ARGP_NUM_SHIFT=$?
    set -- "${return_args_from_getopt[@]}"
    shift $ARGP_NUM_SHIFT

    [[ "$ARGP_DEBUG" ]] && debug_args "$@"

    output_values "$@" >&3
}

check_bash() {
    # don't assume bash-3+ yet
    [ -n "$ARGP_DEBUG" ] && echo "$ARGP_PROG: arpg.sh: debug is on" >&2

    ARGP_GOOD_ENOUGH=""
    if [ "x$BASH_VERSION" = "x" ] || [ "x${BASH_VERSINFO[*]}" = "x" ]; then
        :
    elif [ "${BASH_VERSINFO[0]}" -gt 2 ]; then
        ARGP_GOOD_ENOUGH="1"
    fi
    if [ "x$ARGP_GOOD_ENOUGH" = "x" ]; then
        echo "$0: This version of the shell does not support this program." >&2
        echo "bash-3 or later is required" >&2
        echo "exit 1;" >&3
        exit 1
    fi
}

try_c_version() {
    # don't assume bash-3+ yet
    # shellcheck disable=SC2006
    local override_processor=""
    [ -z "$ARGP_PROCESSOR" ] && ARGP_PROCESSOR=`which argp 2>/dev/null`
    [ -n "$ARGP_PROCESSOR" ] && override_processor=`basename "$ARGP_PROCESSOR"`
    # shellcheck disable=SC2006
    local this=`basename "$0"`

    [ "$override_processor" != "$this" ] && type "$ARGP_PROCESSOR" &>/dev/null && {
        [ -n "$ARGP_DEBUG" ] &&
        echo "$ARGP_PROG: $this exec'ing argp: you can use ARGP_PROCESSOR to override this" >&2
        exec "$ARGP_PROCESSOR" "$@"
    }
}

try_c_version "$@"
check_bash
initialise
main "$@"

# just to make sure we don't return with non-zero $?:
:

# Local Variables:
# mode: shell-script
# time-stamp-pattern: "4/TIME_STAMP=\"%:y%02m%02d.%02H%02M%02S\""
# eval: (add-hook 'before-save-hook 'time-stamp)
# End:
