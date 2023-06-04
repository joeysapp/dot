#!/bin/bash
https://stackoverflow.com/questions/18414054/reading-optarg-for-optional-flags



: means "takes an argument", not "mandatory argument". That is, an option character not followed by : means a flag-style option (no argument), whereas an option character followed by : means an option with an argument.

Thus, you probably want

getopts "a:b:c:d:e:f:" opt

If you want "mandatory" options (a bit of an oxymoron), you can check after argument parsing that your mandatory option values were all set.



  if [[ "$1" =~ ^((-{1,2})([Hh]$|[Hh][Ee][Ll][Pp])|)$ ]]; then
    print_usage; exit 1
  else
    while [[ $# -gt 0 ]]; do
      opt="$1"
      shift;
      current_arg="$1"
      if [[ "$current_arg" =~ ^-{1,2}.* ]]; then
        echo "WARNING: You may have left an argument blank. Double check your command." 
      fi
      case "$opt" in
        "-a"|"--apple"      ) APPLE="$1"; shift;;
        "-b"|"--banana"     ) BANANA="$1"; shift;;
        "-c"|"--cherry"     ) CHERRY="$1"; shift;;
        "-d"|"--dfruit"     ) DFRUIT="$1"; shift;;
        "-e"|"--eggplant"   ) EGGPLANT="$1"; shift;;
        "-f"|"--fig"        ) FIG="$1"; shift;;
        *                   ) echo "ERROR: Invalid option: \""$opt"\"" >&2
                              exit 1;;
      esac
    done
  fi

  if [[ "$APPLE" == "" || "$BANANA" == "" ]]; then
    echo "ERROR: Options -a and -b require arguments." >&2
    exit 1
  fi







OR



hile getopts ":a:b:c:d:e:f:" opt; do
    case $opt in
        a) APPLE="$OPTARG";;
        b) BANANA="$OPTARG";;
        c|d|e|f)
            if test "$OPTARG" = "$(eval echo '$'$((OPTIND - 1)))"; then
                OPTIND=$((OPTIND - 1))
            else
                 case $opt in
                     c) CHERRY="$OPTARG";;
                     d) DFRUIT="$OPTARG";;
                     ...
                esac
            fi ;;
        \?) ... ;;
        :)
             case "$OPTARG" in
                 c|d|e|f) ;; # Ignore missing arguments
                 *) echo "option requires an argument -- $OPTARG" >&2 ;;
            esac ;;
        esac
    done









OR 









usage()
{
    echo "usage: $0 -OPT1 <opt1_arg> -OPT2"
}
while [ "`echo $1 | cut -c1`" = "-" ]
do
    case "$1" in
        -OPT1)
                OPT1_ARGV=$2
                OPT1_BOOL=1
                shift 2
            ;;
        -OPT2)
                OPT2_BOOL=1
                shift 1
            ;;
        *)
                usage
                exit 1
            ;;
esac
done

Short, simple. An engineer's best friend!

I think this can be modified to support "--" options as well...
