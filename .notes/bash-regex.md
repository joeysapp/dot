# REGEXes
* Use ag -G
  -G --file-search-regex  PATTERN Limit search to filenames matching PATTERN
  
# Find
    -regex pattern
        True if the whole path of the file matches pattern using regular
        expression.  To match a file named "./foo/xyzzy", you can use the
        regular expression ".*/[xyz]*" or ".*/foo/.*", but not "xyzzy" or
        "/foo/".
     -E      Interpret regular expressions followed by -regex and -iregex
             primaries as extended (modern) regular expressions rather than
             basic regular expressions (BRE's).  The re_format(7) manual page
             fully describes both formats.
             
# ag
       --parallel
              Parse the input stream as a search term, not data to search. This
              is meant to be used with tools such as GNU parallel. For example:
              echo "foo\nbar\nbaz" | parallel "ag {} ." will run 3 instances of
              ag, searching the current directory for "foo", "bar", and "baz".
       -g PATTERN
              Print filenames matching PATTERN.

       -G --file-search-regex PATTERN
              Only search files whose names match PATTERN.

       -H --[no]heading
              Print filenames above matching contents.


        

# String not containing <given string>
https://stackoverflow.com/questions/717644/regular-expression-that-doesnt-contain-certain-string
^((?!my string).)*$
^aa(?!.*aa.*aa).*aa$
^((?!my string)(\s|\S))*$
aa([^a] | a[^a])aa
/aa([^a]|a[^a])*aa/
RELUCTANT QUALIFIERS: 
/aa.*?aa/
/aa(?:(?!aa).)*aa/



^((?!my string).)*$
It works as follows: it looks for zero or more (*) characters (.) which do not begin (?! - negative lookahead) your string and it stipulates that the entire string must be made up of such characters (by using the ^ and $ anchors). Or to put it an other way:

The entire string must be made up of characters which do not begin a given string, which means that the string doesn't contain the given substring.
