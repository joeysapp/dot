#!/usr/bin/zsh

https://superuser.com/questions/168749/is-there-a-way-to-see-any-tar-progress-per-file
Explanation:

    tar tarball tool
    cf create file
    - use stdout instead of a file (to be able to pipe the output to the next command)
    /folder-with-big-files The input folder to zip
    -P use absolute paths (not necessary, see comments)

pipe to

    pv progress monitor tool
    -s use the following size as the total data size to transfer (for % calculation)
        $(...) evaluate the expression
        du -sb /folder-with-big-files disk usage summarize in one line with bytes. Returns eg 8367213097      folder-with-big-files
        pipe (|) to awk '{print $1}' which returns only the first part of the du output (the bytes, removing the foldername)

pipe to

    gzip gzip compression tool
    big-files.tar.gz output file name
:


tar cf - images/ | pv -s $(($(du -sk images/ | awk '{print $1}') * 1024)) | gzip > images.backup.gz


