#!bin/bash

DIST=dist/*
WWW=/usr/local/var/www/

function publish {
    echo "Publishing following files to /usr/local/var/www/"
    for f in $DIST
    do
        echo "$(cat $f | wc -c) bytes  ($(basename $f))" 
    done

    cp $DIST $WWW
}
