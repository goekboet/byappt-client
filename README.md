# Broker elm-app

Mainly an exercise in writing Elm webapp. I really like it.

## Prereqs

You need npm/node. Then you need to `npm install`

## Build
1. Build the project

`./build.sh` Makes an optimized build

`./build.sh debug` Makes a debug build

2. Copy the artifacts to the appropriate place on the webserver. Example script: 
```
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
```

