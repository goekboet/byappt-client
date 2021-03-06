#! /bin/sh
DIST=dist/*

if [ "$1" == "debug" ]
then
    npx elm make src/Main.elm --debug --output=dist/elm.js
    echo "Built debug:"
else
    npx elm make src/Main.elm --optimize --output=dist/elm.js
    npx uglifyjs dist/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | npx uglifyjs --mangle --output=dist/elm.js
    npx uglifyjs src/index.js -mt -o dist/index.js
    echo "Built optimized:"
fi
cp index.html styles.css dist

for f in $DIST
do
    echo "$(cat $f | wc -c) bytes  ($(basename $f))" 
done