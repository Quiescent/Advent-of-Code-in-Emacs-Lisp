#!/bin/bash

if [ -z "$1" ]; then
    echo "Please supply the day to continuously run..."
    exit 1
fi

if [ "$(uname)" == "Darwin" ]; then
    while [ 1 == 1 ]; do
        echo "======================================"
        echo "Restarting watch loop due to errors..."
        echo "======================================"
        $(PWD)/runEmacsOnIt.sh $1
        echo "fswatch -0 \"$(PWD)/$1\" | xargs -0 -n1 -I '{}' $(PWD)/runEmacsOnIt.sh \"{}\""
        fswatch -0 "$(PWD)/$1" | xargs -0 -n1 -I '{}' $(PWD)/runEmacsOnIt.sh "{}"
    done
else
    echo "TODO: implement this for Linux."
fi
