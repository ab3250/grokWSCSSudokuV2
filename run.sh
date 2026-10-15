#!/usr/bin/env bash

export CHIBI_MODULE_PATH=$CHIBI_MODULE_PATH:/opt/opt.2025/schemelib:./lib:.:/usr/local/share/chibi

sudo killall chibi-scheme > /dev/null 2>&1

sudo killall civetweb > /dev/null 2>&1

civetweb -document_root www -listening_ports 8000 &

#/usr/bin/google-chrome-stable http://localhost:8000/index.html 2> /dev/null &

#civetweb -document_root www -listening_ports 8000 

chibi-scheme -r main.scm &

sleep 1

/usr/bin/google-chrome-stable 2>/dev/null http://localhost:8000/index.html 