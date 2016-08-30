#!/bin/sh
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin \
    -name push2web@127.0.0.1 -config sys -s push2web -setcookie test
