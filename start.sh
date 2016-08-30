#!/bin/sh

Name=push2web@127.0.0.1
start()
{
    erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -setcookie test -name ${Name} -config sys -s push2web -noinput -detached && echo $Name started
}

stop()
{
    ps -ef | grep "beam.*$Name" | grep -v grep | awk '{print $2}' | xargs kill -9 2>/dev/null && echo $Name stoped
}

status()
{
    ps -ef | grep "beam.*$Name" | grep -v grep 1>/dev/null && echo $Name started || echo $Name stoped
}

restart() {
    stop
    start
}

case "$1" in
    start)
        $1
        ;;
    stop)
        $1
        ;;
    restart)
        $1
        ;;
    status)
        $1
        ;;
    *)
        echo $"Usage: $0 {start|stop|status|restart}"
        exit 2
esac
exit $?
