#!/bin/sh

exec curl --connect-to ::speedtest.selectel.ru https://manifest.googlevideo.com/100MB -k -o/dev/null
