local shell = require "resty.shell"
local stdin = ""
local timeout = 5000  -- ms
local ok, stdout, stderr, reason, status = shell.run([[/home/oleg/bin/mjru-git-clone.sh]], stdin, timeout, max_size)
ngx.say(stdout)
