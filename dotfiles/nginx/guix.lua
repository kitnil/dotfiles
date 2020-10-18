local shell = require "resty.shell"
local stdin = ""
local timeout = 1000  -- ms
local max_size = 4096  -- byte
local ok, stdout, stderr, reason, status = shell.run([[/run/current-system/profile/bin/guix describe --format=json --profile=/var/guix/gcroots/profiles/per-user/root/current-guix]], stdin, timeout, max_size)
ngx.say(stdout)
