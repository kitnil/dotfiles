local shell = require "resty.shell"
local stdin = ""
local timeout = 1000  -- ms
local max_size = 4096  -- byte

local args, err = ngx.req.get_uri_args()

if ngx.var.args then
   for key, val in pairs(args) do
      if type(val) == "table" then
         ngx.say(key, ": ", table.concat(val, ", "))
      else
         if key == "channel" and val == "guix" then
            local ok, stdout, stderr, reason, status = shell.run([[/run/current-system/profile/bin/guix describe --format=json --profile=/var/guix/gcroots/profiles/per-user/root/current-guix | /home/oleg/.guix-profile/bin/jq --join-output '.[] | select(.name=="guix")']], stdin, timeout, max_size)
            ngx.say(stdout)
         end
      end
   end
else
   local ok, stdout, stderr, reason, status = shell.run([[/run/current-system/profile/bin/guix describe --format=json --profile=/var/guix/gcroots/profiles/per-user/root/current-guix]], stdin, timeout, max_size)
   ngx.say(stdout)
end
