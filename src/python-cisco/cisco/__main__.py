import os
import sys
import pexpect

def main():
    telnet_password = os.environ['TELNET_PASSWORD']
    enable_password = os.environ['ENABLE_PASSWORD']

    host = sys.argv[1]
    command = " ".join(sys.argv[2:])

    if host == "sw1-dh507.intr" or host == "sw1-dh508.intr":
        prompt = host.strip(".intr") + ">"
        enable_prompt = host.strip(".intr") + "#"
    else:
        prompt = host + ">"
        enable_prompt = host + "#"

    child = pexpect.spawn("telnet " + host)
    child.logfile = None

    child.expect("Password:")
    child.sendline(telnet_password)

    child.expect(prompt)
    child.sendline("terminal length 0")

    child.expect(prompt)

    child.sendline("enable")
    child.expect("Password:")
    child.sendline(enable_password)
    child.expect(enable_prompt)

    child.sendline(command)

    child.expect(enable_prompt)
    child.logfile = sys.stdout.buffer
    output = child.before.decode("utf-8")

    print(output)


if __name__ == '__main__':
    main()
