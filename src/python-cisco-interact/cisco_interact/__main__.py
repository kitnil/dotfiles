import os
import sys
import pexpect

def main():
    telnet_password = os.environ['TELNET_PASSWORD']
    enable_password = os.environ['ENABLE_PASSWORD']

    host = sys.argv[1]

    command = " ".join(sys.argv[2:])

    prompt = [host + ">"]
    child = pexpect.spawn("telnet " + host)
    child.logfile = sys.stdout.buffer
    # TODO: "/tmp/" + host + ".log"

    child.expect("Password:")
    child.sendline(telnet_password)

    child.expect(prompt)
    child.sendline("terminal length 0")

    child.expect(prompt)

    child.sendline("enable")
    child.expect("Password:")
    child.sendline(enable_password)

    child.expect(host + "#")
    child.logfile = None
    # sys.stdout.write(prompt)
    child.interact()

if __name__ == '__main__':
    main()
