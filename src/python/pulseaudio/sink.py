#!/usr/bin/env python3

import json
import re
import subprocess


def main():
    output = []
    index = None
    name = None
    current = None
    for line in subprocess.check_output(["pacmd", "list-sinks"]).decode("UTF-8").split("\n"):
        if re.search(".*index:.*", line):
            index = line.split()[-1]
            current = "*" in line
        if re.search(".*name:.*", line):
            name = line.split()[-1]
        if index is not None and name is not None:
            output.append({ "index": index, "name": name, "current": current })
            index = None
            name = None
            current = None
    for sink in output:
        if sink["current"] is False and not "hdmi" in sink["name"]:
            subprocess.run(["pacmd", "set-default-sink", sink["index"]])


if __name__ == '__main__':
    main()
