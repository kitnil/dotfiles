#!/usr/bin/env python3

import sys, datetime, psutil
from time import sleep, time


class Time:
    @staticmethod
    def date():
        return datetime.datetime.now().strftime(f"%a %b %-e %I:%M:%S %p")


class CPU:
    @staticmethod
    def coretemp():
        return str(
            round(psutil.sensors_temperatures(fahrenheit=False)["k10temp"][0][1], 1)
        )

    @staticmethod
    def freq():
        return str(round(int(psutil.cpu_freq(percpu=False)[0]) / 1000, 1))

    @staticmethod
    def usage():
        u = psutil.cpu_percent(interval=0.1, percpu=False)
        return f"{u}%"


class DISK:
    @staticmethod
    def disk_total(mnt):
        return f"{round(psutil.disk_usage(mnt).total/1000**3, 1)}"

    def disk_used(mnt):
        return f"{round(psutil.disk_usage(mnt).used/1000**3, 1)}"


class RAM:
    @staticmethod
    def ram_total():
        return f"{round(psutil.virtual_memory().total/1000**3, 1)}"

    @staticmethod
    def ram_used():
        return f"{round(psutil.virtual_memory().used/1000**3, 1)}"

    @staticmethod
    def swap_total():
        return f"{round(psutil.swap_memory().total/1000**3, 2)}"

    @staticmethod
    def swap_used():
        return f"{round(psutil.swap_memory().used/1000**3, 2)}"


class Network:

    time_down = 0
    time_up = 0
    bytes_recv = 0
    bytes_sent = 0

    @staticmethod
    def get_bytes_down():
        bytes_delta = psutil.net_io_counters().bytes_recv - Network.bytes_recv
        time_delta = time() - Network.time_down
        download_speed = (bytes_delta / time_delta) // 1000
        Network.time_down = time()
        Network.bytes_recv = psutil.net_io_counters().bytes_recv
        return download_speed

    @staticmethod
    def get_bytes_up():
        bytes_delta = psutil.net_io_counters().bytes_sent - Network.bytes_sent
        time_delta = time() - Network.time_up
        upload_speed = (bytes_delta / time_delta) // 1000
        Network.time_up = time()
        Network.bytes_sent = psutil.net_io_counters().bytes_sent
        return upload_speed


class Battery:
    @staticmethod
    def percent():
        return int(psutil.sensors_battery().percent)


while True:
    sys.stdout.write(
        f"Network: up: {Network.get_bytes_up()}KB/s down: {Network.get_bytes_down()}KB/s   CPU: {CPU.freq()} GHz {CPU.coretemp()} C   /: {DISK.disk_used('/')} GB/{DISK.disk_total('/')} GB   /srv: {DISK.disk_used('/srv')} GB/{DISK.disk_total('/srv')} GB   RAM: {RAM.ram_used()} GB/{RAM.ram_total()} GB   SWAP: {RAM.swap_used()} GB/{RAM.swap_total()} GB   Time: {datetime.datetime.now().strftime('%d.%m.%Y %H:%M:%S')} "
    )
    sys.stdout.flush()
    sleep(0.75)
