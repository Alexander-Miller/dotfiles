from subprocess import Popen, PIPE
from time import sleep

POLYBAR_CMD = 'env POLYBAR_SCREEN={0} POLYBAR_WIFI={1} polybar --log=error --quiet --reload a &'

def kill_polybar():
    run('killall polybar')
    while run('pgrep -x polybar'):
        sleep(1)

def get_screens():
    screen_lines = run('xrandr -q | ag " connected"').split('\n')
    return [line.split(' ')[0] for line in screen_lines]


def get_wifi_iface():
    iface_lines = run('ip link | ag "BROADCAST"').split('\n')
    for line in iface_lines:
        iface = line.split(' ')[1][0:-1]
        if iface.startswith("w"):
            return iface

def run(cmd):
    call = Popen(cmd, shell=True, stdout=PIPE)
    return call.communicate()[0].strip().decode('utf-8')

def main():
    kill_polybar()
    wifi_iface = get_wifi_iface()

    for screen in get_screens():
        Popen(POLYBAR_CMD.format(screen, wifi_iface), shell=True)

if __name__ == '__main__':
    main()
