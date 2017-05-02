from subprocess import Popen, PIPE
from time import sleep

POLYBAR_CMD = 'env POLYBAR_SCREEN={0} POLYBAR_WIFI={1} POLYBAR_ETH={2} POLYBAR_BAT={3} polybar --log=error --quiet a &'

def kill_polybar():
    run('killall polybar')
    while run('pgrep -x polybar'):
        sleep(1)

def get_screens():
    screen_lines = run('xrandr -q | ag " connected"').split('\n')
    return [line.split(' ')[0] for line in screen_lines]

def get_wifi_iface():
    iface_lines = run('ip link | ag "<.*BROADCAST.*UP.*>"').split('\n')
    for line in iface_lines:
        iface = line.split(' ')[1][0:-1]
        if iface.startswith("w"):
            return iface

def get_eth_interface():
    iface_lines = run('ip link | ag "<.*BROADCAST.*UP.*>"').split('\n')
    for line in iface_lines:
        iface = line.split(' ')[1][0:-1]
        if iface.startswith('e'):
            return iface

def get_battery():
    battery_line = run('upower --enumerate | ag battery')
    return battery_line.split('/')[-1][len('battery_'):]

def run(cmd):
    call = Popen(cmd, shell=True, stdout=PIPE)
    return call.communicate()[0].strip().decode('utf-8')

def main():
    kill_polybar()
    wifi_iface = get_wifi_iface()
    eth_iface = get_eth_interface()
    battery = get_battery()

    for screen in get_screens():
        Popen(POLYBAR_CMD.format(screen, wifi_iface, eth_iface, battery), shell=True)

if __name__ == '__main__':
    main()
