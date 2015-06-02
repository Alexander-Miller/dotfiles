# -*- coding: utf-8 -*-

from json import dumps
from subprocess import Popen, PIPE
from sys import argv, stdout
from time import sleep

NAP_TIME = 5
if len(argv) > 1:
    NAP_TIME = float(argv[1])

BLOCKS = []

COLOR_STD       = '#dddddd'
COLOR_ICON      = '#1fc5ff'
COLOR_SEPARATOR = '#66cc66'
COLOR_URGENT    = '#f24444'

ICON_SEPARATOR = '    '
ICON_TIME      = ' '
ICON_CALENDAR  = '  '
ICON_VOLUME    = ' '
ICON_BATTERY   = ' '
ICON_PLUG      = ' '
ICON_WIFI      = ' '
ICON_RAM       = ' '
ICON_CPU       = ' '

CMD_DATE    = 'date +"%a %d %b %T"'
CMD_VOLUME  = 'amixer -D pulse get Master | ag -o "[0-9]*%" | head -n1'
CMD_BATTERY = 'acpi'
CMD_WIFI    = 'iwconfig wlp4s0 | ag -o "ESSID:\".*\"|Quality=[0-9]{1,3}"'
CMD_RAM     = 'free -m | ag "Mem:"'
CMD_CPU     = 'sar 1 1 -P ALL | ag "([0-9][0-9]:?){3}[[[:space:]]+[0-9][[:space:]]+[0-9]+[.,][0-9]+"'


def run(command):
    call = Popen(command, shell=True, stdout=PIPE)
    out  = call.communicate()[0]
    return out.strip().decode('utf-8')


def try_catch(func):
    try:
        func()
    except Exception as ex:
        msg = 'Error {0} @ {1}'.format(str(ex), func.__name__)
        pack(msg, COLOR_URGENT)


def cpu():
    cpus = [line.split() for line in run(CMD_CPU).split('\n')]
    sep()
    pack(ICON_CPU, COLOR_ICON)
    for i in range(len(cpus)):
        line = cpus[i]
        perc = float(line[2].replace(',', '.'))
        load = '{:5.2f}%'.format(perc)
        load = '{:05.2f}%'.format(perc)
        pack(load, COLOR_STD if perc <= 80.0 else COLOR_URGENT)
        if i < len(cpus) - 1:
            pack(' | ', COLOR_STD)


def ram():
    line  = run(CMD_RAM).split()
    total = line[1]
    used  = line[2]
    text  = '{0}/{1}MB'.format(used, total)
    block(ICON_RAM, text, COLOR_STD)


def online():
    wifi = run(CMD_WIFI).split('\n')
    if len(wifi) > 1:
        ess_id   = wifi[0].strip().split(':')[1][1:-1]
        quality  = int(wifi[1].strip().split('=')[1]) * 1.4285
        block(ICON_WIFI, '{:.0f}% @ {}'.format(quality, ess_id), COLOR_STD)


def charge():
    tokens    = run(CMD_BATTERY).split()
    perc_left = tokens[3] if len(tokens) == 4 else tokens[3][:-1]
    time_left = tokens[4] if len(tokens) != 4 else 'Full'
    txt_color = COLOR_URGENT if int(perc_left[:-1]) <= 25 else COLOR_STD
    block(ICON_BATTERY if tokens[2] == 'Discharging,' else ICON_PLUG, time_left, txt_color)


def date_time():
    line = run(CMD_DATE)
    i    = line.rfind(' ')
    date = line[:i]
    time = line[i+1:]
    block(ICON_CALENDAR, date, COLOR_STD)
    block(ICON_TIME, time, COLOR_STD)


def volume():
    line = run(CMD_VOLUME)
    text = 'n/a' if line == '' else line
    block(ICON_VOLUME, text, COLOR_STD)


def block(icon, text, color):
    pack(ICON_SEPARATOR, COLOR_SEPARATOR)
    pack(icon, COLOR_ICON)
    pack(text, color)


def sep():
    pack(ICON_SEPARATOR, COLOR_SEPARATOR)


def pack(text, color):
    BLOCKS.append({
        'full_text': text,
        'color': color,
        'separator': 'false',
        'separator_block_width': 0,
    })


def main():
    global BLOCKS
    stdout.write('{"click_events": true, "version": 1}')
    stdout.write('[')
    stdout.write('[],')
    while True:
        for func in [cpu, ram, online, charge, date_time, volume]:
            try_catch(func)
        pack(' ', COLOR_SEPARATOR)
        stdout.write(dumps(BLOCKS) + ',')
        stdout.flush()
        sleep(NAP_TIME)
        BLOCKS = []

if __name__ == '__main__':
    main()
