from subprocess import Popen, DEVNULL
from sys import argv
import os

def run(cmd):
    call = Popen(cmd, shell=True, stdout=DEVNULL, stderr=DEVNULL)
    return call.wait()#[0].strip().decode('utf-8')


cmd = argv[1] if len(argv) >= 2 else None
args = " ".join(argv[2:])

if cmd == None:
    print("Not enough arguments.")
    exit(1)

# TODO(2020/03/01):
# capture
# update

if cmd == "edit":
    print(f"Opening [{args}] in new graphical frame")
    run(f"emacsclient -nc -a '' {args}")

elif cmd == "edit-tty":
    print(f"Opening [{args}] in new terminal frame")
    os.system(f"emacsclient -c -nw {args}")

elif cmd == "magit":
    status = run(f"git status")
    if status != 0:
        print("Not a git repository")
        exit(1)
    print("Running Magit-Status")
    run("emacsclient -nc -a '' -e '(magit-status)'")

elif cmd == "magit-tty":
    status = run(f"git status")
    if status != 0:
        print("Not a git repository")
        exit(1)
    print("Running Magit-Status")
    os.system("emacsclient -c -nw -e '(magit-status)'")
