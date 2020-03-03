from subprocess import Popen, DEVNULL
from sys import argv
import os

cmd = argv[1] if len(argv) >= 2 else None
args = " ".join(argv[2:])

if cmd == None:
    print("Not enough arguments.")
    exit(1)

def run(cmd):
    call = Popen(cmd, shell=True, stdout=DEVNULL, stderr=DEVNULL)
    return call.wait()

def edit():
    print(f"Opening [{args}] in new graphical frame")
    run(f"emacsclient -nc -a '' {args}")

def edit_tty():
    print(f"Opening [{args}] in new terminal frame")
    os.system(f"emacsclient -c -nw {args}")

def magit():
    status = run(f"git status")
    if status != 0:
        print("Not a git repository")
        exit(1)
    print("Running Magit-Status")
    run("emacsclient -nc -a '' -e '(magit-status)'")

def magit_tty():
    status = run(f"git status")
    if status != 0:
        print("Not a git repository")
        exit(1)
    print("Running Magit-Status")
    os.system("emacsclient -c -nw -e '(magit-status)'")

def cfg_compile():
    os.system(f"emacs --batch -l {os.environ['SPACEMACSDIR']}/tools/config-compile.el x y z")

# TODO(2020/03/01):
# capture
# update

if cmd == "edit":
    edit()
elif cmd == "edit-tty":
    edit_tty
elif cmd == "magit":
    magit()
elif cmd == "magit-tty":
    magit_tty()
elif cmd == "compile":
    cfg_compile()
