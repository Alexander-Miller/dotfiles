function yequake
    if not pgrep emacs
        notify-send "Emacs is not running"
        exit 1
    end
    emacsclient -n -e '(yequake-toggle "FRAMEY Org Capture")'
end
