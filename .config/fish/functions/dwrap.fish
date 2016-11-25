function dwrap --d "Wrapper for long running processes. Will use dunst to send notification about success/failure."
    if eval "$argv"
        notify-send "Success" "Command $argv finished."
    else
        notify-send "Error" "Command $argv failed."
    end
end
