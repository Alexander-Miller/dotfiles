
function dwrap --d "Wrapper for long running processes. Will use dunst to send notification about success/failure."
    if eval "$argv"
        notify-send -u low "Command \n $argv \n succeeded."
    else
        notify-send -u critical "Cmd $argv failed."
    end
end
