function suspend_after -a name
    while pgrep $name > /dev/null
        sleep 10
    end
    notify-send "$name is finished. Suspending in 10 seconds."
    sleep 10
    systemctl suspend
end
