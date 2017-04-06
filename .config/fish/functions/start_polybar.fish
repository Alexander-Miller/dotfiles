function start_polybar
    killall polybar

    while [ (pgrep -x polybar) ]
        sleep 1
    end

    set -l screen_lines (xrandr --query | ag " connected")

    for screen_line in $screen_lines
        set -l screen (echo "$screen_line" | cut -d ' ' -f 1)
        polybar --log=error --quiet --reload a &
    end

end
