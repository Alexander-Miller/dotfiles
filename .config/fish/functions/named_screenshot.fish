function named_screenshot
    set -l name (rofi -dmenu -lines 1 -columns 1 -line-margin 5 -padding 10 -p 'name: ')
    if test -n "$name"
        maim -s ~/Pictures/$name.png
        notify-send "Screenshot '$name' taken" --icon="image"
    end
end
