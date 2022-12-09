function on_i3_restart
    setxkbmap -option ctrl:nocaps
    killall xcape
    killall picom
    xcape
    picom --experimental-backends &
    python ~/.config/polybar/run_polybar.py
    random_wallpaper
end
