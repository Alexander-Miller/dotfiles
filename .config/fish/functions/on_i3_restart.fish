function on_i3_restart
    setxkbmap -option ctrl:nocaps
    killall xcape
    killall picom
    xcape
    picom --experimental-backends &
end
