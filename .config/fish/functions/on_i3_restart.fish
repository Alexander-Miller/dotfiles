function on_i3_restart
    setxkbmap -option ctrl:nocaps
    killall xcape
    killall compton
    xcape
    compton &
end
