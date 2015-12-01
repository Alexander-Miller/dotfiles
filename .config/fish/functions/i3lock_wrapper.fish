#!/usr/bin/fish

function i3lock_wrapper --description "Sets a blurred screenshot of the current desktop as i3lock background."

    set tmpfile (mktemp --tmpdir i3lock-wrapper-XXXXXXXXXX.png)
    scrot $tmpfile
    mogrify -resize 50% -resize 200% -blur 0x4 $tmpfile

    if [ -f $HOME/.config/i3/i3lock.png ]

        set -l DEBUG TRUE

        set i3lock_size   (file ~/.config/i3/i3lock.png | ag -o '[0-9]* x [0-9]*')
        set i3lock_size_x (echo $i3lock_size | cut -d ' ' -f 1)
        set i3lock_size_y (echo $i3lock_size | cut -d ' ' -f 3)
        set resolutions   (xrandr --query | grep ' connected' | cut -f 3 -d ' ')

        if [ $DEBUG = TRUE ]
            echo i3lock_size = $i3lock_size i3lock_x = $i3lock_size_x i3lock_y = $i3lock_size_y
            echo resolutions = $resolutions \n
        end

        for resolution in $resolutions
            set width  (echo $resolution | cut -d 'x' -f 1)
            set height (echo $resolution | cut -d 'x' -f 2 | cut -d '+' -f 1)

            set offset_x (echo $resolution | cut -d 'x' -f 2 | cut -d '+' -f 2)
            set offset_y (echo $resolution | cut -d 'x' -f 2 | cut -d '+' -f 3)

            set center_x (math "$offset_x + $width/2 - $i3lock_size_x/2")
            set center_y (math "$offset_y + $height/2 - $i3lock_size_y/2")

            if [ $DEBUG = TRUE ]
                echo current resolution = $resolution
                echo width = $width, height = $height
                echo offset_x = $offset_x, offset_y = $offset_y
                echo center_x = $center_x, center_y = $center_y \n
            end

            convert $tmpfile $HOME/.config/i3/i3lock.png -geometry +$center_x+$center_y -composite -matte $tmpfile
        end
    end

    i3lock -n -u -i $tmpfile
    rm $tmpfile

end
