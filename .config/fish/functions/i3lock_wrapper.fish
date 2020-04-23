#!/usr/bin/fish

function i3lock_wrapper --description "Sets a blurred screenshot of the current desktop as i3lock background."
    set tmpfile (mktemp --tmpdir i3lock-wrapper-XXXXXXXXXX.png)
    set lock_img ~/.config/i3/i3lock.png

    maim $tmpfile
    convert $tmpfile -gamma 0.7 -scale 25% -scale 400% $tmpfile

    set lock_img_size (file $lock_img | rg -o '[0-9]* x [0-9]*' | string split " x ")
    set lock_img_size_x $lock_img_size[1]
    set lock_img_size_y $lock_img_size[2]
    set resolutions (xrandr --query | rg ' connected' | rg -o "[0-9]+x[0-9]+\+[0-9]+\+[0-9]+")

    for resolution in $resolutions
        set tokens (string split "+" $resolution)
        set w_h    (string split "x" $tokens[1])
        set width  $w_h[1]
        set height $w_h[2]

        set offset_x $tokens[2]
        set offset_y $tokens[3]

        set center_x (math "$offset_x + $width/2 - $lock_img_size_x/2")
        set center_y (math "$offset_y + $height/2 - $lock_img_size_y/2")

        convert $tmpfile $lock_img -geometry +$center_x+$center_y -composite -matte $tmpfile
    end

    i3lock -i $tmpfile
    rm $tmpfile
end
