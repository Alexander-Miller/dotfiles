#!/usr/bin/fish

function i3lock_wrapper --description "Sets a blurred screenshot of the current desktop as i3lock background."
    set tmpfile (mktemp --tmpdir i3lock-wrapper-XXXXXXXXXX.png)
    maim $tmpfile
    mogrify -liquid-rescale 25% -liquid-rescale 400% $tmpfile
    i3lock -i $tmpfile
    rm $tmpfile
end
