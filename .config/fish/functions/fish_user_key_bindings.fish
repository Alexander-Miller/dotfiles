
function fish_user_key_bindings
    fish_vi_key_bindings
    bind --mode insert \e backward-kill-word
    bind --mode insert \n execute
    bind --mode insert \cb backward-char
    bind --mode insert \cf forward-char
    for_all_modes \ca beginning-of-line
    for_all_modes \ce end-of-line
    for_all_modes \cl 'clear; commandline -f repaint'
    for_all_modes \ck kill-line
    for_all_modes \eh __fish_man_page
    for_all_modes \cy yank
    for_all_modes \ej down-or-search
    for_all_modes \ek up-or-search
    for_all_modes \cd delete-char
end

function for_all_modes -a key func -d "Bind $func to $key for all vi modes."
    bind --erase --mode default $key
    bind --erase --mode insert  $key
    bind --erase --mode visual  $key
    bind --mode default $key $func
    bind --mode insert  $key $func
    bind --mode visual  $key $func
end
