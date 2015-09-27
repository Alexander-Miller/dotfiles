alias ew='emacsclient -nw -a=""'

set -xU RUST_SRC_PATH $HOME/Documents/git/rust/src
set -xg fish_color_search_match --background=black --foreground=blue
set -xg fish_color_command blue

function fish_title;end

function yt
    youtube-dl $argv
end

function ytmp3
    yt --extract-audio --audio-format mp3 $argv
end

function fishconf
    fish $HOME/.config/fish/config.fish
end

function setbg
    feh --bg-scale $argv
end

function sp
    systemctl suspend
end
