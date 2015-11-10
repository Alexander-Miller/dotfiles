
function fish_title;end

function emc
    emacsclient --no-wait --alternate-editor="" $argv
end

function yt
    youtube-dl $argv
end

function ytmp3
    yt --extract-audio --audio-format mp3 $argv
end

function fishconf
    fish "$HOME/.config/fish/config.fish"
end

function setbg
    feh --bg-scale $argv
end

function sp
    systemctl suspend
end

function cbr
    cargo build --release
end
