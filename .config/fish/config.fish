#!/usr/bin/fish

fish_vi_mode

set -x RUST_SRC_PATH $HOME/Documents/git/rust/src
set -x fish_color_search_match --background=black --foreground=blue
set -x fish_color_command blue
set -x fish_color_operator magenta
set -x XDG_CONFIG_HOME $HOME/.config

abbr -a yt    'youtube-dl'
abbr -a ytmp3 'yt --extract-audio --audio-format mp3'
abbr -a sp    'systemctl suspend'
abbr -a cbr   'cargo build --release'
abbr -a t2    'tmux -2'
abbr -a setbg 'feh --bg-scale'
abbr -a emc   'emacsclient --no-wait --alternate-editor=""'
abbr -a fishc 'source $HOME/.config/fish/config.fish'
abbr -a gis   'git status'
abbr -a gfa   'git fetch --all'
abbr -a gp    'git pull'
