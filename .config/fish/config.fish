#!/usr/bin/fish

fish_vi_mode

set -g fish_key_bindings fish_user_key_bindings
set -x RUST_SRC_PATH     $HOME/Documents/git/rust/src
set -x XDG_CONFIG_HOME   $HOME/.config
set -e fish_greeting

set -x fish_color_autosuggestion    888888
set -x fish_color_command           blue
set -x fish_color_end               yellow
set -x fish_color_comment           888888
set -x fish_color_cwd               000
set -x fish_color_prefix            000
set -x fish_color_cwd_root          000
set -x fish_color_error             red --bold
set -x fish_color_escape            magenta
set -x fish_color_completion        000
set -x fish_color_history_current   000
set -x fish_color_host              000
set -x fish_color_match             000
set -x fish_color_normal            ccb18b
set -x fish_color_operator          magenta
set -x fish_color_param             cyan
set -x fish_color_quote             green
set -x fish_color_redirection       yellow
set -x fish_color_search_match      --background=1f1f1f
set -x fish_color_selection         000
set -x fish_color_status            000
set -x fish_color_user              000
set -x fish_color_valid_path        --underline
set -x fish_pager_color_completion  ccb18b
set -x fish_pager_color_description green
set -x fish_pager_color_prefix      blue
set -x fish_pager_color_progress    202020 --background=green

abbr -a e     'exit'
abbr -a yt    'youtube-dl'
abbr -a ytmp3 'youtube-dl --extract-audio --audio-format mp3'
abbr -a sp    'systemctl suspend'
abbr -a cbr   'cargo build --release'
abbr -a t2    'tmux -2'
abbr -a setbg 'feh --bg-scale'
abbr -a emc   'emacsclient --no-wait --alternate-editor=""'
abbr -a fishc 'source $HOME/.config/fish/config.fish'
abbr -a gis   'git status'
abbr -a gfa   'git fetch --all'
abbr -a gp    'git pull'
