#!/usr/bin/fish
set -Ux XDG_CONFIG_HOME   $HOME/.config
set -Ux EMACS_HOME        $XDG_CONFIG_HOME/emacs/
set -Ux LS_COLORS         "di=94;1:fi=93;:ln=31:pi=5:so=5:bd=5:cd=5:or=31"
set -gx EDITOR            "em e"
set -e  fish_greeting
set -eU fish_user_paths
set -e  EMACS

bind \ej down-or-search
bind \ek up-or-search

add_to_user_path ~/.sdkman/candidates/java/current/bin sdkman
add_to_user_path ~/.cask/bin

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
abbr -a yt    'yt-dlp'
abbr -a ytmp3 'yt-dlp --no-playlist --extract-audio --audio-format mp3'
abbr -a sp    'systemctl suspend'
abbr -a t2    'tmux -2'
abbr -a setbg 'feh --bg-scale'
abbr -a fishc 'source $HOME/.config/fish/config.fish'
abbr -a R     'paru -Rsn'
abbr -a sys   'systemctl'
abbr -a ss    'sudo systemctl'
abbr -a sysu  'systemctl --user'
abbr -a rmr   'rm -rf'
abbr -a lag   'la | rg'
abbr -a pa    'paru'

set -l LOCAL_CFG "$XDG_CONFIG_HOME/fish/local.fish"
if test -e $LOCAL_CFG
    source $LOCAL_CFG
end
