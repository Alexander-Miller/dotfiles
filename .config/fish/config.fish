#!/usr/bin/fish

set -x RUST_SRC_PATH $HOME/Documents/git/rust/src
set -x __fish_git_prompt_show_informative_status 1
set -x fish_color_search_match --background=black --foreground=blue
set -x fish_color_command blue
set -x fish_color_operator magenta
set -x XDG_CONFIG_HOME $HOME/.config

source $XDG_CONFIG_HOME/fish/aliases/misc.fish
source $XDG_CONFIG_HOME/fish/aliases/git.fish
