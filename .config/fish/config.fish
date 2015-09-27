set -xU RUST_SRC_PATH $HOME/Documents/git/rust/src

# remove ansi term prompt issues
function fish_title;end
alias ew='emacsclient -nw -a=""'
alias sp='systemctl suspend'
alias yt='youtube-dl'

set -gx TERMINAL urxvt

. ~/.config/fish/local.fish
