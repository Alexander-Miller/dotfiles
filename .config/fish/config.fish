
# remove ansi term prompt issues
function fish_title;end
alias ew='emacsclient -nw -a=""'
alias sp='systemctl suspend'
alias yt='youtube-dl'

set -gx TERMINAL urxvt

. ~/.config/fish/local.fish
