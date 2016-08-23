
function change-wallpaper
    set -l wallpaper_dir "$HOME/Documents/git/dotfiles/.wallpapers/"
    set -l wallpapers (string join '\n' (ls $wallpaper_dir))
    set -l wallpaper (echo -e $wallpapers | rofi -dmenu)
    feh --bg-scale "$wallpaper_dir""$wallpaper"
end
