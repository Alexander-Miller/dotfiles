function random_wallpaper
  set -l wallpaper_dir "$HOME/Documents/git/dotfiles/.wallpapers"
  set -l wallpaper (random choice (ls "$wallpaper_dir"))
  feh --bg-scale "$wallpaper_dir/$wallpaper"
end
