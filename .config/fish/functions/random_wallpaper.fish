function random_wallpaper
  set -l wallpaper_dir "$HOME/Documents/git/dotfiles/.wallpapers"
  set -l wallpaper (random choice (ls "$wallpaper_dir"))
  for screen in (hyprctl monitors -j | jq -r '.[].name')
    echo $wallpaper_dir/$wallpaper
    hyprctl hyprpaper wallpaper $screen,$wallpaper_dir/$wallpaper
  end
end
