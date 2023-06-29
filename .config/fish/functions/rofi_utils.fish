function rofi_utils
  set -l change_wallpaper "Change Wallpaper"
  set -l pick_book "Pick Book"
  set -l copy_clipboard "Copy From Clipboard History"
  set -l ytmpv "Watch Video With MPV"
  set -l choice (echo -n $change_wallpaper\n$pick_book\n$copy_clipboard\n$ytmpv | rofi -dmenu)

  switch $choice
    case $change_wallpaper
      set -l wallpaper_dir "$HOME/Documents/git/dotfiles/.wallpapers"
      set -l wallpaper (fd '' $wallpaper_dir -x echo {/} | rofi -dmenu)
      if test -n "$wallpaper"
        feh --bg-scale "$wallpaper_dir/$wallpaper"
      end

    case $pick_book
      set -l books_dir $HOME/Dropbox/Books
      set -l book (fd pdf $books_dir -x echo {/} | rofi -dmenu)
      if test -n "$book"
        zathura "$books_dir/$book" &
      end

    case $copy_clipboard
      history | rofi -dmenu | xargs echo -n | xclip -sel clip

    case "$ytmpv"
      set -l url (xclip -selection c -o)
      set -l resolution (echo -e "480\n720\n1080\nbest" | rofi -dmenu  -columns 2 -line-margin 5 -padding 10 -p 'Resolution: ')
      switch $resolution
        case "best"
          mpv --ytdl-format="best" $url
        case '*'
          mpv --ytdl-format="[height<=?"$resolution"]" $url
      end

    case ''
      # ignore

    case '*'
      notify-send "Unknown command '$choice'"
  end
end
