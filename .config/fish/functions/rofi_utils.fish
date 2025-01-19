function rofi_utils
  set -l change_wallpaper "Change Wallpaper"
  set -l pick_book "Pick Book"
  set -l copy_clipboard "Copy From Clipboard History"
  set -l ytmpv "Watch Video With MPV"
  set -l suspend "Suspend"
  set -l bluetooth "Bluetooth"
  set -l choices \
    ( \
    string join "|" \
    $suspend \
    $change_wallpaper \
    $pick_book \
    $copy_clipboard \
    $ytmpv \
    $bluetooth \
    )
  set -l choice (echo -n "$choices" | rofi -sep "|" -dmenu -i)

  switch $choice
    case $change_wallpaper
      set -l wallpaper_dir "$HOME/Documents/git/dotfiles/.wallpapers"
      set -l wallpaper (fd '' $wallpaper_dir -x echo {/} | rofi -dmenu -i)
      if test -n "$wallpaper"
        feh --bg-scale "$wallpaper_dir/$wallpaper"
      end

    case $pick_book
      set -l books_dir $HOME/Dropbox/Books
      set -l book (fd pdf $books_dir -x echo {/} | rofi -dmenu -i)
      if test -n "$book"
        zathura "$books_dir/$book" &
      end

    case $copy_clipboard
      history | rofi -dmenu -i | xargs echo -n | xclip -sel clip

    case $ytmpv
      set -l url (xclip -selection c -o)
      set -l resolution (echo -e "480\n720\n1080\nbest" | rofi -dmenu -i -columns 2 -line-margin 5 -padding 10 -p 'Resolution: ')
      switch $resolution
        case "best"
          mpv "$url"
        case '*'
          mpv --ytdl-format="bestvideo[height<=$resolution]+bestaudio/best" "$url"
      end

    case $suspend
      notify-send "Suspending"
      systemctl suspend

    case $bluetooth
      set -l bctl_list (bluetoothctl devices)
      set -l device (echo $bctl_list | awk '{ print $3 }' | rofi -dmenu)
      set -l addr (echo $bctl_list | rg $device | awk '{ print $2 }')
      set -l is_connected (bluetoothctl info $addr | rg "Connected: yes")
      if test -n "$is_connected"
        bluetoothctl disconnect $addr
      else
        bluetoothctl connect $addr
      end

    case ''
      # ignore

    case '*'
      notify-send "Unknown command '$choice'"
  end
end
