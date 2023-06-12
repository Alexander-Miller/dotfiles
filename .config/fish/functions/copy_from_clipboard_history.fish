function copy_from_clipboard_history
  history | rofi -dmenu | xargs echo -n | xclip -sel clip
end
