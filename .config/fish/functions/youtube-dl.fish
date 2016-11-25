function youtube-dl
  if command youtube-dl $argv
    notify-send "Success" "youtube-dl has finished."
  else
    notify-send "Error" "youtube-dl has failed."
  end
end
