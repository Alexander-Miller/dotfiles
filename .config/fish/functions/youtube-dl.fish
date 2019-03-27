function youtube-dl
  if command youtube-dl $argv
    notify-send "Success" "youtube-dl has finished." --icon=youtube
  else
    notify-send "Error" "youtube-dl has failed." --icon=youtube
  end
end
