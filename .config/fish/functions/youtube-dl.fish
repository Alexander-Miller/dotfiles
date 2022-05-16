function yt-dlp
  if command yt-dlp $argv
    notify-send "Success" "yt-dlp has finished." --icon=youtube
  else
    notify-send "Error" "yt-dlp has failed." --icon=youtube
  end
end
