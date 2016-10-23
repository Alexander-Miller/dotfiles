
function ytvid -a url video audio
  if test ! $video
    youtube-dl --no-playlist -F "$url" | ag "60fps|audio"
  else
    youtube-dl --no-playlist -f "$video"+"$audio" "$url"
  end
end
