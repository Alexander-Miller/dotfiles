function ytvid -w youtube-dl -a url video audio
  if test ! $video
    youtube-dl --no-playlist -F "$url" | rg "60fps|audio"
  else
    youtube-dl -o "%(title)s.%(ext)s" --no-playlist -f "$video"+"$audio" "$url"
  end
end
