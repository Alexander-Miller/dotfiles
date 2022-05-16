function ytvid -w yt-dlp -a url video audio
  if test ! $video
    yt-dlp --no-playlist -F "$url" | rg "60fps|audio"
  else
    yt-dlp -o "%(title)s.%(ext)s" --no-playlist -f "$video"+"$audio" "$url"
  end
end
