function ytvid -w yt-dlp -a url video audio
  if test ! $video
    yt-dlp --no-playlist -F "$url" | rg "60fps|audio|1080|720|HD"
  else
    if test ! $audio
      yt-dlp -o "%(title)s.%(ext)s" --no-playlist -f "$video" "$url"
    else
      yt-dlp -o "%(title)s.%(ext)s" --no-playlist -f "$video"+"$audio" "$url"
    end
  end
end
