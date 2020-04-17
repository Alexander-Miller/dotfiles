function ytmpv
    set -l url (xclip -selection c -o)
    set -l resolution (echo -e "480\n720\n1080\nbest" | rofi -dmenu  -columns 2 -line-margin 5 -padding 10 -p 'Resolution: ')
    switch $resolution
        case "best"
            mpv --ytdl-format="best" $url
        case '*'
            mpv --ytdl-format="[height<=?"$resolution"]" $url
    end
end
