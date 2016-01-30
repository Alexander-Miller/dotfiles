
function youtube-dl
    if command youtube-dl $argv
        notify-send -u low "youtube-dl finished \n $argv[-1]"
    else
        notify-send -u critical "youtube-dl failed \n $argv[-1]"
    end
end
