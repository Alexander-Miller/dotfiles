killall xcape

while pgrep -x xcape > /dev/null ^&1
    sleep 1
end

xcape
