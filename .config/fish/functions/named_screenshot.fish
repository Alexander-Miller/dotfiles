function named_screenshot
  set -l name (rofi -dmenu -lines 1 -columns 1 -line-margin 5 -padding 10 -p 'name: ')
  maim -s ~/Pictures/$name.png
end
