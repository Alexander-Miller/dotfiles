function named_screenshot
    set -l name (rofi -dmenu -lines 1 -columns 1 -line-margin 5 -padding 10 -p 'name: ')

    if test -z $name
      set name "screenshot-$(date +%F-%T)"
    end

    maim -s ~/Pictures/$name.png
    notify-send "Screenshot '$name' taken" --icon="image"
end
