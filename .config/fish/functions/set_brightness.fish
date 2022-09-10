function set_brightness
  brightnessctl s "$argv[1]"
  notify-send --icon=battery "Brightness set to $(brightnessctl g)"
end
