function ll
  if which eza &> /dev/null
    eza -l $argv
  else
    ls -lh $argv
  end
end
