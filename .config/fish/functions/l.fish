function l
  if which exa > /dev/null ^&1
    exa  $argv
  else
    ls -h $argv
  end
end
