function l
    which eza &> /dev/null
    if test $status -eq 0
        eza  $argv
    else
        ls -h $argv
    end
end
