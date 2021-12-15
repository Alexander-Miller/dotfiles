function l
    which exax &> /dev/null
    if test $status -eq 0
        exa  $argv
    else
        ls -h $argv
    end
end
