function ll
    if which exa > /dev/null ^&1
        exa -l $argv
    else
        ls -lh $argv
    end
end
