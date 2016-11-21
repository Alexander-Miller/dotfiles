function la
    if which exa > /dev/null ^&1
        exa -la $argv
    else
        ls -lah $argv
    end
end
