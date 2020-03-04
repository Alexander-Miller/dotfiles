function la
    if which exa > /dev/null 2>&1
        exa -la $argv
    else
        ls -lah $argv
    end
end
