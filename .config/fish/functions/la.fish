
function la
    if which exa > /dev/null
        exa -la $argv
    else
        ls -lah $argv
    end
end
