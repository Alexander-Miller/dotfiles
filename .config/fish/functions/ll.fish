
function ll
    if which exa > /dev/null
        exa -l $argv
    else
        ls -lh $argv
    end
end
