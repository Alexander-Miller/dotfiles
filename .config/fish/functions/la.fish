function la
    if which eza > /dev/null 2>&1
        eza -la $argv
    else
        ls -lah $argv
    end
end
