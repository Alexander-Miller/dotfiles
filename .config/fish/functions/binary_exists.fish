function binary_exists -a binary
    which $binary > /dev/null ^&1
end
