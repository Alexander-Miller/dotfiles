function l
    set -l has_exa (which exa) &> /dev/null
    if $has_exa
        exa  $argv
    else
        ls -h $argv
    end
end
