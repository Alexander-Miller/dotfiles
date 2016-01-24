
function mkcd
    set -l dir_name (string join ' ' $argv)
    mkdir $dir_name
    cd $dir_name
end
