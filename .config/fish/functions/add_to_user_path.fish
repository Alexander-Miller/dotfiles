function add_to_user_path -a dir
    set -l dir_exists (test -e $dir)
    set -l dir_not_in_path (contains $dir $fish_user_paths)
    if  [ $dir_exsists -a $dir_not_in_path ]
        set -U fish_user_paths $fish_user_paths $dir
    end
end
