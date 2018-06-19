function add_to_user_path -a dir
    if not contains $dir $fish_user_paths
        if test -e $dir
            set -xU fish_user_paths $fish_user_paths $dir
        end
    end
end
