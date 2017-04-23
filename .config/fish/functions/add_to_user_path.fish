function add_to_user_path -a dir
    if test (not contains $dir $fish_user_paths)
        if test -e $dir
            set -U fish_user_paths $fish_user_paths $dir
        end
    end
end
