function add_to_user_path -a dir cmd -d "Add a dir to $fish_user_paths (optionally) depending on the existence of a cmd."
    set -l should_add true

    if not test -e $dir
        set should_add false
    end

    if contains $dir $fish_user_paths
        set should_add false
    end

    if test $cmd && not test (binary_exists $cmd)
        set should_add false
    end

    if test $should_add = true
        set -xU fish_user_paths $fish_user_paths $dir
    end
end
