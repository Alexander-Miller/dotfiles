function magit
    if git status > /dev/null 2>&1
        emacsclient -nw --eval "(call-interactively #'magit-status)"
    else
        echo "Not in a git repo"
        return 1
    end
end
