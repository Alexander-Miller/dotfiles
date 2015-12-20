
# The fish_mode_prompt function is prepended to the prompt
function fish_mode_prompt --description "Displays the current mode"
    # Do nothing if not in vi mode
    if set -q __fish_vi_mode

        set_color blue
        echo -n '['

        switch $fish_bind_mode
            case default
                set_color --bold red
                echo -n 'N'
            case insert
                set_color --bold green
                echo -n 'I'
            case visual
                set_color --bold magenta
                echo -n 'V'
        end

        set_color normal
        set_color blue
        echo -n -s ']-'

    end
end
