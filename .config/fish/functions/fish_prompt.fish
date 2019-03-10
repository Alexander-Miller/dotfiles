
set fish_prompt_color_paren blue
set fish_prompt_color_host  yellow
set fish_prompt_color_pwd   magenta
set fish_prompt_color_git   cyan


function fish_prompt --description 'Write out the prompt'
    switch $TERM
        case dumb
            fish_prompt_simple
        case '*'
            set -g last_status $status
            fish_prompt_init
            fish_prompt_host
            fish_prompt_pwd
            fish_prompt_git
            fish_prompt_status
            fish_prompt_finish
    end
end

function fish_prompt_simple
    set_color green
    echo -n -s ' ' (string split '/' (pwd))[-1]
    set_color cyan --bold
    echo -n -s ' → '
end

function fish_prompt_init

    # Hack; fish_config only copies the fish_prompt function (see #736)
    if not set -q -g __fish_classic_git_functions_defined

        # init git prompt while we're at it
	    set -g __fish_git_prompt_char_dirtystate        '+'
	    set -g __fish_git_prompt_char_cleanstate        '✔'
	    set -g __fish_git_prompt_char_stagedstate       'S'
	    set -g __fish_git_prompt_char_upstream_ahead    '↑'
	    set -g __fish_git_prompt_char_upstream_behind   '↓'
	    set -g __fish_git_prompt_char_stashstate        '$'
	    set -g __fish_git_prompt_char_stateseparator    '|'
	    set -g __fish_git_prompt_char_untrackedfiles    '…'
	    set -g __fish_git_prompt_char_upstream_equal    '='

	    # set -g __fish_git_prompt_char_invalidstate      '#' '✖'
	    # set -g __fish_git_prompt_char_stagedstate       '+' '●'
	    # set -g __fish_git_prompt_char_upstream_diverged '<>'
	    # set -g __fish_git_prompt_char_upstream_prefix   ''

        set -g __fish_classic_git_functions_defined

        function __fish_repaint_user --on-variable fish_color_user --description "Event handler, repaint when fish_color_user changes"
            if status --is-interactive
                commandline -f repaint ^ /dev/null
            end
        end

        function __fish_repaint_host --on-variable fish_color_host --description "Event handler, repaint when fish_color_host changes"
            if status --is-interactive
                commandline -f repaint ^ /dev/null
            end
        end

        function __fish_repaint_status --on-variable fish_color_status --description "Event handler; repaint when fish_color_status changes"
            if status --is-interactive
                commandline -f repaint ^ /dev/null
            end
        end

        function __fish_repaint_bind_mode --on-variable fish_key_bindings --description "Event handler; repaint when fish_key_bindings changes"
            if status --is-interactive
                commandline -f repaint ^ /dev/null
            end
        end

        # initialize our new variables
        if not set -q __fish_classic_git_prompt_initialized
            set -qU fish_color_user
            or set -U fish_color_user -o green
            set -qU fish_color_host
            or set -U fish_color_host -o cyan
            set -qU fish_color_status
            or set -U fish_color_status red
            set -U __fish_classic_git_prompt_initialized
        end
    end
end


function fish_prompt_block -a color
    set_color $fish_prompt_color_paren
    echo -n -s '-('
    set_color $color
    echo -n -s $argv[2..-1]
    set_color $fish_prompt_color_paren
    echo -n -s ')'
end


function fish_prompt_host
    if not set -q __fish_prompt_hostname
        set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
    end
    fish_prompt_block $fish_prompt_color_host $USER '@' $__fish_prompt_hostname
end


function fish_prompt_pwd
    set -l dir (pwd)
    if [ -n "$dir" ]
        fish_prompt_block $fish_prompt_color_pwd (pwd)
    else
        fish_prompt_block $fish_prompt_color_pwd "?"
    end
end


function fish_prompt_git
    set -l dir (pwd)
    if [ -n "$dir" ]
        set git_stat (__fish_git_prompt)
        if [ -n "$git_stat" ]
            set len      (string length $git_stat)
            set git_stat (string sub -s 3 -l (math "$len - 3") $git_stat)
            if [ -n "$git_stat" ]
                fish_prompt_block $fish_prompt_color_git $git_stat
            end
        end
    else
        fish_prompt_block $fish_prompt_color_git "?"
    end
end


function fish_prompt_status
    if [ $last_status -ne 0 ]
        fish_prompt_block red $last_status
    end
end


function fish_prompt_finish
    if [ $last_status -eq 0 ]
        echo -n -s \n (set_color red) ❯ (set_color yellow) ❯ (set_color green) ❯ ' '
    else
        set_color red
        echo -s \n ❯❯❯ ' '
    end
end


# function fish_prompt_old --description 'Write out the prompt'
#     set -l last_status $status

#     # Just calculate this once, to save a few cycles when displaying the prompt
#     if not set -q __fish_prompt_hostname
#         set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
#     end

#     set -l normal (set_color normal)


#     set -l color_cwd
#     set -l prefix
#     switch $USER
#         case root toor
#             if set -q fish_color_cwd_root
#                 set color_cwd $fish_color_cwd_root
#             else
#                 set color_cwd $fish_color_cwd
#             end
#             set suffix '#'
#         case '*'
#             set color_cwd $fish_color_cwd
#             set suffix '>'
#     end

#     set -l prompt_status
#     if test $last_status -ne 0
#         set prompt_status ' ' (set_color $fish_color_status) "[$last_status]" "$normal"
#     end

#     set -l mode_str
#     switch "$fish_key_bindings"
#         case '*_vi_*' '*_vi'
#             # possibly fish_vi_key_bindings, or custom key bindings
#             # that includes the name "vi"
#             set mode_str (
# 			echo -n " "
# 			switch $fish_bind_mode
# 			case default
# 				set_color --bold --background red white
# 				echo -n "[N]"
# 			case insert
# 				set_color --bold green
# 				echo -n "[I]"
# 			case visual
# 				set_color --bold magenta
# 				echo -n "[V]"
# 			end
# 			set_color normal
# 		)
#     end

#     echo -n -s (set_color $fish_color_user) "$USER" $normal @ (set_color $fish_color_host) "$__fish_prompt_hostname" $normal ' ' (set_color $color_cwd) (prompt_pwd) $normal (__fish_git_prompt) $normal $prompt_status "$mode_str" "> "
# en
