function fish_prompt
	and set retc green; or set retc red
    tty|grep -q tty; and set tty tty; or set tty pts

    set_color $retc
    if [ $tty = tty ]
        echo -n .-
    else
        echo -n '┬─'
    end
    #set_color -o green
    #echo -n [
    if [ $USER = root ]
        set_color -o red
    else
        set_color -o yellow
    end
    echo -n $USER
    set_color -o white
    echo -n @
    if [ -z "$SSH_CLIENT" ]
        set_color -o blue
    else
        set_color -o cyan
    end
    echo -n (hostname)
    set_color -o yellow
    #echo -n :(prompt_pwd)
    echo -n :(pwd|sed "s=$HOME=~=")
    set_color -o green
    echo -n -s (__fish_git_prompt) "$__fish_prompt_normal" "$prompt_status" "$delim"
    set_color normal
    set_color $retc
    #if [ $tty = tty ]
    #    echo -n '-'
    #else
    #    echo -n '─'
    #end
    #set_color -o green
    #echo -n '['
    #set_color normal
    #set_color $retc
    #echo -n (date +%X)
    #set_color -o green
    #echo -n ]

    # Check if acpi exists
    if not set -q __fish_nim_prompt_has_acpi
    	if type acpi > /dev/null
    		set -g __fish_nim_prompt_has_acpi ''
    	else
    		set -g __fish_nim_prompt_has_acpi '' # empty string
    	end
    end

    if test "$__fish_nim_prompt_has_acpi"
		if [ (acpi -a 2> /dev/null | grep off) ]
			echo -n '─['
			set_color -o red
			echo -n (acpi -b|cut -d' ' -f 4-)
			set_color -o green
			echo -n ']'
		end
	end
    echo
    set_color normal
    for job in (jobs)
        set_color $retc
        if [ $tty = tty ]
            echo -n '; '
        else
            echo -n '│ '
        end
        set_color brown
        echo $job
    end
    set_color normal
    set_color $retc
    if [ $tty = tty ]
        echo -n "'-> "
    else
        echo -n '╰─> '
    end
    #set_color -o red
    #echo -n '$ '
    set_color normal

end
