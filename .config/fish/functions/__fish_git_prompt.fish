
function __fish_git_prompt

	set -l prompt_color_line $argv[1]
	set -l git_color cyan
	set -l git_status (git status ^ /dev/null)

	if test -n "$git_status"
		set -l git_branch (echo $git_status | ag -o "On branch .*" | cut -d ' ' -f 3)
		set -l git_detached (echo $git_status | ag -o "HEAD detached at [[:alnum:]]*")
		set -l out

		if test -n "$git_branch"
			set out $git_branch
		else if test -n "$git_detached"
			set out $git_detached
		end

		if set -q out
			echo -n -s (set_color $prompt_color_line) '-[' (set_color $git_color) ' ' $out (set_color $prompt_color_line) ']'
		end
	end
end
