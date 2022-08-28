
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

function fish_prompt_pwd
  set -l dir (pwd)
  set_color $fish_prompt_color_pwd
  if [ -n "$dir" ]
    echo -n -s " $dir "
  else
    echo -n -s " ? "
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
        set_color $fish_prompt_color_git
        echo -n -s "$git_stat "
      end
    end
  end
end

function fish_prompt_status
  if [ $last_status -ne 0 ]
    set_color red
    echo -n -s "$last_status "
  end
end

function fish_prompt_finish
  if [ $last_status -eq 0 ]
    echo -n -s \n (set_color green) ' $ '
  else
    echo -n -s \n (set_color red) ' $ '
  end
end
