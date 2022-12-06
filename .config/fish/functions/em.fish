function em
  switch $argv[1]
	case "compile"
	  emacs -q --debug-init --batch -l $EMACS_HOME/tools/compile.el $argv[2..-1]
	case "sync"
	  emacs -q --batch -l $EMACS_HOME/tools/sync.el $argv[2..-1]
	case "prune"
	  emacs -q --batch -l $EMACS_HOME/tools/prune.el $argv[2..-1]
    case "git" "magit"
      if git status > /dev/null 2>&1
        emacsclient -nw --eval "(call-interactively #'magit-status)"
      else
        echo "Not in a git repo"
        return 1
      end
    case 'e'
      if test -e $argv[2]
        emacsclient --no-wait --create-frame $argv[2]
      else
        echo " File '$argv[2]' not found"
        return 1
      end
    case '*'
      if test -e $argv[1]
        emacsclient --no-wait --create-frame $argv[1]
      else
        echo " Nothing to do for args [$argv]"
        return 1
      end
  end
end
