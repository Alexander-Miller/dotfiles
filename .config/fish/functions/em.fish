function em
  switch $argv[1]
	case "compile"
	  emacs -q --debug-init --batch -l $EMACS_HOME/tools/compile.el $argv[2..-1]
	case "sync"
	  emacs -q --batch -l $EMACS_HOME/tools/sync.el $argv[2..-1]
	case "prune"
	  emacs -q --batch -l $EMACS_HOME/tools/prune.el $argv[2..-1]
  end
end
