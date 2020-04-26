function em
    switch $argv[1]
	    case "compile"
	        emacs -q --debug-init --batch -l $EMACS_HOME/tools/compile.el
	    case "sync"
	        env EMACS_INIT_PACKAGES=1 emacs -q --batch -l $EMACS_HOME/tools/sync.el $argv[2..-1]
	    case "prune"
	        env EMACS_INIT_PACKAGES=1 emacs -q --batch -l $EMACS_HOME/tools/prune.el
    end
end
