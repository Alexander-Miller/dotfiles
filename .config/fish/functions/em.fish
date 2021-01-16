function em
    set -l EMACS ~/Documents/git/emacs/src/emacs
    switch $argv[1]
	    case "compile"
	        $EMACS -q --debug-init --batch -l $EMACS_HOME/tools/compile.el
	    case "sync"
	        env EMACS_INIT_PACKAGES=1 $EMACS -q --batch -l $EMACS_HOME/tools/sync.el $argv[2..-1]
	    case "prune"
	        env EMACS_INIT_PACKAGES=1 $EMACS -q --batch -l $EMACS_HOME/tools/prune.el $argv[2..-1]
    end
end
