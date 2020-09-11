((magit-log:magit-log-mode "-n256" "--graph" "--color" "--decorate")
 (magit-fetch "--prune")
 (magit-stash "--include-untracked")
 (magit-rebase "--autostash"))
