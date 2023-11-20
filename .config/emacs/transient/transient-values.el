((magit-log:magit-log-mode "-n256" "--graph" "--color" "--decorate")
 (magit-fetch "--prune" "--tags" "--force")
 (magit-stash "--include-untracked")
 (magit-rebase "--autostash"))
