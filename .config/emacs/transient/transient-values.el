((magit-log:magit-log-mode "-n256" "--graph" "--color" "--decorate")
 (magit-fetch "--prune")
 (magit-rebase "--autostash"))
