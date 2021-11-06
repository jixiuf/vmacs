((magit-log:magit-log-mode "-n50" "--graph" "--color" "--decorate")
 (magit-pull "--rebase" "--autostash")
 (magit-push "--force-with-lease"))
