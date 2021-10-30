((magit-log:magit-log-mode "-n50")
 (magit-pull "--rebase" "--autostash")
 (magit-push "--force-with-lease"))
