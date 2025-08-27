((gnus-select-group nil)
 (magit-blame
  ("-w"))
 (magit-branch nil)
 (magit-cherry-pick
  ("--ff")
  nil)
 (magit-commit
  ("--gpg-sign=F0DD604F43BD7D28BAB3AEEFDCCDED2EB72F6BAC"))
 (magit-diff
  ("--no-ext-diff" "--stat")
  ("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "--stat")
  ("--ignore-all-space" "--no-ext-diff" "--stat"))
 (magit-dispatch nil)
 (magit-fetch nil)
 (magit-file-dispatch nil)
 (magit-gitignore nil)
 (magit-log
  ("-n50" "--graph" "--color" "--decorate")
  ("-n256" "--decorate"))
 (magit-merge nil)
 (magit-pull
  ("--rebase" "--autostash"))
 (magit-push
  ("--force-with-lease"))
 (magit-rebase nil
               ("--autostash"))
 (magit-remote
  ("-f"))
 (magit-reset nil)
 (magit-revert
  ("--edit"))
 (magit-shortlog
  ("--numbered" "--summary"))
 (magit-show-refs nil)
 (magit-stash nil)
 (magit-submodule nil)
 (magit-tag nil))
