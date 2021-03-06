;; ------------------------------------------------------------------------
;; @exec-path-from-shell
;; Make Emacs use the $PATH set up by the user's shell
;; https://github.com/purcell/exec-path-from-shell
(el-get-bundle exec-path-from-shell)
(exec-path-from-shell-initialize)
(let ((envs '("PATH" "VIRTUAL_ENV" "GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))
