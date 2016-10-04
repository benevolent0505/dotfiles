;; default c-x c-f
(setq default-directory "~/")

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; タブ幅
(setq-default tab-width 2)

;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; emacs-lisp-modeのフックをセット
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (when (require 'eldoc nil t)
               (setq eldoc-idle-delay 0.2)
               (setq eldoc-echo-area-use-multiline-p t)
               (turn-on-eldoc-mode))))

;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
