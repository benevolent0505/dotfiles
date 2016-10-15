;; For Perl
;; Reference from https://github.com/shibayu36/emacs/blob/master/emacs.d/inits/20-edit-mode-perl.el
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-h f") 'cperl-perldoc)))
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cgi$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.psgi$" . cperl-mode) auto-mode-alist))
;; `Unquoted string "requires" may clash with future reserved word` とかいうエラーがcpanfileで出るので一時止める
;; (setq auto-mode-alist (cons '("cpanfile$" . cperl-mode) auto-mode-alist))

(setq cperl-indent-level 4
      cperl-continued-statement-offset 4
      cperl-close-paren-offset -4
      cperl-comment-column 40
      cperl-highlight-variables-indiscriminately t
      cperl-indent-parens-as-block t
      cperl-label-offset -4
      cperl-tab-always-indent nil
      cperl-font-lock t)
(add-hook 'cperl-mode-hook
          '(lambda ()
             (progn
               (setq indent-tabs-mode nil)
               (setq tab-width nil))))

;; flycheck settings
(add-hook 'cperl-mode-hook 'flycheck-mode)

;; disable my module error
;; 要Project::Libs
;; http://m0t0k1ch1st0ry.com/blog/2014/07/07/flycheck/
(flycheck-define-checker perl-project-libs
  "A perl syntax checker."
  :command ("perl"
            "-MProject::Libs lib_dirs => [qw(local/lib/perl5)]"
            "-wc"
            source-inplace)
  :error-patterns ((error line-start
                          (minimal-match (message))
                          " at " (file-name) " line " line
                          (or "." (and ", " (zero-or-more not-newline)))
                          line-end))
  :modes (cperl-mode))
(add-hook 'cperl-mode-hook
          (lambda ()
            (flycheck-mode t)
            (setq flycheck-checker 'perl-project-libs)))

;; 要carton
(defun run-perl-test ()
  (interactive)
  (compile (format "cd %s; carton exec -- prove -lvr %s" (vc-git-root default-directory) (buffer-file-name (current-buffer)))))

