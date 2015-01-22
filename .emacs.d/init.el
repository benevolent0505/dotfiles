;; Cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; ------------------------------------------------------------------------
;; @ load-path
;; load-path§Œƒ…≤√¥ÿøÙ
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-path§Àƒ…≤√§π§Î•’•©•Î•¿
;; 2§ƒ∞ æÂ•’•©•Î•¿§ÚªÿƒÍ§π§ÎæÏπÁ§Œ∞˙øÙ => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp" "conf" "public_repo")

;; ------------------------------------------------------------------------
;; @ general
;; common lisp
(require 'cl)
;;c-x c-f§Œ•«•’•©•Î•»
(setq default-directory "~/")

<<<<<<< HEAD
;;  ∏ª˙•≥°º•…
(set-language-environment "Japanese")
=======
;; ÊñáÂ≠ó„Ç≥„Éº„Éâ
;; (set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
>>>>>>> 223f5aeec3f157a0c410e835f186936eb3cea1ff
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (prefer-coding-system 'utf-8-unix)
         (set-default-coding-systems 'utf-8-unix)
         (setq file-name-coding-system 'sjis)
         (setq locale-coding-system 'utf-8))
        ((eq ws 'ns)
         (require 'ucs-normalize)
         (prefer-coding-system 'utf-8-hfs)
         (setq file-name-coding-system 'utf-8-hfs)
         (setq locale-coding-system 'utf-8-hfs))))

;; Windows§«±—øÙ§»∆¸À‹∏Ï§ÀMeiryo§ÚªÿƒÍ
;; Mac§«±—øÙ§»∆¸À‹∏Ï§ÀRicty§ÚªÿƒÍ
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-face-attribute 'default nil
                             :family "Meiryo" ;; ±—øÙ
                             :height 100)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo"))) ;; ∆¸À‹∏Ï
        ((eq ws 'ns)
         (set-face-attribute 'default nil
                             :family "Ricty" ;; ±—øÙ
                             :height 140)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))))) ;; ∆¸À‹∏Ï

;; •π•ø°º•»•¢•√•◊»Û…Ωº®
(setq inhibit-startup-screen t)

;; scratch§ŒΩÈ¥¸•·•√•ª°º•∏æ√µÓ
(setq initial-scratch-message "")

;; •ƒ°º•Î•–°º»Û…Ωº®
(tool-bar-mode -1)

;; •·•À•Â°º•–°º§Ú»Û…Ωº®
(menu-bar-mode -1)

;; •π•Ø•Ì°º•Î•–°º»Û…Ωº®
(set-scroll-bar-mode nil)

;; •ø•§•»•Î•–°º§À•’•°•§•Î§Œ•’•Î•—•π…Ωº®
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; π‘»÷πÊ…Ωº®
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#800"
                    :height 0.9)


;; π‘»÷πÊ•’•©°º•ﬁ•√•»
;; (setq linum-format "%4d")

;; ≥Á∏Ã§Œ»œ∞œ∆‚§Ú∂Øƒ¥…Ωº®
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; ≥Á∏Ã§Œ»œ∞œøß
(set-face-background 'show-paren-match-face "#500")

;; ¡™¬ÚŒŒ∞Ë§Œøß
(set-face-background 'region "#555")

;; π‘Àˆ§Œ∂ı«Ú§Ú∂Øƒ¥…Ωº®
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; •ø•÷§Ú•π•⁄°º•π§«∞∑§¶
(setq-default indent-tabs-mode nil)

;; •ø•÷…˝
(custom-set-variables '(tab-width 4))

;; yes or no§Úy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; •–•√•Ø•¢•√•◊§Úªƒ§µ§ §§
;; (setq make-backup-files nil)

;; π‘¥÷
(setq-default line-spacing 0)

;; 1π‘§∫§ƒ•π•Ø•Ì°º•Î
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; •’•Ï°º•‡§Œ∆©Ã¿≈Ÿ
(set-frame-parameter (selected-frame) 'alpha '(0.7))

;; C-m§Ànewline-and-indent§Ú≥‰§Í≈ˆ§∆§Î°£ΩÈ¥¸√Õ§œnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; "C-t" §«•¶•£•Û•…•¶§Ú¿⁄§Í¬ÿ§®§Î°£ΩÈ¥¸√Õ§œtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; •´•È•‡»÷πÊ§Ú…Ωº®
(column-number-mode t)

;; ª˛∑◊§Ú…Ωº®
(setq display-time-day-and-date t)
;; (setq display-time-24hr-format t)
(display-time-mode t)

;; •–•√•∆•Í°ºªƒŒÃ§Ú…Ωº®(marvericks§¿§»patch§Ú≈ˆ§∆§ §§§»…Ωº®§µ§Ï§ §§)
;; (display-battery-mode t)

;; •–•√•Ø•¢•√•◊§»•™°º•»•ª°º•÷•’•°•§•Î§Ú~/.emacs.d/backups/§ÿΩ∏§·§Î
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; emacs-lisp-mode§Œ•’•√•Ø§Ú•ª•√•»
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (when (require 'eldoc nil t)
               (setq eldoc-idle-delay 0.2)
               (setq eldoc-echo-area-use-multiline-p t)
               (turn-on-eldoc-mode))))

;; ruby-mode-hookÕ—§Œ¥ÿøÙ§ÚƒÍµ¡
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  (ruby-electric-mode t)
  (ruby-block-mode t))

;; ruby-mode-hook§Àƒ…≤√
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)

;; Emacs24∞ πﬂ…∏Ω‡§ŒColor theme§Úª»§¶
(load-theme 'deeper-blue t)

;; 80ÊñáÂ≠ó„Åß„ÅÆËá™ÂãïÊîπË°å„Çíoff
(setq text-mode-hook 'turn-off-auto-fill)

;; flycheck„ÅÆË®≠ÂÆö
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ------------------------------------------------------------------------
;; @ markdown-mode
;; Major mode for editing Markdown-formatted text files in GNU Emacs.
;; http://jblevins.org/projects/markdown-mode/
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ------------------------------------------------------------------------
;; @ js2-mode
;; Improved JavaScript editing mode for GNU Emacs
;; https://github.com/mooz/js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\.js$" . js2-mode))

;; ------------------------------------------------------------------------
;; @ web-mode.el
;; major mode for editing html templates
;; http://web-mode.org/
(require 'web-mode)
;;; emacs 23∞ ≤º§Œ∏ﬂ¥π
(when (< emacs-major-version 24)
  (defalias 'prog-mode 'fundamental-mode))
;;; ≈¨Õ—§π§Î≥»ƒ•ª“
(add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
;;; •§•Û•«•Û•»øÙ
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset 2)
  (setq web-mode-css-offset 2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset 2)
  (setq web-mode-java-offset 2)
  (setq web-mode-asp-offset 2))
(add-hook 'web-mode-hook 'web-mode-hook)

;; ------------------------------------------------------------------------
<<<<<<< HEAD
=======
;; @ yatex

;; Emacs DE TeX
;; http://www.yatex.org/
;; Êã°ÂºµÂ≠ê„Åå .tex „Å™„Çâ yatex-mode „Å´
(setq auto-mode-alist
  (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; YaTeX „ÅåÂà©Áî®„Åô„ÇãÂÜÖÈÉ®„Ç≥„Éû„É≥„Éâ„ÇíÂÆöÁæ©„Åô„Çã
;; (setq tex-command "platex2pdf") ;; Ëá™‰Ωú„Åó„Åü„Ç≥„Éû„É≥„Éâ„Çí
;; (cond
;;   ((eq system-type 'gnu/linux) ;; GNU/Linux „Å™„Çâ
;;     (setq dvi2-command "evince")) ;; evince „Åß PDF „ÇíÈñ≤Ë¶ß
;;   ((eq system-type 'darwin) ;; Mac „Å™„Çâ
;;     (setq dvi2-command "open -a Preview"))) ;; „Éó„É¨„Éì„É•„Éº„Åß
;; (add-hook 'yatex-mode-hook '(lambda () (setq auto-fill-function nil)))

;; ------------------------------------------------------------------------
;; @ twittering-mode

;; Emacs DE Twitter
;; https://github.com/hayamiz/twittering-mode/tree/master
;; (require 'twittering-mode)
;; (setq twittering-use-master-password t)
;; (setq twittering-icon-mode t)

;; ------------------------------------------------------------------------
>>>>>>> 223f5aeec3f157a0c410e835f186936eb3cea1ff
;; @ egg.el
;; Emacs DE Git
;; https://github.com/byplayer/egg
(when (executable-find "git")
  (require 'egg nil t))

;; ------------------------------------------------------------------------
<<<<<<< HEAD
=======
;; @ rhtml.el

;; rhtml
;; https://github.com/eschulte/rhtml
(when (require 'rhtml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.rhtml\\'" .rhtml-mode)))

;; ------------------------------------------------------------------------
;; @ groovy-mode.el

;; Groovy mode
;; https://github.com/russel/Emacs-Groovy-Mode
(add-to-list 'load-path "~/.emacs.d/emacs-groovy-mode")
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("grooy" . groovy-mode))

;; ------------------------------------------------------------------------
;; @ multi-term.el

;; multi-term
;; http://www.emacswiki.org/emacs/MultiTerm
;; (require 'multi-term)
;; (setq multi-term-program "/bin/bash")

;; ------------------------------------------------------------------------
;; @ howm

;; howm
;; http://howm.sourceforge.jp/index.html
;; howm„É°„É¢‰øùÂ≠ò„ÅÆÂ†¥ÊâÄ
;; (setq hown-directory (concat user-emacs-directory "howm"))
;; ;; howm-menu„ÅÆË®ÄË™û„ÇíÊó•Êú¨Ë™û„Å´
;; (setq howm-menu-lang 'ja)
;; ;; howm-mode„ÇíË™≠„ÅøËæº„ÇÄ
;; (when (require 'howm-mode nil t)
;;   (define-key global-map (kbd "C-c ,,") 'howm-menu))
;; ;; howm„É°„É¢„Çí‰øùÂ≠ò„Å®ÂêåÊôÇ„Å´Èñâ„Åò„Çã
;; (defun howm-save-buffer-and-kill ()
;;   (interactive)
;;   (when (and (buffer-file-name)
;;              (string-match "\\.howm" (buffer-file-name)))
;;     (save-buffer)
;;     (kill-buffer nil)))

;; ;; C-c C-c„Åß„É°„É¢„ÅÆ‰øùÂ≠ò„Å®ÂêåÊôÇ„Å´„Éê„ÉÉ„Éï„Ç°„ÇíÈñâ„Åò„Çã
;; (define-key howm-mode-map (kbd "C-c C-c") 'howm-save-buffer-and-kill)

;; ------------------------------------------------------------------------
;; @ undo-tree.el

;; undo-tree
;; http://melpa.milkbox.net/#/undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; ------------------------------------------------------------------------
;; @ undohist.el

;; undohist
;; https://github.com/m2ym/undohist-el
(when (require 'undohist nil t)
  (undohist-initialize))

;; ------------------------------------------------------------------------
;; @ wgrep.el

;; wgrep
;; https://github.com/mhayashi1120/Emacs-wgrep
(require 'wgrep nil t)

;; ------------------------------------------------------------------------
;; @ color-moccur.el

;; multi-buffer occur (grep) mode
;; http://www.emacswiki.org/emacs/color-moccur.el
(when (require 'color-moccur nil t)
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  (setq moccur-split-word t)
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (setq moccur-use-migemo t)))

;; ------------------------------------------------------------------------
;; @ moccur-edit.el

;; apply replaces to multiple files
;; http://www.emacswiki.org/emacs/moccur-edit.el
(require 'moccur-edit nil t)

;; moccur-edit-finish-edit„Å®ÂêåÊôÇ„Å´„Éï„Ç°„Ç§„É´„Çí‰øùÂ≠ò„Åô„Çã
(defadvice moccur-edit-change-file
  (after save-after-moccur-edit-buffer activate)
  (save-buffer))

;; ------------------------------------------------------------------------
>>>>>>> 223f5aeec3f157a0c410e835f186936eb3cea1ff
;; @ anything.el
;; anything
;; http://www.emacswiki.org/emacs/anything.el
(when (require 'anything nil t)
  (setq
   anything-idle-delay 0.3
   anything-input-idle-delay 0.2
   anything-candidate-number-limit 100
   anything-quick-update t
   anything-enable-shortcuts 'alphabet)
  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo"))
  (require 'anything-match-plugin nil t)
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))
  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150))
  (require 'anything-show-completion nil t)
  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))
  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install)))
;; C-x b§Àanything-for-files§Ú≥‰§Í≈ˆ§∆§Î
(define-key global-map (kbd "C-x b") 'anything-for-files)
;; M-y§Àanything-show-kill-ring§Ú≥‰§Í≈ˆ§∆§Î
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

;; ------------------------------------------------------------------------
;; @ auto-complete.el
;; º´∆∞ ‰¥∞µ°«Ω
;; https://github.com/m2ym/auto-complete
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (setq ac-ignore-case t)
  (ac-config-default))

;; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(setq ruby-insert-encoding-magic-comment nil)

;; ------------------------------------------------------------------------
;; @ ruby-electric.el
;; a minor mode that makes writing and editing Ruby code easier.
;; https://github.com/qoobaa/ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;; ------------------------------------------------------------------------
;; @ ruby-block.el
;; ruby§Œend§À¬–±˛§π§Îπ‘§Ú•œ•§•È•§•»
;; http://www.emacswiki.org/emacs/ruby-block.el
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))

;; ------------------------------------------------------------------------
;; @ inf-ruby.el
;; inf-ruby provides a REPL buffer connected to a Ruby subprocess
;; https://github.com/nonsequitur/inf-ruby/blob/master/inf-ruby.el
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

