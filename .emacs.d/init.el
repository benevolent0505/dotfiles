(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; ------------------------------------------------------------------------
;; @ general

;; common lisp
(require 'cl)

;;c-x c-fのデフォルト
(setq default-directory "~/")

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
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

;; Windowsで英数と日本語にMeiryoを指定
;; Macで英数と日本語にRictyを指定
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-face-attribute 'default nil
                             :family "Meiryo" ;;英数
                             :height 100)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo"))) ;; 日本語
        ((eq ws 'ns)
         (set-face-attribute 'default nil
                             :family "Ricty" ;;英数
                             :height 140)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty")))
        ((eq ws 'x)
         (set-face-attribute 'default nil
                             :family "Inconsolata"
                             :height 120)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty")))))

;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバー非表示
(menu-bar-mode -1)

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; タイトルパーにファイルのフルパスを表示する
(setq frame-title-format "%f")

;; 行番号を表示する
(global-linum-mode t)

;; 括弧の範囲内を強制表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; 括弧内の範囲色
(set-face-background 'show-paren-match-face "#500")

;; 選択領域の色
(set-face-background 'region "#555")

;; 行末の空白を強制表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(setq-default tab-width 2)

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 行間
(setq-default line-spacing 0)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.85))

;; C-mにnewline-and-indentを割り当てる.初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; "C-t" でウィンドウを切り替える.初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; カラム番号を表示
(column-number-mode t)

;; カーソルのある行をハイライトする
(global-hl-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "color-236")))))

;; 時計を表示
(setq display-time-day-and-date t)
;; (setq display-time-24hr-format t)
(display-time-mode t)

;; バッテリー残量を表示(marvericksだとpatchを当てないと表示されない)
;; (display-battery-mode t)

;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
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

;; Emacs24以降標準のColor themeを使う
(load-theme 'misterioso t)

;; 80文字での自動改行をoff
(setq text-mode-hook 'turn-off-auto-fill)

;; ------------------------------------------------------------------------
;; @helm
;; Emacs incremental completion and selection narrowing framework
;; https://github.com/emacs-helm/helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
(helm-autoresize-mode 1)
;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)

(require 'helm-ls-git)

;; ------------------------------------------------------------------------
;; @ Magit
;; http://magit.vc/
;; Magit! A Git Porcelain inside Emacs
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.1")

;; ------------------------------------------------------------------------
;; @ auto-complete.el
;; 自動補完機能
;; https://github.com/m2ym/auto-complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'org-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)
(setq ac-use-fuzzy t)


;; Setting for JavaScript

;; ------------------------------------------------------------------------
;; @ js2-mode
;; https://github.com/mooz/js2-mode
;; Improved JavaScript editing mode for GNU Emacs
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset 2)
             (setq indent-tabs-mode nil)))
(add-hook 'js-mode-hook 'js2-minor-mode)

;; ------------------------------------------------------------------------
;; @ tern
;; https://github.com/marijnh/tern
;; A JavaScript code analyzer for deep, cross-editor language support
(require 'tern)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; ------------------------------------------------------------------------
;; @ web-mode.el
;; major mode for editing html templates
;; http://web-mode.org/
(require 'web-mode)
;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
;;; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0))
(add-hook 'web-mode-hook 'web-mode-hook)

;; ------------------------------------------------------------------------
;; @ flycheck
;; modern on-the-fly syntax checking extension.
;; https://github.com/flycheck/flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
;; flycheck pos tip
(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; ------------------------------------------------------------------------
;; @ anzu
;; Emacs Port of anzu.vim
;; https://github.com/syohex/emacs-anzu
(anzu-mode +1)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000))

;; ------------------------------------------------------------------------
;; @ powerline
;; emacs powerline
;; https://github.com/milkypostman/powerline
(require 'powerline)
(powerline-default-theme)

;; ------------------------------------------------------------------------
;; @ git-gutter
;; Emacs port of GitGutter which is Sublime Text Plugin
;; https://github.com/syohex/emacs-git-gutter
(global-git-gutter-mode +1)

;; ------------------------------------------------------------------------
;; @ simple-httpd
;; Extensible Emacs HTTP 1.1 server
;; https://github.com/skeeto/emacs-web-server
;; (require 'simple-httpd)
;; (setq httpd-root "/var/www")
;; (httpd-start)

;; ------------------------------------------------------------------------
;; @ skewer-mode
;; Live web development in Emacs
;; https://github.com/skeeto/skewer-mode
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; ------------------------------------------------------------------------
;; @ yasnippet
;; A template system for Emacs
;; https://github.com/capitaomorte/yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)

;; ------------------------------------------------------------------------
;; @ neotree
;; A emacs tree plugin like NerdTree for Vim.
;; https://github.com/jaypei/emacs-neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; ------------------------------------------------------------------------
;; @ SLIME
;; The Superior Lisp Interaction Mode for Emacs.
;; https://github.com/slime/slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner slime-indentation))
(setq slime-net-coding-system 'utf-8-unix)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)


;; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(setq ruby-insert-encoding-magic-comment nil)

;; ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;;ruby-block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; inf-ruby
(require 'inf-ruby)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:"
                       (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims")
                      (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

;; robe
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)


;; Scss-mode
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


;; python-mode
(add-hook 'python-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq indent-level 4)
             (setq python-indent 4)
             (setq tab-width 4)))
;; jedi
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
;;これを入れるとjediを手動<C-tab>で立ち上げないといけない。
;(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; ac-python
(require 'ac-python)
(add-to-list 'ac-modes 'python-2-mode)
