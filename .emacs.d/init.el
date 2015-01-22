;; Cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; ------------------------------------------------------------------------
;; @ load-path
;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp" "conf" "public_repo")

;; ------------------------------------------------------------------------
;; @ general
;; common lisp
(require 'cl)
;;c-x c-fのデフォルト
(setq default-directory "~/")

<<<<<<< HEAD
;; 文字コード
(set-language-environment "Japanese")
=======
;; ���絖���潟�若��
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

;; Windowsで英数と日本語にMeiryoを指定
;; Macで英数と日本語にRictyを指定
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-face-attribute 'default nil
                             :family "Meiryo" ;; 英数
                             :height 100)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo"))) ;; 日本語
        ((eq ws 'ns)
         (set-face-attribute 'default nil
                             :family "Ricty" ;; 英数
                             :height 140)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))))) ;; 日本語

;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバーを非表示
(menu-bar-mode -1)

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 行番号表示
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#800"
                    :height 0.9)


;; 行番号フォーマット
;; (setq linum-format "%4d")

;; 括弧の範囲内を強調表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; 括弧の範囲色
(set-face-background 'show-paren-match-face "#500")

;; 選択領域の色
(set-face-background 'region "#555")

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables '(tab-width 4))

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; バックアップを残さない
;; (setq make-backup-files nil)

;; 行間
(setq-default line-spacing 0)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.7))

;; C-mにnewline-and-indentを割り当てる。初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; "C-t" でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; カラム番号を表示
(column-number-mode t)

;; 時計を表示
(setq display-time-day-and-date t)
;; (setq display-time-24hr-format t)
(display-time-mode t)

;; バッテリー残量を表示(marvericksだとpatchを当てないと表示されない)
;; (display-battery-mode t)

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

;; ruby-mode-hook用の関数を定義
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  (ruby-electric-mode t)
  (ruby-block-mode t))

;; ruby-mode-hookに追加
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)

;; Emacs24以降標準のColor themeを使う
(load-theme 'deeper-blue t)

;; 80���絖���с����������壕�����off
(setq text-mode-hook 'turn-off-auto-fill)

;; flycheck���荐�絎�
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
;;; emacs 23以下の互換
(when (< emacs-major-version 24)
  (defalias 'prog-mode 'fundamental-mode))
;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
;;; インデント数
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
;; ��≦宍絖���� .tex ������ yatex-mode ���
(setq auto-mode-alist
  (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; YaTeX �����������������������潟����潟�����絎�臂�������
;; (setq tex-command "platex2pdf") ;; ���篏���������潟����潟�����
;; (cond
;;   ((eq system-type 'gnu/linux) ;; GNU/Linux ������
;;     (setq dvi2-command "evince")) ;; evince ��� PDF �����画Η
;;   ((eq system-type 'darwin) ;; Mac ������
;;     (setq dvi2-command "open -a Preview"))) ;; �����������ャ�若��
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
;; howm��＜�≫��絖������贋��
;; (setq hown-directory (concat user-emacs-directory "howm"))
;; ;; howm-menu���荐�茯������ユ��茯����
;; (setq howm-menu-lang 'ja)
;; ;; howm-mode���茯���粋昭���
;; (when (require 'howm-mode nil t)
;;   (define-key global-map (kbd "C-c ,,") 'howm-menu))
;; ;; howm��＜�≪��篆�絖����������������������
;; (defun howm-save-buffer-and-kill ()
;;   (interactive)
;;   (when (and (buffer-file-name)
;;              (string-match "\\.howm" (buffer-file-name)))
;;     (save-buffer)
;;     (kill-buffer nil)))

;; ;; C-c C-c��с�＜�≪��篆�絖������������������������＜�����������
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

;; moccur-edit-finish-edit�����������������＜�ゃ�����篆�絖�������
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
;; C-x bにanything-for-filesを割り当てる
(define-key global-map (kbd "C-x b") 'anything-for-files)
;; M-yにanything-show-kill-ringを割り当てる
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

;; ------------------------------------------------------------------------
;; @ auto-complete.el
;; 自動補完機能
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
;; rubyのendに対応する行をハイライト
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

