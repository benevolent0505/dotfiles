;; Cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; ------------------------------------------------------------------------
;; @ load-path
;; load-path���ɲôؿ�
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-path���ɲä���ե����
;; 2�İʾ�ե��������ꤹ����ΰ��� => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp" "conf" "public_repo")

;; ------------------------------------------------------------------------
;; @ general
;; common lisp
(require 'cl)
;;c-x c-f�Υǥե����
(setq default-directory "~/")

;; ʸ��������
(set-language-environment "Japanese")
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

;; Windows�Ǳѿ������ܸ��Meiryo�����
;; Mac�Ǳѿ������ܸ��Ricty�����
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-face-attribute 'default nil
                             :family "Meiryo" ;; �ѿ�
                             :height 100)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo"))) ;; ���ܸ�
        ((eq ws 'ns)
         (set-face-attribute 'default nil
                             :family "Ricty" ;; �ѿ�
                             :height 140)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))))) ;; ���ܸ�

;; �������ȥ��å���ɽ��
(setq inhibit-startup-screen t)

;; scratch�ν����å������õ�
(setq initial-scratch-message "")

;; �ġ���С���ɽ��
(tool-bar-mode -1)

;; ��˥塼�С�����ɽ��
(menu-bar-mode -1)

;; ��������С���ɽ��
(set-scroll-bar-mode nil)

;; �����ȥ�С��˥ե�����Υե�ѥ�ɽ��
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; ���ֹ�ɽ��
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#800"
                    :height 0.9)


;; ���ֹ�ե����ޥå�
;; (setq linum-format "%4d")

;; ��̤��ϰ����Ĵɽ��
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; ��̤��ϰϿ�
(set-face-background 'show-paren-match-face "#500")

;; �����ΰ�ο�
(set-face-background 'region "#555")

;; �����ζ����Ĵɽ��
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; ���֤򥹥ڡ����ǰ���
(setq-default indent-tabs-mode nil)

;; ������
(custom-set-variables '(tab-width 4))

;; yes or no��y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; �Хå����åפ�Ĥ��ʤ�
;; (setq make-backup-files nil)

;; �Դ�
(setq-default line-spacing 0)

;; 1�Ԥ��ĥ�������
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; �ե졼���Ʃ����
(set-frame-parameter (selected-frame) 'alpha '(0.7))

;; C-m��newline-and-indent�������Ƥ롣����ͤ�newline
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; "C-t" �ǥ�����ɥ����ڤ��ؤ��롣����ͤ�transpose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; ������ֹ��ɽ��
(column-number-mode t)

;; ���פ�ɽ��
(setq display-time-day-and-date t)
;; (setq display-time-24hr-format t)
(display-time-mode t)

;; �Хåƥ꡼���̤�ɽ��(marvericks����patch�����Ƥʤ���ɽ������ʤ�)
;; (display-battery-mode t)

;; �Хå����åפȥ����ȥ����֥ե������~/.emacs.d/backups/�ؽ����
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; emacs-lisp-mode�Υեå��򥻥å�
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (when (require 'eldoc nil t)
               (setq eldoc-idle-delay 0.2)
               (setq eldoc-echo-area-use-multiline-p t)
               (turn-on-eldoc-mode))))

;; ruby-mode-hook�Ѥδؿ������
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  (ruby-electric-mode t)
  (ruby-block-mode t))

;; ruby-mode-hook���ɲ�
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)

;; Emacs24�ʹ�ɸ���Color theme��Ȥ�
(load-theme 'deeper-blue t)

; ------------------------------------------------------------------------
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
;;; emacs 23�ʲ��θߴ�
(when (< emacs-major-version 24)
  (defalias 'prog-mode 'fundamental-mode))
;;; Ŭ�Ѥ����ĥ��
(add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
;;; ����ǥ�ȿ�
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
;; @ egg.el
;; Emacs DE Git
;; https://github.com/byplayer/egg
(when (executable-find "git")
  (require 'egg nil t))

;; ------------------------------------------------------------------------
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
;; C-x b��anything-for-files�������Ƥ�
(define-key global-map (kbd "C-x b") 'anything-for-files)
;; M-y��anything-show-kill-ring�������Ƥ�
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

;; ------------------------------------------------------------------------
;; @ auto-complete.el
;; ��ư�䴰��ǽ
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
;; ruby��end���б�����Ԥ�ϥ��饤��
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

