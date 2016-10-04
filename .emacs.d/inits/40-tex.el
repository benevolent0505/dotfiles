;; For LaTeX
;; require mercurial
(el-get-bundle yatex)
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))

(with-eval-after-load 'yatex
  (setq YaTeX-inhibit-prefix-letter t)
  (setq YaTeX-kanji-code nil)
  (setq YaTeX-use-LaTeX2e t)
  (setq YaTeX-use-AMS-LaTeX t)
  (setq YaTeX-dvi2-command-ext-alist
        '(("Preview\\|TeXShop\\|TeXworks\\|Skim\\|mupdf\\|xpdf\\|Firefox\\|Adobe" . ".pdf")))
  (setq tex-command "/usr/texbin/ptex2pdf -l -ot '-synctex=1'")
  (setq bibtex-command (cond ((string-match "uplatex\\|-u" tex-command) "/usr/texbin/upbibtex")
                             ((string-match "platex" tex-command) "/usr/texbin/pbibtex")
                             ((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "/usr/texbin/bibtexu")
                             ((string-match "pdflatex\\|latex" tex-command) "/usr/texbin/bibtex")
                             (t "/usr/texbin/pbibtex")))
  (setq makeindex-command (cond ((string-match "uplatex\\|-u" tex-command) "/usr/texbin/mendex")
                                ((string-match "platex" tex-command) "/usr/texbin/mendex")
                                ((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "/usr/texbin/texindy")
                                ((string-match "pdflatex\\|latex" tex-command) "/usr/texbin/makeindex")
                                (t "/usr/texbin/mendex")))
  ;; (setq dvi2-command "/usr/bin/open -a Preview")
  (setq dvi2-command "/usr/bin/open -a Skim")
  (setq dviprint-command-format "/usr/bin/open -a \"Adobe Reader\" `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")
  (auto-fill-mode -1)
  (reftex-mode 1))

(defun skim-forward-search ()
  (interactive)
  (progn
    (process-kill-without-query
     (start-process
      "displayline"
      nil
      "/Applications/Skim.app/Contents/SharedSupport/displayline"
      (number-to-string (save-restriction
                          (widen)
                          (count-lines (point-min) (point))))
      (expand-file-name
       (concat (file-name-sans-extension (or YaTeX-parent-file
                                             (save-excursion
                                               (YaTeX-visit-main t)
                                               buffer-file-name)))
               ".pdf"))
      buffer-file-name))))

(defun my-yatex-mode-hook ()
  (define-key YaTeX-mode-map (kbd "C-c s") 'skim-forward-search)
  (define-key YaTeX-mode-map (kbd "<f8>") 'YaTeX-typeset-menu)
  (define-key YaTeX-mode-map (kbd "s-R") 'YaTeX-typeset-menu)
  (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
  (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region))
