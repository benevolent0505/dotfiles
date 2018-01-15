(el-get-bundle open-junk-file
  :type emacswiki
  (setq open-junk-file-format "~/local/tmp/%Y/%m/%Y-%m-%d-%H%M%S-")
  (global-set-key (kbd "C-c j") 'open-junk-file))
