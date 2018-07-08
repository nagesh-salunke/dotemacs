;; multi-term

(setq multi-term-program "/usr/local/bin/fish -l")

(when (require 'multi-term nil t)
  (global-set-key (kbd "<s-return>") 'multi-term)
  (setq multi-term-buffer-name "term"
        multi-term-program "/usr/local/bin/fish"))

(provide 'personal-multi-term)
