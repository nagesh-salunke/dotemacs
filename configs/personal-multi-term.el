;; multi-term

(setq multi-term-program "/bin/bash -l")

(when (require 'multi-term nil t)
  (global-set-key (kbd "<s-return>") 'multi-term)
  (setq multi-term-buffer-name "term"
        multi-term-program "/bin/bash"))

(provide 'personal-multi-term)
