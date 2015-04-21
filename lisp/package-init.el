;;;; package-init.el
;;;;
;;;; Post-package initialization


;;;; Configure Cider and nREPL

(setq nrepl-hide-special-buffers t)
(setq nrepl-port 53095)

(setq cider-repl-print-length 10) 

;; disable to hopefully fix hang
;;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)


