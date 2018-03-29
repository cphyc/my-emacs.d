;;; Code:

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(global-set-key [f5] 'compile)
(global-set-key [f6] 'recompile)
(global-set-key [f7] 'next-error)
(global-set-key (kbd "C-/")	'ffap)
(global-set-key [mouse-8] 'ffap)
(global-set-key (kbd "C-!")	'delete-trailing-whitespace)
(global-set-key (kbd "C-c f")	'fold-this)
(global-set-key (kbd "C-c u f") 'fold-this-unfold-all)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-o") 'ace-window)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(define-key global-map (kbd "C-c m m") 'vr/mc-mark)
(define-key global-map (kbd "C-x C-SPC") 'mc/mark-pop)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c s") 'helm-tramp)
(global-set-key (kbd "C-@") 'hippie-expand-dabbrev-limited-chars)
(global-set-key (kbd "M-/") 'hippie-expand-file-name)
(setq smerge-command-prefix "\C-cv")

(provide 'base-global-keys)
;;; base-global-keys ends here
