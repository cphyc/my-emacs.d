
(use-package achievements)

(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode))

(use-package ace-window)

(use-package autopair
:ensure ;TODO:
  :config
  (autopair-global-mode 1))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-flip-when-above t)
  (progn
    (require 'company-auctex)
    (company-auctex-init))
  )

(use-package company-c-headers)

;; (require 'dashboard)
;; (dashboard-setup-startup-hook)
;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-items '((recents  . 5)
;;                         (bookmarks . 5)
;;                         (projects . 5)
;;                         (agenda . 5)
;;                         (registers . 5)))
;;   (add-to-list 'dashboard-items '(agenda) t))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package exec-path-from-shell
  :config
  ;; Add GOPATH to shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-gfortran-language-standard "f2008ts")
  (setq flycheck-gfortran-args "-cpp")
  (global-flycheck-mode)
  )

(use-package flymake)

(use-package flyspell
  :config
  (let ((langs '("american" "english" "francais")))
    (setq lang-ring (make-ring (length langs)))
    (dolist (elem langs) (ring-insert lang-ring elem)))
  :bind
  ("<f8>" . flyspell-buffer)
  ("<f9>" . cycle-ispell-languages))


(use-package ggtags
  :config
  (ggtags-mode 1)
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
		(ggtags-mode 1))))
  (add-hook 'dired-mode-hook 'ggtags-mode)
  (add-hook 'eshell-mode-hook 'ggtags-mode)
  (add-hook 'c-mode-hook 'ggtags-mode)
  (add-hook 'c++-mode-hook 'ggtags-mode)
  (add-hook 'asm-mode-hook 'ggtags-mode)
  (add-hook 'f90-mode-hook 'ggtags-mode)
  (add-hook 'python-mode-hook 'ggtags-mode)

  (dolist (map (list ggtags-mode-map))
    (define-key map (kbd "C-c g s") 'ggtags-find-other-symbol)
    (define-key map (kbd "C-c g h") 'ggtags-view-tag-history)
    (define-key map (kbd "C-c g r") 'ggtags-find-reference)
    (define-key map (kbd "C-c g f") 'ggtags-find-file)
    (define-key map (kbd "C-c g c") 'ggtags-create-tags)
    (define-key map (kbd "C-c g u") 'ggtags-update-tags)
    (define-key map (kbd "M-.")     'ggtags-find-tag-dwim)
    (define-key map (kbd "M-,")     'pop-tag-mark)
    (define-key map (kbd "C-c <")   'ggtags-prev-mark)
    (define-key map (kbd "C-c >")   'ggtags-next-mark)))

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :config
  (setq helm-split-window-in-side-p t
        helm-split-window-default-side 'below
	helm-idle-delay 0.0
	helm-input-idle-delay 0.01
	helm-quick-update t
	helm-ff-skip-boring-files t
	helm-M-x-fuzzy-match t)
  (helm-mode 1)
  (helm-autoresize-mode t)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-m" . helm-M-x)
   ("C-x f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("M-y" . helm-show-kill-ring)
   ("C-?" . helm-info-at-point)
   ("M-s o" . helm-occur)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)))

(use-package helm-ag)
(use-package helm-ack)

(use-package helm-flycheck)
(use-package helm-dictionary)
(use-package helm-company)
;; (use-package helm-git)
(use-package helm-git-grep)
(use-package helm-git-files)
(use-package helm-tramp)

(use-package helm-projectile
  :bind
  ("C-c p ." . helm-projectile-ack)
  ("C-x v" . helm-projectile)
  ("C-x c p" . helm-projectile-ag))

(use-package helm-swoop
  :bind
  ("C-x c s" . helm-swoop))

(use-package helm-gtags
  :config

  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (add-hook 'f90-mode-hook 'helm-gtags-mode)
  :bind
  ("C-c g a" . helm-gtags-tags-in-this-function)
  ("C-c <" . helm-gtags-previous-history)
  ("C-c >" . helm-gtags-next-history))


(use-package hlinum
  :config
  (hlinum-activate))

;; Filename expansion
(use-package hippie-exp-ext)

(use-package indent-guide
  :config
  (indent-guide-global-mode))

(use-package linum
  :config
  (setq linum-format " %3d ")
  (define-global-minor-mode my-global-linum-mode linum-mode
    (lambda ()
      (when (not (memq major-mode
		       (list 'pdf-view-mode 'ibuffer-mode)))
	(linum-mode 1))))
  (my-global-linum-mode 1))

(use-package magit
  :ensure t
  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive)
  :config
  (setq magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256"))))

(use-package magit-popup)

(use-package magit-gitflow)

(use-package magit-gh-pulls)

(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-SPC" . set-rectangular-region-anchor)
         ("H-SPC" . set-rectangular-region-anchor)
         ("C-c C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-previous-like-this)
         ( "C-c c s" . mc/mark-all-like-this)
         ("H-<mouse-1>" . mc/add-cursor-on-click)))

;; (use-package multiple-cursors
;;   :bind
;;   ("C-S-c C-S-c" . mc/edit-lines)
;;   ("C->" . mc/mark-next-like-this)
;;   ("C-<" . mc/mark-previous-like-this)
;;   ("C-c C->" . mc/mark-all-like-this))

(use-package neotree
  :config
  (setq neo-theme 'arrow
        neotree-smart-optn t
        neo-window-fixed-size nil)
  ;; Disable linum for neotree
  (add-hook 'neo-after-create-hook 'disable-neotree-hook))

(use-package org
  :config
  (setq org-directory "~/Notes"
        org-default-notes-file (concat org-directory "/todo.org")
	org-latex-create-formula-image-program 'imagemagick
	org-format-latex-options (plist-put org-format-latex-options :scale 1.4)
	org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package projectile
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))
  (setq projectile-completion-system 'helm)
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-file-exists-remote-cache-expire nil)
  (helm-projectile-on))

(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode t))))

(use-package org-alert)

;; (use-package org-caldav)

(use-package page-break-lines)

(use-package rainbow-mode
  :ensure t
  :diminish ""
  :init
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'fortran-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode))

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/recentf"))
  (recentf-mode 1))

(use-package semantic
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (semantic-mode 1))

(use-package smartparens)

(use-package smex)

(use-package undo-tree
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1)
  :bind
  ("C-x u" . undo-tree-visualize))

(use-package visual-regexp)

(use-package which-key
  :config
  (which-key-mode))

(use-package which-func
  :config
  (which-function-mode 1))

(use-package windmove
  :bind
  ("C-x <up>" . windmove-up)
  ("C-x <down>" . windmove-down)
  ("C-x <left>" . windmove-left)
  ("C-x <right>" . windmove-right))

(use-package wgrep)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-triggers-in-field t)
  (defun yas-latex-closing-delimiter-for (text)
  (cond ((string= text "{") "\rbrace")
	((string= text "<") "\rangle")
	((string= text "|") "|")
        ((string= text "[") "]")
        ((string= text "(") ")")))
  (defun do-yas-expand ()
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand))))

(use-package auto-yasnippet
  :bind
  ("C-x <C-tab>" . aya-expand))

;; (use-package whitespace-cleanup-mode
;;   :config
;;   (whitespace-cleanup-mode))

;;; Remote PATH
(use-package tramp
  :config
  (add-to-list 'tramp-remote-path "/home/cadiou/bin")
  (add-to-list 'tramp-remote-path "/home/cadiou/data2/anaconda3/bin"))

(use-package helm-spotify-plus
  :config
  :bind
  ("C-c C-s s" . helm-spotify-plus)
  ("C-c C-s n" . helm-spotify-plus-next)
  ("C-c C-s p" . helm-spotify-plus-previous)
  ("C-c C-s SPC" . helm-spotify-plus-toggle-play-pause))


;; Base set of pretty symbols.
(defvar base-prettify-symbols-alist '(("<=" . ?≤)
                                      (">=" . ?≥)
                                      ("<-" . ?←)
                                      ("->" . ?→)
				      ("!=" . ?≠)
                                      ("lambda" . ?Λ)))


(defun my-lisp-prettify-symbols-hook ()
  "Set pretty symbols for Lisp modes."
  (setq prettify-symbols-alist base-prettify-symbols-alist))

(defun my-js-prettify-symbols-hook ()
  "Set pretty symbols for JavaScript."
  (setq prettify-symbols-alist
        (append '(("function" . ?ƒ)) base-prettify-symbols-alist)))

(defun my-python-prettify-symbols-hook ()
  "Set pretty symbols for Python."
  (setq prettify-symbols-alist
        (append '(("lambda" . ?ƒ)) base-prettify-symbols-alist)))

(defun my-f90-prettify-symbols-hook ()
  "Set pretty symbols for Fortran 90."
  (setq prettify-symbols-alist
        (append '(("/=" . ?≠))
		  base-prettify-symbols-alist)))

(defun my-prettify-symbols-hook ()
  "Set pretty symbols for non-lisp programming modes."
  (setq prettify-symbols-alist
        (append '(("!=" . ?≠)) base-prettify-symbols-alist)))



;; Hook 'em up.
(add-hook 'emacs-lisp-mode-hook 'my-lisp-prettify-symbols-hook)
(add-hook 'web-mode-hook 'my-prettify-symbols-hook)
(add-hook 'js-mode-hook 'my-js-prettify-symbols-hook)
(add-hook 'python-mode-hook 'my-python-prettify-symbols-hook)
(add-hook 'f90-mode-hook 'my-f90-prettify-symbols-hook)
(add-hook 'prog-mode-hook 'my-prettify-symbols-hook)

(global-prettify-symbols-mode 1)

(provide 'base-extensions)
;;; base-extensions.el ends here
