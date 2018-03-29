;;; web --- web mode
;;; Commentary:

;;; Code:
(use-package web-mode
  :ensure t
  :bind (("C-c ]" . emmet-next-edit-point)
         ("C-c [" . emmet-prev-edit-point)
         ("C-c o b" . browse-url-of-file))
  :mode
  (("\\.js\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.phtml?\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.jsx$" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)

  (add-hook 'web-mode-hook 'jsx-flycheck)

  ;; highlight enclosing tags of the element under cursor
  (setq web-mode-enable-current-element-highlight t)

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  ;; editing enhancements for web-mode
  ;; https://github.com/jtkDvlp/web-mode-edit-element
  (use-package web-mode-edit-element
    :config (add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode))

  ;; snippets for HTML
  ;; https://github.com/smihica/emmet-mode
  (use-package emmet-mode
    :init (setq emmet-move-cursor-between-quotes t) ;; default nil
    :diminish (emmet-mode . " e"))
  (add-hook 'web-mode-hook 'emmet-mode)

  (defun my-web-mode-hook ()
    "Hook for `web-mode' config for company-backends."
    (set (make-local-variable 'company-backends)
         '((company-tern company-css company-web-html company-files))))
  (add-hook 'web-mode-hook 'my-web-mode-hook)

  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    "Set `tern-mode' based on current language before running company-tern."
    (message "advice")
    (if (equal major-mode 'web-mode)
	(let ((web-mode-cur-language
	       (web-mode-language-at-pos)))
	  (if (or (string= web-mode-cur-language "javascript")
		  (string= web-mode-cur-language "jsx"))
	      (unless tern-mode (tern-mode))
	    (if tern-mode (tern-mode -1))))))
  (add-hook 'web-mode-hook 'company-mode)

  ;; to get completion data for angularJS
  (use-package ac-html-angular :defer t)
  ;; to get completion for twitter bootstrap
  (use-package ac-html-bootstrap :defer t)

  ;; to get completion for HTML stuff
  ;; https://github.com/osv/company-web
  (use-package company-web
    :ensure t)

  (add-hook 'web-mode-hook 'company-mode))

(use-package emmet-mode
  :ensure t
  :commands (emmet-expand-line emmet-expand)
  :defer 2
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode))
  :config
  (progn
    (bind-key "C-j" 'emmet-expand-line emmet-mode-keymap)
    (bind-key "<C-return>" 'emmet-expand emmet-mode-keymap)
    (setq emmet-indentation 2)
    (defadvice emmet-preview-accept (after expand-and-fontify activate)
      "Update the font-face after an emmet expantion."
      (font-lock-fontify-buffer))))


;; impatient mode - Live refresh of web pages
;; https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  :ensure t
  :diminish (impatient-mode . " i")
  :commands (impatient-mode))

(provide 'lang-web)
;;; lang-web ends here
