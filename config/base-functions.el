;;; Code:


(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
	    (null (do-yas-expand)))
	(if (check-expansion)
	    (company-complete-common)
	  (indent-for-tab-command)))))

;; From http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
	(backward-char 1)
	(if (looking-at "->") t nil)))))


;; Search for tags up the dir tree
;; (defun find-file-upwards (file-to-find)
;;   "Recursively search each parent directory starting from the default directory.
;; looking for a file with name FILE-TO-FIND.  Returns the path to
;; it or nil if not found."
;;   (labels
;;       ((find-file-r (path)
;;                     (let* ((parent (file-name-directory path))
;;                            (possible-file (concat parent file-to-find)))
;;                       (cond
;;                        ((file-exists-p possible-file) possible-file) ; Found
;;                        ;; The parent of ~ is nil and the parent of / is itself.
;;                        ;; Thus the terminating condition for not finding the file
;;                        ;; accounts for both.
;;                        ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
;;                        (t (find-file-r (directory-file-name parent))))))) ; Continue
;;     (find-file-r default-directory)))
;; (let ((my-tags-file (find-file-upwards "TAGS")))
;;   (when my-tags-file
;;     (message "Loading tags file: %s" my-tags-file)
;;     (visit-tags-table my-tags-file)))

(infer-indentation-style)

(provide 'base-functions)
;;; base-functions ends here
