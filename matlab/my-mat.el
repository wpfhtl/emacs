;;; matlab-mode
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))

;; matlab-mode-hook
(defun my-matlab-mode-hook ()
  (setq matlab-indent-function t)
  (linum-mode t)
  (auto-fill-mode -1))
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)

;; matlab-shell-mode
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(setq matlab-shell-command "/Applications/MATLAB.app/bin/matlab")
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))

(defun my-matlab-modify-date ()
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "%   modify" nil t))
      (if pt1
          (progn
            (message "done")
            (search-forward "gmail.com), ")
            (setq pt1 (point))
            (end-of-line)
            (setq pt2 (point))
            (delete-region pt1 pt2)
            (insert (format-time-string time-format (current-time))))
        (message "modify xxx not found")))))

(defun my-matlab-save-hook ()
  (if (eq major-mode 'matlab-mode)
      (progn
        (message "%s is matlab-mode" (buffer-file-name))
        (my-matlab-modify-date))))

(add-hook 'before-save-hook 'my-matlab-save-hook)

(provide 'my-mat)
;;; my-mat.el ends here
