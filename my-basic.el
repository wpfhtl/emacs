;;; package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/personal/matlab")
(add-to-list 'load-path "~/.emacs.d/personal/evernote")

(scroll-bar-mode -1)
(delete-selection-mode 1)
(setq frame-title-format '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(require 'multiple-cursors)
(desktop-save-mode 1)
(setq-default truncate-lines -1)
(server-start)
(setq kill-buffer-query-functions nil)

(setq doc-view-resolution 800)

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; option key
(setq mac-option-modifier 'hyper)

;; path
(setq exec-path (append exec-path '("/usr/local/bin" "/usr/texbin")))
(setenv "PATH"
        (concat
         "/usr/local/bin" ":" "/usr/texbin" ":" (getenv "PATH")))
(setq ispell-program-name "/opt/local/bin/aspell")

;; dired
(setq dired-listing-switches "-alh")

;; ibuffer
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired" (mode . dired-mode))
               ("Matlab" (mode . matlab-mode))
               ("C++" (or
                       (mode . c-mode)
                       (mode . c++-mode)))
               ("Tex" (or
                       (mode . latex-mode)
                       (mode . plain-tex-mode)
                       (mode . bibtex-mode)))
               ("Html" (or
                        (mode . html-mode)
                        (mode . nxml-mode)
                        (mode . css-mode)))
               ("Lisp" (mode . emacs-lisp-mode))
               ("Python" (mode . python-mode))
               ("Shell" (mode . sh-mode))
               ("Console" (name . "^\\*.*\\*$"))
               ))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "default")))

;; disable key binding C-.
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-,") nil))
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") nil))

;; latex-mode-hook
(add-hook 'LaTeX-mode-hook
          (lambda()
            (TeX-PDF-mode t)
            (setq TeX-save-query nil)
            (toggle-truncate-lines)))
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)
(load "preview.el" nil t t)

;; read in PDF
(custom-set-variables
 '(LaTeX-command "latex -synctex=1")
 '(TeX-view-program-list
   ;; (quote (("Preview" "open -a Preview.app %o"))))
   (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r %n %o %b"))))
 '(TeX-view-program-selection
   (quote (((output-dvi style-pstricks) "dvips and gv")
           (output-dvi "xdvi")
           (output-pdf "Skim")
            ;; (output-pdf "Preview")
           (output-html "xdg-open")))))

(defun my-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (yes-or-no-p "Are you sure you want to remove this file? ")
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; buffer-move
(require 'buffer-move)

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
(cond
 ((string-equal system-type "darwin")
  (setq matlab-shell-command "/Applications/MATLAB.app/bin/matlab"))
  ((string-equal system-type "gnu/linux")
  (setq matlab-shell-command "/usr/bin/matlab")))

(setq matlab-shell-command-switches '("-nodesktop -nosplash"))

;; Update the date in the comment automatically after changing the file.
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

;; using ipython as the default python console
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--pylab")

;; python-mode-hook
(defun my-python-mode-hook ()
  (linum-mode t))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; python shell (remap up key)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)

;; python mode (save C-C C-p for other use)
(eval-after-load "python"
  '(define-key python-mode-map (kbd "C-c C-p") nil))

;; line timer
(require 'mode-line-timer)

;; org todo key-words
(setq org-todo-keywords '((sequence "TODO" "DOING" "RUNNING" "|" "DONE" "FINISH")))

;; key for switching between key-words
(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)
     (define-key org-mode-map "\M-o" 'org-todo-state-map)
     (define-key org-todo-state-map "t"
       #'(lambda nil (interactive) (org-todo "TODO")))
     (define-key org-todo-state-map "i"
       #'(lambda nil (interactive) (org-todo "DOING")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "FINISH")))
     (define-key org-todo-state-map "r"
       #'(lambda nil (interactive) (org-todo "RUNNING")))))

;; org key
(defun my-org-mode-keys ()
  "my keybindings for org-mode"
  (define-key org-mode-map (kbd "<S-up>") 'windmove-up)
  (define-key org-mode-map (kbd "<S-down>") 'windmove-down)
  (define-key org-mode-map (kbd "<S-left>") 'windmove-left)
  (define-key org-mode-map (kbd "<S-right>") 'windmove-right)
  (define-key org-mode-map (kbd "<H-up>") 'org-shiftup)
  (define-key org-mode-map (kbd "<H-down>") 'org-shiftdown)
  (define-key org-mode-map (kbd "<H-left>") 'org-shiftleft)
  (define-key org-mode-map (kbd "<H-right>") 'org-shiftright))
(add-hook 'org-mode-hook 'my-org-mode-keys)

;; initial visibility for org file
(setq org-startup-folded nil)

;; my key-binding in prelude mode
(defun my-prelude-mode-keys ()
  "my keybindings for prelude-mode"
  (define-key prelude-mode-map (kbd "M-o") nil))
(add-hook 'prelude-mode-hook 'my-prelude-mode-keys)

;; multi-term
(require 'multi-term)

;; my prefix key
(define-prefix-command 'my-key-map)
(global-set-key (kbd "M-m") 'my-key-map)
(define-key my-key-map (kbd "l") 'matlab-shell)
(define-key my-key-map (kbd "p") 'python-shell-switch-to-shell)
(define-key my-key-map (kbd "m") 'multi-term)
(define-key my-key-map (kbd "n") 'multi-term-next)
(define-key my-key-map (kbd "t") 'mode-line-timer-start)
(define-key my-key-map (kbd "s") 'prelude-googles)

; global key
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "<H-M-up>")     'buf-move-up)
(global-set-key (kbd "<H-M-down>")   'buf-move-down)
(global-set-key (kbd "<H-M-left>")   'buf-move-left)
(global-set-key (kbd "<H-M-right>")  'buf-move-right)

(provide 'my-basic)
;;; my-basic.el ends here
