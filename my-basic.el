;;; package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/personal/matlab")

;; bug in prelude-package (to del in the future)
(require 'prelude-key-chord)
(ido-mode 1)

;; basic setting
(scroll-bar-mode -1)
(delete-selection-mode 1)
(setq frame-title-format '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(desktop-save-mode 1)
(setq-default truncate-lines -1)
(server-start)
(setq kill-buffer-query-functions nil)
(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)
(setq whitespace-line-column 80000)

;; PDF->JPG resolution
(setq doc-view-resolution 800)

;; multiple-cursor
(prelude-require-package 'multiple-cursors)

;; package
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)

;; tramp
(setq tramp-default-method "ssh")
(setq tramp-chunksize 500)
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; helm-swoop
(prelude-require-package 'helm-swoop)

;; environment variables
(setenv "PATH"
        (concat "/usr/local/bin:/usr/texbin"
                ":" (getenv "PATH")
                ":" (getenv "HOME") "/anaconda/bin"))
(setenv "PYTHONPATH"
        (concat "/usr/local/lib/python2.7/site-packages"
                ":" (getenv "PYTHONPATH")
                ":" (getenv "HOME") "/Code/tool"
                ":" (getenv "HOME") "/Code/lib"))
(setenv "DYLD_FALLBACK_LIBRARY_PATH"
        (concat "/usr/local/cuda/lib:/usr/local/lib:/usr/lib"
                ":" (getenv "HOME") "/anaconda/lib"))
(setenv "PYTHONDONTWRITEBYTECODE" "1")

;; ispell
(setq ispell-program-name "/usr/local/bin/aspell")

;; dired
(setq dired-listing-switches "-alh")

;; ibuffer
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired" (mode . dired-mode))
               ("Matlab" (mode . matlab-mode))
               ("Org" (or
                       (mode . org-mode)
                       (mode . markdown-mode)))
               ("C++" (or
                       (mode . c-mode)
                       (mode . c++-mode)
                       (mode . cuda-mode)))
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

;; disable key binding in flyspell
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-,") nil))
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") nil))

;; latex
(prelude-require-package 'auctex)
(add-hook 'LaTeX-mode-hook
          (lambda()
            (TeX-PDF-mode t)
            (setq TeX-save-query nil)
            (toggle-truncate-lines)))
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)
;; (load "preview.el" nil t t)

;; read in PDF
(custom-set-variables
 '(LaTeX-command "latex -synctex=1")
 '(TeX-view-program-list
   (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r %n %o %b"))))
 '(TeX-view-program-selection
   (quote (((output-dvi style-pstricks) "dvips and gv")
           (output-dvi "xdvi")
           (output-pdf "Skim")
           (output-html "xdg-open")))))

;; delete a file but ask for double check
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
(prelude-require-package 'buffer-move)

;; matlab
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))

;; matlab-mode-hook
(defun my-matlab-mode-hook ()
  "My hook for `matlab-mode'."
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

;; update modifying date field in the comment area (for matlab)
(defun my-matlab-modify-date ()
  "Update modifying date field in the comment area (for matlab)."
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

;; update creating date in the comment area (for matlab)
(defun my-matlab-create-date ()
  "Update creating date in the comment area (for matlab)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "%   create" nil t))
      (if pt1
          (progn
            (message "done")
            (search-forward "gmail.com), ")
            (setq pt1 (point))
            (end-of-line)
            (setq pt2 (point))
            (delete-region pt1 pt2)
            (insert (format-time-string time-format (current-time))))
        (message "create xxx not found")))))

(defun my-matlab-save-hook ()
  "My hook for saving matlab file."
  (if (eq major-mode 'matlab-mode)
      (progn
        (message "%s is matlab-mode" (buffer-file-name))
        (my-matlab-modify-date))))
(add-hook 'before-save-hook 'my-matlab-save-hook)

;; using ipython as the default python console
(elpy-enable)
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--pylab")

;; python-mode-hook
(defun my-python-mode-hook ()
  "My hook for `python-mode'."
  (linum-mode t)
  (flyspell-mode nil))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(defun my-insert-double-space ()
  (interactive)
  (insert " ")
  (forward-char 2)
  (insert " "))

(defun my-insert-single-space ()
  (interactive)
  (insert " ")
  (forward-char 1)
  (insert " "))

(defun my-split-window ()
  "Split window as 2x3."
  (interactive)
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (balance-windows)
  (windmove-left)
  (windmove-left))

;; python shell (remap up key)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)

;; python mode (save C-C C-p for other use)
(eval-after-load "python"
  '(define-key python-mode-map (kbd "C-c C-p") nil))

;; org todo key-words
(setq org-todo-keywords '((sequence "TODO" "DOING" "CANCELED" "|" "DONE" "FINISH")))

;; key for switching between key-words
(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)
     (define-key org-mode-map "\M-o" 'org-todo-state-map)
     (define-key org-todo-state-map "t"
       #'(lambda nil (interactive) (org-todo "TODO")))
     (define-key org-todo-state-map "c"
       #'(lambda nil (interactive) (org-todo "CANCELED")))
     (define-key org-todo-state-map "i"
       #'(lambda nil (interactive) (org-todo "DOING")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "FINISH")))))

;; org key
(defun my-org-mode-keys ()
  "My keybindings for `org-mode'."
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
  "My keybindings for prelude-mode."
  (define-key prelude-mode-map (kbd "M-o") nil)
  (define-key prelude-mode-map (kbd "<M-S-up>") nil)
  (define-key prelude-mode-map (kbd "<M-S-down>") nil)
  (define-key prelude-mode-map (kbd "<C-S-up>") nil)
  (define-key prelude-mode-map (kbd "<C-S-down>") nil))
(add-hook 'prelude-mode-hook 'my-prelude-mode-keys)

;; cuda-mode
;; (prelude-require-package 'cude-mode)
(autoload 'cuda-mode "cuda-mode.el")
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))

(require 'sr-speedbar)
(speedbar-add-supported-extension ".cu")
(speedbar-add-supported-extension ".cuh")
(add-to-list 'speedbar-fetch-etags-parse-list '("\\.cu" . speedbar-parse-c-or-c++tag))
(add-to-list 'speedbar-fetch-etags-parse-list '("\\.cuh" . speedbar-parse-c-or-c++tag))

;; cuda-mode-hook
(defun my-cuda-mode-hook ()
  (linum-mode t))
(add-hook 'cuda-mode-hook 'my-cuda-mode-hook)

;; multi-term
(prelude-require-package 'multi-term)
(defun my-term-mode-keys ()
  "my keybindings for term-mode"
  (define-key term-mode-map (kbd "C-x C-c") nil)
  (define-key term-mode-map (kbd "C-c C-f") 'term-line-mode)
  (define-key term-mode-map (kbd "C-c C-k") 'term-char-mode)
  (define-key term-mode-map (kbd "C-a") nil))
(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("C-c C-f" . term-line-mode))
            ))
(add-hook 'term-mode-hook 'my-term-mode-keys)
(setq multi-term-buffer-name "term"
      multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook
          (lambda () (setq truncate-lines 0)))
(defun term-window-width () 200)

;; search engine
(prelude-install-search-engine "googles" "http://scholar.google.com/scholar?q=" "Google Scholar: ")
(prelude-install-search-engine "dblp" "http://www.dblp.org/search/index.php#query=" "DBLP: ")

;; sync Org files with evernote through geeknote API every 2 hours
;; (defun geeknote-sync ()
;;   (interactive)
;;   (eshell-command
;;    (format "python ~/Code/script/geeknote/gnsync.py --mask \\*.md --path ~/Code/org --format markdown --logpath ~/Code/script/geeknote/geeknote.log --notebook Emacs")))
;; (run-with-timer 0 (* 120 60) 'geeknote-sync)

;; timebar (30 mins counter down)
(defun timebar-run ()
  (interactive)
  (eshell-command
   (format "~/Code/script/core/timebar -d 1800")))

;; my macro
(fset 'last-kbd-macro
      [?\C-s ?\[ ?2 ?0 ?\C-a ?* ?* ?  ?\C-e ?\C-f ?\C-  ?\C-s ?\[ ?2 ?\C-a ?\C-p ?\C-p ?\S-\C-c ?\S-\C-c ?\M-d ?- ?  ?\[ ?X ?\] return ?\C-e])

;; powerline
;; (prelude-require-package 'powerline)
;; (powerline-default-theme)

;; my key (M-m)
(define-prefix-command 'my-key-map)
(global-set-key (kbd "M-m") 'my-key-map)
(define-key my-key-map (kbd "l") 'matlab-shell)
(define-key my-key-map (kbd "p") 'python-shell-switch-to-shell)
(define-key my-key-map (kbd "m") 'multi-term)
(define-key my-key-map (kbd "n") 'multi-term-next)
(define-key my-key-map (kbd "T") 'mode-line-timer-start)
(define-key my-key-map (kbd "t") 'timebar-run)
(define-key my-key-map (kbd "g") 'rgrep)
(define-key my-key-map (kbd "s") 'prelude-googles)
(define-key my-key-map (kbd "d") 'prelude-dblp)
(define-key my-key-map (kbd "h") 'helm-swoop)
(define-key my-key-map (kbd "i") 'sr-speedbar-toggle)
(define-key my-key-map (kbd "b") 'helm-mini)
(define-key my-key-map (kbd "q") 'last-kbd-macro)
(define-key my-key-map (kbd "c") 'my-matlab-create-date)
(define-key my-key-map (kbd "f") 'find-name-dired)
(define-key my-key-map (kbd ",") 'my-insert-double-space)
(define-key my-key-map (kbd ".") 'my-insert-single-space)
(define-key my-key-map (kbd "e") 'ediff)
(define-key my-key-map (kbd "o") 'ace-window)
(define-key my-key-map (kbd "u") 'my-split-window)

; global key
(global-set-key (kbd "<f5>") 'kmacro-set-counter)
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)
(global-set-key (kbd "<H-M-up>") 'buf-move-up)
(global-set-key (kbd "<H-M-down>") 'buf-move-down)
(global-set-key (kbd "<H-M-left>") 'buf-move-left)
(global-set-key (kbd "<H-M-right>") 'buf-move-right)
(global-set-key (kbd "M-x") 'helm-M-x)

(provide 'my-basic)
;;; my-basic.el ends here
