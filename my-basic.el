;;; package --- Summary
;;; Commentary:
;;; Code:

;; additional packages are available in the folder "3rd"
(add-to-list 'load-path "~/.emacs.d/personal/3rd")

;; basic setting
(setq visible-bell -1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(delete-selection-mode 1)
(setq frame-title-format '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq-default truncate-lines -1)
(server-start)
(setq kill-buffer-query-functions nil)
(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)
(setq whitespace-line-column 80000)
(setq doc-view-resolution 800)
(set-default-font "Monaco 12")
(fringe-mode 0)
(set-fringe-style '(0 . 0))
(ido-mode 1)

;; enable prelude sub-modules
(require 'prelude-key-chord)
(require 'prelude-latex)

;; tramp
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-default-method "ssh")
(setq tramp-chunksize 500)

;; disable git backend to speed up sshfs file load among other things
(setq vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Mtn Arch)))

;; save desktop only on mac
(cond
 ((string-equal system-type "darwin")
  (desktop-save-mode 1))
 ((string-equal system-type "gnu/linux")
  (desktop-save-mode 0)))

;; ace search
(prelude-require-package 'ace-isearch)
(global-ace-isearch-mode 1)

;; multiple-cursor
(prelude-require-package 'multiple-cursors)

;; package
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)

;; helm-swoop
(prelude-require-package 'helm-swoop)

;; environment variables
(setenv "PATH"
        (concat "/usr/local/bin:/usr/texbin"
                ":" (getenv "PATH")
                ":" (getenv "HOME") "/anaconda/bin"))
(setenv "PYTHONPATH"
        (concat (getenv "PYTHONPATH")
                ":" (getenv "HOME") "/work/py"
                ":" "/usr/local/ia/lib/python2.7/site-packages"))
(setenv "DYLD_FALLBACK_LIBRARY_PATH"
        (concat "/usr/local/cuda/lib:/usr/local/lib:/usr/lib"
                ":" (getenv "HOME") "/anaconda/lib"))
(setenv "PYTHONDONTWRITEBYTECODE" "1")
(setenv "s3" "/ssh:feng@skyserver3k:")
(setenv "cvt" "/ssh:root@www.cvtell.com:")
(setenv "ec2" "/ssh:ubuntu@54.69.53.225:")
(setenv "work" (concat (getenv "HOME") "/work/"))
(setenv "caf" (concat (getenv "HOME") "/work/py/test/caf/"))
(setenv "caffe" (concat (getenv "HOME") "/work/tool/caffe/"))
(setenv "caffeP" (concat (getenv "HOME") "/work/tool/caffeP/"))
(setenv "gpu" (concat (getenv "HOME") "/work/tool/caffe_gpu/"))
(setenv "p2p" (concat (getenv "HOME") "/work/tool/caffe_p2p/"))
(setenv "cvtell" (concat (getenv "HOME") "/work/web/cvtell/"))
(setenv "bgl" (concat (getenv "HOME") "/work/pub/2015_iccv_bgl/tex/"))
(setenv "cv" (concat (getenv "HOME") "/work/pub/cv/"))
(setenv "misc" (concat (getenv "HOME") "/work/pub/misc/"))
(setenv "car" (concat (getenv "HOME") "/work/save/car/caffe/"))
(setenv "food" (concat (getenv "HOME") "/work/save/food/caffe/"))

;; ispell
(setq ispell-program-name
      (cond
       ((string-equal system-type "darwin")
        "/usr/local/bin/aspell")
       ((string-equal system-type "gnu/linux")
        "/usr/bin/aspell")))

;; dired
(setq dired-listing-switches "-alh")

;; ace-jump only search filename in dired
(add-hook 'dired-mode-hook
          (lambda ()
            (setq-local ace-jump-search-filter
                        (lambda ()
                          (get-text-property (point) 'dired-filename)))))

;; ibuffer
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Image" (mode . image-mode))
               ("Python" (mode . python-mode))
               ("C++" (or
                       (mode . c-mode)
                       (mode . c++-mode)
                       (mode . cuda-mode)))
               ("Tex" (or
                       (mode . latex-mode)
                       (mode . plain-tex-mode)
                       (mode . bibtex-mode)))
               ("Matlab" (mode . matlab-mode))
               ("Org" (or
                       (mode . org-mode)
                       (mode . markdown-mode)))
               ("Web" (or
                       (mode . html-mode)
                       (mode . nxml-mode)
                       (mode . css-mode)))
               ("Shell" (or
                         (mode . emacs-lisp-mode)
                         (mode . sh-mode)))
               ("Dired" (mode . dired-mode))
               ("Console" (name . "^\\*.*\\*$"))
               ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
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
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)

;; read in PDF
(custom-set-variables
 '(LaTeX-command "latex -synctex=1")
 '(font-latex-math-environments
   (quote
    ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "xxalignat" "flalign" "aligns")))
 '(TeX-view-program-list
   (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r %n %o %b"))))
 '(TeX-view-program-selection
   (quote (((output-dvi style-pstricks) "dvips and gv")
           (output-dvi "xdvi")
           (output-pdf "Skim")
           (output-html "xdg-open")))))
(setq-default TeX-master nil)

;; delete a file but ask for double check
(defun my-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (yes-or-no-p "Are you sure you want to delete this file? ")
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; buffer-move
(prelude-require-package 'buffer-move)

;; switch between horizontal split and vertical split
(defun my-toggle-window-split ()
  "Switch between horizontal split and vertical split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; matlab
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))

;; matlab-mode-hook
(defun my-matlab-mode-hook ()
  "My hook for `matlab-mode'."
  (setq matlab-indent-function t)
  ;; (linum-mode t)
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
(prelude-require-package 'elpy)
(elpy-enable)
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--pylab")

;; update modifying date field in the comment area (for python)
(defun my-python-modify-date ()
  "Update modifying date field in the comment area (for python)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "  modify" nil t))
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

;; update creating date in the comment area (for python)
(defun my-python-create-date ()
  "Update creating date in the comment area (for python)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "  create" nil t))
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

(defun my-python-save-hook ()
  "My hook for saving python file (*.py)."
  (if (eq major-mode 'python-mode)
      (progn
        (message "%s is python-mode" (buffer-file-name))
        (my-python-modify-date))))
(add-hook 'before-save-hook 'my-python-save-hook)

;; update modifying date field in the comment area (for sh)
(defun my-sh-modify-date ()
  "Update modifying date field in the comment area (for sh)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "  modify" nil t))
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

;; update creating date in the comment area (for sh)
(defun my-sh-create-date ()
  "Update creating date in the comment area (for sh)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "#   create" nil t))
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

(defun my-sh-save-hook ()
  "My hook for saving python file (*.py)."
  (if (eq major-mode 'sh-mode)
      (progn
        (message "%s is sh-mode" (buffer-file-name))
        (my-sh-modify-date))))
(add-hook 'before-save-hook 'my-sh-save-hook)

;; my utility functions
(defun my-insert-double-space ()
  "Insert space so that a|bc -> a bc |."
  (interactive)
  (insert " ")
  (forward-char 2)
  (insert " "))

(defun my-insert-single-space ()
  "Insert space so that a|bc -> a b |c."
  (interactive)
  (insert " ")
  (forward-char 1)
  (insert " "))

(defun my-split-window-2-3 ()
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

(defun my-split-window-2-2 ()
  "Split window as 2x2."
  (interactive)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (windmove-left))

;; python shell (remap up key)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)

;; python mode (save C-C C-p for other use)
(eval-after-load "python"
  '(define-key python-mode-map (kbd "C-c C-p") nil))

;; org agenda file
(setq org-agenda-files (list "~/work/my/org/day.org"
                             "~/work/my/org/season.org"))

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

;; start org clock when the state is switched to "doing"
(defun org-clock-in-if-doing ()
  "Clock in when the task is marked STARTED."
  (when (and (string= org-state "DOING")
             (not (string= org-last-state org-state)))
    (org-clock-in)))
(add-hook 'org-after-todo-state-change-hook
          'org-clock-in-if-doing)

;; initial visibility for org file
;; (setq org-startup-folded nil)

;; my key-binding in prelude mode
(defun my-prelude-mode-keys ()
  "My keybindings for prelude-mode."
  (define-key prelude-mode-map (kbd "M-o") nil)
  (define-key prelude-mode-map (kbd "C-c s") nil)
  (define-key prelude-mode-map (kbd "<M-S-up>") nil)
  (define-key prelude-mode-map (kbd "<M-S-down>") nil)
  (define-key prelude-mode-map (kbd "<C-S-up>") nil)
  (define-key prelude-mode-map (kbd "<C-S-down>") nil))
(add-hook 'prelude-mode-hook 'my-prelude-mode-keys)

;; julia-mode
(require 'julia-mode)
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

;; cuda-mode
;; (prelude-require-package 'cude-mode)
;; (require 'cude-mode)
;; (autoload 'cuda-mode "cuda-mode.el")
(prelude-require-package 'ggtags)
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))
(add-hook 'cuda-mode-hook
          (lambda()
            ;; (linum-mode t)
            (ggtags-mode t))) ; maybe need call

;; speedbar
; (prelude-require-package'sr-speedbar)
; (speedbar-add-supported-extension ".cu")
; (speedbar-add-supported-extension ".cuh")
; (add-to-list 'speedbar-fetch-etags-parse-list '("\\.cu" . speedbar-parse-c-or-c++tag))
; (add-to-list 'speedbar-fetch-etags-parse-list '("\\.cuh" . speedbar-parse-c-or-c++tag))

;; c++-mode
(add-hook 'c++-mode-hook
          (lambda()
            ;; (linum-mode t)
            (ggtags-mode t)))

;; c-mode
(add-hook 'c-mode-hook
          (lambda()
            ;; (linum-mode t)
            (ggtags-mode t)))

;; multi-term
(prelude-require-package 'multi-term)
(defun term-send-C-x-C-c ()
  "Send C-x C-c in term mode."
  (interactive)
  (term-send-raw-string "\C-x\C-c"))
(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("C-c C-f" . term-line-mode))
            (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
            (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-C-x-C-c))))

;; default shell form multi-term
(cond
 ((string-equal system-type "darwin")
  (setq multi-term-buffer-name "term" multi-term-program "/bin/zsh"))
 ((string-equal system-type "gnu/linux")
  (setq multi-term-buffer-name "term" multi-term-program (concat (getenv "HOME") "/bin/zsh"))))

(add-hook 'term-mode-hook
          (lambda () (setq truncate-lines 0)))

;; better visualization for markdown file
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode 1)))

;; revert buffer without confirmation
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;; search engine
(prelude-install-search-engine "googles" "http://scholar.google.com/scholar?q=" "Google Scholar: ")
(prelude-install-search-engine "dblp" "http://www.dblp.org/search/index.php#query=" "DBLP: ")

;; timebar (30 mins counter down) only used for mac
;; timebar can be download at https://itunes.apple.com/us/app/timebar/id617829225?mt=12
(defun timebar-run ()
  "Run timebar for 30 mins."
  (interactive)
  (eshell-command
   (format "~/work/dot/zsh/timebar -d 1800")))

;; powerline
(require 'powerline)
(powerline-default-theme)

;; workaround for incorrect render on Emacs 24.4
(add-hook 'desktop-after-read-hook 'powerline-reset)
(defadvice desktop-kill(before clear-power-line-cache () activate)
  (set-frame-parameter nil 'powerline-cache nil))

;; my key starts with (M-m)
(define-prefix-command 'my-key-map)
(global-set-key (kbd "M-m") 'my-key-map)

;; my key for shell
(define-key my-key-map (kbd "l") 'matlab-shell)
(define-key my-key-map (kbd "p") 'elpy-shell-switch-to-shell)
(define-key my-key-map (kbd "m") 'multi-term)
(define-key my-key-map (kbd "n") 'multi-term-next)

;; my key for coding
(define-key my-key-map (kbd "t") 'git-timemachine)
(define-key my-key-map (kbd "g") 'rgrep)
(define-key my-key-map (kbd "e") 'ediff-files)
(define-key my-key-map (kbd "h") 'helm-swoop)
(define-key my-key-map (kbd "s") 'sr-speedbar-toggle)
(define-key my-key-map (kbd "b") 'helm-mini)
(define-key my-key-map (kbd "f") 'find-name-dired)
(define-key my-key-map (kbd "d") '(lambda () (interactive) (find-file "~/work/my/org/day.org")))
(define-key my-key-map (kbd "k") 'magit-status)
(define-key my-key-map (kbd "j") 'magit-file-log)
(define-key my-key-map (kbd "J") 'magit-log)

;; my key for editing
(define-key my-key-map (kbd "q") 'last-kbd-macro)
(define-key my-key-map (kbd "r") 'revert-buffer-no-confirm)
(define-key my-key-map (kbd "c") 'my-matlab-create-date)
(add-hook 'matlab-mode-hook
          (lambda () (local-set-key (kbd "M-m c") 'my-matlab-create-date)))
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "M-m c") 'my-python-create-date)))
(add-hook 'sh-mode-hook
          (lambda () (local-set-key (kbd "M-m c") 'my-sh-create-date)))
(define-key my-key-map (kbd "c") 'my-python-create-date)
(define-key my-key-map (kbd ",") 'my-insert-double-space)
(define-key my-key-map (kbd ".") 'my-insert-single-space)

;; my key for window management
(define-key my-key-map (kbd "o") 'ace-window)
(define-key my-key-map (kbd "O") 'ace-swap-window)
(define-key my-key-map (kbd "u") 'my-split-window-2-3)
(define-key my-key-map (kbd "U") 'my-split-window-2-2)
(define-key my-key-map (kbd "|") 'my-toggle-window-split)
(define-key my-key-map (kbd "a") 'prelude-copy-file-name-to-clipboard)

;; my key for web
(define-key my-key-map (kbd "G") 'prelude-googles)
(define-key my-key-map (kbd "D") 'prelude-dblp)
(define-key my-key-map (kbd "T") 'timebar-run)

;; global key
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
(global-set-key (kbd "C-M-k") 'sp-kill-sexp)

(provide 'my-basic)
;;; my-basic.el ends here
