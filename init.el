;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; CUSTOM FILE
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

(package-initialize)

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

;;(package-refresh-contents)
;;(package-install-selected-packages)

;; Check for {tool,menu,scroll}-bars and get rid of them
;; all this functionality is on the keyboard
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode tabbar-mode))
  (when (fboundp mode) (funcall mode -1)))

(when (fboundp 'show-paren-mode) (show-paren-mode t))

(setq inhibit-startup-screen t)

;; Add time to the info bar
(display-time-mode)

;; Blink the cursor so it's easier for my old eyes to find
(blink-cursor-mode)

;; NO JUNK
;; (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
;;       backup-directory-alist `((".*" . ,temporary-file-directory)))
;; Set directory for backup files
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat (getenv "HOME") "/emacs.d" "/backups")))))

;; ;; EL-GET
;; (add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))
;; (defun el-get-sync-recipes (overlay)
;;   (let* ((recipe-files (directory-files (locate-user-emacs-file (concat overlay "/recipes")) t "rcp"))
;;          (recipes (mapcar 'el-get-read-from-file recipe-files)))
;;     (mapcar (lambda (r) (add-to-list 'el-get-sources r)) recipes)
;;     (el-get 'sync (mapcar 'el-get-source-name recipes))))
;; (setq el-get-user-package-directory user-emacs-directory)

;; EL-GET SYNC OVERLAYS
;; (setq haskell-mode-map (make-sparse-keymap))
;; (el-get-sync-recipes "el-get-haskell")
;; (el-get-sync-recipes "el-get-user")

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq eol-mnemonic-dos "(DOS)")
(setq eol-mnemonic-unix "\\")

;; Syntax colouring, show line and column numbers in status bar
(setq-default fill-column 92)
(setq-default global-font-lock-mode t)
(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default desktop-save-mode t)
(setq-default desktop-save t)
(setq-default default-left-fringe-width 0
              default-right-fringe-width 0)
(setq-default text-scale-mode t)

;; Enable recent file tracking & opening
(recentf-mode t)

;; Get rid of old buffers on schedule
(setq-default midnight-mode t)

(require 'desktop+)

;; (exec-path-from-shell-initialize)

(global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; Generate inline-css so syntax-colored source code
;; is easier to cut-n-paste elsewhere
(setq htmlize-output-type "inline-css")

;; Fix linum mode space between line numbers and text
(setq linum-format "%d  ")

;; I use aspell instead of ispell, installed with homebrew on OS X
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "aspell"))

;; window switching made easy
(windmove-default-keybindings)

;; better window management
(winner-mode t)

;; better navigation
(ido-mode t)
(setq ido-enable-flex-matching t)

(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

;; Make lines wrap automagically in text mode
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; spaces instead of tabs by default
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; visual marking of regions 
(setq-default transient-mark-mode t)
(setq-default colon-double-space t)

;; I like ediff to split horizontally.  Default is vertically
(setq-default ediff-split-window-function 'split-window-horizontally)

;; I like raspberries.  No, really I like to have git recognize me
;; for things like gists
(let ((user (shell-command-to-string "git config --global --list | grep github.user | cut -d\"=\" -f2 | awk '{printf \"%s\", $0}'"))
      (token (shell-command-to-string "git config --global --list | grep github.token | cut -d\"=\" -f2 | awk '{printf \"%s\", $0}'")))
      (setq github-user user)
      (setq github-token token))

;;(server-start) ;; startup emacsclient support

;;(type-break-mode) ;; get me to stop working once in a while

(fset 'yes-or-no-p 'y-or-n-p) ;; answer 'y' instead of 'yes'

;; narrow-to-region is normally disabled.  I enable it
(put 'narrow-to-region 'disabled nil)

;; I have gpg2 installed here, and epa/epg are available
(when (boundp 'epg-gpg-program) (setq epg-gpg-program "/usr/bin/gpg2"))

;; (let ((homedir-by-system-type
;;        (cond
;;          ((or (eq system-type 'cygwin)
;;               (eq system-type 'gnu/linux))
;;           "/home/cb")
;;          ((eq system-type 'darwin)
;;           "/Users/cb")
;;          (t "c:/home/cb"))))
;;   (defvar *home-path* (file-name-as-directory homedir-by-system-type))
;;   (defvar *home-elisp-path* (concat *home-path* "/Projects/emacs-config"))
;;   (load-file (expand-file-name "subdirs.el" *home-elisp-path*)))

;;
;; Global key settings
;; This feels wrong and artificial, because it cuts across all modes and libraries
;; which may or may not be installed.  Nonetheless, it makes finding all those
;; weird bindings easier when debugging [skeptomai]
;;

;; (global-set-key "\C-xwf" 'confluence-get-page)
(global-unset-key "\C-\\")
(global-set-key "\C-xt" 'toggle-frame-fullscreen)
(global-set-key "\C-xg" 'magit-status)
(global-set-key "\C-x," 'tags-loop-continue)
(global-set-key "\C-x\C-m" 'mark-defun)
(global-set-key "\C-cm" 'execute-extended-command)
(global-set-key "\C-x\M-m" 'manual-entry) 
(global-set-key "\C-cs" 'save-buffer)
(global-set-key "\C-cb" 'end-of-buffer)
(global-set-key "\C-ct" 'beginning-of-buffer)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-ck" 'kill-buffer)
(global-set-key "\C-cc" 'save-buffers-kill-emacs)
(global-set-key "\C-cf" 'ido-find-file)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-ce" 'fc-eval-and-replace)
(global-set-key (quote [f12]) 'slime-selector)
(global-set-key "\C-cd" 'toggle-selective-display)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key (kbd "<s-backspace>") 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\M-r" 'isearch-backward-regexp)
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\C-cl" 'cltl)
(global-set-key "\C-c\C-xr" 'run-ruby)
(global-set-key "\M-g" 'grep-find)
(global-set-key "\C-q" 'scroll-n-lines-behind)
(global-set-key "\C-z" 'scroll-n-lines-ahead)
(global-set-key "\C-x\C-q" 'quoted-insert)
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key "\C-x\C-p" 'other-window-backward)
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(setq haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"))
