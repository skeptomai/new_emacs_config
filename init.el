;; Speed things up by getting rid of startup screen
(setq inhibit-startup-screen t)

;; Check for {tool,menu,scroll}-bars and get rid of them
;; all this functionality is on the keyboard
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode tabbar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;;package setup
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)
(package-refresh-contents t)
(package-install-selected-packages)

;; Setup path for tools from shell
(if (or (eq system-type 'gnu/linux)
        (eq system-type 'darwin))
    (exec-path-from-shell-initialize))

;; Add time to the info bar
(display-time-mode)

;; Blink the cursor so it's easier for my old eyes to find
(blink-cursor-mode)

;; Toggle vis of matching parens
(show-paren-mode t)

;; Try making company mode global..
(require 'company)
(add-hook 'after-init-hook (lambda () (progn (global-company-mode)
					     (add-to-list 'company-backends 'company-ghc)
					     (add-to-list 'company-backends 'company-go)
					     (add-to-list 'company-backends 'company-jedi)
					     (add-to-list 'company-backends 'company-irony)
					     (custom-set-variables '(company-ghc-show-info t)))))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;(define-key c++-mode-map [(tab)] 'company-complete)

;; no junk
(defconst backup-dir (expand-file-name (concat (getenv "HOME") "/.emacs.d" "/backups")))
(setq auto-save-file-name-transforms `((".*" ,backup-dir t))
       backup-directory-alist `((".*" . ,backup-dir)))

;; file coding / line ending defaults
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq eol-mnemonic-dos "(DOS)")
(setq eol-mnemonic-unix "\\")

;; syntax colouring, show line and column numbers in status bar
(setq-default fill-column 92)
(setq-default global-font-lock-mode t)
(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default default-left-fringe-width 0
              default-right-fringe-width 0)
(setq-default text-scale-mode t)

;; answer 'y' instead of 'yes'
(fset 'yes-or-no-p 'y-or-n-p)

;; narrow-to-region is normally disabled.  I enable it
(put 'narrow-to-region 'disabled nil)

;; Enable recent file tracking & opening
(recentf-mode t)

;; Get rid of old buffers on schedule
(setq-default midnight-mode t)

;; Setup desktop
(require 'desktop+)
(setq-default desktop-save-mode t)
(setq-default desktop-save t)

;; Setup flycheck, but not annoying one for elisp
(global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; Generate inline-css so syntax-colored source code
;; is easier to cut-n-paste elsewhere
(setq htmlize-output-type "inline-css")

;; Fix linum mode space between line numbers and text
(require 'linum)
(setq linum-format "%d  ")

;; I use aspell instead of ispell, installed with homebrew on OS X
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "aspell"))

;; window switching made easy
(windmove-default-keybindings)

;; better window management
(winner-mode t)

;; better navigation
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; org mode initialization
(require 'org)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-startup-folded nil)

;; Speeds up by loading folding mode only when needed
(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

;; Octave mode
(autoload 'octave-mode "octave-mode" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (font-lock-mode 1)))

;; Make lines wrap automagically in text mode
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Python
(require 'elpy)
(elpy-enable)
(setq elpy-rpc-backend "jedi")
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; spaces instead of tabs by default
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; cpp / c++ stuff
;; https://www.emacswiki.org/emacs/CPlusPlusMode
(c-add-style "my-style" 
	     '("stroustrup"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 4)            ; indent by four spaces
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
				   (brace-list-open . 0)
				   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)         
  (c-toggle-auto-hungry-state 1))

;; autoinsert C/C++ header
(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
  '(nil
    "// " (file-name-nondirectory buffer-file-name) "\n"
    "//\n"
    "// last-edit-by: <> \n"
    "//\n"
    "// Description:\n"
    "//\n"
    (make-string 70 ?/) "\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat (upcase nopath) "_H")))
      (concat "#ifndef " ident "\n"
              "#define " ident  " 1\n\n\n"
              "\n\n#endif // " ident "\n"))
    (make-string 70 ?/) "\n"
    "// $Log:$\n"
    "//\n"
    ))

;; auto insert C/C++
(define-auto-insert
  (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C++ implementation")
  '(nil
    "// " (file-name-nondirectory buffer-file-name) "\n"
    "//\n"
    "// last-edit-by: <> \n"
    "// \n"
    "// Description:\n"
    "//\n"
    (make-string 70 ?/) "\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat nopath ".h")))
      (if (file-exists-p ident)
          (concat "#include \"" ident "\"\n")))
    (make-string 70 ?/) "\n"
    "// $Log:$\n"
    "//\n"
    ))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

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

;; ROS launch files are xml
(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))

;; Haskell stuff, with stack..
(setq-default ghc-command "stack exec ghc-mod")
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
;; for things like ghc-mod
(custom-set-variables '(haskell-process-type 'stack-ghci))
(setq haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"))
(setq haskell-hoogle-command "hoogle")

(add-hook 'haskell-mode-hook (lambda () (progn (ghc-init)
                                               (define-key haskell-mode-map "\C-ch" 'haskell-hoogle))))
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Go stuff
(setenv "GOPATH" (expand-file-name (concat (getenv "HOME") "/Projects/gopath")))
(setenv "GOROOT" "/usr/local/opt/go/libexec")
(add-hook 'before-save-hook 'gofmt-before-save)

;; On linux, I have gpg2 installed here, and epa/epg are available
(if (eq system-type 'gnu/linux)
    (when (boundp 'epg-gpg-program) (setq epg-gpg-program "/usr/bin/gpg2")))

;; Startup emacsclient support
(server-start)

;; LOTS O' KEY BINDINGS
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
(global-set-key "\C-x\C-q" 'quoted-insert)
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
