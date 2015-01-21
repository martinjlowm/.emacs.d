(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-fringe-mode 0)
(setq ring-bell-function 'ignore)

;; Mail
(setq mm-text-html-renderer 'w3m)
(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
				   "martin@martinjlowm.dk" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
(setq smtpmail-auth-credentials "~/.mailauth.gpg")
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "\n\nOn %a, %b %d %Y, %f wrote:")
;; (gnus-demon-add-handler 'gnus-demon-scan-news 2 t)
(setq gnus-signature-file "~/.signature")
(setq user-mail-address "martin@martinjlowm.dk")


(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<escape>") 'keyboard-quit)
(setq-default indent-tabs-mode nil)
(show-paren-mode t)

(set-face-attribute 'default nil :height 140)

(add-to-list 'load-path "~/.emacs.d/vendor/names")
(add-to-list 'load-path "~/.emacs.d/vendor/aggressive-indent-mode")
(require 'aggressive-indent)

(add-to-list 'load-path "~/.emacs.d/vendor/google-c-style")
(require 'google-c-style)

(defun c++-hook ()
  (google-set-c-style))
(add-hook 'c++-mode-hook 'c++-hook)

(add-hook 'org-mode-hook (lambda ()
                           (local-unset-key (kbd "S-<up>"))
                           (local-unset-key (kbd "S-<down>"))
                           (local-unset-key (kbd "S-<left>"))
                           (local-unset-key (kbd "S-<right>"))))

(setq set-fill-column 72)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(defun run-arara ()
  (interactive)
  (message (shell-command-to-string (concat "arara " (buffer-file-name)))))

(add-hook 'TeX-mode-hook (lambda ()
                           (TeX-PDF-mode t)
                           (setq TeX-view-program-selection (quote
                                                             (((output-dvi style-pstricks)
                                                               "dvips and gv")
                                                              (output-dvi "xdvi")
                                                              (output-pdf "xdg-open")
                                                              (output-html "xdg-open"))))
                           (define-key LaTeX-mode-map (kbd "C-c C-c") 'run-arara)))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)

(defun task-sync ()
  (interactive)
  (shell-command-to-string "michel-orgmode --sync --orgfile ~/.emacs.d/tasks.org")
  (revert-buffer t t))

(defun tasks ()
  (interactive)
  (find-file "~/.emacs.d/tasks.org"))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(setq sprunge-major-modes (make-hash-table :test 'equal))
(puthash 'emacs-lisp-mode "scm" sprunge-major-modes)
(puthash 'latex-mode "latex" sprunge-major-modes)
(puthash 'malabar-mode "java" sprunge-major-modes)
(puthash 'lua-mode "lua" sprunge-major-modes)
(puthash 'haskell-mode "haskell" sprunge-major-modes)
(puthash 'javascript-mode "javascript" sprunge-major-modes)
(puthash 'ruby-mode "ruby" sprunge-major-modes)
(puthash 'sage-mode "python" sprunge-major-modes)
;; Does not work with shell commented characters
(defun sprunge ()
  "Sprungify current selected region"
  (interactive)
  (if mark-active
      (let ((selection (buffer-substring-no-properties (region-beginning) (region-end)))
            (cur-buffer (current-buffer)))
        (unless (= (length selection) 0)
          (progn
            (let ((sprunge-link (concat (substring (shell-command-to-string
                                                    (concat
                                                     "curl -s -F 'sprunge=<-' http://sprunge.us <<EOF\n"
                                                     selection
                                                     "\nEOF")) 0 -1)
                                        "?"
                                        (gethash
                                         (with-current-buffer cur-buffer
                                           major-mode) sprunge-major-modes "None"))))
              (kill-new sprunge-link)
              (message "Sprunge complete: %s" sprunge-link)))))))

(add-to-list 'load-path "~/.emacs.d/vendor/color-theme")
(require 'color-theme)
(add-to-list 'load-path "~/.emacs.d/themes")
(require 'color-theme-julie)
(color-theme-julie)

(add-to-list 'load-path "~/.emacs.d/vendor/web-mode")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Ido
(require 'ido)
(ido-mode t)

; Winner mode
(winner-mode t)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

; Cut functionality for Dired
(add-to-list 'load-path "~/.emacs.d/vendor/dired-copy-paste")
(require 'dired-copy-paste)

(add-to-list 'load-path "~/.emacs.d/vendor/csv-mode")
(require 'csv-mode)

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-calfw")
(require 'calfw-org)
(require 'calfw-ical)
;; (cfw:open-ical-calendar "http://www.google.com/calendar/ical/martin%40martinjlowm.dk/public/basic.ics")

;; Eclim
(add-to-list 'load-path "~/.emacs.d/vendor/s.el")
(require 's)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/emacs-eclim"))
;; only add the vendor path when you want to use the libraries provided with emacs-eclim
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/emacs-eclim/vendor"))
(require 'eclim)

(setq eclim-auto-save t)
(global-eclim-mode)

(require 'eclimd)

; Parenthesis!
(add-to-list 'load-path "~/.emacs.d/vendor/parenthesis")
(require 'parenthesis)
(global-set-key (kbd "M-9") 'parenthesis-insert-parens)
(global-set-key (kbd "M-[") 'parenthesis-insert-brackets)
(global-set-key (kbd "M-]") 'parenthesis-insert-braces)

; Multiple cursors
(add-to-list 'load-path "~/.emacs.d/vendor/multiple-cursors.el")
(require 'multiple-cursors)
(global-set-key (kbd "C-x M-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Markdown
(add-to-list 'load-path "~/.emacs.d/vendor/markdown-mode")
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
; Gnuplot
(autoload 'gnuplot-mode "gnuplot")
(add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))

;; C# mode
(add-to-list 'load-path "~/.emacs.d/vendor/csharp-mode")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

;; F# mode
(setq load-path (cons "~/.emacs.d/vendor/fsharp-mode" load-path))
(setq auto-mode-alist (cons '("\\.fs[iylx]?$" . fsharp-mode) auto-mode-alist))
(autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
(autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)

;; XLSL mode
(add-to-list 'load-path "~/.emacs.d/vendor/xlsl-mode")
(require 'xlsl-mode)

; Smex
(add-to-list 'load-path "~/.emacs.d/vendor/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

; Inline calculation
(add-to-list 'load-path "~/.emacs.d/vendor/calc-inline")
(require 'calc-inline)

;; NeoTree
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-neotree")
(require 'neotree)

; Doc mode
(add-to-list 'load-path "~/.emacs.d/vendor/doc-mode")
(require 'doc-mode)

; Enable X clipboard
(setq x-select-enable-clipboard t)

;; Dropdown-list
(add-to-list 'load-path "~/.emacs.d/vendor/dropdown-list")
(require 'dropdown-list)

; YAsnippet
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))
;; Autopair
(add-to-list 'load-path "~/.emacs.d/vendor/autopair")
(require 'autopair)
(autopair-global-mode)

; Sage
(add-to-list 'load-path "~/.emacs.d/vendor/python-mode")
(require 'python-mode)
(add-to-list 'load-path "~/.emacs.d/vendor/sage-mode/emacs")
(require 'sage-mode)
(setq sage-command "/opt/sage/sage")

;; Zenburn MOTHAFUCKA!
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-emacs")
;; (add-to-list 'load-path "~/.emacs.d/themes/solarized-emacs")
;; (load-theme 'zenburn t)

; Enable Solarized dark
(add-to-list 'load-path "~/.emacs.d/themes/solarized-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-emacs")
;; (load-theme 'solarized-dark t)
; CoffeeScript Mode
(add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
(autoload 'coffee-mode "coffee-mode")
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
(setq coffee-tab-width 4)
; Popup dependency
(add-to-list 'load-path "~/.emacs.d/vendor/popup-el")

; Auto Complete
(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")
(ac-config-default)

;; Auto Complete Eclim
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

;; Magit
(add-to-list 'load-path "~/.emacs.d/vendor/dash.el")
(require 'dash)
(add-to-list 'load-path "~/.emacs.d/vendor/git-modes")
(require 'git-commit-mode)
(add-to-list 'load-path "~/.emacs.d/vendor/magit")
(require 'magit)
(add-to-list 'load-path "~/.emacs.d/vendor/magit-clone")
(require 'magit-clone)

; Android
(add-to-list 'load-path "~/.emacs.d/vendor/android")
(require 'android)

; Pending delete mode on
(pending-delete-mode 1)

; YAML mode
(add-to-list 'load-path "~/.emacs.d/vendor/yaml-mode")
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

; SCSS mode
(add-to-list 'load-path "~/.emacs.d/vendor/scss-mode")
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

; Lua mode
(add-to-list 'load-path "~/.emacs.d/vendor/lua-mode")
(autoload 'lua-mode "lua-mode")
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

; RHTML
(add-to-list 'load-path "~/.emacs.d/vendor/rhtml")
(autoload 'rhtml-mode "rhtml-mode")
(add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.erubis\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))

; Unity JS
(add-to-list 'load-path "~/.emacs.d/vendor/unityjs-mode")
(autoload 'unityjs-mode "unityjs-mode")
(require 'unityjs-mode)

; Ruby
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-mode))

; Ace jump mode
(add-to-list 'load-path "~/.emacs.d/vendor/ace-jump-mode")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

; JS2
(add-to-list 'load-path "~/.emacs.d/vendor/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; SGML-mode
(require 'sgml-mode)

; Expand region
(add-to-list 'load-path "~/.emacs.d/vendor/expand-region.el")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

; Smooth scrolling
(add-to-list 'load-path "~/.emacs.d/vendor/smooth-scrolling")
(require 'smooth-scrolling)
(setq auto-window-vscroll nil)

; Fix for hard links.
(setq backup-by-copying-when-linked t)

; New bindings for window resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<up>") 'shrink-window)
(global-set-key (kbd "S-C-<down>") 'enlarge-window)


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")


(put 'ido-exit-minibuffer 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asm-comment-char 35)
 '(custom-safe-themes
   (quote
    ("e3c85c5da800be5fe6c1cd0e7884031edf065e44e42aa07096aa26d117c28092" "baed08a10ff9393ce578c3ea3e8fd4f8c86e595463a882c55f3bd617df7e5a45" "0bac11bd6a3866c6dee5204f76908ec3bdef1e52f3c247d5ceca82860cccfa9d" default)))
 '(matlab-mode-install-path
   (quote
    ("/home/martinjlowm/matlab/toolbox/" "/home/martinjlowm/Dropbox/Kommunikationsteknologi")))
 '(mm-text-html-renderer (quote gnus-w3m))
 '(neo-banner-message "")
 '(neo-window-width 35)
 '(neo-header-height 0)
 '(vhdl-project-alist
   (quote
    (("digital_electronics_lab_2" "VHDL laboratory exercise" "~/Documents/02138_digital_electronics_1/projects/lab_2/"
      ("src/")
      ""
      (("GHDL" "\\2" "\\2" nil))
      "lib/" "digital_lab_2_lib" "lib/digital_lab_2_lib/" "Makefile" "")
     ("Example 1" "Source files in two directories, custom library name, VHDL'87" "~/example1/"
      ("src/system/" "src/components/")
      ""
      (("ModelSim" "-87 \\2" "-f \\1 top_level" nil)
       ("Synopsys" "-vhdl87 \\2" "-f \\1 top_level"
        ((".*/datapath/.*" . "-optimize \\3")
         (".*_tb\\.vhd"))))
      "lib/" "example3_lib" "lib/example3/" "Makefile_\\2" "")
     ("Example 2" "Individual source files, multiple compilers in different directories" "$EXAMPLE2/"
      ("vhdl/system.vhd" "vhdl/component_*.vhd")
      "" nil "\\1/" "work" "\\1/work/" "Makefile" "")
     ("Example 3" "Source files in a directory tree, multiple compilers in same directory" "/home/me/example3/"
      ("-r ./*/vhdl/")
      "/CVS/" nil "./" "work" "work-\\1/" "Makefile-\\1" "-------------------------------------------------------------------------------
-- This is a multi-line project description
-- that can be used as a project dependent part of the file header.
")))))
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

   With a prefix ARG prompt for a file to visit.
   Will also prompt for a file to visit if current
   buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "C-x M-f") 'sudo-edit)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:family "Droid Sans Mono")))))
