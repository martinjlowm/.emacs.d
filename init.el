(require 'cl)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-fringe-mode 0)
(global-subword-mode)
(setq column-number-mode 1)
(setq ring-bell-function 'ignore)
(setq-default fill-column 80)

;; Update PATH
(add-to-list 'load-path "~/.emacs.d/vendor/exec-path-from-shell")
(require 'exec-path-from-shell)

(add-to-list 'load-path "~/.emacs.d/vendor/osx-keychain.el")
(require 'osx-keychain)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Mail
(setq mm-text-html-renderer 'w3m)
(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
               (nnimap-user "martin@martinjlowm.dk")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
				   "martin@martinjlowm.dk"
                                   (find-keychain-internet-password
                                    "martin@martinjlowm.dk"
                                    "imap.gmail.com")))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "\n\nOn %a, %b %d %Y, %f wrote:")
;; (gnus-demon-add-handler 'gnus-demon-scan-news 2 t)
;; (setq gnus-signature-file "~/.signature")
(setq user-mail-address "martin@martinjlowm.dk")

(add-to-list 'load-path "~/.emacs.d/vendor/bbdb/lisp")
(require 'bbdb-loaddefs)
(bbdb-initialize 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<escape>") 'keyboard-quit)
(setq-default indent-tabs-mode nil)
;; (setq indent-tabs-mode t)
(show-paren-mode t)

(setq resize-mini-windows 'nil)
(set-face-attribute 'default nil :height 130)

(add-to-list 'load-path "~/.emacs.d/vendor/names")
(add-to-list 'load-path "~/.emacs.d/vendor/aggressive-indent-mode")
(require 'aggressive-indent)

(add-to-list 'load-path "~/.emacs.d/vendor/google-c-style")
(require 'google-c-style)

(defun c++-hook ()
  (google-set-c-style))
(add-hook 'c++-mode-hook 'c++-hook)

(add-to-list 'load-path "~/.emacs.d/vendor/let-alist")
(require 'let-alist)

(add-hook 'org-mode-hook (lambda ()
                           (local-unset-key (kbd "S-<up>"))
                           (local-unset-key (kbd "S-<down>"))
                           (local-unset-key (kbd "S-<left>"))
                           (local-unset-key (kbd "S-<right>"))))

(setq set-fill-column 72)

;; (add-to-list 'load-path "~/.emacs.d/vendor/auctex")
;; (load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)

;; (defun run-arara ()
;;   (interactive)
;;   (message (shell-command-to-string (concat "arara " (buffer-file-name)))))

(add-to-list 'load-path "~/.emacs.d/vendor/auctex-latexmk")
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (require 'auctex-latexmk)
            (auctex-latexmk-setup)
            (setq TeX-command-default "LatexMk")
            (setq TeX-clean-confirm nil)
            (defun TeX-run-latexmk (name command file)
              (interactive)
              (let ((buf (current-buffer))
                    ;; (TeX-process-asynchronous nil)
                    (TeX-sentinel-default-function 'Latexmk-sentinel)
                    (pair (assq buffer-file-coding-system auctex-latexmk-encoding-alist)))
                (unless (null pair)
                  (setenv "LATEXENC" (cdr pair)))
                (TeX-run-TeX name command file)
                (setenv "LATEXENC" nil)
                (with-current-buffer buf
                  (TeX-clean))))
            (append LaTeX-clean-intermediate-suffixes
                        '("\\.pyg"))))


;; (add-hook 'TeX-mode-hook (lambda ()
;;                            (TeX-PDF-mode t)
;;                            (setq TeX-view-program-selection (quote
;;                                                              (((output-dvi style-pstricks)
;;                                                                "dvips and gv")
;;                                                               (output-dvi "xdvi")
;;                                                               (output-pdf "xdg-open")
;;                                                               (output-html "xdg-open"))))
;;                            (define-key LaTeX-mode-map (kbd "C-c C-c") 'run-arara)))

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

(add-to-list 'load-path "~/.emacs.d/vendor/web-mode")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4))
(add-hook 'web-mode-hook  'web-mode-hook)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Ido
(require 'ido)
(ido-mode t)

;; PO mode
(add-to-list 'load-path "/usr/local/Cellar/gettext/0.19.5.1/share/emacs/site-lisp")
(require 'po-mode)

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

(add-to-list 'load-path "~/.emacs.d/vendor/fic-mode")
(require 'fic-mode)

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-calfw")
(require 'calfw-org)
(require 'calfw-ical)
;; (cfw:open-ical-calendar "http://www.google.com/calendar/ical/martin%40martinjlowm.dk/public/basic.ics")


;; PHP mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/php-mode")
;; (require 'php-mode)


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




(add-to-list 'load-path "~/.emacs.d/vendor/ediff-trees")
(require 'ediff-trees)


; Inline calculation
(add-to-list 'load-path "~/.emacs.d/vendor/calc-inline")
(require 'calc-inline)

;; NeoTree
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-neotree")
(require 'neotree)

;; Ignore `C-x 1' for certain buffers
(ad-unadvise 'delete-other-windows)
(defvar ignore-windows '())
(defadvice delete-other-windows (around neotree-delete-other-windows activate)
  "Delete all windows except neotree."
  (interactive)
  (mapc (lambda (window)
          (if (not (member (buffer-name (window-buffer window)) ignore-windows))
        (delete-window window)))
          (cdr (window-list))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asm-comment-char 35)
 '(auth-sources
   (quote
    (macos-keychain-internet "~/.authinfo.gpg" "~/.netrc")))
 '(compilation-scroll-output (quote first-error))
 '(custom-safe-themes
   (quote
    ("e3c85c5da800be5fe6c1cd0e7884031edf065e44e42aa07096aa26d117c28092" "baed08a10ff9393ce578c3ea3e8fd4f8c86e595463a882c55f3bd617df7e5a45" "0bac11bd6a3866c6dee5204f76908ec3bdef1e52f3c247d5ceca82860cccfa9d" default)))
 '(eclim-eclipse-dirs (quote ("/usr/share/eclipse/plugins/org.eclim_2.4.0")))
 '(eclim-executable "/usr/share/eclipse/plugins/org.eclim_2.4.0/bin/eclim")
 '(flycheck-display-errors-function (quote flycheck-display-error-messages-unless-error-list))
 '(flycheck-googlelint-extensions "cc,h,cpp,cu,cuh,hpp")
 '(flycheck-googlelint-filter "+build/include,-whitespace,+whitespace/braces")
 '(flycheck-googlelint-linelength "80")
 '(flycheck-googlelint-root "src")
 '(flycheck-googlelint-verbose "3")
 '(flycheck-phpcs-standard "WordPress")
 '(global-flycheck-mode t)
 '(latex-indent-within-escaped-parens t)
 '(matlab-shell-command "/Applications/MATLAB_R2015b.app/bin/matlab")
 '(mm-text-html-renderer (quote gnus-w3m))
 '(neo-banner-message "")
 '(neo-header-height 0 t)
 '(neo-tree-display-cur-dir nil)
 '(neo-window-width 20)
 '(powerline-default-separator (quote utf-8))
 '(vhdl-project nil)
 '(vhdl-project-alist
   (quote
    (("Edge Detection Accelerator" "" "~/Documents/02203_design_of_digital_systems/edge_detector/"
      ("src/*.vhd" "tests/*.vhd")
      "" nil "./" "work" "work/" "Makefile" "")
     ("digital_electronics_lab_2" "VHDL laboratory exercise" "~/Documents/02138_digital_electronics_1/projects/lab_2/"
      ("src/*.vhd")
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
(setq py-install-directory "~/.emacs.d/vendor/python-mode")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

(add-to-list 'load-path "~/.emacs.d/vendor/sage-mode/emacs")
(require 'sage "sage")
(setq sage-command "/usr/local/bin/sage")

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
(add-to-list 'load-path "~/.emacs.d/vendor/magit/lisp")
(require 'magit)
(add-to-list 'load-path "~/.emacs.d/vendor/magit-clone")
(require 'magit-clone)

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-async")
(add-to-list 'load-path "~/.emacs.d/vendor/helm")
(require 'helm-config)
(add-to-list 'load-path "~/.emacs.d/vendor/helm-descbinds")
(require 'helm-descbinds)

(add-to-list 'load-path "~/.emacs.d/vendor/projectile")
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-o") 'helm-projectile)

;; Flycheck
(add-to-list 'load-path "~/.emacs.d/vendor/seq.el")
(require 'seq)
(add-to-list 'load-path "~/.emacs.d/vendor/flycheck")
(require 'flycheck)
(flycheck-add-mode 'php 'web-mode)
(flycheck-add-mode 'php-phpcs 'web-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-to-list 'load-path "~/.emacs.d/vendor/flycheck-google-cpplint")
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-clang
                                'c/c++-googlelint)))


(flycheck-define-checker vhdl-ghdl
  "A VHDL syntax checker using ghdl."
  :command ("ghdl" "-s" "--std=02" "--ieee=synopsys" "-fexplicit" source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column
	  ": " (message) line-end))
  :modes vhdl-mode)
(flycheck-set-checker-executable 'vhdl-ghdl)

(add-hook 'vhdl-mode-hook
          '(lambda ()
             (setq flycheck-checker 'vhdl-ghdl)
             (flycheck-mode 1)))


(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list (expand-file-name "~/Documents/bachelor_project/telecan/src")
                                 (expand-file-name "~/Documents/02393_programming_in_c++/projects/02393_tile_ping_pong/framework")
                                 (expand-file-name "~/Documents/02393_programming_in_c++/projects/02393_tile_ping_pong/framework/irrlicht-1.8.1/include")
                                 (expand-file-name "~/src/libeboks/src")
                                 (expand-file-name "~/src/libeboks/include")
                                 (expand-file-name "~/src/libeboks/lib/gmock-1.7.0/include")
                                 (expand-file-name "~/src/libeboks/lib/gtest-1.7.0/include")
                                 (expand-file-name "/usr/local/opt/openssl/include")))))



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
(define-key global-map (kbd "<clear>") 'ace-jump-mode)

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

;; CMake mode
(add-to-list 'load-path "~/.emacs.d/vendor/cmake-mode")
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

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
      browse-url-generic-program "open")


(put 'ido-exit-minibuffer 'disabled nil)

(add-to-list 'load-path "~/.emacs.d/vendor/matlab-emacs")
(load-library "matlab-load")
(add-hook 'matlab-mode
          (lambda ()
            (local-set-key (kbd "<up>") 'previous-line)
            (local-set-key (kbd "<down>") 'next-line)))

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

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(add-to-list 'load-path "~/.emacs.d/vendor/powerline")
(require 'powerline)

(add-to-list 'load-path "~/.emacs.d/vendor/color-theme")
(require 'color-theme)
(add-to-list 'load-path "~/.emacs.d/themes")
(require 'color-theme-julie)
(color-theme-julie)
(powerline-mjlm-theme)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (global-set-key (kbd "<prior>") 'backward-paragraph)
  (global-set-key (kbd "<next>") 'forward-paragraph)
  (global-set-key (kbd "<home>") 'backward-word)
  (global-set-key (kbd "<end>") 'forward-word)
  (global-set-key (kbd "<C-delete>") 'backward-kill-word)
  (setq ns-alternate-modifier nil)
  (setq ns-right-alternate-modifier 'alt)
  (global-set-key (kbd "S-<home>") 'shrink-window-horizontally)
  (global-set-key (kbd "S-<end>") 'enlarge-window-horizontally)
  (global-set-key (kbd "S-<prior>") 'shrink-window)
  (global-set-key (kbd "S-<next>") 'enlarge-window)
  (global-set-key (kbd "<kp-enter>") 'cua-set-rectangle-mark)
  )

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-gradle-mode")
(require 'gradle-mode)
(gradle-mode 1)

(add-to-list 'load-path "~/.emacs.d/vendor/groovy-emacs-modes")
(require 'groovy-mode)

(add-to-list 'load-path "~/.emacs.d/vendor/rainbow-mode")
(require 'rainbow-mode)

;; FIXME: GPG temp fix
;; (defun epg--list-keys-1 (context name mode)
;;   (let ((args (append (if (epg-context-home-directory context)
;; 			  (list "--homedir"
;; 				(epg-context-home-directory context)))
;; 		      '("--with-colons" "--no-greeting" "--batch"
;; 			"--with-fingerprint" "--with-fingerprint")
;; 		      (unless (eq (epg-context-protocol context) 'CMS)
;; 			'("--fixed-list-mode"))))
;; 	(list-keys-option (if (memq mode '(t secret))
;; 			      "--list-secret-keys"
;; 			    (if (memq mode '(nil public))
;; 				"--list-keys"
;; 			      "--list-sigs")))
;; 	(coding-system-for-read 'binary)
;; 	keys string field index)
;;     (if name
;; 	(progn
;; 	  (unless (listp name)
;; 	    (setq name (list name)))
;; 	  (while name
;; 	    (setq args (append args (list list-keys-option (car name)))
;; 		  name (cdr name))))
;;       (setq args (append args (list list-keys-option))))
;;     (with-temp-buffer
;;       (apply #'call-process
;; 	     (epg-context-program context)
;; 	     nil (list t nil) nil args)
;;       (goto-char (point-min))
;;       (while (re-search-forward "^[a-z][a-z][a-z]:.*" nil t)
;; 	(setq keys (cons (make-vector 15 nil) keys)
;; 	      string (match-string 0)
;; 	      index 0
;; 	      field 0)
;; 	(while (and (< field (length (car keys)))
;; 		    (eq index
;; 			(string-match "\\([^:]+\\)?:" string index)))
;; 	  (setq index (match-end 0))
;; 	  (aset (car keys) field (match-string 1 string))
;; 	  (setq field (1+ field))))
;;       (nreverse keys))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-indentation ((t (:foreground "gray19"))))
 '(whitespace-space-after-tab ((t (:foreground "gray19")))))

(setq doc-view-continuous t)

(with-current-buffer (get-buffer " *Echo Area 0*")
  (setq-local face-remapping-alist '((default (:height 120) variable-pitch))))
(with-current-buffer (get-buffer " *Echo Area 1*")
  (setq-local face-remapping-alist '((default (:height 120) variable-pitch))))

(when (fboundp 'toggle-frame-fullscreen)
  (toggle-frame-fullscreen))
;; (setq comment-start "/*" comment-end "*/")

; Cut functionality for Dired
(add-to-list 'load-path "~/.emacs.d/vendor/applescript-mode")
(require 'applescript-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-request")
(require 'request)
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-websocket")

(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))

(defun* auth-source-macos-keychain-search (&rest
                                    spec
                                    &key backend create delete
                                    type max
                                    &allow-other-keys)
  "Search the MacOS Keychain; spec is like `auth-source'.

All search keys must match exactly.  If you need substring
matching, do a wider search and narrow it down yourself.

You'll get back all the properties of the token as a plist.

The :type key is either `macos-keychain-internet' or
`macos-keychain-generic'.

For the internet keychain type, the :label key searches the
item's labels (\"-l LABEL\" passed to \"/usr/bin/security\").
Similarly, :host maps to \"-s HOST\", :user maps to \"-a USER\",
and :port maps to \"-P PORT\" or \"-r PROT\"
\(note PROT has to be a 4-character string).

For the generic keychain type, the :label key searches the item's
labels (\"-l LABEL\" passed to \"/usr/bin/security\").
Similarly, :host maps to \"-c HOST\" (the \"creator\" keychain
field), :user maps to \"-a USER\", and :port maps to \"-s PORT\".

Here's an example that looks for the first item in the default
generic MacOS Keychain:

 (let ((auth-sources \\='(macos-keychain-generic)))
    (auth-source-search :max 1)

Here's another that looks for the first item in the internet
MacOS Keychain collection whose label is `gnus':

 (let ((auth-sources \\='(macos-keychain-internet)))
    (auth-source-search :max 1 :label \"gnus\")

And this one looks for the first item in the internet keychain
entries for git.gnus.org:

 (let ((auth-sources \\='(macos-keychain-internet\")))
    (auth-source-search :max 1 :host \"git.gnus.org\"))
"
  ;; TODO
  (assert (not create) nil
          "The MacOS Keychain auth-source backend doesn't support creation yet")
  ;; TODO
  ;; (macos-keychain-delete-item coll elt)
  (assert (not delete) nil
          "The MacOS Keychain auth-source backend doesn't support deletion yet")

  (let* ((coll (oref backend source))
         (max (or max 5000))     ; sanity check: default to stop at 5K
         ;; Filter out ignored keys from the spec
         (ignored-keys '(:create :delete :max :backend :label :host :port))
         ;; Build a search spec without the ignored keys
         (search-keys (loop for i below (length spec) by 2
                            unless (memq (nth i spec) ignored-keys)
                            collect (nth i spec)))
         ;; If a search key value is nil or t (match anything), we skip it
         (search-spec (apply #'append (mapcar
                                      (lambda (k)
                                        (if (or (null (plist-get spec k))
                                                (eq t (plist-get spec k)))
                                            nil
                                          (list k (plist-get spec k))))
                                      search-keys)))
         ;; needed keys (always including host, login, port, and secret)
         (returned-keys (mm-delete-duplicates (append
                                               '(:host :login :port :secret)
                                               search-keys)))
         ;; Extract host and port from spec
         (hosts (plist-get spec :host))
         (hosts (if (and hosts (listp hosts)) hosts `(,hosts)))
         (ports (plist-get spec :port))
         (ports (if (and ports (listp ports)) ports `(,ports)))
         ;; Loop through all combinations of host/port and pass each of these to
         ;; auth-source-macos-keychain-search-items
         (items (catch 'match
                  (dolist (host hosts)
                    (dolist (port ports)
                      (let* ((port (format "%S" port))
                             (items (apply #'auth-source-macos-keychain-search-items
                                           coll
                                           type
                                           max
                                           host port
                                           search-spec)))
                        (when items
                          (throw 'match items)))))))

         ;; ensure each item has each key in `returned-keys'
         (items (mapcar (lambda (plist)
                          (append
                           (apply #'append
                                  (mapcar (lambda (req)
                                            (if (plist-get plist req)
                                                nil
                                              (list req nil)))
                                          returned-keys))
                           plist))
                        items)))
    items))

(defun* auth-source-macos-keychain-search-items (coll _type _max
                                                      host port
                                                      &key label type
                                                      user
                                                      &allow-other-keys)

  (let* ((keychain-generic (eq type 'macos-keychain-generic))
         (args `(,(if keychain-generic
                      "find-generic-password"
                    "find-internet-password")
                 "-g"))
         (ret (list :type type)))
    (when label
      (setq args (append args (list "-l" label))))
    (when host
      (setq args (append args (list (if keychain-generic "-c" "-s") host))))
    (when user
      (setq args (append args (list "-a" user))))

    (when port
      (if keychain-generic
          (setq args (append args (list "-s" port)))
        (setq args (append args (list
                                 (if (string-match "[0-9]+" port) "-P" "-r")
                                 port)))))

      (unless (equal coll "default")
        (setq args (append args (list coll))))

      (with-temp-buffer
        (apply #'call-process "/usr/bin/security" nil t nil args)
        (goto-char (point-min))
        (while (not (eobp))
          (cond
           ((looking-at "^password: \"\\(.+\\)\"$")
            (setq ret (auth-source-macos-keychain-result-append
                       ret
                       keychain-generic
                       "secret"
                       (lexical-let ((v (match-string 1)))
                         (lambda () v)))))
           ;; TODO: check if this is really the label
           ;; match 0x00000007 <blob>="AppleID"
           ((looking-at "^[ ]+0x00000007 <blob>=\"\\(.+\\)\"")
            (setq ret (auth-source-macos-keychain-result-append
                       ret
                       keychain-generic
                       "label"
                       (match-string 1))))
           ;; match "crtr"<uint32>="aapl"
           ;; match "svce"<blob>="AppleID"
           ((looking-at "^[ ]+\"\\([a-z]+\\)\"[^=]+=\"\\(.+\\)\"")
            (setq ret (auth-source-macos-keychain-result-append
                       ret
                       keychain-generic
                       (match-string 1)
                       (match-string 2)))))
          (forward-line)))
      ;; return `ret' iff it has the :secret key
      (and (plist-get ret :secret) (list ret))))
