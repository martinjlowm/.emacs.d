(setq gc-cons-threshold 400000000)

;; General GUI changes
(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 0))


;; Blank scratch message
(setq inhibit-startup-message t)
(setq initial-scratch-message "")


;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))
(package-initialize)

;;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; From use-package README
(eval-when-compile
  (require 'use-package))
(add-to-list 'load-path "~/.emacs.d/vendor/diminish.el")
(require 'diminish)
(require 'bind-key)


;; Install newest org mode
(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))
(require 'org)


(org-babel-load-file (concat user-emacs-directory "config/user.org"))
(org-babel-load-file (concat user-emacs-directory "config/" (system-name) ".org"))
(org-babel-load-file (concat user-emacs-directory "config/mail.org"))
(org-babel-load-file (concat user-emacs-directory "config/theme.org"))

(when (eq system-type 'darwin)
  (org-babel-load-file (concat user-emacs-directory "config/mac.org")))

(org-babel-load-file (concat user-emacs-directory "config/prog-modes.org"))
(org-babel-load-file (concat user-emacs-directory "config/convenience.org"))

(setq gc-cons-threshold 800000)

;; Maximize Emacs window
(when (fboundp 'toggle-frame-fullscreen)
  (toggle-frame-fullscreen))

;; (require 'cl)




;; (setq ispell-program-name "hunspell")
;; (setq ispell-local-dictionary "en_US-large")
;; (setq ispell-local-dictionary-alist
;;       '(("en_US-large" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)
;;         ("en_GB-ise" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))



;; (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
;; (global-unset-key (kbd "C-z"))


;; Use spaces for indentation by default
;; (setq-default indent-tabs-mode nil)
;; (show-paren-mode t)



;; emacs-deferred
;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-deferred")

;; ;; emacs-ctable
;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-ctable")

;; ;; emacs-epc
;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-epc")

;; ;; emacs-edbi
;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-edbi")
;; (require 'edbi)

;; ;; smali-mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/Emacs-Smali")
;; (require 'smali-mode)



;; Electric Align
;; (add-to-list 'load-path "~/.emacs.d/vendor/electric-align")
;; (require 'electric-align)

;; Rainbow Delimiters
;; (add-to-list 'load-path "~/.emacs.d/vendor/rainbow-delimiters")
;; (require 'rainbow-delimiters)

;; loc-changes
;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-loc-changes")
;; (require 'loc-changes)

;; (add-to-list 'load-path "~/.emacs.d/vendor/levenshtein")
;; (require 'levenshtein)




;; RealGUD (requires loc-changes)
;; (add-to-list 'load-path "~/.emacs.d/vendor/realgud/emacs-load-relative")
;; (add-to-list 'load-path "~/.emacs.d/vendor/realgud")
;; (require 'realgud)

;; (add-to-list 'load-path "~/.emacs.d/vendor/realgud-lldb")
;; (require 'realgud-lldb)


;; (setq resize-mini-windows 'nil)
;; (set-face-attribute 'default nil :height 130)

;; (add-to-list 'load-path "~/.emacs.d/vendor/names")
;; (add-to-list 'load-path "~/.emacs.d/vendor/aggressive-indent-mode")
;; (require 'aggressive-indent)


;; (add-to-list 'load-path "~/.emacs.d/vendor/let-alist")
;; (require 'let-alist)





;; (add-to-list 'load-path "~/.emacs.d/vendor/perspective-el")
;; (require 'perspective)
;; (persp-mode)


;; (defun eval-and-replace ()
;;   "Replace the preceding sexp with its value."
;;   (interactive)
;;   (backward-kill-sexp)
;;   (condition-case nil
;;       (prin1 (eval (read (current-kill 0)))
;;              (current-buffer))
;;     (error (message "Invalid expression")
;;            (insert (current-kill 0)))))


;; (add-to-list 'load-path "~/.emacs.d/vendor/web-mode")
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; (setq web-mode-engines-alist
;;       '(("liquid"    . "\\(_layouts\\|_includes\\)*\\.html")
;;         ))


;; (defun web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq tab-width 2)
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))
;; (add-hook 'web-mode-hook  'web-mode-hook)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

; Ido
;; (require 'ido)
;; (ido-mode t)

;; PO mode
;; (add-to-list 'load-path "/usr/local/Cellar/gettext/0.19.8.1/share/emacs/site-lisp/gettext")
;; (require 'po-mode)

; Winner mode
;; (winner-mode t)
;; (global-set-key (kbd "S-<left>") 'windmove-left)
;; (global-set-key (kbd "S-<right>") 'windmove-right)
;; (global-set-key (kbd "S-<up>") 'windmove-up)
;; (global-set-key (kbd "S-<down>") 'windmove-down)

;; ; Cut functionality for Dired
;; (add-to-list 'load-path "~/.emacs.d/vendor/dired-copy-paste")
;; (require 'dired-copy-paste)

;; (add-to-list 'load-path "~/.emacs.d/vendor/csv-mode")
;; (require 'csv-mode)

;; (add-to-list 'load-path "~/.emacs.d/vendor/fic-mode")
;; (require 'fic-mode)

;; (load "~/.emacs.d/auth-source-fixes.el")




;; PHP mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/php-mode")
;; (require 'php-mode)


;; Eclim
;; (add-to-list 'load-path "~/.emacs.d/vendor/s.el")
;; (require 's)
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/emacs-eclim"))
;; ;; only add the vendor path when you want to use the libraries provided with emacs-eclim
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/emacs-eclim/vendor"))

;; (require 'eclim)

;; (setq eclim-auto-save t)
;; (global-eclim-mode)

;; (require 'eclimd)

;; (add-to-list 'load-path "~/.emacs.d/vendor/dash.el")
;; (require 'dash)

;; (add-to-list 'load-path "~/.emacs.d/vendor/epl")
;; (add-to-list 'load-path "~/.emacs.d/vendor/pkg-info.el")
;; (require 'pkg-info)

;; ;; Flycheck
;; (add-to-list 'load-path "~/.emacs.d/vendor/seq.el")
;; (require 'seq)
;; (add-to-list 'load-path "~/.emacs.d/vendor/flycheck")
;; (require 'flycheck)
;; (flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (add-to-list 'load-path "~/.emacs.d/vendor/flycheck-google-cpplint")
;; (eval-after-load 'flycheck
;;   '(progn
;;      (require 'flycheck-google-cpplint)
;;      ;; Add Google C++ Style checker.
;;      ;; In default, syntax checked by Clang and Cppcheck.
;;      (flycheck-add-next-checker 'c/c++-clang
;;                                 'c/c++-googlelint)))


;; (flycheck-define-checker vhdl-ghdl
;;   "A VHDL syntax checker using ghdl."
;;   :command ("ghdl" "-s" "--std=02" "--ieee=synopsys" "-fexplicit" source)
;;   :error-patterns
;;   ((error line-start (file-name) ":" line ":" column
;; 	  ": " (message) line-end))
;;   :modes vhdl-mode)
;; (flycheck-set-checker-executable 'vhdl-ghdl)

;; (add-hook 'vhdl-mode-hook
;;           '(lambda ()
;;              (setq flycheck-checker 'vhdl-ghdl)
;;              (flycheck-mode 1)))

;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-memoize")
;; (add-to-list 'load-path "~/.emacs.d/vendor/jdee")
;; (require 'jdee)
;; (setq jdee-server-dir "~/.emacs.d/vendor/jdee-server/target")

;; (defun c-java-lineup-cascaded-calls (langelem)
;;   (save-excursion
;;     (back-to-indentation)
;;     (let ((operator (and (looking-at "\\.")
;;                          (regexp-quote (match-string 0))))
;;           (stmt-start (c-langelem-pos langelem)) col)

;;       (when (and operator
;;                  (looking-at operator)
;;                  (zerop (c-backward-token-2 1 t stmt-start)))
;;         (if (and (eq (char-after) ?\()
;;                  (zerop (c-backward-token-2 2 t stmt-start))
;;                  (looking-at operator))
;;             (progn
;;               (setq col (current-column))

;;               (while (and (zerop (c-backward-token-2 1 t stmt-start))
;;                           (eq (char-after) ?\()
;;                           (zerop (c-backward-token-2 2 t stmt-start))
;;                           (looking-at operator))
;;                 (setq col (current-column)))
;;               (vector col))
;;           (vector (+ (current-column)
;;                      c-basic-offset)))))))

;; (defun setup-jdee ()
;;   "Set up JDEE.
;; This hook sets up JDEE with personal changes, including:
;; - chained method call indentation, handy for factory pattern
;; - adds checkstyle syntax checking after javac"
;;   (c-preprend-offset 'arglist-cont 'c-java-lineup-cascaded-calls)
;;   (c-preprend-offset 'statement-cont 'c-java-lineup-cascaded-calls)
;;   (require 'jdee-flycheck)
;;   (flycheck-add-next-checker #'jdee-flycheck-javac-checker #'checkstyle)

;;   (add-to-list 'load-path "~/.emacs.d/vendor/javadoc-lookup")
;;   (require 'javadoc-lookup)
;;   (require 'maven-fetch)
;;   (javadoc-add-artifacts [javax.ws.rs javax.ws.rs-api "2.0.1"]
;;                          [org.glassfish.jersey.media jersey-media-multipart "2.26"]
;;                          [javax.servlet javax.servlet-api "3.1.0"]))
;; (add-hook 'jdee-mode-hook 'setup-jdee)


;; (add-to-list 'load-path "~/.emacs.d/vendor/flycheck-checkstyle")
;; (require 'flycheck-checkstyle)
;; (setq flycheck-checkstylerc "checkstyle.xml")
;; (flycheck-add-mode 'checkstyle 'jdee-mode)

;; ;; C/C++ include paths
;; (defun clang-include-path ()
;;   (setq flycheck-clang-include-path
;;           (list (expand-file-name "~/Documents/bachelor_project/telecan/src")
;;                 (expand-file-name "~/Documents/02393_programming_in_c++/projects/02393_tile_ping_pong/framework")
;;                 (expand-file-name "~/Documents/02393_programming_in_c++/projects/02393_tile_ping_pong/framework/irrlicht-1.8.1/include")
;;                 (expand-file-name "~/src/fenix_os/src")
;;                 (expand-file-name "~/src/fenix_os/include")
;;                 (expand-file-name "~/src/libeboks/src")
;;                 (expand-file-name "~/src/libeboks/include")
;;                 (expand-file-name "~/src/libeboks/lib/gmock-1.7.0/include")
;;                 (expand-file-name "~/src/libeboks/lib/gtest-1.7.0/include")
;;                 (expand-file-name "/usr/local/opt/openssl/include"))))

;; (add-hook 'c-mode-hook 'clang-include-path)
;; (add-hook 'c++-mode-hook 'clang-include-path)

; Company-mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/company-mode")
;; (require 'company)
;; (global-company-mode t)

;; Matlab
;; (setq matlab-shell-command "/Applications/MATLAB_R2017a.app/bin/matlab")
;; (add-to-list 'load-path "~/.emacs.d/vendor/matlab-mode")
;; (require 'matlab-mode)
;; (matlab-mode-common-setup)

; Parenthesis!
;; (add-to-list 'load-path "~/.emacs.d/vendor/parenthesis")
;; (require 'parenthesis)
;; (global-set-key (kbd "M-9") 'parenthesis-insert-parens)
;; (global-set-key (kbd "M-[") 'parenthesis-insert-brackets)
;; (global-set-key (kbd "M-]") 'parenthesis-insert-braces)

;; ; Multiple cursors
;; (add-to-list 'load-path "~/.emacs.d/vendor/multiple-cursors.el")
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-x M-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Markdown
;; (add-to-list 'load-path "~/.emacs.d/vendor/markdown-mode")
;; (autoload 'markdown-mode "markdown-mode"
;;    "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Gnuplot
;; (add-to-list 'load-path "~/.emacs.d/vendor/gnuplot-mode")
;; (autoload 'gnuplot-mode "gnuplot")
;; (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))

;; C# mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/csharp-mode")
;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;; (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;; F# mode
;; (setq load-path (cons "~/.emacs.d/vendor/fsharp-mode" load-path))
;; (setq auto-mode-alist (cons '("\\.fs[iylx]?$" . fsharp-mode) auto-mode-alist))
;; (autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
;; (autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)

;; XLSL mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/xlsl-mode")
;; (require 'xlsl-mode)




;; (add-to-list 'load-path "~/.emacs.d/vendor/ediff-trees")
;; (require 'ediff-trees)

;; (add-to-list 'load-path "~/.emacs.d/vendor/haskell-mode")
;; (require 'haskell-mode)


; Inline calculation
;; (add-to-list 'load-path "~/.emacs.d/vendor/calc-inline")
;; (require 'calc-inline)

;; NeoTree
;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-neotree")
;; (require 'neotree)

;; Ignore `C-x 1' for certain buffers
;; (ad-unadvise 'delete-other-windows)
;; (defvar ignore-windows '())
;; (defadvice delete-other-windows (around neotree-delete-other-windows activate)
;;   "Delete all windows except neotree."
;;   (interactive)
;;   (mapc (lambda (window)
;;           (if (not (member (buffer-name (window-buffer window)) ignore-windows))
;;         (delete-window window)))
;;           (cdr (window-list))))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(asm-comment-char 35)
;;  '(auth-sources
;;    (quote
;;     (macos-keychain-internet "~/.authinfo.gpg" "~/.netrc")))
;;  '(compilation-scroll-output (quote first-error))
;;  '(custom-safe-themes
;;    (quote
;;     ("e3c85c5da800be5fe6c1cd0e7884031edf065e44e42aa07096aa26d117c28092" "baed08a10ff9393ce578c3ea3e8fd4f8c86e595463a882c55f3bd617df7e5a45" "0bac11bd6a3866c6dee5204f76908ec3bdef1e52f3c247d5ceca82860cccfa9d" default)))
;;  '(eclim-eclipse-dirs (quote ("/usr/share/eclipse/plugins/org.eclim_2.4.0")))
;;  '(eclim-executable "/usr/share/eclipse/plugins/org.eclim_2.4.0/bin/eclim")
;;  '(flycheck-display-errors-function (quote flycheck-display-error-messages-revised))
;;  '(flycheck-googlelint-extensions "cc,h,cpp,cu,cuh,hpp")
;;  '(flycheck-googlelint-filter "+build/include,-whitespace,+whitespace/braces")
;;  '(flycheck-googlelint-linelength "80")
;;  '(flycheck-googlelint-root "src")
;;  '(flycheck-googlelint-verbose "3")
;;  '(flycheck-phpcs-standard "WordPress")
;;  '(global-flycheck-mode t)
;;  '(latex-indent-within-escaped-parens t)
;;  '(ledger-reports
;;    (quote
;;     (("register" "ledger assets")
;;      ("Assets" "ledger -p \"this month\"")
;;      ("bal" "ledger -f %(ledger-file) bal")
;;      ("reg" "ledger -f %(ledger-file) reg --amount-width 15")
;;      ("payee" "ledger -f %(ledger-file) reg @%(payee)")
;;      ("account" "ledger -f %(ledger-file) reg %(account) --amount-width 15"))))
;;  '(lua-indent-level 4)
;;  '(matlab-indent-level 2)
;;  '(mm-text-html-renderer (quote gnus-w3m))
;;  '(neo-banner-message "")
;;  '(neo-header-height 0 t)
;;  '(neo-tree-display-cur-dir nil)
;;  '(neo-window-width 20)
;;  '(org-agenda-files nil)
;;  '(org-format-latex-options
;;    (quote
;;     (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
;;      ("begin" "$1" "$" "$$" "\\(" "\\["))))
;;  '(org-latex-classes
;;    (quote
;;     (("dtu_report" "\\documentclass[12pt]{dtu_report}"
;;       ("\\section{%s}" . "\\section*{%s}")
;;       ("\\subsection{%s}" . "\\subsection*{%s}")
;;       ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
;;      ("article" "\\documentclass[11pt]{article}"
;;       ("\\section{%s}" . "\\section*{%s}")
;;       ("\\subsection{%s}" . "\\subsection*{%s}")
;;       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;       ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
;;      ("report" "\\documentclass[11pt]{report}"
;;       ("\\part{%s}" . "\\part*{%s}")
;;       ("\\chapter{%s}" . "\\chapter*{%s}")
;;       ("\\section{%s}" . "\\section*{%s}")
;;       ("\\subsection{%s}" . "\\subsection*{%s}")
;;       ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
;;      ("book" "\\documentclass[11pt]{book}"
;;       ("\\part{%s}" . "\\part*{%s}")
;;       ("\\chapter{%s}" . "\\chapter*{%s}")
;;       ("\\section{%s}" . "\\section*{%s}")
;;       ("\\subsection{%s}" . "\\subsection*{%s}")
;;       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
;;  '(org-latex-default-packages-alist
;;    (quote
;;     (("" "fontspec" t)
;;      ("" "amsmath" t)
;;      ("" "amssymb" t)
;;      ("" "shellesc" t)
;;      ("" "minted" t)
;;      ("" "hyperref" t))))
;;  '(org-latex-pdf-process (quote ("latexmk -g -pdf %f")))
;;  '(package-selected-packages (quote (use-package auctex)))
;;  '(powerline-default-separator (quote utf-8))
;;  '(safe-local-variable-values
;;    (quote
;;     ((css-indent-offset . 2)
;;      (eval font-lock-add-keywords nil
;;            (quote
;;             (("defexamples\\|def-example-group\\| => "
;;               (0
;;                (quote font-lock-keyword-face))))))
;;      (nxml-attribute-indent . 4)
;;      (nxml-child-indent . 4)
;;      (cmake-tab-width . 4)
;;      (TeX-master)
;;      (eval setq flycheck-clang-include-path
;;            ("/usr/local/include" "src" "include"))
;;      (encoding . utf-8)
;;      (eval setq flycheck-clang-include-path
;;            (list
;;             ((expand-file-name "src")
;;              (expand-file-name "include"))))
;;      (flycheck-clang-include-path list
;;                                   (expand-file-name "./src/")
;;                                   (expand-file-name "./include/"))
;;      (flycheck-clang-include-path
;;       (list
;;        (expand-file-name "./src/")
;;        (expand-file-name "./include/")))
;;      (flycheck-clang-include-path
;;       (list
;;        (expand-file-name "src")
;;        (expand-file-name "include")))
;;      (lua-indent-level . 4))))
;;  '(vhdl-project nil)
;;  '(vhdl-project-alist
;;    (quote
;;     (("Edge Detection Accelerator" "" "~/Documents/02203_design_of_digital_systems/edge_detector/"
;;       ("src/*.vhd" "tests/*.vhd")
;;       "" nil "./" "work" "work/" "Makefile" "")
;;      ("digital_electronics_lab_2" "VHDL laboratory exercise" "~/Documents/02138_digital_electronics_1/projects/lab_2/"
;;       ("src/*.vhd")
;;       ""
;;       (("GHDL" "\\2" "\\2" nil))
;;       "lib/" "digital_lab_2_lib" "lib/digital_lab_2_lib/" "Makefile" "")
;;      ("Example 1" "Source files in two directories, custom library name, VHDL'87" "~/example1/"
;;       ("src/system/" "src/components/")
;;       ""
;;       (("ModelSim" "-87 \\2" "-f \\1 top_level" nil)
;;        ("Synopsys" "-vhdl87 \\2" "-f \\1 top_level"
;;         ((".*/datapath/.*" . "-optimize \\3")
;;          (".*_tb\\.vhd"))))
;;       "lib/" "example3_lib" "lib/example3/" "Makefile_\\2" "")
;;      ("Example 2" "Individual source files, multiple compilers in different directories" "$EXAMPLE2/"
;;       ("vhdl/system.vhd" "vhdl/component_*.vhd")
;;       "" nil "\\1/" "work" "\\1/work/" "Makefile" "")
;;      ("Example 3" "Source files in a directory tree, multiple compilers in same directory" "/home/me/example3/"
;;       ("-r ./*/vhdl/")
;;       "/CVS/" nil "./" "work" "work-\\1/" "Makefile-\\1" "-------------------------------------------------------------------------------
;; -- This is a multi-line project description
;; -- that can be used as a project dependent part of the file header.
;; ")))))

;; ; Doc mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/doc-mode")
;; (require 'doc-mode)

;; Ledger mode
;; (add-to-list 'load-path "/usr/local/Cellar/ledger/3.1.1_5/share/emacs/site-lisp/ledger")
;; (require 'ledger-mode)

;; ; Enable X clipboard
;; (setq x-select-enable-clipboard t)

;; ;; Dropdown-list
;; (add-to-list 'load-path "~/.emacs.d/vendor/dropdown-list")
;; (require 'dropdown-list)

;; ; YAsnippet
;; (add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (setq yas-prompt-functions '(yas-dropdown-prompt
;;                              yas-ido-prompt
;;                              yas-completing-prompt))
;; ;; Autopair
;; (add-to-list 'load-path "~/.emacs.d/vendor/autopair")
;; (require 'autopair)
;; (autopair-global-mode)

;; nXhtml
;; (add-to-list 'load-path "~/.emacs.d/vendor/nxhtml")
;; (load "~/.emacs.d/vendor/nxhtml/autostart.el")

;; Python
;; (setq py-install-directory "~/.emacs.d/vendor/python-mode")
;; (add-to-list 'load-path py-install-directory)
;; (require 'python-mode)

;; Sage
;; (add-to-list 'load-path "~/.emacs.d/vendor/sage-mode/emacs")
;; (require 'sage "sage")
;; (setq sage-command "/usr/local/bin/sage")

;; (setq org-src-fontify-natively t)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((python . t)
;;    (matlab . t)))

;; Django
;; (add-to-list 'load-path "~/.emacs.d/vendor/python-django.el")
;; (require 'python-django)

;; Zenburn MOTHAFUCKA!
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-emacs")
;; (add-to-list 'load-path "~/.emacs.d/themes/solarized-emacs")
;; (load-theme 'zenburn t)

; Enable Solarized dark
;; (add-to-list 'load-path "~/.emacs.d/themes/solarized-emacs")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-emacs")
;; (load-theme 'solarized-dark t)
; CoffeeScript Mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
;; (autoload 'coffee-mode "coffee-mode")
;; (add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
;; (setq coffee-tab-width 4)
; Popup dependency
;; (add-to-list 'load-path "~/.emacs.d/vendor/popup-el")

;; Clojure
;; (add-to-list 'load-path "~/.emacs.d/vendor/clojure-mode")
;; (require 'clojure-mode)
;; (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))
;; (add-hook 'clojure-mode-hook #'paredit-mode)
;; (add-hook 'clojure-mode-hook #'smartparens-strict-mode)

;; Auto Complete Eclim
;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)


;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-async")
;; (add-to-list 'load-path "~/.emacs.d/vendor/helm")
;; (require 'helm-config)
;; (add-to-list 'load-path "~/.emacs.d/vendor/helm-descbinds")
;; (require 'helm-descbinds)

;; (add-to-list 'load-path "~/.emacs.d/vendor/projectile")
;; (require 'projectile)
;; (require 'helm-projectile)
;; (projectile-global-mode)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-h a") 'helm-apropos)
;; (global-set-key (kbd "C-h b") 'helm-descbinds)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "M-o") 'helm-projectile)


;; Android
;; (add-to-list 'load-path "~/.emacs.d/vendor/android")
;; (require 'android)

;; Pending delete mode on
;; (pending-delete-mode 1)

; YAML mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/yaml-mode")
;; (autoload 'yaml-mode "yaml-mode")
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

; SCSS mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/scss-mode")
;; (autoload 'scss-mode "scss-mode")
;; (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
;; (setq scss-compile-at-save nil)

; Lua mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/lua-mode")
;; (autoload 'lua-mode "lua-mode")
;; (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

; RHTML
;; (add-to-list 'load-path "~/.emacs.d/vendor/rhtml")
;; (autoload 'rhtml-mode "rhtml-mode")
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
;; (add-to-list 'auto-mode-alist '("\\.erubis\\'" . rhtml-mode))
;; (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))

; Unity JS
;; (add-to-list 'load-path "~/.emacs.d/vendor/unityjs-mode")
;; (autoload 'unityjs-mode "unityjs-mode")
;; (require 'unityjs-mode)

; Ruby
;; (add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-mode))
;; (setq ruby-align-chained-calls t)

; JS2
;; (add-to-list 'load-path "~/.emacs.d/vendor/js2-mode")
;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-hook 'js2-mode-hook
;;           '(lambda()
;;              (setq js-indent-level 2)))

; RJSX
;; (add-to-list 'load-path "~/.emacs.d/vendor/rjsx-mode")
;; (autoload 'rjsx-mode "rjsx-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
;; (add-hook 'rjsx-mode-hook
;;           '(lambda()
;;              (setq js-indent-level 2)))
;; (setq js2-strict-trailing-comma-warning nil)

;; TypeScript
;; (add-to-list 'load-path "~/.emacs.d/vendor/typescript.el")
;; (add-to-list 'load-path "~/.emacs.d/vendor/tide")
;; (require 'tide)

;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))
;; enable typescript-tslint checker
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

;; Kotlin
;; (add-to-list 'load-path "~/.emacs.d/vendor/kotlin-mode")
;; (require 'kotlin-mode)
;; (setq kotlin-tab-width 4)

; SGML-mode
;; (require 'sgml-mode)

; SQL-indent
;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-sql-indent")
;; (require 'sql-indent)

; Expand region
;; (add-to-list 'load-path "~/.emacs.d/vendor/expand-region.el")
;; (require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)

;; CMake mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/cmake-mode")
;; (require 'cmake-mode)
;; (setq auto-mode-alist
;;       (append '(("CMakeLists\\.txt\\'" . cmake-mode)
;;                 ("\\.cmake\\'" . cmake-mode))
;;               auto-mode-alist))

; Smooth scrolling
;; (add-to-list 'load-path "~/.emacs.d/vendor/smooth-scrolling")
;; (require 'smooth-scrolling)
;; (setq auto-window-vscroll nil)

; New bindings for window resizing
;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<up>") 'shrink-window)
;; (global-set-key (kbd "S-C-<down>") 'enlarge-window)


;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "open")


;; (put 'ido-exit-minibuffer 'disabled nil)

;; (add-to-list 'load-path "~/.emacs.d/vendor/matlab-emacs")
;; (load-library "matlab-load")
;; (add-hook 'matlab-mode
;;           (lambda ()
;;             (local-set-key (kbd "<up>") 'previous-line)
;;             (local-set-key (kbd "<down>") 'next-line)))

;; (put 'set-goal-column 'disabled nil)
;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)


;; (defun sudo-edit (&optional arg)
;;   "Edit currently visited file as root.

;;    With a prefix ARG prompt for a file to visit.
;;    Will also prompt for a file to visit if current
;;    buffer is not visiting a file."
;;   (interactive "P")
;;   (if (or arg (not buffer-file-name))
;;       (find-file (concat "/sudo:root@localhost:"
;;                          (ido-read-file-name "Find file (as root): ")))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; (global-set-key (kbd "C-x M-f") 'sudo-edit)

;; (defun what-face (pos)
;;   (interactive "d")
;;   (let ((face (or (get-char-property (point) 'read-face-name)
;;                   (get-char-property (point) 'face))))
;;     (if face (message "Face: %s" face) (message "No face at %d" pos))))




;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-gradle-mode")
;; (require 'gradle-mode)
;; (gradle-mode 1)

;; (add-to-list 'load-path "~/.emacs.d/vendor/groovy-emacs-modes")
;; (require 'groovy-mode)
;; (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
;; (add-hook 'groovy-mode-hook
;;           '(lambda()
;;              (setq c-basic-offset 4)))

;; (add-to-list 'load-path "~/.emacs.d/vendor/rainbow-mode")
;; (require 'rainbow-mode)

;; (add-to-list 'load-path "~/.emacs.d/vendor/rtliber")
;; (require 'rt-liberation)
;; (setq rt-liber-rest-url "itsupport.student.dtu.dk"
;;       rt-liber-base-url "https://itsupport.student.dtu.dk/"
;;       rt-liber-rt-version "4.2.10"
;;       rt-liber-jump-to-latest t
;;       rt-liber-update-default-queue "Latex Support"
;;       rt-liber-rest-username "martinjlm"
;;       rt-liber-username "martinjlm"
;;       rt-liber-rest-password (funcall (plist-get (car (auth-source-search
;;                                          :max 1
;;                                          :host "itsupport.student.dtu.dk"
;;                                          :port 443
;;                                          :require '(:secret))) :secret)))

;; (require 'rt-liberation-gnus)
;; (setq rt-liber-gnus-comment-address "latex-support-comment@student.dtu.dk"
;;       rt-liber-gnus-address         "latex-support@student.dtu.dk"
;;       rt-liber-gnus-subject-name    "[Latex Support]")

;; (require 'rt-liberation-update)

;; (defun fetch-rt-tickets ()
;;   (interactive)
;;   (rt-liber-browse-query
;;    (rt-liber-compile-query (and (queue "Latex Support")
;;                                 ((or (status "open") (status "new")))
;;                                 ((or (owner "Nobody") (owner "martinjlm")))
;;                                 (created nil "2016-01-01")))))

;; (load "~/.emacs.d/rt-unicode.el")
;; (message "%s" (rt-liber-compile-query (and (queue "Latex Support")
;;                                            ((or (status "open") (status "new")))
;;                                            ((or (owner "Nobody") (owner "martinjlm")))
;;                                            (created "2016-01-01"))))
;;   (rt-liber-browse-query "Queue = 'Latex Support' AND (Status = 'open' OR Status = 'new') AND (Owner = 'Nobody' OR Owner = 'martinjlm') AND Created > '2016-01-01'")


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
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(whitespace-indentation ((t (:foreground "gray19"))))
;;  '(whitespace-space-after-tab ((t (:foreground "gray19")))))

;; (setq doc-view-continuous t)

;; (with-current-buffer (get-buffer " *Echo Area 0*")
;;   (setq-local face-remapping-alist '((default (:height 120) variable-pitch))))
;; (with-current-buffer (get-buffer " *Echo Area 1*")
;;   (setq-local face-remapping-alist '((default (:height 120) variable-pitch))))


;; (setq comment-start "/*" comment-end "*/")

; Cut functionality for Dired
;; (add-to-list 'load-path "~/.emacs.d/vendor/applescript-mode")
;; (require 'applescript-mode)
;; (setq-default flycheck-emacs-lisp-load-path 'inherit)
;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-request")
;; (require 'request)
;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-websocket")

;; (add-to-list 'load-path "~/.emacs.d/vendor/graphql-mode")
;; (require 'graphql-mode)

;; (eval-after-load "lisp-mode"
;;   '(defun lisp-indent-function (indent-point state)
;;      "This function is the normal value of the variable `lisp-indent-function'.
;; The function `calculate-lisp-indent' calls this to determine
;; if the arguments of a Lisp function call should be indented specially.
;; INDENT-POINT is the position at which the line being indented begins.
;; Point is located at the point to indent under (for default indentation);
;; STATE is the `parse-partial-sexp' state for that position.
;; If the current line is in a call to a Lisp function that has a non-nil
;; property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
;; it specifies how to indent.  The property value can be:
;; * `defun', meaning indent `defun'-style
;;   \(this is also the case if there is no property and the function
;;   has a name that begins with \"def\", and three or more arguments);
;; * an integer N, meaning indent the first N arguments specially
;;   (like ordinary function arguments), and then indent any further
;;   arguments like a body;
;; * a function to call that returns the indentation (or nil).
;;   `lisp-indent-function' calls this function with the same two arguments
;;   that it itself received.
;; This function returns either the indentation to use, or nil if the
;; Lisp function does not specify a special indentation."
;;      (let ((normal-indent (current-column))
;;            (orig-point (point)))
;;        (goto-char (1+ (elt state 1)))
;;        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
;;        (cond
;;         ;; car of form doesn't seem to be a symbol, or is a keyword
;;         ((and (elt state 2)
;;               (or (not (looking-at "\\sw\\|\\s_"))
;;                   (looking-at ":")))
;;          (if (not (> (save-excursion (forward-line 1) (point))
;;                      calculate-lisp-indent-last-sexp))
;;              (progn (goto-char calculate-lisp-indent-last-sexp)
;;                     (beginning-of-line)
;;                     (parse-partial-sexp (point)
;;                                         calculate-lisp-indent-last-sexp 0 t)))
;;          ;; Indent under the list or under the first sexp on the same
;;          ;; line as calculate-lisp-indent-last-sexp.  Note that first
;;          ;; thing on that line has to be complete sexp since we are
;;          ;; inside the innermost containing sexp.
;;          (backward-prefix-chars)
;;          (current-column))
;;         ((and (save-excursion
;;                 (goto-char indent-point)
;;                 (skip-syntax-forward " ")
;;                 (not (looking-at ":")))
;;               (save-excursion
;;                 (goto-char orig-point)
;;                 (looking-at ":")))
;;          (save-excursion
;;            (goto-char (+ 2 (elt state 1)))
;;            (current-column)))
;;         (t
;;          (let ((function (buffer-substring (point)
;;                                            (progn (forward-sexp 1) (point))))
;;                method)
;;            (setq method (or (function-get (intern-soft function)
;;                                           'lisp-indent-function)
;;                             (get (intern-soft function) 'lisp-indent-hook)))
;;            (cond ((or (eq method 'defun)
;;                       (and (null method)
;;                            (> (length function) 3)
;;                            (string-match "\\`def" function)))
;;                   (lisp-indent-defform state indent-point))
;;                  ((integerp method)
;;                   (lisp-indent-specform method state
;;                                         indent-point normal-indent))
;;                  (method
;;                   (funcall method indent-point state)))))))))



;; (defun flycheck-display-error-messages-revised (errors)
;;   "Display the messages of ERRORS.

;; Concatenate all non-nil messages of ERRORS separated by empty
;; lines, and display them with `display-message-or-buffer', which
;; shows the messages either in the echo area or in a separate
;; buffer, depending on the number of lines.  See Info
;; node `(elisp)Displaying Messages' for more information.

;; In the latter case, show messages in
;; `flycheck-error-message-buffer'."
;;   (when (and errors (flycheck-may-use-echo-area-p))
;;     (let ((messages (seq-map #'flycheck-error-format-message-and-id errors)))
;;       (message "%s" (string-join messages "\n\n")))))

;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-calfw")
;; (require 'calfw)
;; (require 'calfw-org)

;; (add-to-list 'load-path "~/.emacs.d/vendor/calfw-gcal.el")
;; (require 'calfw-gcal)
;; (require 'calfw-ical)
;; (setq cfw:gcal-user "martin@martinjlowm.dk")
;; (setq cfw:gcal-pass
;;       (funcall (plist-get (car (auth-source-search
;;                                 :max 1
;;                                 :user "martin@martinjlowm.dk"
;;                                 :host "accounts.google.com"
;;                                 :require '(:secret))) :secret)))
;(cfw:open-ical-calendar "https://calendar.google.com/calendar/ical/martin%40martinjlowm.dk/private-47f49342d5b1931ec54eaa00ef963e72/basic.ics")

;; (add-to-list 'load-path "~/.emacs.d/vendor/weechat.el")
;; (require 'weechat)



;; (add-to-list 'load-path "~/.emacs.d/")
;; (require 'dired-details)
;; (setq-default dired-details-hidden-string "--- ")
;; (dired-details-install)
