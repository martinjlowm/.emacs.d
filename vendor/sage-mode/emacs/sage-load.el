;;; sage-load.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "cython-mode" "cython-mode.el" (21841 7651
;;;;;;  0 0))
;;; Generated autoloads from cython-mode.el

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))

(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))

(autoload 'cython-mode "cython-mode" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "sage-build" "sage-build.el" (21841 7651 0
;;;;;;  0))
;;; Generated autoloads from sage-build.el

(defvar sage-build-history nil)

(defalias 'restart-sage 'rerun-sage)

(defalias 'sage-restart 'rerun-sage)

(defalias 'sage-rerun 'rerun-sage)

(autoload 'rerun-sage "sage-build" "\
Kill a running sage and spawn a new sage in the same buffer.
Use `sage-build' with a prefix argument to rebuild sage and then
rerun the freshly built sage.

\(fn)" t nil)

(autoload 'sage-build-process-setup "sage-build" "\
Setup compilation variables and buffer for `sage-build'.
Set up `compilation-exit-message-function' and run `sage-build-setup-hook'.

\(fn)" nil nil)

(autoload 'sage-default-build-command "sage-build" "\
Compute the default sage build command for C-u M-x sage-build to offer.

\(fn)" nil nil)

(defalias 'build-sage 'sage-build)

(autoload 'sage-build "sage-build" "\
Build sage (like sage -b), with user-specified args, and collect output in a buffer.
While sage-build runs asynchronously, you can use
\\[next-error] (M-x next-error), or
\\<sage-build-mode-map>\\[compile-goto-error] in the sage-build XXX
output buffer, to go to the lines where sage-build found matches.

With prefix arg (or andrun), act like sage -br: build sage and
spawn a new sage on success.

This command uses a special history list for its COMMAND-ARGS, so you can
easily repeat a sage-build command.

\(fn COMMAND-ARGS)" t nil)

;;;***

;;;### (autoloads nil "sage-latex" "sage-latex.el" (21841 7651 0
;;;;;;  0))
;;; Generated autoloads from sage-latex.el

(autoload 'sage-run-sagetex "sage-latex" "\
Function to be used in `TeX-command-list' to run sage on sagetex files.
Makes `TeX-command-default' the next command to be run since we
need to retypeset after running sage.
For details of NAME, COMMAND and FILE see the documetation for `TeX-command-list'.

\(fn NAME COMMAND FILE)" nil nil)

(autoload 'sage-auctex-setup "sage-latex" "\
Hooks Sage support into AUCTeX.
Must be called after AUCTeX has been loaded.

Adds entries `TeX-expand-list' and `TeX-command-list' and advises
`TeX-LaTeX-sentinel'.

\(fn)" nil nil)

(autoload 'sage-org-latex-setup "sage-latex" "\
Set up `org-mode' variables to support using SageTeX.
Currently only `org-latex-to-pdf-process' is affected.

\(fn)" nil nil)

(eval-after-load 'tex '(sage-auctex-setup))

(eval-after-load 'org-latex '(sage-org-latex-setup))

;;;***

;;;### (autoloads nil "sage-mode" "sage-mode.el" (21841 7651 0 0))
;;; Generated autoloads from sage-mode.el

(autoload 'inferior-sage-mode "sage-mode" "\
Major mode for interacting with an inferior Sage process.

\(fn)" t nil)

(defvar sage-run-history nil)

(defalias 'sage 'run-sage)

(defalias 'sage-run 'run-sage)

(autoload 'run-sage "sage-mode" "\
Run an inferior Sage process, input and output via buffer *Sage*.

NEW non-nil means always create a new buffer and Sage process.
CMD is the Sage command to run.
NOSHOW non-nil means toggle whether to show the buffer automatically.
The default value depends on `sage-display-inferior-buffer'.
If NOSHOW is t (from lisp), then the buffer will not be displayed.

Normally, if there is a process already running in `sage-buffer',
switch to that buffer.  A new process is started if: one isn't
running attached to `sage-buffer', or interactively the default
`sage-command', or argument NEW is non-nil.  See also the
documentation for `sage-buffer'.

Runs the hook `inferior-sage-mode-hook' (after the
`comint-mode-hook' is run).  (Type \\[describe-mode] in the process
buffer for a list of commands.)

\(fn &optional NEW CMD NOSHOW)" t nil)

(autoload 'sage-mode "sage-mode" "\
Major mode for editing Sage files.

The major entry points are:

`sage', to spawn a new sage session.

`sage-send-buffer', to send the current buffer to the inferior sage, using
\"%runfile\"; `sage-send-region', to send the current region to the inferior
sage, using \"%runfile\"; and `sage-send-doctest', to send the docstring point is
currently looking at to the inferior sage interactively.

`sage-test', to execute \"sage -t\" and friends and parse the output

`sage-build', to execute \"sage -b\" and friends and parse the output.

`sage-rerun' to restart an inferior sage in an existing buffer, and
`sage-build' with a prefix argument to execute \"sage -br\" to rebuild sage
and restart a fresh inferior sage in an existing buffer.

\\{sage-mode-map}

\(fn)" t nil)

(add-to-list 'interpreter-mode-alist '("sage" . sage-mode))

(add-to-list 'auto-mode-alist '("\\.sage\\'" . sage-mode))

(autoload 'sage-send-buffer "sage-mode" "\
Send the current buffer to the inferior sage process.
The buffer is loaded using sage's \"%runfile\" command.

\(fn)" t nil)

(autoload 'sage-send-region "sage-mode" "\
Send the region to the inferior Sage process.
The region is treated as a temporary \".sage\" file with minimal
processing.  The logic is that this command is intended to
emulate interactive input, although this isn't perfect: sending
the region \"2\" does not print \"2\".

\(fn START END)" t nil)

(autoload 'sage-pcomplete-or-help "sage-mode" "\
If point is after ?, describe preceding symbol; otherwise, pcomplete.

\(fn)" t nil)

(autoload 'sage-send-command "sage-mode" "\
Evaluate COMMAND in inferior Python process.

If ECHO-INPUT is non-nil, echo input in process buffer.
If NEEDS-PROMPT is non-nil, sents a print newline statement in an attempt
to ensure that the prompt appears correctly.

\(fn COMMAND &optional ECHO-INPUT NEEDS-PROMPT)" t nil)

(autoload 'sage-find-symbol "sage-mode" "\
Find the definition of the SYMBOL near point.

Finds the source file containing the defintion of the SYMBOL near point and
places point before the definition.
Set mark before moving, if the buffer already existed.

\(fn SYMBOL)" t nil)

(autoload 'sage-find-symbol-other-window "sage-mode" "\
Find, in another window, the definition of SYMBOL near point.

See `sage-find-symbol' for details.

\(fn SYMBOL)" t nil)

(autoload 'sage-find-symbol-other-frame "sage-mode" "\
Find, in another frame, the definition of SYMBOL near point.

See `sage-find-symbol' for details.

\(fn SYMBOL)" t nil)

;;;***

;;;### (autoloads nil "sage-test" "sage-test.el" (21841 7651 0 0))
;;; Generated autoloads from sage-test.el

(defvar sage-test-history nil)

(autoload 'sage-test "sage-test" "\
Run sage-test, with user-specified args, and collect output in a buffer.
While sage-test runs asynchronously, you can use \\[next-error] (M-x next-error), or
\\<sage-test-mode-map>\\[compile-goto-error] in the sage-test
output buffer, to go to the lines where sage-test found matches.

With prefix arg, default to sage -tnew.

This command uses a special history list for its COMMAND-ARGS, so you can
easily repeat a sage-test command.

\(fn COMMAND-ARGS)" t nil)

(autoload 'sage-send-doctest "sage-test" "\
If looking at a sage: prompt, send the current doctest line to the inferior sage.
With prefix argument, send all doctests (at sage: prompts) until
the end of the docstring.

\(fn &optional ALL)" t nil)

;;;***

;;;### (autoloads nil "sage-view" "sage-view.el" (21841 7651 0 0))
;;; Generated autoloads from sage-view.el

(autoload 'sage-view-enable-inline-output "sage-view" "\
Enable inline output pretty-printing, i.e. typeset output from sage in the `inferior-sage-mode' buffer.
WARNING: this communicates with the sage process.  Only use this
when `sage-view' mode is enabled and sage is running.

\(fn)" t nil)

(autoload 'sage-view-disable-inline-output "sage-view" "\
Disable inline output pretty-printing, i.e. do not typeset output from sage in the `inferior-sage-mode' buffer.
WARNING: this communicates with the sage process.  Only use this
when `sage-view' mode is enabled and sage is running.

\(fn)" t nil)

(autoload 'sage-view-enable-inline-plots "sage-view" "\
Enable inline plotting, i.e. display plots in the `inferior-sage-mode' buffer and do not spawn an external viewer.
WARNING: this communicates with the sage process.  Only use this
when `sage-view' mode is enabled and sage is running.

\(fn)" t nil)

(autoload 'sage-view-disable-inline-plots "sage-view" "\
Disable inline plotting, i.e. do not display plots in the `inferior-sage-mode' buffer and instead spawn an external viewer.
WARNING: this communicates with the sage process.  Only use this
when `sage-view' mode is enabled and sage is running.

\(fn)" t nil)

(autoload 'sage-view "sage-view" "\
Toggle automatic typesetting of Sage output.

Typesetting of math formulas is done by LATEX subprocesses and
PDF to PNG conversions.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("sage-blocks.el" "sage-compat.el" "sage-flymake.el"
;;;;;;  "sage.el") (21841 10118 115937 0))

;;;***

(provide 'sage-load)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sage-load.el ends here
