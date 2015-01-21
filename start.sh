#!/bin/sh

emacsclient -e '(unless (find-if (lambda (f)
                                   (let ((p (frame-parameters f)))
                                     (assq '\''window-system p)))
                                 (frame-list))
                  (make-frame-on-display (getenv "DISPLAY")))'
emacsclient "$0"
