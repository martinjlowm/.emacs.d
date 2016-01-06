(let ((buf (current-buffer)))
  (list-flycheck-errors)
  (split-window-vertically 50)
  (windmove-down)
  (with-current-buffer
      (current-buffer)
    (setq-local window-size-fixed 'height))
  (set-window-dedicated-p (selected-window) 1)
  (windmove-up)
  (switch-to-buffer buf))

(defvar ignore-windows (cons "*Flycheck errors*" ignore-windows))
