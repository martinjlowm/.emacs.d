(neotree)
(with-current-buffer
    (current-buffer)
  (set-char-table-extra-slot standard-display-table 0 ?\ ))

(windmove-right)

(setq ignore-windows (append '(" *NeoTree*") ignore-windows))
