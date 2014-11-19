(defun magit-clone (url)
  "Clone git repository in the DIR directory."
  (interactive "MRepository URL to clone: ")
  (magit-run* (list magit-git-executable "clone" url)))

(provide 'magit-clone)
