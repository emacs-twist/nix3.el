;;; nix26-format.el --- Formatting utilities -*- lexical-binding: t -*-

(defun nix26-format--column-width (limit items)
  (let ((max (apply #'max (mapcar #'length items))))
    (if limit
        (min limit max)
      max)))

(provide 'nix26-format)
;;; nix26-format.el ends here
