;;; nix26-utils.el ---  -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'nix26-core)

(defun nix26-put-overlay-on-region (beg end &rest properties)
  "A shorthand for putting overlay properties on a region."
  (declare (indent 2))
  (let ((ov (make-overlay beg end)))
    (cl-loop for (prop value) on properties by #'cddr
             do (overlay-put ov prop value))))

(defun nix26-format--column-width (limit items)
  (let ((max (apply #'max (mapcar #'length items))))
    (if limit
        (min limit max)
      max)))

(provide 'nix26-utils)
;;; nix26-utils.el ends here
