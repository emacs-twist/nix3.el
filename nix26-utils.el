;;; nix26-utils.el ---  -*- lexical-binding: t -*-

(defun nix26-put-overlay-on-region (beg end &rest properties)
  "A shorthand for putting overlay properties on a region."
  (declare (indent 2))
  (let ((ov (make-overlay beg end)))
    (cl-loop for (prop value) on properties by #'cddr
             do (overlay-put ov prop value))))

(provide 'nix26-utils)
;;; nix26-utils.el ends here
