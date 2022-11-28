;;; nix3-section.el --- Magit sections -*- lexical-binding: t -*-

(require 'magit-section)

(defmacro nix3-section--heading (body)
  `(propertize ,body 'face 'magit-section-heading))

(defmacro nix3-section-dlist (indent-level &rest rows)
  (declare (indent 1))
  (let* ((width (1+ (cl-loop for header in (mapcar #'car rows)
                             maximize (length header))))
         (indent-string (make-string indent-level ?\ ))
         (th (format "%s%%-%ds" indent-string width)))
    `(progn
       ,@(mapcar (pcase-lambda (`(,header ,visible ,exp))
                   `(when ,visible
                      (magit-insert-section ('row ,header)
                        (insert (nix3-section--heading (format ,th ,header)))
                        ,exp
                        (newline))))
                 rows))))

(provide 'nix3-section)
;;; nix3-section.el ends here
