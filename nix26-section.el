;;; nix26-section.el --- Magit sections -*- lexical-binding: t -*-

(require 'magit-section)

(defmacro nix26-section--heading (body)
  `(propertize ,body 'face 'magit-section-heading))

(defmacro nix26-section-dlist (indent-level &rest rows)
  (declare (indent 1))
  (let* ((width (1+ (cl-loop for header in (mapcar #'car rows)
                             maximize (length header))))
         (indent-string (make-string indent-level ?\ ))
         (th (format "%s%%-%ds" indent-string width)))
    `(progn
       ,@(mapcar (pcase-lambda (`(,header ,visible ,exp))
                   `(when ,visible
                      (magit-insert-section ('row ,header)
                        (insert (nix26-section--heading (format ,th ,header)))
                        ,exp
                        (newline))))
                 rows))))

(provide 'nix26-section)
;;; nix26-section.el ends here
