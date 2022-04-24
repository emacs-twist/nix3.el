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

(defun nix26-flake-ref-alist-to-url (url-alist)
  "Convert ORIGIN into a plain URL format."
  (let-alist url-alist
    (concat (pcase \.type
              ("github" (format "github:%s/%s" \.owner \.repo))
              ("git" (concat "git+" \.url))
              ("tarball" \.url)
              ("path" (concat "path:" \.path))
              ("indirect" (concat "indirect:" \.id))
              (_ (format "error: %s: %s" \.type url-alist)))
            (if (or \.ref \.rev)
                "?"
              "")
            (if \.ref
                (format "ref=%s" \.ref)
              "")
            (if (and \.ref \.rev)
                "&"
              "")
            (if \.rev
                (format "rev=%s" \.rev)
              ""))))

(defun nix26-format-duration (seconds)
  (cond
   ((< seconds 60)
    "just now")
   ((< seconds 3600)
    (format "%.f minutes ago" (/ seconds 60)))
   ((< seconds 86400)
    (format "%.f hours ago" (/ seconds 3600)))
   ((< seconds (* 86400 2))
    "yesterday")
   ((< seconds (* 86400 7))
    (format "%.f days ago" (/ seconds 86400)))
   ((< seconds (* 86400 30))
    (format "%.f weeks ago" (/ seconds (* 86400 7))))
   ((< seconds (* 86400 365))
    (format "%.f months ago" (/ seconds (* 86400 30))))
   (t
    (format "%.f years ago" (/ seconds (* 86400 365))))))

(defun nix26-format-timestamp (time)
  (cl-etypecase time
    (number
     (let ((offset (car (current-time-zone))))
       (format "%s (%s)"
               (format-time-string "%F %X" time)
               (nix26-format-duration (- (float-time)
                                         offset
                                         time)))))))

(provide 'nix26-utils)
;;; nix26-utils.el ends here
