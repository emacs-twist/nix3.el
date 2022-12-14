;;; nix3-utils.el ---  -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'nix3-core)

(defun nix3-put-overlay-on-region (beg end &rest properties)
  "A shorthand for putting overlay properties on a region."
  (declare (indent 2))
  (let ((ov (make-overlay beg end)))
    (cl-loop for (prop value) on properties by #'cddr
             do (overlay-put ov prop value))))

(defun nix3-format--column-width (limit items)
  (let ((max (apply #'max (mapcar #'length items))))
    (if limit
        (min limit max)
      max)))

(defun nix3-flake-ref-alist-to-url (url-alist)
  "Convert ORIGIN into a plain URL format."
  (let-alist url-alist
    (concat (pcase \.type
              ("github" (format "github:%s/%s" \.owner \.repo))
              ("gitlab" (format "gitlab:%s/%s" \.owner \.repo))
              ("sourcehut" (format "sourcehut:%s/%s" \.owner \.repo))
              ("git" (concat "git+" \.url))
              ("tarball" \.url)
              ("path" (concat "path:" \.path))
              ("indirect" (concat "flake:" \.id))
              (_ (format "error: %s: %s" \.type url-alist)))
            (if (and \.ref (equal \.type "github"))
                (concat "/" .ref)
              (concat (if (or \.ref \.rev)
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
                        ""))))))

(defun nix3-format-duration (seconds)
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

(defun nix3-format-timestamp (time)
  (cl-etypecase time
    (number
     (let ((offset (car (current-time-zone))))
       (format "%s (%s)"
               (format-time-string "%F %X" time)
               (nix3-format-duration (- (float-time)
                                        offset
                                        time)))))))

(defun nix3-lookup-tree (path data)
  "Look up PATH in a tree DATA."
  (cl-reduce (lambda (acc f)
               (cdr (assq f acc)))
             (cdr path)
             :initial-value
             (cdr (assq (car path) data))))

(provide 'nix3-utils)
;;; nix3-utils.el ends here
