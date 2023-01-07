;;; nix3-utils.el ---  -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
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

(defun nix3-git-url-to-flake-alist (git-url)
  ;; TODO: Add tests
  "Return a flake reference alist corresponding to a git remote url."
  ;; git+https://example.org/my/repo
  ;; git+https://example.org/my/repo?dir=flake1
  ;; git+ssh://git@github.com/NixOS/nix?ref=v1.2.3
  ;; git://github.com/edolstra/dwarffs?ref=unstable&rev=e486d8d40e626a20e06d792db8cc5ac5aba9a5b4
  ;; git+file:///home/my-user/some-repo/some-repo
  (cl-flet
      ((host-to-type (domain)
         (pcase-exhaustive domain
           ("github.com" "github")
           ("gitlab.com" "gitlab")
           ("git.sr.ht" "sourcehut"))))
    (pcase-exhaustive git-url
      ((rx bol "https://" (group (or "github.com" "gitlab.com" "git.sr.ht")) "/"
           (group (+ (not (any "/")))) "/" (group (+? (not (any "/"))))
           (?  ".git")
           (or eol "/"))
       (list (cons 'type (host-to-type (match-string 1 git-url)))
             (cons 'owner (match-string 2 git-url))
             (cons 'repo (match-string 3 git-url))))
      ((rx bol "git@" (group (or "github.com" "gitlab.com" "git.sr.ht")) ":"
           (group (+ (not (any "/")))) "/" (group (+? (not (any "/"))))
           (?  ".git")
           eol)
       (list (cons 'type (host-to-type (match-string 1 git-url)))
             (cons 'owner (match-string 2 git-url))
             (cons 'repo (match-string 3 git-url))))
      ((rx bol (or "https://" "git://"))
       (list (cons 'type "git")
             (cons 'url git-url))))))

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

(defun nix3-git-remotes ()
  "Return an alist of remotes of the current repository."
  (cl-flet
      ((remote-url (string)
         (when (string-match (rx bol "remote." (group (+ (not (any "."))))
                                 ".url=" (group (+ anything)))
                             string)
           (cons (match-string 1 string)
                 (match-string 2 string)))))
    (thread-last
      (nix3--git-config-list)
      (mapcar #'remote-url)
      (delq nil))))

(provide 'nix3-utils)
;;; nix3-utils.el ends here
