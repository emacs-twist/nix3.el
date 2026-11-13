;;; nix3-utils.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022,2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; URL: https://github.com/emacs-twist/nix3.el

;; This file is not part of GNU Emacs.

;;; License:

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

;;; Commentary:

;; This library provides utility functions for nix3.el, including flake URL
;; conversion, Git remote handling, display formatting, and store-path and
;; terminal integration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'nix3-core)
(eval-when-compile
  (require 'rx))

(declare-function nix-store-realise "ext:nix-store")

(declare-function vterm "ext:vterm")
(declare-function vterm-send-string "ext:vterm")
(declare-function vterm-send-return "ext:vterm")

(defun nix3-put-overlay-on-region (beg end &rest properties)
  "Put PROPERTIES on an overlay spanning BEG through END."
  (declare (indent 2))
  (let ((ov (make-overlay beg end)))
    (cl-loop for (prop value) on properties by #'cddr
             do (overlay-put ov prop value))))

(defun nix3-format--column-width (limit items)
  "Return the display width for ITEMS, constrained by LIMIT.

When LIMIT is nil, return the width of the longest item."
  (let ((max (apply #'max (mapcar #'length items))))
    (if limit
        (min limit max)
      max)))

(defun nix3-build-git-clone-url (url-alist)
  "Return a Git clone URL for URL-ALIST."
  (let-alist url-alist
    (pcase \.type
      ("github" (format "https://github.com/%s/%s.git" \.owner \.repo))
      ("gitlab" (format "https://gitlab.com/%s/%s.git" \.owner \.repo))
      ("sourcehut" (format "https://git.sr.ht/%s/%s" \.owner \.repo))
      ("git" \.url)
      ("indirect" (error "Indirect URL: %s" \.id))
      (_ (error "Cannot build a remote URL from %s" \.type)))))

(defun nix3-flake-ref-alist-to-url (url-alist)
  "Convert URL-ALIST into a plain flake URL."
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
                        "")
                      (if \.dir
                          (format "dir=%s" \.dir)
                        ""))))))

(defun nix3-git-url-to-flake-alist (git-url)
  ;; TODO: Add tests
  "Return the flake reference alist corresponding to GIT-URL.

Known forges are converted to their dedicated flake reference types,
while other Git URLs are represented as generic Git references."
  ;; git+https://example.org/my/repo
  ;; git+https://example.org/my/repo?dir=flake1
  ;; git+ssh://git@github.com/NixOS/nix?ref=v1.2.3
  ;; git://github.com/edolstra/dwarffs?ref=unstable&rev=e486d8d40e626a20e06d792db8cc5ac5aba9a5b4
  ;; git+file:///home/my-user/some-repo/some-repo
  (cl-labels
      ((git-alist (url)
         (list (cons 'type "git")
               (cons 'url url)))
       (path-parts (path)
         "Return (OWNER REPO) from PATH, or nil if it is not a repo path."
         (save-match-data
           (when (string-match (rx bol
                                   (group (+ (not (any "/"))))
                                   "/"
                                   (group (+ (not (any "/")))))
                               path)
             (list (match-string 1 path)
                   (string-remove-suffix ".git" (match-string 2 path))))))
       (forge-alist (host path)
         "Return a flake alist for PATH on HOST, if HOST is a known forge."
         (when-let* ((parts (and (not (string-match-p (rx (any "?#")) path))
                                 (path-parts path)))
                     (owner (car parts))
                     (repo (cadr parts)))
           (pcase host
             ("github.com"
              (list (cons 'type "github")
                    (cons 'owner owner)
                    (cons 'repo repo)))
             ("gitlab.com"
              (list (cons 'type "gitlab")
                    (cons 'owner owner)
                    (cons 'repo repo)))
             ("git.sr.ht"
              (list (cons 'type "sourcehut")
                    (cons 'owner owner)
                    (cons 'repo repo))))))
       (ssh-url (user host path)
         (format "ssh://%s%s/%s"
                 (if user (concat user "@") "")
                 host path)))
    (pcase git-url
      ;; A generic Git URL prefixed with git+ is represented by its URL
      ;; without the prefix, which is added back when formatting the flake URL.
      ((rx bol "git+")
       (nix3-git-url-to-flake-alist (substring git-url 4)))
      ;; A URL with an explicit scheme, optionally including userinfo and a
      ;; port: https://user@example.org:8443/owner/repo.git
      ((rx bol (group (+ (any alnum "+-" ".")))
           "://"
           (?  (group (+ (not (any "/@")))) "@")
           (group (* (not (any "/:"))))
           (?  ":" (+ (char digit)))
           (?  "/" (group (* anything))))
       (let ((scheme (match-string 1 git-url))
             (host (match-string 3 git-url))
             (path (or (match-string 4 git-url) "")))
         (pcase scheme
           ((or "https" "ssh")
            (or (forge-alist host path)
                (git-alist git-url)))
           (_ (git-alist git-url)))))
      ;; SCP-like syntax: [USER@]HOST:PATH
      ((rx bol (?  (group (+ (not (any "/:@")))) "@")
           (group (+ (not (any "/:"))))
           ":" (group (+ anything)))
       (let ((user (match-string 1 git-url))
             (host (match-string 2 git-url))
             (path (match-string 3 git-url)))
         (or (forge-alist host path)
             (git-alist (ssh-url user host path)))))
      (_ (git-alist git-url)))))

(defun nix3-format-duration (seconds)
  "Format SECONDS as a human-readable relative duration."
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
  "Format TIME with the local date and a relative duration."
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

;;;###autoload
(defun nix3-vterm-shell-command (shell-command)
  "Run SHELL-COMMAND in `vterm'.

It always creates a new buffer."
  (with-current-buffer (save-window-excursion (vterm 'new))
    (rename-buffer (generate-new-buffer-name "*nix run*"))
    (vterm-send-string shell-command)
    (vterm-send-return)
    (pop-to-buffer (current-buffer))))

(defun nix3-realise-and-show-store (path)
  "Show PATH using nix-store.el.  Realise it if necessary."
  (cond
   ((file-directory-p path)
    (dired path))
   ((file-readable-p path)
    (dired-jump nil path))
   (t
    (cl-flet ((sentinel (process _event)
                (when (eq 'exit (process-status process))
                  (if (= 0 (process-exit-status process))
                      (nix3-realise-and-show-store path)
                    (error "Failed to realise the store path %s" path)))))
      (message "Realising %s..." path)
      (let ((proc (nix-store-realise path)))
        (set-process-sentinel proc #'sentinel))))))

;;;###autoload
(defun nix3-default-compilation-buffer (_command)
  "Returns the name of a compilation buffer."
  (project-prefixed-buffer-name "nix3-compilation"))

(provide 'nix3-utils)
;;; nix3-utils.el ends here
