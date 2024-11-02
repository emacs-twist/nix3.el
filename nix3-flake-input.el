;;; nix3-flake-input.el ---  -*- lexical-binding: t -*-

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

;; FIXME

;;; Code:

(require 'transient)
(require 'nix3-core)
(require 'nix3-utils)
(require 'nix3-browse-url)
(require 'subr-x)
(require 'magit-section)

;; Set to non-nil when the flake is not local.
(defvar nix3-flake-url)

(declare-function nix3-flake-show-url "nix3-flake")
(declare-function nix3-flake--get-metadata-result "nix3-flake")
(declare-function nix3-flake-html-url "nix3-flake")
(declare-function project-prompt-project-dir "project")

(defvar nix3-flake-input-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") #'nix3-flake-input-return)
    map))

(defvar nix3-flake-input nil)

(defun nix3-flake-input--local-p ()
  "Return non-nil when the flake is local."
  (not nix3-flake-url))

(defun nix3-flake-input-return ()
  "Push the button at point or dispatch a transient interface."
  (interactive)
  (if (get-text-property (point) 'button)
      (push-button (point))
    (setq nix3-flake-input (nix3-flake-input-at-point))
    (call-interactively #'nix3-flake-input-dispatch)))

(defun nix3-flake-input--original-url ()
  (thread-last
    (cdr nix3-flake-input)
    (alist-get 'original)
    (nix3-flake-ref-alist-to-url)))

(defun nix3-flake-input--locked-url ()
  (thread-last
    (cdr nix3-flake-input)
    (alist-get 'locked)
    (nix3-flake-ref-alist-to-url)))

(defun nix3-flake-input--last-modified ()
  (thread-last
    (cdr nix3-flake-input)
    (nix3-lookup-tree '(locked lastModified))))

(defun nix3-flake-input--revision ()
  (thread-last
    (cdr nix3-flake-input)
    (nix3-lookup-tree '(locked rev))))

(defun nix3-flake-input--html-url ()
  (nix3-flake-html-url (assq 'original (cdr nix3-flake-input))))

(defun nix3-flake-input--direct-p ()
  (rassoc (car nix3-flake-input)
          (thread-last
            (nix3-flake--get-metadata-result)
            (nix3-lookup-tree '(locks nodes root inputs)))))

(defun nix3-flake-input--updatable-p ()
  (and (nix3-flake-input--local-p)
       (nix3-flake-input--direct-p)))

(transient-define-prefix nix3-flake-input-dispatch ()
  [:description
   (lambda () (format "Original: %s" (nix3-flake-input--original-url)))
   ("d" "Show the flake" nix3-flake-show-original-input)
   ("r" "Browse remote" nix3-flake-input-browse-remote)
   ("c" "Clone" ignore)]
  [:description
   (lambda () (format "Locked: %s (%s)"
                      (nix3-flake-input--locked-url)
                      (format-time-string "%F %R"
                                          (nix3-flake-input--last-modified))))
   ("w" "Copy revision" nix3-flake-input-copy-revision)
   ;; ("l" "Magit log" ignore)
   ;; "Edit worktree"
   ]
  ["Update input"
   :if nix3-flake-input--updatable-p
   :class transient-row
   ("-c" "" "--accept-flake-config")
   ("ul" "Latest" nix3-flake-input-update)
   ("ur" "Revision" nix3-flake-input-update-to-rev)
   ("uf" "Ref" nix3-flake-input-update-to-ref)
   ("uu" "Url" nix3-flake-input-update-to-url)
   ("up" "Project" nix3-flake-input-update-to-project)]
  (interactive)
  (unless nix3-flake-input
    (user-error "No flake input at point"))
  (transient-setup 'nix3-flake-input-dispatch))

(defun nix3-flake-input-at-point ()
  "Return a cons cell of (NAME . DATA) of the input."
  (when-let (section (magit-current-section))
    (when (eq (slot-value section 'type) 'flake-input)
      (oref section value))))

(defun nix3-flake-show-original-input ()
  (interactive)
  (nix3-flake-show-url (nix3-flake-input--original-url)))

(defun nix3-flake-show-locked-input ()
  (interactive)
  (nix3-flake-show-url (nix3-flake-input--locked-url)))

(defun nix3-flake-input-copy-revision ()
  (interactive)
  (kill-new (nix3-flake-input--revision))
  (message "Saved the revision into kill ring"))

(defun nix3-flake-input-browse-remote ()
  (interactive)
  (require 'nix3-browse-url)
  (funcall nix3-browse-url-for-repository
           (nix3-flake-input--html-url)))

;;;; Commands

(defun nix3-flake-input-update (&optional url-or-alist)
  (interactive)
  (pcase nix3-flake-input
    (`(,name . ,_)
     ;; TODO This is a quick-and-dirty implementation, so rewrite it
     (apply #'nix3-run-process-background
            nix3-nix-executable
            (append (if (version<= "2.19" (nix3-nix-version))
                        (list "flake" "update" name)
                      (list "flake" "lock" "--update-input" name))
                    (transient-args 'nix3-flake-input-dispatch)
                    (pcase-exhaustive url-or-alist
                      (`nil)
                      ((pred stringp)
                       (list "--override-input" name url-or-alist))
                      ((pred sequencep)
                       (list "--override-input" name
                             (nix3-flake-ref-alist-to-url url-or-alist)))))))
    (_
     (user-error "No input at point"))))

(defun nix3-flake-input-update-to-rev (rev)
  (interactive "sRevision: ")
  (let ((alist (alist-get 'original (cdr nix3-flake-input))))
    (if-let (cell (assq 'rev alist))
        (setcdr cell rev)
      (setq alist (cons (cons 'rev rev) alist)))
    (nix3-flake-input-update alist)))

(defun nix3-flake-input-update-to-ref (ref)
  (interactive "sRef: ")
  (let ((alist (alist-get 'original (cdr nix3-flake-input))))
    (if-let (cell (assq 'ref alist))
        (setcdr cell ref)
      (setq alist (cons (cons 'ref ref) alist)))
    (nix3-flake-input-update alist)))

(defun nix3-flake-input-update-to-url (url)
  (interactive (let* ((input nix3-flake-input)
                      (default (nix3-flake-ref-alist-to-url (cdr (assq 'original input)))))
                 (list (read-from-minibuffer (format-prompt
                                              (format "Update %s to flake url"
                                                      (car input))
                                              default)
                                             nil nil nil nil default))))
  (nix3-flake-input-update url))

(defun nix3-flake-input-update-to-project (dir)
  (interactive (list (progn
                       (require 'project)
                       (project-prompt-project-dir))))
  (nix3-flake-input-update (concat "path:" (file-truename dir))))

(provide 'nix3-flake-input)
;;; nix3-flake-input.el ends here
