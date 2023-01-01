;;; magit-nix3.el --- Nix3.el integration for Magit -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (nix3 "0.1") (promise "1.1"))
;; Keywords: processes
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

;; This library integrates magit with Nix.

;;; Code:

(require 'map)
(require 'magit-section)

(defgroup magit-nix3 nil
  "Nix flake integration for Magit."
  :prefix "magit-nix3-"
  :group 'magit-status
  :group 'nix3)

;;;; magit-status-mode integration

(defcustom magit-nix3-sections
  '(nix3-flake-insert-outputs
    nix3-flake-insert-inputs)
  "Sections inserted into the magit buffer."
  :type 'hook)

(defcustom magit-nix3-insert-positions nil
  "Position at which nix flake sections are inserted.

The value should be either nil or one of the existing members of
`magit-status-sections-hook'."
  :type '(choice function
                 (const nil)))

(defcustom magit-nix3-append t
  "Whether to insert the section after the position."
  :type 'boolean)

(defcustom magit-nix3-wait 0.5
  "Number of seconds to wait for loading the flake."
  :type 'number)

(defun magit-nix3-flake-sections ()
  (require 'promise)
  ;; Load the library without adding autoloads
  (require 'nix3-flake)
  (when (file-exists-p "flake.nix")
    (nix3-flake--get-promise (nix3-normalize-path default-directory)
                             nil
                             :sections magit-nix3-sections)
    (run-hooks 'magit-nix3-sections)))

;;;###autoload
(define-minor-mode magit-nix3-flake-mode
  "Turn on flake features in `magit-status-mode' buffers."
  :global t
  (cond
   (magit-nix3-flake-mode
    (magit-add-section-hook 'magit-status-sections-hook
                            #'magit-nix3-flake-sections
                            magit-nix3-insert-positions
                            magit-nix3-append))
   (t
    (remove-hook 'magit-status-sections-hook #'magit-nix3-flake-sections))))

;;;; magit-diff-mode integration

(defface magit-nix3-lock-file-heading
  '((t (:inherit magit-diff-file-heading)))
  "Face for flake.lock file paths.")

(defface magit-nix3-lock-node-heading
  '((t (:inherit magit-section-heading)))
  "Face for flake.lock nodes.")

(defun magit-nix3-diff-section ()
  (require 'nix3-flake-lock)
  (when-let (files (magit-nix3--search-flake-locks))
    (cl-flet*
        ((format-mtime (locked)
           (format-time-string "%Y-%m-%d" (alist-get 'lastModified locked)))
         (insert-node (old-or-new node)
           (insert (format "  %s: %s\n"
                           old-or-new
                           (nix3-flake-ref-alist-to-url
                            (alist-get 'locked node)))))
         (insert-node-change (event id &optional old new)
           (magit-insert-section (flake-lock-node id t)
             (magit-insert-heading
               (propertize (format "%-8s %s" event id)
                           'face 'magit-nix3-lock-node-heading)
               (cond
                ((and new old)
                 (format " (%s -> %s)"
                         (format-mtime (alist-get 'locked old))
                         (format-mtime (alist-get 'locked new))))
                (new
                 (format " (%s)"
                         (format-mtime (alist-get 'locked new))))))
             (when old
               (insert-node "old" old))
             (when new
               (insert-node "new" new)))))
      (pcase-exhaustive (nix3-flake-lock--range)
        (`(,lrev ,rrev)
         (dolist (file files)
           (magit-insert-section (flake-lock file)
             (magit-insert-heading (propertize file 'face 'magit-nix3-lock-file-heading))
             (pcase-exhaustive (nix3-flake-lock--diff-entries default-directory
                                                              file lrev rrev)
               ((map :added :removed :changed)
                (pcase-dolist (`(,id . ,new) added)
                  (insert-node-change "added" id nil new))
                (pcase-dolist (`(,id . ,old) removed)
                  (insert-node-change "removed" id old))
                (pcase-dolist (`(,id ,old ,new) changed)
                  (insert-node-change "changed" id old new)))))))))
    (insert ?\n)))

(defun magit-nix3--search-flake-locks ()
  (let (files)
    (save-excursion
      (while (text-property-search-backward 'magit-section)
        (let ((section (magit-current-section)))
          (when (and (eq (oref section type) 'file)
                     (string-match-p (rx (or bol "/")
                                         "flake.lock" eol)
                                     (oref section value)))
            (push (oref section value) files)))))
    (seq-uniq files #'equal)))

;;;###autoload
(define-minor-mode magit-nix3-flake-lock-mode
  "Minor mode to show flake.lock diffs in `magit-diff-mode' buffers."
  :global t
  (cond
   (magit-nix3-flake-lock-mode
    (magit-add-section-hook 'magit-diff-sections-hook
                            #'magit-nix3-diff-section
                            'magit-insert-diff
                            'append))
   (t
    (remove-hook 'magit-diff-sections-hook #'magit-nix3-diff-section))))

(provide 'magit-nix3-flake)
;;; magit-nix3-flake.el ends here
