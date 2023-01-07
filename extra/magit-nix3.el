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

(declare-function nix3-flake-lock-diff-section "ext:nix3-flake-lock")
(declare-function nix3-flake-run-section "ext:nix3-flake")
(declare-function nix3-normalize-path "ext:nix3-core")

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

(defun magit-nix3-flake-sections ()
  (require 'promise)
  ;; Load the library without adding autoloads
  (require 'nix3-flake)
  (when (file-exists-p "flake.nix")
    (nix3-flake-run-section 'magit-nix3-sections
                            (nix3-normalize-path default-directory)
                            nil)))

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

(defun magit-nix3-diff-section ()
  (require 'nix3-flake-lock)
  ;; When the user edits a commit message, magit displays diffs via
  ;; `magit-revision-sections-hook', so this section will be called as well.
  ;; However, it seems to block creating an actual commit, so I want to prevent
  ;; this function from being called when creating a commit as a workaround. It
  ;; seems that `this-command' is nil during hook call after a commit is made
  ;; with a message, so use it as a guard.
  (when this-command
    (nix3-flake-lock-diff-section)))

;;;###autoload
(define-minor-mode magit-nix3-flake-lock-mode
  "Minor mode to show flake.lock diffs in `magit-diff-mode' buffers."
  :global t
  (cond
   (magit-nix3-flake-lock-mode
    (magit-add-section-hook 'magit-diff-sections-hook
                            #'magit-nix3-diff-section
                            'magit-insert-diff
                            'append)
    (magit-add-section-hook 'magit-revision-sections-hook
                            #'magit-nix3-diff-section
                            'magit-insert-revision-diff
                            'append))
   (t
    (remove-hook 'magit-diff-sections-hook #'magit-nix3-diff-section)
    (remove-hook 'magit-revision-sections-hook #'magit-nix3-diff-section))))

(provide 'magit-nix3-flake)
;;; magit-nix3-flake.el ends here
