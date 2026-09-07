;;; nix3-flake-context.el --- Current Nix flake context -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Akira Komamura

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

;; This library owns the small amount of dynamic state that identifies the
;; current flake.  Keeping it below registry, display, input, and transient
;; libraries lets all of them share the context without depending on each
;; other.

;;; Code:

(require 'cl-lib)
(require 'nix3-core)
(eval-when-compile
  (require 'rx))

(defvar nix3-flake-url nil
  "URL of the current flake, or nil when the current flake is local.")

(cl-defun nix3-flake-location (&key allow-missing local dir)
  "Return the URL or path to the current flake.

If the current buffer is not on a remote flake and ALLOW-MISSING
is nil, signal an error if there is no flake.nix.

If LOCAL is non-nil, only return a flake on the file system.

If DIR is non-nil, return a flake at or above that directory.  DIR
implies LOCAL."
  (or (unless (or local dir)
        nix3-flake-url)
      (if-let* ((root (locate-dominating-file (or dir default-directory)
                                               "flake.nix")))
          (nix3-normalize-path root)
        (unless allow-missing
          (error "No flake.nix is found")))))

(defun nix3-flake--resolve-path (path)
  "Resolve relative flake input PATH against the current flake."
  (if (string-prefix-p "/" path)
      path
    (save-match-data
      (if (string-match (rx bol "./" (group (+ anything))) path)
          (let ((relative (match-string 1 path)))
            (cond
             ((and nix3-flake-url
                   (string-match-p (rx (any "=&") "dir=") nix3-flake-url))
              (error "The parent flake \"%s\" already has dir parameter, so it cannot be resolved"
                     nix3-flake-url))
             (nix3-flake-url
              (concat nix3-flake-url
                      (if (string-match-p (rx "?") nix3-flake-url)
                          "&"
                        "?")
                      "dir=" relative))
             (t
              (nix3-normalize-path
               (expand-file-name relative default-directory)))))
        (error "Failed to match against a path \"%s\"" path)))))

(provide 'nix3-flake-context)
;;; nix3-flake-context.el ends here
