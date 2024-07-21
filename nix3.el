;;; nix3.el --- Frontend to experimental commands of Nix -*- lexical-binding: t -*-

;; Copyright (C) 2022,2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (promise "1.1") (magit-section "3.3") (s "1.12"))
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

;;; Code:

(require 'nix3-core)
(require 'promise)
(require 'nix3-flake)
(require 'nix3-transient)

(defvar nix3-transient-flake)
(defvar nix3-transient-flake-output)

(declare-function nix-store-show-path "ext:nix-store")
(declare-function nix-store-realise "ext:nix-store")
(declare-function compilation-read-command "compile")
(declare-function nix3-transient-run "nix3-transient")
(declare-function nix3-transient-build "nix3-transient")
(declare-function nix3-flake-select-output "nix3-flake")

(defvar nix3-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'nix3-flake-show)
    (define-key map "b" #'nix3-build)
    (define-key map "r" #'nix3-run)
    map)
  "Prefix map for Nix commands.

This is EXPERIMENTAL.")

;;;###autoload
(defun nix3-build (output)
  "Build a derivation in the current flake."
  (interactive (list (nix3-flake-select-output
                      (format "nix build (%s): " (nix3-flake-location))
                      "build")))
  (setq nix3-transient-flake (nix3-flake-location))
  (setq nix3-transient-flake-output output)
  (call-interactively #'nix3-transient-build))

;;;###autoload
(defun nix3-run (output)
  "Run an app in the current flake."
  (interactive (list (nix3-flake-select-output
                      (format "nix run (%s): " (nix3-flake-location))
                      "run")))
  (setq nix3-transient-flake (nix3-flake-location))
  (setq nix3-transient-flake-output output)
  (call-interactively #'nix3-transient-run))

(defun nix3-realise-and-show-store (path)
  "Show PATH using nix-store.el. Realise it if necessary."
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

(provide 'nix3)
;;; nix3.el ends here
