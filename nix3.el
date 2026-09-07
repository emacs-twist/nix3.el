;;; nix3.el --- Frontend to experimental commands of Nix -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Assisted-by: Codex:gpt-5.6-sol
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (promise "1.1") (compat "29.1") (magit-section "4.5") (s "1.12") (transient "0.13"))
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

;; This library provides interactive Emacs commands for working with Nix
;; flakes.  It offers commands to inspect flake outputs, build derivations,
;; and run flake applications through a transient interface.

;;; Code:

(require 'nix3-core)
(require 'nix3-utils)
(require 'promise)
(require 'nix3-flake)
(require 'nix3-transient)
(require 'nix3-registry)

(defvar nix3-transient-flake)
(defvar nix3-transient-flake-output)

(declare-function compilation-read-command "compile")

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
  "Build an OUTPUT in the current flake."
  (interactive (list (nix3-flake-select-output
                      (format "nix build (%s): " (nix3-flake-location))
                      "build")))
  (setq nix3-transient-flake (nix3-flake-location))
  (setq nix3-transient-flake-output output)
  (call-interactively #'nix3-transient-build))

;;;###autoload
(defun nix3-run (output)
  "Run an app OUTPUT in the current flake."
  (interactive (list (nix3-flake-select-output
                      (format "nix run (%s): " (nix3-flake-location))
                      "run")))
  (setq nix3-transient-flake (nix3-flake-location))
  (setq nix3-transient-flake-output output)
  (call-interactively #'nix3-transient-run))

(provide 'nix3)
;;; nix3.el ends here
