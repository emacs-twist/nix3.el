;;; nix3-flake-data.el --- Cached Nix flake data -*- lexical-binding: t -*-

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

;; This library owns cached `nix flake show' and `nix flake metadata' results.
;; It sits below both the flake display and input interfaces so those libraries
;; can share data without depending on each other.

;;; Code:

(require 'nix3-flake-context)
(require 'subr-x)

(defvar nix3-flake-show-results nil
  "Cache of `nix flake show' results keyed by flake location.")

(defun nix3-flake--ensure-show-cache ()
  "Initialize the flake-show result cache when necessary."
  (unless nix3-flake-show-results
    (setq nix3-flake-show-results (make-hash-table :test #'equal))))

(defun nix3-flake-show--put (directory-or-url result)
  "Cache RESULT for DIRECTORY-OR-URL as flake-show data."
  (nix3-flake--ensure-show-cache)
  (puthash directory-or-url result nix3-flake-show-results))

(defun nix3-flake-show--get (directory)
  "Return cached flake-show data for DIRECTORY, if available."
  (nix3-flake--ensure-show-cache)
  (gethash (string-remove-suffix "/" directory)
           nix3-flake-show-results))

(defvar nix3-flake-metadata-results nil
  "Cache of `nix flake metadata' results keyed by flake location.")

(defun nix3-flake--ensure-metadata-cache ()
  "Initialize the flake-metadata result cache when necessary."
  (unless nix3-flake-metadata-results
    (setq nix3-flake-metadata-results (make-hash-table :test #'equal))))

(defun nix3-flake-metadata--put (directory-or-url result)
  "Cache RESULT for DIRECTORY-OR-URL as flake metadata."
  (nix3-flake--ensure-metadata-cache)
  (puthash directory-or-url result nix3-flake-metadata-results))

(defun nix3-flake-metadata--get (directory)
  "Return cached flake metadata for DIRECTORY, if available."
  (nix3-flake--ensure-metadata-cache)
  (gethash (string-remove-suffix "/" directory)
           nix3-flake-metadata-results))

(defun nix3-flake--get-show-result ()
  "Return the cached show result for the current flake."
  (nix3-flake-show--get (nix3-flake-location)))

(defun nix3-flake--get-metadata-result ()
  "Return the cached metadata result for the current flake."
  (nix3-flake-metadata--get (nix3-flake-location)))

(provide 'nix3-flake-data)
;;; nix3-flake-data.el ends here
