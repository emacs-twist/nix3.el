;;; nix3-registry-list.el --- Browse Nix registry entries -*- lexical-binding: t -*-

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

;; This library connects registry completion to a configurable presentation
;; action.  It is deliberately separate from `nix3-registry-core' so the data
;; layer does not depend on its higher-level consumer.

;;; Code:

(require 'nix3-registry-core)

(defcustom nix3-registry-list-action 'nix3-flake-show-url
  "Default action to run in `nix3-registry-list' command.

This should be a function that takes a name in a registry as an argument."
  :type 'function
  :group 'nix3-registry)

(defcustom nix3-registry-list-completion-options
  (list :add-to-registry t
        :require-match nil
        :no-exact t)
  "Plist of options for completion in `nix3-registry-list' command.

These options are passed as arguments to `nix3-registry-complete'."
  :type 'plist
  :group 'nix3-registry)

;;;###autoload
(defun nix3-registry-list ()
  "Display a list of entries in the flake registries."
  (interactive)
  (let ((name (apply #'nix3-registry-complete "Flake: "
                     nix3-registry-list-completion-options)))
    (message "Selected a registry entry %s" name)
    (funcall nix3-registry-list-action name)))

(provide 'nix3-registry-list)
;;; nix3-registry-list.el ends here
