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

(defgroup magit-nix3 nil
  "Nix flake integration for Magit."
  :prefix "magit-nix3-"
  :type 'magit-status
  :type 'nix3)

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

(provide 'magit-nix3-flake)
;;; magit-nix3-flake.el ends here
