;;; magit-nix3-flake.el ---  -*- lexical-binding: t -*-

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

;; 

;;; Code:

(require 'nix3)
(require 'nix3-flake)
(require 'promise)

(defcustom magit-nix3-sections
  '(nix3-flake-insert-outputs)
  "Sections inserted into the magit buffer."
  :type 'hook)

(defcustom magit-nix3-insert-positions 'magit-insert-status-headers
  "Position at which nix flake sections are inserted.

This should be a value which is already included in
`magit-status-sections-hook'."
  :type 'function)

(defcustom magit-nix3-append t
  "Whether to insert the section after the position."
  :type 'boolean)

(defun magit-nix3-flake-sections ()
  (when (file-exists-p "flake.nix")
    (let ((truename (nix3-normalize-path default-directory)))
      (promise-wait 0.5
        (promise-new (apply-partially #'nix3-flake--make-show-process
                                      truename nil)))
      (run-hooks 'magit-nix3-sections))))

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
