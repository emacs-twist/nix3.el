;;; magit-nix26-flake.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (nix26 "0.1") (promise "1.1"))
;; Keywords: processes
;; URL: https://github.com/emacs-twist/nix26.el

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

(require 'nix26)
(require 'nix26-flake)
(require 'promise)

(defun magit-nix26-flake-output-section ()
  (when (file-exists-p "flake.nix")
    (let ((truename (nix26-normalize-path default-directory)))
      (promise-wait 0.5
        (promise-new (apply-partially #'nix26-flake--make-show-process
                                      truename nil)))
      (nix26-flake-insert-outputs))))

;;;###autoload
(define-minor-mode magit-nix26-flake-mode
  "Turn on flake features in `magit-status-mode' buffers."
  :global t
  (cond
   (magit-nix26-flake-mode
    (magit-add-section-hook 'magit-status-sections-hook #'magit-nix26-flake-output-section
                            'magit-insert-status-headers
                            'append))
   (t
    (remove-hook 'magit-status-sections-hook #'magit-nix26-flake-output-section))))

(provide 'magit-nix26-flake)
;;; magit-nix26-flake.el ends here
