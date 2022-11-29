;;; nix3.el --- Frontend to experimental commands of Nix -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (promise "1.1") (project "0.6")
;;                    (magit-section "3.3") (s "1.12"))
;; Keywords: processes
;; URL: https://github.com/emacs-twist/nix3.el

;; This file is not part of GNU Emacs.

;;; License:

;;; Commentary:

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

;;; Code:

(require 'nix3-core)
(require 'promise)
(require 'nix3-flake)

(defvar nix3-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'nix3-flake-show)
    (define-key map "b" #'nix3-build)
    (define-key map "r" #'nix3-run)
    map)
  "Prefix map for Nix commands.

This is EXPERIMENTAL.")

;;;###autoload
(defun nix3-build ()
  "Build an output in the flake."
  (interactive)
  (promise-chain (nix3--demand-outputs)
    (then (lambda (_)
            (nix3--compile-output "nix build (%s): " "build")))))

;;;###autoload
(defun nix3-run ()
  "Run an app in the flake."
  (interactive)
  (promise-chain (nix3--demand-outputs)
    (then (lambda (_)
            (nix3--compile-output "nix run (%s): " "run")))))

(defun nix3--compile-output (prompt nix-command)
  (let* ((default-directory (nix3-flake--buffer-url))
         (attr (nix3--select-output-attribute
                (format prompt (abbreviate-file-name default-directory))
                nix-command)))
    (compile (mapconcat #'shell-quote-argument
                        `(,nix3-nix-executable
                          ,@(if (listp nix-command)
                                nix-command
                              (list nix-command))
                          ,(concat ".#" attr))
                        " "))))

(defun nix3--demand-outputs ()
  (promise-new (apply-partially
                #'nix3-flake--make-show-process
                (string-remove-suffix
                 "/"
                 (file-truename (locate-dominating-file default-directory "flake.nix")))
                nil)))

(defun nix3--select-output-attribute (prompt command)
  (let ((alist (nix3-flake--filter-outputs command)))
    (cl-labels
        ((group (candidate transform)
           (if transform
               candidate
             (or (cdr (assoc candidate alist))
                 "")))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'nix3-attribute)
                           (cons 'group-function #'group)))
             (complete-with-action action alist string pred))))
      (completing-read prompt #'completions))))

(provide 'nix3)
;;; nix3.el ends here
