;;; nix3-flake-ref.el --- Browsable Nix flake references -*- lexical-binding: t -*-

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

;; This library translates resolved or registry-backed flake references into
;; repository URLs suitable for a web browser.

;;; Code:

(require 'cl-lib)
(require 'nix3-registry-core)
(require 'subr-x)

(defun nix3-flake-html-url (alist)
  "Return a browsable HTTPS URL for the flake reference ALIST."
  (cl-labels
      ((check-url (url)
         (if (string-prefix-p "https://" url)
             url
           (error "Not an https url, so cannot retrieve an HTML url: %s" url)))
       (to-url (alist)
         (let-alist alist
           (pcase \.type
             ("indirect"
              (if-let* ((entry (thread-last
                               (nix3-registry--collect-entries)
                               (cl-remove-if
                                (lambda (x)
                                  (equal (cdr (assq 'type (cddr x))) "path")))
                               (assoc \.id))))
                  (to-url (cddr entry))
                (error "Failed to find a registry entry for %s" \.id)))
             ("github"
              (format "https://github.com/%s/%s/%s" \.owner \.repo
                      (if \.ref (concat "tree/" \.ref) "")))
             ("gitlab"
              (format "https://gitlab.com/%s/%s/%s" \.owner \.repo
                      (if \.ref (concat "tree/" \.ref) "")))
             ("sourcehut"
              (format "https://git.sr.ht/%s/%s/%s" \.owner \.repo
                      (if \.ref (concat "log/" \.ref) "")))
             ("url"
              (check-url (thread-last
                           \.url
                           (string-remove-prefix "git+")
                           (string-remove-suffix ".git"))))
             ("git" (check-url \.url))
             ("path" (error "Path entry, so cannot be accessed using URL"))
             (_ (error "Unsupported scheme for HTML URL: %s" \.type))))))
    (to-url alist)))

(provide 'nix3-flake-ref)
;;; nix3-flake-ref.el ends here
