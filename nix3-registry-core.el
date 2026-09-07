;;; nix3-registry-core.el --- Nix registry data and completion -*- lexical-binding: t -*-

;; Copyright (C) 2022,2023 Akira Komamura

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

;; This library provides access, completion, and mutation operations for Nix
;; flake entries from the global, system, and user registries.  Presentation
;; commands live in `nix3-registry-list'.

;;; Code:

(require 'nix3-core)
(require 'nix3-flake-context)
(require 'nix3-utils)
(require 'subr-x)

(declare-function xdg-config-home "xdg")

;; Silence the byte compiler
(defvar url-http-end-of-headers)

(defgroup nix3-registry nil
  "Support for flake registries."
  :group 'nix3)

(defun nix3-registry--global-url ()
  "Return the URL to the global registry."
  (nix3-config-lookup "flake-registry"))

(defcustom nix3-registry-system-file "/etc/nix/registry.json"
  "File containing the system-wide Nix registry."
  :type 'file)

(defcustom nix3-registry-user-file
  (expand-file-name "nix/registry.json"
                    ;; Nix seems to respect XDG_CONFIG_HOME environment variable
                    ;; even on Mac, so we will use the xdg library.
                    (if (require 'xdg nil t)
                        (xdg-config-home)
                      "~/.config/"))
  "File containing the current user's Nix registry."
  :type 'file)

(defface nix3-registry-type-face
  '((t :inherit font-lock-type-face))
  "Face used for Nix registry types."
  :group 'nix3-registry)

(defface nix3-registry-url-face
  '((t :inherit font-lock-comment-face))
  "Face used for Nix registry URLs."
  :group 'nix3-registry)

(defvar nix3-registry-global-cache nil)

(defvar nix3-registry-table nil
  "Hash table that stores registry entries during completion.")

(defun nix3-registry--parse-buffer ()
  "Parse registry JSON in the current buffer and return its entries."
  (goto-char (point-min))
  (thread-last
    (json-parse-buffer :object-type 'alist :array-type 'list)
    (assq 'flakes)
    (cdr)))

(defun nix3-registry--global-entries ()
  "Return entries from the global registry, using the cache when valid."
  (if (and nix3-registry-global-cache
           (< (car nix3-registry-global-cache)
              (+ (float-time) (nix3-config-lookup "tarball-ttl"))))
      (cdr nix3-registry-global-cache)
    (let ((buffer (url-retrieve-synchronously (nix3-registry--global-url) t t))
          (message-log-max nil))
      (message "Fetching the global registry...")
      (unwind-protect
          (with-current-buffer buffer
            (when url-http-end-of-headers
              (delete-region (point-min) url-http-end-of-headers))
            (message nil)
            (let ((value (nix3-registry--parse-buffer)))
              (setq nix3-registry-global-cache (cons (float-time) value))
              value))
        (kill-buffer buffer)))))

(defun nix3-registry--from-file (file)
  "Read registry entries from FILE, or return nil when unavailable."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (nix3-registry--parse-buffer))))

(cl-defun nix3-registry--collect-entries (&key no-exact
                                               (global t)
                                               (system t)
                                               (user t))
  "Return a hash table of registry entries.

If NO-EXACT is non-nil, exclude exact registry entries.  GLOBAL,
SYSTEM, and USER control whether to include entries from the
corresponding registries.
Each value in the resulting hash table will be a cons cell of the
registry type and the \"to\" value of the entry."
  (let ((table (make-hash-table :test #'equal)))
    (cl-flet
        ((add-entries (type entries)
           (dolist (entry entries)
             (let ((from (alist-get 'from entry))
                   (to (alist-get 'to entry))
                   (exact (alist-get 'exact entry)))
               (when (and (not (and no-exact exact))
                          (equal "indirect" (alist-get 'type from)))
                 (puthash (alist-get 'id from)
                          (cons type to)
                          table))))))
      (when global
        (add-entries 'global (nix3-registry--global-entries)))
      (when system
        (add-entries 'system (nix3-registry--from-file nix3-registry-system-file)))
      (when user
        (add-entries 'user (nix3-registry--from-file nix3-registry-user-file))))
    table))

(cl-defun nix3-registry-complete (prompt &key
                                         extra-entries
                                         (require-match t)
                                         add-to-registry
                                         no-exact
                                         (global t)
                                         (system t)
                                         (user t))
  "Read a registry entry with completion using PROMPT.

EXTRA-ENTRIES are additional candidates.  REQUIRE-MATCH, ADD-TO-REGISTRY,
NO-EXACT, GLOBAL, SYSTEM, and USER control completion and registry sources."
  (let* ((table (nix3-registry--collect-entries :no-exact no-exact
                                                :global global
                                                :system system
                                                :user user))
         (items (append extra-entries (map-keys table))))
    (setq nix3-registry-table table)
    (cl-labels
        ((annotator (id)
           (pcase-exhaustive (gethash id table)
             (`nil)
             (`(,_type . ,dest)
              (concat " "
                      (propertize (nix3-flake-ref-alist-to-url dest)
                                  'face 'nix3-registry-url-face)))))
         (group (id transform)
           (if transform
               id
             (if-let* ((cell (gethash id table)))
                 (format "%s registry"
                         (capitalize (symbol-name (car cell))))
               "")))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'nix3-registry-entry)
                           (cons 'group-function #'group)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action items string pred))))
      (let ((input (completing-read prompt #'completions)))
        (if-let* ((entry (gethash input table)))
            (cons input (cdr entry))
          (when (and add-to-registry
                     (nix3-registry--flake-url-p input)
                     (not require-match)
                     user
                     (yes-or-no-p "Add the flake to the user registry?"))
            (let* ((default-name (save-match-data
                                   (when (string-match (rx (+ (not (any "/"))) eol)
                                                       input)
                                     (match-string 0 input))))
                   (name (read-from-minibuffer "Name in the registry: "
                                               default-name)))
              (nix3-registry-add name input)))
          input)))))

(defun nix3-registry-completion-lookup (name)
  "Retrieve a registry entry by NAME during completion."
  (unless nix3-registry-table
    (error "Not initialized `nix3-registry-table'"))
  (cdr (gethash name nix3-registry-table)))

(defun nix3-registry--non-indirect (url-alist)
  "Resolve indirect references in URL-ALIST to a concrete flake URL."
  (let (hash)
    (cl-labels
        ((go (x)
           (let-alist x
             (if (equal "indirect" .type)
                 (progn
                   (unless hash
                     (setq hash (nix3-registry--collect-entries
                                 :global t :system t :user t)))
                   (go (cdr (or (gethash .id hash)
                                (error "Registry entry for %s is not found" .id)))))
               x))))
      (go url-alist))))

;;;###autoload
(defun nix3-registry-add (name flake)
  "Add NAME for FLAKE to the user registry."
  (interactive
   (let* ((url (read-from-minibuffer
                "Url: "
                ;; TODO: flake url at point
                (or (bound-and-true-p nix3-flake-url)
                    (nix3-registry--maybe-origin-flake-url))))
          (name (read-from-minibuffer
                 (format "Registry name for %s: " url)
                 (when (string-match (rx (+ (not (any "/"))) eol)
                                     url)
                   (match-string 0 url)))))
     (list name url)))
  (unless (nix3-registry--flake-url-p flake)
    (user-error "Invalid flake URL: %s" flake))
  (call-process nix3-nix-executable nil nil nil
                "registry" "add"
                name flake))

(defun nix3-registry--maybe-origin-flake-url ()
  "Return the current repository's origin as a flake URL, if available."
  (when-let* ((git-url (cdr (assoc "origin" (nix3-git-remotes)))))
    (nix3-flake-ref-alist-to-url
     (nix3-git-url-to-flake-alist git-url))))

(defun nix3-registry--flake-url-p (url)
  "Return non-nil when URL looks like a flake reference."
  (and (not (string-match-p "#" url))
       (string-match-p ":" url)))

(provide 'nix3-registry-core)
;;; nix3-registry-core.el ends here
