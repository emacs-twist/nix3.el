;;; nix3-transient.el --- Transient interface -*- lexical-binding: t -*-

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

;; This library provides transient.el commands for nix3.

;;; Code:

(require 'transient)
(require 'nix3-flake)
(require 'nix3-core)
(require 'nix3-utils)
(require 'nix3-browse-url)

(declare-function nix3-realise-and-show-store "nix3")
(declare-function nix3-vterm-shell-command "nix3-utils")
(declare-function term "term")
(declare-function nix3-flake-input-dispatch "nix3-flake-input")
(declare-function nix3-help-parse "nix3-help")
(declare-function nix3-help--read-command "nix3-help")
(defvar nix3-flake-input)

(defcustom nix3-terminal-function #'term
  "Function to run a shell command in a terminal.

This is a function that takes a command line as an argument."
  :group 'nix3
  :type '(choice (const :tag "Built-in term" term)
                 (const :tag "nix3-vterm-shell-command (requires vterm.el)"
                        nix3-vterm-shell-command)
                 (function :tag "Any function")))

(defvar-local nix3-transient-flake nil
  "Location of the flake.")

(defvar nix3-transient-flake-output nil)

(defvar nix3-transient-flake-output-type nil
  "Type of the attribute at point. Set temporarily.")

(defvar nix3-transient-nix-command nil)

;;;; Infixes and suffixes

;;;;; General classes

(defclass nix3-transient-string-variable (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj nix3-transient-string-variable))
  (oset obj value (eval (oref obj variable))))

(cl-defmethod transient-infix-read ((obj nix3-transient-string-variable))
  (read-from-minibuffer (oref obj prompt) (oref obj value)))

(cl-defmethod transient-infix-set ((obj nix3-transient-string-variable) value)
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj nix3-transient-string-variable))
  (let ((value (oref obj value)))
    (concat (propertize "(" 'face 'transient-inactive-value)
            (if value
                (propertize value 'face 'transient-value)
              "")
            (propertize ")" 'face 'transient-inactive-value))))

;;;;; System

(defvar nix3-transient-all-systems nil)

(defvar nix3-transient-system nil
  "Current target system.")

(defclass nix3-transient-system-variable (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj nix3-transient-system-variable))
  (oset obj value (eval (oref obj variable))))

(cl-defmethod transient-infix-set ((obj nix3-transient-system-variable) value)
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-infix-read ((obj nix3-transient-system-variable))
  (unless nix3-transient-all-systems
    (message "Reading the list of systems...")
    (setq nix3-transient-all-systems (nix3--all-systems)))
  (let ((value (oref obj value)))
    (unless value
      (completing-read (oref obj prompt) nix3-transient-all-systems
                       nil nil nil nil value))))

(cl-defmethod transient-format-value ((obj nix3-transient-system-variable))
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (if value
         (propertize value 'face 'transient-value)
       "")
     (propertize ")" 'face 'transient-inactive-value))))

(transient-define-infix nix3-transient-target-system ()
  :description "Target system"
  :prompt "Target system: "
  :class 'nix3-transient-system-variable
  :variable 'nix3-transient-system)

;;;;; Output

(defclass nix3-transient-output-variable (transient-variable)
  ((variable :initarg :variable)
   (required :initarg :required :initform t)))

(cl-defmethod transient-init-value ((obj nix3-transient-output-variable))
  (if-let (value (eval (oref obj variable)))
      (oset obj value value)
    (when (oref obj required)
      (let ((value (transient-infix-read obj)))
        (oset obj value value)
        (transient-infix-set obj value)))))

(cl-defmethod transient-infix-read ((obj nix3-transient-output-variable))
  (nix3-flake-select-output (oref obj prompt)
                            nix3-transient-nix-command
                            (oref obj value)
                            :system nix3-transient-system))

(cl-defmethod transient-infix-set ((obj nix3-transient-output-variable) value)
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj nix3-transient-output-variable))
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (if value
         (propertize value 'face 'transient-value)
       "")
     (propertize ")" 'face 'transient-inactive-value))))

(transient-define-infix nix3-transient-set-output ()
  :class 'nix3-transient-output-variable
  :description "Flake attribute"
  :prompt "Flake attribute: "
  :required t
  :variable 'nix3-transient-flake-output)

(transient-define-infix nix3-transient-set-optional-output ()
  :class 'nix3-transient-output-variable
  :description "Flake attribute"
  :prompt "Flake attribute: "
  :required nil
  :variable 'nix3-transient-flake-output)

;;;;; Directory

(defvar nix3-transient-directory nil)

(defun nix3-transient--default-directory ()
  "Return a directory in which Nix should be run."
  (if nix3-flake-url
      default-directory
    (thread-last
      (or nix3-transient-flake
          (error "Call this function after nix3-transient-flake is set"))
      (file-name-as-directory))))

(defclass nix3-transient-directory-variable (nix3-transient-string-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-infix-read ((obj nix3-transient-directory-variable))
  (read-directory-name (oref obj prompt)
                       (oref obj value)
                       nil
                       t))

(cl-defmethod transient-format-value ((obj nix3-transient-directory-variable))
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (propertize (abbreviate-file-name value) 'face 'transient-value)
     (propertize ")" 'face 'transient-inactive-value))))

(transient-define-infix nix3-transient-set-directory ()
  :class 'nix3-transient-directory-variable
  :description "Working directory"
  :prompt "Directory: "
  :variable 'nix3-transient-directory)

(defmacro nix3-transient-with-directory (&rest body)
  "Evaluate BODY with the default directory."
  `(let ((default-directory (or nix3-transient-directory
                                ;; The directory should be set, but just in case
                                (nix3-transient--default-directory))))
     ,@body))

;;;;; --update-input

(defvar nix3-transient-updated-inputs nil)

(defclass nix3-transient-direct-inputs (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj nix3-transient-direct-inputs))
  (oset obj value (eval (oref obj variable))))

(cl-defmethod transient-infix-read ((obj nix3-transient-direct-inputs))
  (completing-read-multiple (oref obj prompt)
                            (nix3-flake--direct-inputs)
                            nil nil
                            (string-join (oref obj value) nix3-crm-separator)))

(cl-defmethod transient-infix-set ((obj nix3-transient-direct-inputs) value)
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj nix3-transient-direct-inputs))
  (concat
   (propertize "(" 'face 'transient-inactive-value)
   (propertize (string-join (oref obj value) ",") 'face 'transient-value)
   (propertize ")" 'face 'transient-inactive-value)))

(transient-define-infix nix3-transient-set-updated-inputs ()
  :description "--update-input"
  :class 'nix3-transient-direct-inputs
  :prompt "Updated inputs: "
  :variable 'nix3-transient-updated-inputs)

;;;;; Flags

(defclass nix3-transient-multi-select (transient-variable)
  ((variable :initarg :variable)
   (make-table :initarg :make-table)))

(cl-defmethod transient-init-value ((obj nix3-transient-multi-select))
  (oset obj value (eval (oref obj variable))))

(cl-defmethod transient-infix-read ((obj nix3-transient-multi-select))
  (let ((table (oref obj make-table)))
    (completing-read-multiple (oref obj prompt)
                              (cl-etypecase table
                                (function (funcall table)))
                              nil nil
                              (string-join (oref obj value) ","))))

(cl-defmethod transient-infix-set ((obj nix3-transient-multi-select) value)
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj nix3-transient-multi-select))
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (propertize (string-join value ",") 'face 'transient-value)
     (propertize ")" 'face 'transient-inactive-value))))

(defvar nix3-transient-flags nil)

;; TODO: Add a completion table function for flags (with annotations and groups)

(transient-define-infix nix3-transient-set-flags ()
  :description "Other flags"
  :class 'nix3-transient-multi-select
  :variable 'nix3-transient-flags
  :prompt "Flags: "
  ;; TODO: Add more suggestions
  :make-table 'nix3-transient--complete-flags)

(defun nix3-transient--complete-flags ()
  (require 'nix3-help)
  (apply #'nix3-help-parse 'options
         (ensure-list nix3-transient-nix-command)))

;;;;; Command (nix run)

(defvar nix3-transient-command-args nil)

(transient-define-infix nix3-transient-set-command-args ()
  :class 'nix3-transient-string-variable
  :description "Arguments"
  :prompt "Arguments: "
  :variable 'nix3-transient-command-args)

;;;; Main command

;; This command must be invoked after fetching data, so it should be private.
(transient-define-prefix nix3-transient--dispatch ()
  [:description
   nix3-transient--flake-description
   :class transient-row
   ("h" "Show metadata and outputs" nix3-transient-show
    :if-not nix3-transient--show-mode-p)
   ("i" "Input" nix3-transient-input :transient t)
   ("-t" nix3-transient-target-system)]
  ["Nix commands"
   :class transient-row
   ("b" "build" nix3-transient-build
    :transient transient--do-stay)
   ("r" "run" nix3-transient-run
    :transient transient--do-stay)
   ("c" "flake check" nix3-transient-flake-check
    :transient transient--do-stay)
   ("l" "flake lock" nix3-transient-flake-lock
    :transient transient--do-stay)
   ("!" "Other commands" nix3-transient-generic-command
    :transient transient--do-stay)]
  (interactive)
  (unless nix3-transient-flake
    (user-error "Variable nix3-transient-flake must be set in advance"))
  (setq nix3-transient-flake-output nil)
  (transient-setup 'nix3-transient--dispatch))

;;;###autoload
(defun nix3-transient (&optional refresh)
  "Dispatch a transient interface to Nix commands.

With a universal prefix argument, nix flake show/metadata cache
will be refreshed."
  (interactive "P")
  (if (nix3-transient--show-mode-p)
      ;; If the current buffer is already showing the flake via
      ;; `nix3-flake-show-mode', there will be no need to refetch the data.
      (progn
        (setq nix3-transient-flake (nix3-flake--buffer-url))
        (call-interactively #'nix3-transient--dispatch))
    (if (setq nix3-transient-flake (nix3-flake-location :allow-missing t))
        (if (and (not refresh)
                 (nix3-flake-metadata--get nix3-transient-flake)
                 (nix3-flake-show--get nix3-transient-flake))
            (call-interactively #'nix3-transient--dispatch)
          (promise-chain (nix3-flake--get-promise nix3-transient-flake nix3-flake-url)
            (then (lambda (_)
                    (call-interactively #'nix3-transient--dispatch)))
            (promise-catch #'nix3-flake--handle-process-error)))
      (if (file-writable-p default-directory)
          (when (yes-or-no-p "There is no flake.nix. Initialize a flake? ")
            (funcall nix3-flake-init-function))
        (message "The directory is not writable")))))

(defun nix3-transient--show-mode-p ()
  (eq major-mode 'nix3-flake-show-mode))

(defun nix3-transient--flake-description ()
  (format "Flake: %s" nix3-transient-flake))

(defun nix3-transient-show ()
  (interactive)
  (nix3-flake-switch-to-buffer (nix3-flake-show-buffer nix3-transient-flake
                                                       nix3-flake-url)))

;;;; An alternative entry point on a flake output attribute

(transient-define-prefix nix3-transient-on-output ()
  [:description
   nix3-transient-output-description
   :class transient-row
   ("b" "build" nix3-transient-build
    :if nix3-transient--output-buildable-p)
   ("r" "run" nix3-transient-run
    :if nix3-transient--output-runnable-p)
   ("d" "View the template path" nix3-transient-browse-template
    :if nix3-transient--output-template-p)
   ("e" "Explore or eval" nix3-transient-explore-output)
   ("m" "Browse metadata of the derivation" nix3-transient-meta)]
  (interactive)
  (unless nix3-transient-flake-output
    (user-error "You need to set `nix3-transient-flake-output'"))
  (unless nix3-transient-flake-output-type
    (user-error "You need to set `nix3-transient-flake-output-type'"))
  (transient-setup 'nix3-transient-on-output))

(defun nix3-transient-output-description ()
  (format "%s (%s)" nix3-transient-flake-output nix3-transient-flake-output-type))

(defun nix3-transient--output-template-p ()
  (equal nix3-transient-flake-output-type "template"))

(defun nix3-transient-browse-template ()
  (interactive)
  (dired (nix3-flake-eval-json (concat (nix3-flake-output-path-at-point)
                                       ".path"))))

(defun nix3-transient--output-buildable-p ()
  (member nix3-transient-flake-output-type '("derivation")))

(defun nix3-transient--output-runnable-p ()
  (member nix3-transient-flake-output-type '("derivation"
                                             "app"
                                             "nixos-configuration")))

(defun nix3-transient-explore-output ()
  "Explore the attribute at point."
  (interactive)
  ;; For nix3-realise-and-show-store
  (require 'nix3)
  (cl-labels
      ((get-attr-names (path)
         (nix3-flake-eval-json path :apply "builtins.attrNames"))
       (get-type (path)
         (message "nix eval %s" (concat (nix3-flake--buffer-url)
                                        "#" path))
         (nix3-flake-eval-json path :apply "builtins.typeOf"))
       (is-path (path)
         (nix3-flake-eval-json path :apply "builtins.isPath"))
       (get-value (path)
         (nix3-flake-eval-json path))
       (make-candidate (parent attr)
         (concat (propertize (concat parent ".") 'invisible t)
                 (nix3-flake--escape-attr-name attr)))
       (print-value (path string)
         (if (or (string-match-p "\n" string)
                 (> (length string) (frame-width)))
             (with-electric-help `(lambda ()
                                    (insert ,string))
                                 (get-buffer-create
                                  (format "*nix eval<%s>*" path)))
           (message "%s: %s" path string)))
       (go (path)
         (if-let (names (get-attr-names path))
             (let ((new-path (completing-read (format "Attribute (%s): " path)
                                              ;; Build candidates that are full
                                              ;; attribute paths. This will make
                                              ;; the completion more useful for
                                              ;; integration with packages like
                                              ;; embark.
                                              (mapcar (apply-partially #'make-candidate path)
                                                      names)
                                              nil t)))
               (pcase (get-type new-path)
                 ("set" (go new-path))
                 ("string" (let ((value (get-value new-path)))
                             (if (and (string-prefix-p "/nix/store/" value)
                                      (require 'nix-store nil t)
                                      (yes-or-no-p "Looks like a store path. Realise it?"))
                                 (nix3-realise-and-show-store value)
                               (print-value new-path (get-value new-path)))))
                 (_ (print-value new-path
                                 (string-chop-newline (nix3-flake-eval-nix new-path))))))
           (message "%s is an empty attribute set" path))))
    (go nix3-transient-flake-output)))

(defvar nix3-transient-meta nil)

(transient-define-prefix nix3-transient-meta ()
  [:description
   nix3-transient-output-description
   ("h" nix3-transient-package-homepage)
   ("l" nix3-transient-package-license)
   ("m" nix3-transient-package-maintainers)
   ("p" nix3-transient-package-position)
   ;; TODO: Display details in a help buffer
   ;; ("d" "Details")
   ]
  (interactive)
  (setq nix3-transient-meta (nix3-flake-eval-json
                             (concat nix3-transient-flake-output ".meta")))
  (transient-setup 'nix3-transient-meta))

(defmacro nix3-transient--package-homepage ()
  '(cdr (assq 'homepage nix3-transient-meta)))

(transient-define-suffix nix3-transient-package-homepage ()
  :if (lambda () (nix3-transient--package-homepage))
  :description (lambda () (format "Homepage: %s" (nix3-transient--package-homepage)))
  (interactive)
  (funcall nix3-browse-url-for-repository (nix3-transient--package-homepage)))

(defmacro nix3-transient--package-license ()
  '(cdr (assq 'license nix3-transient-meta)))

(transient-define-suffix nix3-transient-package-license ()
  :if (lambda () (nix3-transient--package-license))
  :description (lambda ()
                 (let ((val (nix3-transient--package-license)))
                   (concat "Browse license details "
                           (propertize "(" 'face 'transient-inactive-value)
                           (if (assq 'fullName val)
                               (cdr (assq 'shortName val))
                             ;; Multiple licenses
                             (mapconcat (lambda (a)
                                          (alist-get 'shortName a))
                                        val
                                        ","))
                           (propertize ")" 'face 'transient-inactive-value))))
  (interactive)
  (if-let (licenses (nix3-transient--package-license))
      (nix3-transient--browse-license-url
       (if (assq 'fullName licenses)
           licenses
         ;; Multiple licenses
         (nix3-transient--complete-license
          (format "License(s) of %s: "
                  (cdr (assq 'name nix3-transient-meta)))
          licenses)))
    (user-error "No license")))

(defun nix3-transient--browse-license-url (license)
  (if-let (url (alist-get 'url license))
      (browse-url url)
    (user-error "License %s has no URL"
                (or (alist-get 'spdxId license)
                    (alist-get 'fullName license)))))

(defun nix3-transient--complete-license (prompt licenses)
  (let ((alist (mapcar (lambda (a)
                         (cons (alist-get 'fullName a)
                               a))
                       licenses)))
    (cl-labels
        ((annotator (candidate)
           (when-let (license (cdr (assoc candidate alist)))
             (format " (%s)"
                     (mapconcat (lambda (x)
                                  (symbol-name (car x)))
                                (seq-filter (lambda (x) (eq t (cdr x)))
                                            license)
                                ", "))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'nix3-license-name)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action alist string pred))))
      (completing-read prompt #'completions nil t))))

(defmacro nix3-transient--package-maintainers ()
  '(cdr (assq 'maintainers nix3-transient-meta)))

(transient-define-suffix nix3-transient-package-maintainers ()
  :if (lambda () (nix3-transient--package-maintainers))
  :description "Display maintainers"
  (interactive)
  (let* ((alist (mapcar (lambda (x)
                          (cons (alist-get 'name x)
                                x))
                        (nix3-transient--package-maintainers)))
         (name (completing-read "Maintainer: " alist nil t)))
    ;; TODO: Display in help buffer
    (pp-display-expression (cdr (assoc name alist))
                           "*maintainer*")))

(defmacro nix3-transient--package-position ()
  '(cdr (assq 'position nix3-transient-meta)))

(transient-define-suffix nix3-transient-package-position ()
  :if (lambda () (nix3-transient--package-position))
  :description "Find the position"
  (interactive)
  (let ((spec (nix3-transient--package-position)))
    (if (string-match (rx bos (group (+ anything)) ":" (group (+ digit)) eos) spec)
        (let ((file (match-string 1 spec))
              (line (string-to-number (match-string 2 spec))))
          ;; TODO: Make this function customizable
          (find-file-read-only-other-window file)
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line line)))
      (error "Failed to match against source position %s" spec))))

;;;; Generate items programmatically

(eval-and-compile
  (defvar nix3-transient-common-options
    ["Common options"
     :class transient-row
     ("-q" "" "--no-build-output")
     ("-L" "" ("-L" "--print-build-logs"))]))

;;;; Nix commmands

(transient-define-prefix nix3-transient-build ()
  ["nix build"
   ("#" nix3-transient-set-output)
   ("--" nix3-transient-set-flags)]
  nix3-transient-common-options
  ["Suffixes"
   ("RET" "Build in compile" nix3-transient--build-compile)]
  (interactive)
  (setq nix3-transient-nix-command "build")
  (setq nix3-transient-directory (nix3-transient--default-directory))
  (transient-setup 'nix3-transient-build))

(defun nix3-transient--build-compile ()
  (interactive)
  (nix3-transient-with-directory
   (compile (nix3-transient--shell-command
             nix3-transient-flake-output
             (transient-args 'nix3-transient-build)))))

(transient-define-prefix nix3-transient-run ()
  ["nix run"
   ("#" nix3-transient-set-output)
   ("--" nix3-transient-set-flags)
   ("-c" nix3-transient-set-command-args)
   ("-d" nix3-transient-set-directory)]
  nix3-transient-common-options
  ["Suffixes"
   :class transient-row
   ("RET" "Run in compile" nix3-transient--run-compile)
   ("t" "Run in terminal" nix3-transient--run-term)
   ("&" "Run async" nix3-transient--run-async)]
  (interactive)
  (setq nix3-transient-nix-command "run")
  (setq nix3-transient-directory (nix3-transient--default-directory))
  (transient-setup 'nix3-transient-run))

(defun nix3-transient--run-compile ()
  (interactive)
  (nix3-transient-with-directory
   (compile (concat (nix3-transient--shell-command
                     nix3-transient-flake-output
                     (transient-args 'nix3-transient-run))
                    (if nix3-transient-command-args
                        (concat " -- " nix3-transient-command-args)
                      "")))))

(defun nix3-transient--run-term ()
  (interactive)
  (nix3-transient-with-directory
   (funcall nix3-terminal-function
            (concat (nix3-transient--shell-command
                     nix3-transient-flake-output
                     (transient-args 'nix3-transient-run))
                    (if nix3-transient-command-args
                        (concat " -- " nix3-transient-command-args)
                      "")))))

(defun nix3-transient--run-async ()
  (interactive)
  (nix3-transient-with-directory
   (shell-command (concat (nix3-transient--shell-command
                           nix3-transient-flake-output
                           (transient-args 'nix3-transient-run))
                          (if nix3-transient-command-args
                              (concat " -- " nix3-transient-command-args)
                            "")
                          "&"))))

(transient-define-prefix nix3-transient-flake-check ()
  ["nix flake check"
   ("--" nix3-transient-set-flags)]
  nix3-transient-common-options
  ["Suffixes"
   ("c" "Run in compile" nix3-transient--flake-check-compile)]
  (interactive)
  (setq nix3-transient-nix-command '("flake" "check"))
  (setq nix3-transient-directory (nix3-transient--default-directory))
  (transient-setup 'nix3-transient-flake-check))

(defun nix3-transient--flake-check-compile ()
  (interactive)
  (nix3-transient-with-directory
   (compile (nix3-transient--shell-command
             nil
             (transient-args 'nix3-transient-flake-check)))))

(transient-define-prefix nix3-transient-flake-lock ()
  ["nix flake lock"
   ("--" nix3-transient-set-flags)
   ("-u" nix3-transient-set-updated-inputs)]
  nix3-transient-common-options
  ["Suffixes"
   ("RET" "Run in compile" nix3-transient--flake-lock-compile)]
  (interactive)
  (setq nix3-transient-nix-command '("flake" "lock"))
  (setq nix3-transient-directory (nix3-transient--default-directory))
  (transient-setup 'nix3-transient-flake-lock))

(defun nix3-transient--flake-lock-compile ()
  (interactive)
  (nix3-transient-with-directory
   (compile (nix3-transient--shell-command
             nil
             (append (transient-args 'nix3-transient-flake-lock)
                     (mapcar (lambda (input)
                               (list "--update-input" input))
                             nix3-transient-updated-inputs))))))

(defun nix3-transient-input ()
  (interactive)
  (require 'nix3-flake-input)
  (setq nix3-transient-directory (nix3-transient--default-directory))
  (nix3-transient-with-directory
   (let* ((alist (nix3-flake--direct-inputs))
          (input (completing-read "Select input: " alist)))
     (setq nix3-flake-input (cons input (cdr (assq (intern input) alist))))
     (nix3-flake-input-dispatch))))

(transient-define-prefix nix3-transient-generic-command ()
  [:description
   nix3-transient--command-description
   ("#" nix3-transient-set-optional-output)
   ("--" nix3-transient-set-flags)]
  nix3-transient-common-options
  ["Suffixes"
   ("RET" "Build in compile" nix3-transient--generic-compile)]
  (interactive)
  (require 'nix3-help)
  (setq nix3-transient-nix-command (cdr (split-string (nix3-help--read-command
                                                       "Nix command: ")
                                                      " ")))
  (setq nix3-transient-directory (nix3-transient--default-directory))
  (transient-setup 'nix3-transient-generic-command))

(defun nix3-transient--generic-compile ()
  (interactive)
  (nix3-transient-with-directory
   (compile (nix3-transient--shell-command
             nix3-transient-flake-output))))

(defun nix3-transient--command-description ()
  (string-join (cons "nix" nix3-transient-nix-command) " "))

;;;; Utilities

(defun nix3-transient--shell-command (attr-or-nil &rest args)
  (let ((nix-command nix3-transient-nix-command)
        (args (append nix3-transient-flags args)))
    (concat (shell-quote-argument nix3-nix-executable)
            " "
            (if (listp nix-command)
                (string-join nix-command " ")
              nix-command)
            " "
            (if attr-or-nil
                (format "%s#%s" (nix3-flake-location) attr-or-nil)
              (nix3-flake-location))
            (if args
                (concat " " (mapconcat #'shell-quote-argument (flatten-list args) " "))
              ""))))

(provide 'nix3-transient)
;;; nix3-transient.el ends here
