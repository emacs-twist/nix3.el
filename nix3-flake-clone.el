;;; nix3-flake-clone.el --- Cloning Git repositories -*- lexical-binding: t -*-

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

;; This library asynchronously clones Git repositories referenced by remote
;; flake URLs into local directories.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'project)
(require 'nix3-utils)
(require 'nix3-registry-core)
(require 'async)

(defgroup nix3-flake-clone
  nil
  "Clone Git repositories from flake URLs."
  :group 'git
  :group 'nix3)

(defcustom nix3-flake-clone-root nil
  "Root directory of the repository hierarchy."
  :type 'directory)

(defcustom nix3-flake-clone-find-function
  #'dired
  "Function used to open a directory."
  :type 'function)

;;;###autoload
(defun nix3-flake-clone-worktree (url-alist success &optional error)
  "Clone URL-ALIST into a local Git worktree asynchronously.

Call SUCCESS with the destination directory when the worktree becomes
available.  Call ERROR with an error description if cloning fails."
  (let* ((origin (nix3-build-git-clone-url
                  (nix3-registry--non-indirect url-alist)))
         (dir (nix3-flake-clone--default-dest origin)))
    (if (file-directory-p dir)
        (progn
          (funcall success dir)
          dir)
      (let ((parent (file-name-directory dir)))
        (unless (file-directory-p parent)
          (make-directory parent t)))
      (nix3-flake-clone-async origin dir success error))))

(defun nix3-flake-clone--default-dest (url-or-alist)
  "Return the local destination for URL-OR-ALIST."
  (if (stringp url-or-alist)
      (if (string-match (rx bol "https://") url-or-alist)
          (thread-last
            (substring url-or-alist (match-end 0))
            (string-remove-suffix "/")
            (string-remove-suffix ".git")
            (concat (file-name-as-directory
                     (or nix3-flake-clone-root
                         (error "You must set `nix3-flake-clone-root'")))))
        (error "Non-https url is not supported at present"))
    (nix3-flake-clone--default-dest
     (nix3-build-git-clone-url
      (nix3-registry--non-indirect url-or-alist)))))

(cl-defun nix3-flake-clone-async (origin dest success &optional error)
  "Clone the Git repository at ORIGIN into DEST asynchronously.

Call SUCCESS with the destination when cloning succeeds.  Call ERROR with
an error description when cloning fails."
  (message "Cloning %s to %s..." origin dest)
  (async-start
   `(lambda ()
      (condition-case err
          (let* ((stderr-file (make-temp-file "nix3-git-clone"))
                 (output
                  (with-temp-buffer
                    (let ((command ',(list nix3-git-executable
                                           "clone"
                                           "--filter=blob:none"
                                           origin
                                           (expand-file-name dest))))
                      (cons (apply #'call-process
                                   (car command) nil
                                   (list (current-buffer) stderr-file)
                                   nil (cdr command))
                            (buffer-string)))))
                 (code (car output))
                 (stderr (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (buffer-string))))
            (delete-file stderr-file)
            (if (zerop code)
                (list :success ,dest)
              (list :error stderr code)))
        (error (list :error (error-message-string err) -1))))
   (lambda (result)
     (pcase result
       (`(:success ,dir)
        (funcall (or success nix3-flake-clone-find-function) dir))
       (`(:error ,message ,_code)
        (if error
            (funcall error (list :error message))
          (message "Returned non-zero from git-clone: %s"
                   (string-trim message))))))))

(provide 'nix3-flake-clone)
;;; nix3-flake-clone.el ends here
