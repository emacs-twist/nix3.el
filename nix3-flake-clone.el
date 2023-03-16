;;; nix3-flake-clone.el --- Cloning Git repositories -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'rx)
(require 'project)
(require 'nix3-utils)

(declare-function nix3-registry--non-indirect "nix3-registry")
(declare-function promise-new "ext:promise-core")

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
(defun nix3-flake-clone-promise (url-alist)
  "Return a promise that resolves to a local Git working directory."
  (require 'nix3-registry)
  (promise-new
   `(lambda (resolve _)
      (let* ((url-alist ',url-alist)
             (origin (nix3-build-git-clone-url
                      (nix3-registry--non-indirect
                       url-alist)))
             (dir (nix3-flake-clone--default-dest origin)))
        (if (file-directory-p dir)
            (funcall resolve dir)
          (let ((parent (file-name-directory dir)))
            (unless (file-directory-p parent)
              (make-directory parent t)))
          (nix3-flake-clone-async origin dir :callback resolve))))))

(defun nix3-flake-clone--default-dest (url-or-alist)
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
    (require 'nix3-registry)
    (nix3-flake-clone--default-dest
     (nix3-build-git-clone-url
      (nix3-registry--non-indirect url-or-alist)))))

(cl-defun nix3-flake-clone-async (origin dest &key callback)
  "Clone a Git repository."
  (let ((parent (file-name-directory (string-remove-suffix "/" dest))))
    (unless (file-directory-p parent)
      (make-directory parent t)))
  (message "Cloning %s to %s..." origin dest)
  (make-process :name "nix flake clone"
                :buffer "*Nix3-Flake-Clone*"
                :command (list nix3-git-executable
                               "clone"
                               "--filter=blob:none"
                               origin
                               (expand-file-name dest))
                :sentinel
                `(lambda (process _event)
                   (when (eq 'exit (process-status process))
                     (if (= 0 (process-exit-status process))
                         (funcall ,(or callback
                                       nix3-flake-clone-find-function)
                                  ,dest)
                       (message "Returned non-zero from git-clone"))))))

(provide 'nix3-flake-clone)
;;; nix3-flake-clone.el ends here
