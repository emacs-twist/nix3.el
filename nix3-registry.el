;;; nix3-registry.el --- A wrapper for Nix registries -*- lexical-binding: t -*-

(require 'nix3-core)
(require 'nix3-utils)
(require 'subr-x)

(declare-function xdg-config-home "xdg")

;; Silent byte-compile
(defvar url-http-end-of-headers)

(defgroup nix3-registry nil
  "Support for flake registries."
  :group 'nix3)

(defun nix3-registry--global-url ()
  "Return the URL to the global registry."
  (nix3-config-lookup "flake-registry"))

(defcustom nix3-registry-system-file "/etc/nix/registry.json"
  ""
  :type 'file)

(defcustom nix3-registry-user-file
  (cl-case system-type
    (gnu/linux
     (progn
       (require 'xdg)
       (expand-file-name "nix/registry.json" (xdg-config-home))))
    (otherwise
     (error "Not defined for %s" system-type)))
  ""
  :type 'file)

(defcustom nix3-registry-list-action #'nix3-flake-show-url
  "Default action to run in `nix3-registry-list' command.

This should be a function that takes a name in a registry as an argument."
  :type 'function)

(defface nix3-registry-type-face
  '((t :inherit font-lock-type-face))
  ""
  :group 'nix3-registry)

(defface nix3-registry-url-face
  '((t :inherit font-lock-comment-face))
  ""
  :group 'nix3-registry)

(defvar nix3-registry-global-cache nil)

(defvar nix3-registry-table nil
  "Hash table that stores registry entries during completion.")

(defun nix3-registry--parse-buffer ()
  (goto-char (point-min))
  (thread-last
    (json-parse-buffer :object-type 'alist :array-type 'list)
    (assq 'flakes)
    (cdr)))

(defun nix3-registry--global-entries ()
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
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (nix3-registry--parse-buffer))))

(cl-defun nix3-registry--collect-entries (&key no-exact
                                               (global t)
                                               (system t)
                                               (user t))
  "Return a hash table of registry entries.

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
             (`(,type . ,dest)
              (concat " "
                      (propertize (nix3-flake-ref-alist-to-url dest)
                                  'face 'nix3-registry-url-face)))))
         (group (id transform)
           (if transform
               id
             (if-let (cell (gethash id table))
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
        (if-let (entry (gethash input table))
            (cons input (cddr entry))
          (when (and add-to-registry
                     (nix3-registry--flake-url-p input)
                     (not require-match)
                     user
                     (yes-or-no-p "Add the flake to the user registry?"))
            (let ((name (read-string (format "Name for %s: " input))))
              (nix3-registry-add name input)))
          input)))))

(defun nix3-registry-completion-lookup (name)
  "Retrieve a registry entry by NAME during completion."
  (unless nix3-registry-table
    (error "Not initialized `nix3-registry-table'"))
  (cdr (gethash name nix3-registry-table)))

(defun nix3-registry--non-indirect (url-alist)
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
  "Add a new entry to the user registry."
  (interactive (let* ((url (read-from-minibuffer "Url: "
                                                 ;; TODO: flake url at point
                                                 (or (bound-and-true-p nix3-flake-url)
                                                     (nix3-registry--maybe-origin-flake-url))))
                      (name (read-from-minibuffer (format "Registry name for %s: " url)
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
  (when-let (git-url (cdr (assoc "origin" (nix3-git-remotes))))
    (nix3-flake-ref-alist-to-url
     (nix3-git-url-to-flake-alist git-url))))

(defun nix3-registry--flake-url-p (url)
  (and (not (string-match-p "#" url))
       (string-match-p ":" url)))

;;;###autoload
(defun nix3-registry-insert (name-or-entry)
  "Insert a flake input entry from the registry."
  (interactive (list (nix3-registry-complete "Add flake Input: " :system nil)))
  (pcase-let
      ((`(,name . ,alist) (pcase name-or-entry
                            ((and `(,name . ,alist)
                                  (guard (stringp name))
                                  (guard (listp alist)))
                             name-or-entry)
                            ((pred stringp)
                             ;; TODO: Don't fetch all entries
                             (cdr (gethash name-or-entry (nix3-registry--collect-entries
                                                          :global t :system nil :user t)))))))
    (insert (format "inputs.%s.url = \"%s\";"
                    name
                    (nix3-flake-ref-alist-to-url alist)))))

;;;###autoload
(defun nix3-registry-list ()
  "Display a list of entries in the flake registries."
  (interactive)
  (let ((name (car (nix3-registry-complete "Flake: "
                                           :add-to-registry t
                                           :require-match nil
                                           :no-exact t))))
    (message "Selected a registry entry %s" name)
    (funcall nix3-registry-list-action name)))

(provide 'nix3-registry)
;;; nix3-registry.el ends here
