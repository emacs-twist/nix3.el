;;; nix26-registry.el --- A wrapper for Nix registries -*- lexical-binding: t -*-

(require 'nix26-core)
(require 'nix26-utils)

(defgroup nix26-registry nil
  "Support for flake registries."
  :group 'nix26)

(defun nix26-registry--global-url ()
  "Return the URL to the global registry."
  (nix26-config-lookup-value "flake-registry"))

(defcustom nix26-registry-system-file "/etc/nix/registry.json"
  ""
  :type 'file)

(defcustom nix26-registry-user-file
  (cl-case system-type
    (gnu/linux
     (progn
       (require 'xdg)
       (expand-file-name "nix/registry.json" (xdg-config-home))))
    (otherwise
     (error "Not defined for %s" system-type)))
  ""
  :type 'file)

(defface nix26-registry-type-face
  '((t :inherit font-lock-type-face))
  ""
  :group 'nix26-registry)

(defface nix26-registry-url-face
  '((t :inherit font-lock-comment-face))
  ""
  :group 'nix26-registry)

(defvar nix26-registry-global-cache nil)

(defvar nix26-registry-entries nil)

(defun nix26-registry--parse-buffer ()
  (goto-char (point-min))
  (thread-last
    (json-parse-buffer :object-type 'alist :array-type 'list)
    (assq 'flakes)
    (cdr)))

(defun nix26-registry--global-entries ()
  (if (and nix26-registry-global-cache
           (< (car nix26-registry-global-cache)
              (+ (float-time) (nix26-config-lookup-value "tarball-ttl"))))
      (cdr nix26-registry-global-cache)
    (let ((buffer (url-retrieve-synchronously (nix26-registry--global-url) t t))
          (message-log-max nil))
      (message "Fetching the global registry...")
      (unwind-protect
          (with-current-buffer buffer
            (when url-http-end-of-headers
              (delete-region (point-min) url-http-end-of-headers))
            (message nil)
            (let ((value (nix26-registry--parse-buffer)))
              (setq nix26-registry-global-cache (cons (float-time) value))
              value))
        (kill-buffer buffer)))))

(defun nix26-registry--from-file (file)
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (nix26-registry--parse-buffer))))

(cl-defun nix26-registry--collect-entries (&key no-exact
                                                (global t)
                                                (system t)
                                                (user t))
  (let (result)
    (pcase-dolist (`(,type . ,entries)
                   `((global . ,(when global
                                  (nix26-registry--global-entries)))
                     (system . ,(when system
                                  (nix26-registry--from-file
                                   nix26-registry-system-file)))
                     (user . ,(when user
                                (nix26-registry--from-file
                                 nix26-registry-user-file)))))
      (dolist (entry entries)
        (let ((from (alist-get 'from entry))
              (to (alist-get 'to entry))
              (exact (alist-get 'exact entry)))
          (when (and (not (and no-exact exact))
                     (equal "indirect" (alist-get 'type from)))
            (push (cons (alist-get 'id from)
                        (cons type to))
                  result)))))
    result))

(cl-defun nix26-registry-complete (prompt &key
                                          extra-entries
                                          (require-match t)
                                          add-to-registry
                                          no-exact
                                          (global t)
                                          (system t)
                                          (user t))
  (let* ((entries (setq nix26-registry-entries
                        (nix26-registry--collect-entries
                         :no-exact no-exact
                         :global global
                         :system system
                         :user user)))
         (table `(lambda (string pred action)
                   (if (eq action 'metadata)
                       '(metadata . ((category . nix26-registry-entry)
                                     (annotation-function . nix26-registry-annotate)))
                     (complete-with-action action ',(append extra-entries
                                                            (mapcar #'car entries))
                                           string pred))))
         (input (completing-read prompt table nil require-match))
         (entry (assoc input entries)))
    (if entry
        (cons input (cddr entry))
      (when (and add-to-registry
                 (not (string-match-p "#" input))
                 (string-match-p ":" input)
                 (not require-match)
                 user
                 (yes-or-no-p "Add the flake to the user registry?"))
        (let ((name (read-string (format "Name for %s: " input))))
          (call-process nix26-nix-executable nil nil nil
                        "registry" "add"
                        name input)))
      input)))

(defun nix26-registry-annotate (id)
  (when-let (entry (assoc id nix26-registry-entries))
    (let ((type (cadr entry))
          (dest (cddr entry)))
      (concat " "
              (propertize (nix26-flake-ref-alist-to-url dest)
                          'face 'nix26-registry-url-face)
              " "
              (propertize (symbol-name type)
                          'face 'nix26-registry-type-face)))))

(provide 'nix26-registry)
;;; nix26-registry.el ends here
