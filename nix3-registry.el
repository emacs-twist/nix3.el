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

(defface nix3-registry-type-face
  '((t :inherit font-lock-type-face))
  ""
  :group 'nix3-registry)

(defface nix3-registry-url-face
  '((t :inherit font-lock-comment-face))
  ""
  :group 'nix3-registry)

(defvar nix3-registry-global-cache nil)

(defvar nix3-registry-entries nil)

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
  (let (result)
    (pcase-dolist (`(,type . ,entries)
                   `((global . ,(when global
                                  (nix3-registry--global-entries)))
                     (system . ,(when system
                                  (nix3-registry--from-file
                                   nix3-registry-system-file)))
                     (user . ,(when user
                                (nix3-registry--from-file
                                 nix3-registry-user-file)))))
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

(cl-defun nix3-registry-complete (prompt &key
                                          extra-entries
                                          (require-match t)
                                          add-to-registry
                                          no-exact
                                          (global t)
                                          (system t)
                                          (user t))
  (let* ((entries (setq nix3-registry-entries
                        (nix3-registry--collect-entries
                         :no-exact no-exact
                         :global global
                         :system system
                         :user user)))
         (table `(lambda (string pred action)
                   (if (eq action 'metadata)
                       '(metadata . ((category . nix3-registry-entry)
                                     (annotation-function . nix3-registry-annotate)))
                     (complete-with-action action ',(append extra-entries
                                                            (mapcar #'car entries))
                                           string pred))))
         (input (completing-read prompt table nil require-match))
         (entry (assoc input entries)))
    (if entry
        (cons input (cddr entry))
      (when (and add-to-registry
                 (nix3-registry--flake-url-p input)
                 (not require-match)
                 user
                 (yes-or-no-p "Add the flake to the user registry?"))
        (let ((name (read-string (format "Name for %s: " input))))
          (nix3-registry-add name input)))
      input)))

(defun nix3-registry-annotate (id)
  (when-let (entry (assoc id nix3-registry-entries))
    (let ((type (cadr entry))
          (dest (cddr entry)))
      (concat " "
              (propertize (nix3-flake-ref-alist-to-url dest)
                          'face 'nix3-registry-url-face)
              " "
              (propertize (symbol-name type)
                          'face 'nix3-registry-type-face)))))

;;;###autoload
(defun nix3-registry-add (name flake)
  "Add a new entry to the user registry."
  (interactive (let* ((url (read-string "Url: "))
                      (name (read-string (format "Name for \"%s\": " url))))
                 (list name url)))
  (unless (nix3-registry--flake-url-p flake)
    (user-error "Invalid flake URL: %s" flake))
  (call-process nix3-nix-executable nil nil nil
                "registry" "add"
                name flake))

(defun nix3-registry--flake-url-p (url)
  (and (not (string-match-p "#" url))
       (string-match-p ":" url)))

(provide 'nix3-registry)
;;; nix3-registry.el ends here
