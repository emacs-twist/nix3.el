;;; nix3-core.el --- Foundations for nix3.el -*- lexical-binding: t -*-

(require 'map)

(declare-function string-remove-suffix "subr-x")

(defgroup nix3 nil
  "Interface to experimental commands of Nix."
  :prefix "nix3-"
  :group 'nix)

;;;; Custom variables

(defcustom nix3-nix-executable "nix"
  "Executable file of Nix."
  :type 'file)

(defcustom nix3-config-expiration 5
  "Number of seconds for which `nix3--config' is memoized."
  :type 'number)

(defcustom nix3-git-executable "git"
  "Name of the Git executable."
  :type 'file)

(defcustom nix3-crm-separator ","
  "String used as a separator in `completing-read-multiple'."
  :type 'string)

;;;; Variables

(defvar nix3-config-cache nil)

;;;; Functions

(defun nix3-read-nix-command (&rest args)
  "Run nix and return its output string."
  (with-temp-buffer
    (let ((err-file (make-temp-file "nix3"))
          (coding-system-for-read 'utf-8))
      (unwind-protect
          (unless (zerop (apply #'call-process
                                nix3-nix-executable
                                nil (list (current-buffer) err-file) nil
                                args))
            (error "Failed to run %s: %s"
                   (cons nix3-nix-executable args)
                   (with-temp-buffer
                     (insert-file-contents err-file)
                     (buffer-string))))
        (delete-file err-file)))
    (buffer-string)))

(defun nix3-read-nix-json-command (&rest args)
  "Run nix and return its output string."
  (json-parse-string (apply #'nix3-read-nix-command args)
                     :false-object nil
                     :object-type 'alist
                     :array-type 'list))

(defun nix3-run-process-background (cmd &rest args)
  "Run a system command in the background.

This command discard the exit code or output of the command."
  ;; Use compile for now, but it may be a better way
  (compile (mapconcat #'shell-quote-argument
                      (cons cmd args)
                      " ")))

(defun nix3-system ()
  "Return the system name of Nix."
  (nix3-read-nix-command "eval" "--expr" "builtins.currentSystem" "--impure"
                          "--raw"))

(defun nix3-normalize-path (dir)
  (string-remove-suffix "/" (file-truename dir)))

(defun nix3--config ()
  "Return the output from \"nix show-config\" command."
  (with-temp-buffer
    (insert (nix3-read-nix-command "show-config" "--json"))
    (goto-char (point-min))
    ;; "max-free" contains a big integer, which cannot be parsed using
    ;; `json-parse-string' right now. Thus it is necessary to delete the
    ;; attribute.
    (when (search-forward "\"max-free\"")
      (let ((start (car (match-data))))
        (search-forward "{")
        (backward-char)
        (goto-char (cdr (bounds-of-thing-at-point 'sexp)))
        (when (eq ?, (char-after (point)))
          (forward-char))
        (delete-region start (point))))
    (goto-char (point-min))
    (json-parse-buffer :array-type 'list)))

(defun nix3--config-memoized ()
  "Return a memoized value of the Nix configuration."
  (if (and nix3-config-cache
           (< (car nix3-config-cache)
              (+ nix3-config-expiration (float-time))))
      (cdr nix3-config-cache)
    (let ((value (nix3--config)))
      (setq nix3-config-cache (cons (float-time) value))
      value)))

(defun nix3-config-lookup (key)
  "Look up the configuration of Nix."
  (if-let (h (map-elt (nix3--config-memoized) key))
      (map-elt h "value")
    (error "Key %s is not found in the nix conf" key)))

(defun nix3--git-config-list (&optional scope)
  (apply #'process-lines nix3-git-executable "config" "--list"
         (pcase scope
           (`local "--local")
           (`global "--global"))))

(defun nix3--all-systems ()
  (nix3-read-nix-json-command "eval" "nixpkgs#lib.systems.doubles.all" "--json"))

(provide 'nix3-core)
;;; nix3-core.el ends here
