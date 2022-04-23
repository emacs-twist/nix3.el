;;; nix26-core.el ---  -*- lexical-binding: t -*-

(require 'map)

(defgroup nix26 nil
  ""
  :prefix "nix26-"
  :group 'nix26)

(defcustom nix26-nix-executable "nix"
  ""
  :type 'file)

(defcustom nix26-config-expiration 5
  "Number of seconds for which `nix26--config' is memorized."
  :type 'number)

(defvar nix26-config-cache nil)

(defun nix26-read-nix-command (&rest args)
  "Run nix and return its output string."
  (with-temp-buffer
    (let ((err-file (make-temp-file "nix26")))
      (unwind-protect
          (unless (zerop (apply #'call-process
                                nix26-nix-executable
                                nil (list (current-buffer) err-file) nil
                                args))
            (error "Failed to run %s: %s"
                   (cons nix26-nix-executable args)
                   (with-temp-buffer
                     (insert-file-contents err-file)
                     (buffer-string))))
        (delete-file err-file)))
    (buffer-string)))

(defun nix26-read-nix-json-command (&rest args)
  "Run nix and return its output string."
  (json-parse-string (apply #'nix26-read-nix-command args)
                     :object-type 'alist
                     :array-type 'list))

(defun nix26-system ()
  "Return the system name of Nix."
  (nix26-read-nix-command "eval" "--expr" "builtins.currentSystem" "--impure"
                          "--raw"))

(defun nix26-normalize-path (dir)
  (string-remove-suffix "/" (file-truename dir)))

(defun nix26--config ()
  "Return the output from \"nix show-config\" command."
  (with-temp-buffer
    (insert (nix26-read-nix-command "show-config" "--json"))
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

(defun nix26--config-memorized ()
  (if (and nix26-config-cache
           (< (car nix26-config-cache)
              (+ nix26-config-expiration (float-time))))
      (cdr nix26-config-cache)
    (let ((value (nix26--config)))
      (setq nix26-config-cache (cons (float-time) value))
      value)))

(defun nix26-config-lookup-value (key)
  (if-let (h (map-elt (nix26--config-memorized) key))
      (map-elt h "value")
    (error "Key %s is not found in the nix conf" key)))

(provide 'nix26-core)
;;; nix26-core.el ends here
