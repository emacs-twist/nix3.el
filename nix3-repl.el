;;; nix3-repl.el ---  -*- lexical-binding: t -*-

(require 'eieio)
(require 'comint)
(require 'json)
(require 'nix3-core)

(defgroup nix3-repl nil
  ""
  :prefix "nix3-repl-"
  :group 'nix3)

;; Based on https://emacs.stackexchange.com/a/18884
(defconst nix3-repl-ansi-escape-re
  (rx (or ?\233 (and ?\e ?\[))
      (zero-or-more (char (?0 . ?\?)))
      (zero-or-more (char ?\s ?- ?\/))
      (char (?@ . ?~))))

(defvar nix3-repl-local-sessions nil)

(defvar nix3-repl-flake-url nil)

(defcustom nix3-repl-default-timeout 500
  "Default timeout from nix repl in milliseconds."
  :type 'number)

(defun nix3-repl--buffer-process-live-p (buffer)
  (and (buffer-live-p buffer)
       (when-let (process (get-buffer-process buffer))
         (process-live-p process))))

(defun nix3-repl--local-sessions ()
  (or nix3-repl-local-sessions
      (setq nix3-repl-local-sessions
            (make-hash-table :test #'equal))))

(defun nix3-repl-local-session (dir)
  (let* ((path (nix3-normalize-path dir))
         (table (nix3-repl--local-sessions))
         (existing (gethash path table)))
    (if (and existing (nix3-repl--buffer-process-live-p existing))
        existing
      (unless (file-exists-p (expand-file-name "flake.nix" dir))
        (error "Directory %s does not contain flake.nix" dir))
      (let* ((default-directory (file-name-as-directory dir))
             (buffer (get-buffer-create
                      (format "*Nix-Repl<%s>*" (file-name-nondirectory path)))))
        (puthash path buffer table)
        (nix3-repl--new-session buffer (concat "path:" path))
        buffer))))

(defmacro nix3-repl-with-local-session (dir &rest progn)
  (declare (indent 1))
  `(with-current-buffer (nix3-repl-local-session ,dir)
     ,@progn))

(defun nix3-repl--new-session (buffer url)
  (make-comint-in-buffer (concat "Nix-Repl"
                                 (save-match-data
                                   (let ((name (buffer-name buffer)))
                                     (string-match "<.+>" name)
                                     (match-string 0 name))))
                         buffer nix3-nix-executable nil "repl")
  (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
  ;; TODO: Add cleanup to kill-emacs-hook
  (with-current-buffer buffer
    (setq-local nix3-repl-flake-url url)
    (erase-buffer)
    ;; Clear existing data if necessary

    ;; Read initial output
    (nix3-repl--accept-output 100)

    (nix3-repl--send-command "lib = (builtins.getFlake \"nixpkgs\").lib"
                              300)

    (nix3-repl--send-command ":a builtins"
                              100)

    ;; builtins.getFlake may not work. In that case, it may worth trying out
    ;; :lf.
    (nix3-repl--send-command (format ":a getFlake \"%s\"" url)
                              100)))

(defun nix3-repl--accept-output (&optional timeout)
  "Read output from the process with TIMEOUT."
  (while (accept-process-output (get-buffer-process (current-buffer))
                                nil
                                (or timeout nix3-repl-default-timeout))))

(defun nix3-repl--send-command (command timeout)
  "Send a command to the nix repl session and ignore its output."
  (insert command)
  (comint-send-input)
  (nix3-repl--accept-output timeout))

(cl-defun nix3-repl-eval (input &optional timeout)
  "Evaluate INPUT and return its result, with optional TIMEOUT."
  (let ((redirect-buffer (generate-new-buffer "*Nix-Repl-Eval*")))
    ;; `comint-redirect-send-command' seems to be flakey. I don't know why, but
    ;; using `comint-redirect-setup' seem to have solved the issue.
    (comint-redirect-setup redirect-buffer (current-buffer) "^nix-repl> ")
    (add-function :around (process-filter (get-buffer-process (current-buffer)))
                  #'comint-redirect-filter)
    (process-send-string (current-buffer) (concat input "\n"))
    (nix3-repl--accept-output timeout)
    ;; (while (= start (point))
    ;;   (nix3-repl--accept-output 300))
    (prog1
        (with-current-buffer redirect-buffer
          (goto-char (point-min))

          (save-excursion
            ;; If the input is long, it will be wrapped by ^H, which breaks
            ;; `search-forward'. Erase them.
            (while (re-search-forward (rx (* space) "") nil t)
              (replace-match "")))

          (save-excursion
            ;; Every output is wrapped in escape sequences for highlighting.
            ;; I only need the data, so replace them as well.
            (while (re-search-forward nix3-repl-ansi-escape-re nil t)
              (replace-match "")))

          ;; nix-repl echoes every user input to the terminal. Find the end of the
          ;; input and move the point to right after the echoed input.
          (search-forward input)

          ;; Skip whitespaces
          (when (looking-at (rx (+ space)))
            (goto-char (match-end 0)))

          ;; Every response is printed as a Nix string wrapped in double quotes.
          ;; Decode them as a JSON string.
          (condition-case-unless-debug err
              (let ((json-false nil))
                (json-read))
            (error (error "Error while reading the input: %s in %s"
                          err
                          (buffer-string)))))
      (kill-buffer redirect-buffer))))

(cl-defun nix3-repl-eval-json (expression &optional timeout &key json-args)
  (apply #'json-parse-string
         (nix3-repl-eval (format "toJSON (%s)" expression) timeout)
         (append json-args
                 (list :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object nil))))

(defun nix3-repl-type-of (expression)
  (nix3-repl-eval (format "typeOf (%s)" expression)
                    100))

(defun nix3-repl-derivation-p (expression)
  (nix3-repl-eval (format "lib.isDerivation (%s)" expression)
                    100))

(defun nix3-repl-child-attrsets (output)
  "Return the names of attrsets of OUTPUT."
  (nix3-repl-eval-json (format "attrNames (lib.filterAttrs (_: isAttrs) outputs.%s)"
                                 output)
                         800))

(provide 'nix3-repl)
;;; nix3-repl.el ends here
