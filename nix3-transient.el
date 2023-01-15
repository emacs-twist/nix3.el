;;; nix3-transient.el --- Transient interface -*- lexical-binding: t -*-

(require 'transient)
(require 'nix3-flake)
(require 'nix3-core)
(require 'nix3-utils)

(declare-function nix3-realise-and-show-store "nix3")

(defvar-local nix3-transient-flake nil
  "Location of the flake.")

(defvar nix3-transient-flake-output nil)

(defvar nix3-transient-flake-output-type nil
  "Type of the attribute at point. Set temporarily.")

(defvar nix3-transient-nix-command nil)

;;;; Infixes and suffixes

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
                            (oref obj value)))

(cl-defmethod transient-infix-set ((obj nix3-transient-output-variable) value)
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj nix3-transient-output-variable))
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (propertize value 'face 'transient-value)
     (propertize ")" 'face 'transient-inactive-value))))

(transient-define-infix nix3-transient-set-output ()
  :class 'nix3-transient-output-variable
  :description "Flake attribute"
  :prompt "Flake attribute: "
  :required t
  :variable 'nix3-transient-flake-output)

;;;;; Flags

(defclass nix3-transient-multi-select (transient-variable)
  ((variable :initarg :variable)
   (table :initarg :table)))

(cl-defmethod transient-init-value ((obj nix3-transient-multi-select))
  (oset obj value (eval (oref obj variable))))

(cl-defmethod transient-infix-read ((obj nix3-transient-multi-select))
  (completing-read-multiple (oref obj prompt)
                            (oref obj table)
                            nil nil
                            (string-join (oref obj value) ",")))

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
  :table '("--impure"
           "--dry-run"))

;;;;; Command (nix run)

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

(defvar nix3-transient-command-args nil)

(transient-define-infix nix3-transient-set-command-args ()
  :class 'nix3-transient-string-variable
  :description "Arguments"
  :prompt "Arguments: "
  :variable 'nix3-transient-command-args)

;;;; Main command

;; This command must be invoked after fetching data, so it should be private.
(transient-define-prefix nix3-transient--dispatch ()
  [:if-not
   nix3-transient--show-mode-p
   :description
   nix3-transient--flake-description
   ("h" "Show metadata and outputs" nix3-transient-show)]
  ["Nix commands on installable"
   :class transient-row
   ("b" "build" nix3-transient-build)
   ("r" "run" nix3-transient-run)
   ("c" "flake check" nix3-transient-flake-check)]
  (interactive)
  (unless nix3-transient-flake
    (user-error "Variable nix3-transient-flake must be set in advance"))
  (setq nix3-transient-flake-output nil)
  (transient-setup 'nix3-transient--dispatch))

;;;###autoload
(defun nix3-transient ()
  "Dispatch a transient interface to Nix commands."
  (interactive)
  (if (nix3-transient--show-mode-p)
      ;; If the current buffer is already showing the flake via
      ;; `nix3-flake-show-mode', there will be no need to refetch the data.
      (progn
        (setq nix3-transient-flake (nix3-flake--buffer-url))
        (call-interactively #'nix3-transient--dispatch))
    (setq nix3-transient-flake (nix3-flake-location))
    (if (and (nix3-flake-metadata--get nix3-transient-flake)
             (nix3-flake-show--get nix3-transient-flake))
        (call-interactively #'nix3-transient--dispatch)
      (promise-chain (nix3-flake--get-promise nix3-transient-flake nix3-flake-url)
        (then (lambda (_)
                (call-interactively #'nix3-transient--dispatch)))
        (promise-catch #'nix3-flake--handle-process-error)))))

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
   ("e" "Explore or eval" nix3-transient-explore-output)]
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

;;;; Generate items programmatically

(eval-and-compile
  (defvar nix3-transient-common-options
    ["Common options"
     :class transient-row
     ("-q" "No build output" "--no-build-output")
     ("-L" "Print full build logs" ("-L" "--print-build-logs"))]))

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
  (transient-setup 'nix3-transient-build))

(defun nix3-transient--build-compile ()
  (interactive)
  (compile (nix3-transient--shell-command
            nix3-transient-flake-output
            (transient-args 'nix3-transient-build))))

(transient-define-prefix nix3-transient-run ()
  ["nix run"
   ("#" nix3-transient-set-output)
   ("--" nix3-transient-set-flags)
   ("-c" nix3-transient-set-command-args)]
  nix3-transient-common-options
  ["Suffixes"
   ("RET" "Run in compile" nix3-transient--run-compile)
   ;; ("t" "Run in term" nix3-transient--run-term)
   ]
  (interactive)
  (setq nix3-transient-nix-command "run")
  (transient-setup 'nix3-transient-run))

(defun nix3-transient--run-compile ()
  (interactive)
  (compile (concat (nix3-transient--shell-command
                    nix3-transient-flake-output
                    (transient-args 'nix3-transient-run))
                   (if nix3-transient-command-args
                       (concat " -- " nix3-transient-command-args)
                     ""))))

(transient-define-prefix nix3-transient-flake-check ()
  ["nix flake check"
   ("--" nix3-transient-set-flags)]
  nix3-transient-common-options
  ["Suffixes"
   ("c" "Run in compile" nix3-transient--flake-check-compile)]
  (interactive)
  (setq nix3-transient-nix-command '("flake" "check"))
  (transient-setup 'nix3-transient-flake-check))

(defun nix3-transient--flake-check-compile ()
  (interactive)
  (compile (nix3-transient--shell-command
            nil
            (transient-args 'nix3-transient-flake-check))))

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
                (format "%s#%s" (or nix3-flake-url ".") attr-or-nil)
              (or nix3-flake-url "."))
            (if args
                (concat " " (mapconcat #'shell-quote-argument (flatten-list args) " "))
              ""))))

(provide 'nix3-transient)
;;; nix3-transient.el ends here
