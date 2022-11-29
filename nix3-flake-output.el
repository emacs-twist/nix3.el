;;; nix3-flake-output.el --- Transient on flake output -*- lexical-binding: t -*-

(require 'transient)
(require 'nix3-core)
(require 'nix3-utils)

(defvar nix3-flake-output-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'nix3-flake-output-return)
    (define-key map "a" #'nix3-flake-output-browse-attribute)
    (define-key map "b" #'nix3-flake-output-build)
    (define-key map "r" #'nix3-flake-output-run)
    map))

(defvar nix3-flake-output-type nil
  "Type of the attribute at point. Set temporarily.")

(defun nix3-flake-output-return ()
  (interactive)
  (when (nix3-flake-output-path-at-point)
    (call-interactively #'nix3-flake-output-dispatch)))

(defun nix3-flake-output-path-at-point ()
  (when-let (section (magit-current-section))
    (when (eq (oref section type) 'flake-output)
      (oref section value))))

(defun nix3-flake-output-type ()
  (save-excursion
    (catch 'output-type
      ;; Use of `while-let' could simplify this code
      (let (section)
        (while (setq section (magit-current-section))
          (when (eq (oref section type) 'flake-output-type)
            (throw 'output-type (oref section value)))
          (if-let (parent (oref section parent))
              (let ((pos (point)))
                (magit-section-goto parent)
                ;; If there is no heading for the parent branch, the position
                ;; doesn't change, so move the position to the previous line.
                (when (eq pos (point))
                  (previous-line)))
            (throw 'output-type nil)))))))

(transient-define-prefix nix3-flake-output-dispatch ()
  [:description
   nix3-flake-output-description
   ("b" "nix build" nix3-flake-output-build
    :if nix3-flake-output--buildable-p)
   ("r" "nix run" nix3-flake-output-run
    :if nix3-flake-output--runnable-p)
   ("d" "dired" nix3-flake-output-dired-template
    :if nix3-flake-output--template-p)
   ("a" "Attributes" nix3-flake-output-browse-attribute)]
  (interactive)
  (unless (nix3-flake-output-path-at-point)
    (user-error "No output at point"))
  (setq nix3-flake-output-type (nix3-flake-output-type))
  (transient-setup 'nix3-flake-output-dispatch))

(defun nix3-flake-output-description ()
  (format "%s (%s)" (nix3-flake-output-path-at-point)
          nix3-flake-output-type))

(defun nix3-flake-output--template-p ()
  (equal nix3-flake-output-type "template"))

(defun nix3-flake-output-dired-template ()
  (interactive)
  (dired (nix3-read-nix-json-command "eval"
                                     (concat (nix3-flake--buffer-url)
                                             "#" (concat (nix3-flake-output-path-at-point)
                                                         ".path"))
                                     "--json")))

(defun nix3-flake-output--buildable-p ()
  (member nix3-flake-output-type '("derivation")))

(defun nix3-flake-output-build ()
  "Build the output at point."
  (interactive)
  (nix3-flake-output--compile-it "build"))

(defun nix3-flake-output--runnable-p ()
  (member nix3-flake-output-type '("derivation"
                                   "app"
                                   "nixos-configuration")))

(defun nix3-flake-output-run ()
  "Run the output at point."
  (interactive)
  (nix3-flake-output--compile-it "run"))

(defun nix3-flake-output--compile-it (command attr)
  (compile (mapconcat #'shell-quote-argument
                      `(,nix3-nix-executable
                        ,@(if (listp command)
                              command
                            (list command))
                        ,(concat (or nix3-flake-url ".")
                                 "#" (nix3-flake-output-path-at-point)))
                      " ")))

(defun nix3-flake-output-browse-attribute ()
  "Explore the attribute at point."
  (interactive)
  (cl-labels
      ((get-attr-names (path)
         (nix3-read-nix-json-command "eval"
                                     (concat (or nix3-flake-url ".")
                                             "#" path)
                                     "--apply" "builtins.attrNames"
                                     "--json"))
       (get-type (path)
         (message "nix eval %s" (concat (nix3-flake--buffer-url)
                                        "#" path))
         (nix3-read-nix-json-command "eval"
                                     (concat (or nix3-flake-url ".")
                                             "#" path)
                                     "--apply" "builtins.typeOf"
                                     "--json"))
       (is-path (path)
         (nix3-read-nix-json-command "eval"
                                     (concat (or nix3-flake-url ".")
                                             "#" path)
                                     "--json"
                                     "--apply" "builtins.isPath"))
       (get-value (path)
         (nix3-read-nix-json-command "eval"
                                     (concat (or nix3-flake-url ".")
                                             "#" path)
                                     "--json"))
       (print-obj (obj)
         (pp-display-expression obj "*nix eval*"))
       (go (path)
         (if-let (names (get-attr-names path))
             (let ((new-path (thread-last
                               (completing-read (format "Attribute (%s): " path)
                                                names
                                                nil t)
                               (nix3-flake--escape-attr-name)
                               (concat path "."))))
               (pcase (get-type new-path)
                 ("set" (go new-path))
                 ("string" (let ((value (get-value new-path)))
                             (if (string-prefix-p "/nix/store/" value)
                                 (if (fboundp 'nix-store-show-path)
                                     (nix-store-show-path value)
                                   (find-file value))
                               (print-obj (get-value new-path)))))
                 ("lambda" (message "%s is a function, so its value cannot be displayed"
                                    new-path))
                 (_ (print-obj (get-value new-path)))))
           (message "%s is an empty attribute set" path))))
    (go (nix3-flake-output-path-at-point))))

(provide 'nix3-flake-output)
;;; nix3-flake-output.el ends here
