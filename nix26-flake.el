;;; nix26-flake.el --- Support for Nix flakes -*- lexical-binding: t -*-

(require 'nix26-core)
(require 'nix26-repl)
(require 'nix26-utils)
(require 'nix26-flake-input)
(require 'nix26-registry)

(require 's)
(require 'magit-section)
(require 'project)
(require 'promise)

(defgroup nix26-flake nil
  ""
  :prefix "nix26-flake-"
  :group 'nix26)

(defconst nix26-flake-show-error-buffer "*Nix-Flake-Show Errors*")
(defconst nix26-flake-metadata-error-buffer "*Nix-Flake-Metadata Errors*")
(defconst nix26-flake-init-buffer "*Nix-Flake-Init*")

(defcustom nix26-flake-show-sections
  '(nix26-flake-insert-outputs
    nix26-flake-insert-inputs)
  ""
  :type 'hook)

(defcustom nix26-flake-toplevel-sections-unfolded t
  "Whether to unfold the top-level section by default."
  :type 'boolean)

(defcustom nix26-flake-input-name-max-width 20
  ""
  :type '(choice number (const nil)))

(defcustom nix26-flake-init-reverted-modes
  '(dired-mode
    magit-status-mode)
  "List of major modes that should be reverted after flake init."
  :type '(repeat symbol))

(defface nix26-flake-drv-type-face
  '((t :inherit font-lock-constant-face))
  "")

(defface nix26-flake-drv-parent-face
  '((t :inherit default))
  "")

(defface nix26-flake-drv-name-face
  '((t :inherit magit-section-secondary-heading))
  "")

(defface nix26-flake-input-name-face
  '((t :inherit magit-section-secondary-heading))
  "")

(define-button-type 'nix26-flake-url-link
  :supertype 'help-xref
  'help-function #'nix26-flake-show-url
  'help-echo (purecopy "mouse-2, RET: Show the flake"))

(defvar nix26-flake-url nil
  "Set to the URL of a flake when the flake is not local.")

(defvar nix26-flake-show-history nil)

(defvar nix26-flake-template-history nil)

(defvar nix26-flake-template-alist nil)

(defun nix26-flake-lookup-tree (path data)
  "Look up PATH in a tree DATA.

This is a helper macro for traversing a tree."
  (cl-reduce (lambda (acc f)
               (cdr (assq f acc)))
             (cdr path)
             :initial-value
             (cdr (assq (car path) data))))

(defun nix26-flake--path-p (url-alist)
  "Return non-nil if URL-ALIST points to a path."
  (equal "path" (assq 'type url-alist)))

(defun nix26-flake--resolve-path (path)
  (if (string-prefix-p "/" path)
      path
    (save-match-data
      (if (string-match (rx bol "./" (group (+ anything))) path)
          (let ((relative (match-string 1 path)))
            (cond
             ((and nix26-flake-url
                   (string-match-p (rx (any "=&") "dir=") nix26-flake-url))
              (error "The parent flake \"%s\" already has dir parameter, so it cannot be resolved"
                     nix26-flake-url))
             (nix26-flake-url
              (concat nix26-flake-url
                      (if (string-match-p (rx "?") nix26-flake-url)
                          "&"
                        "?")
                      "dir="
                      relative))
             (t
              (nix26-normalize-path (expand-file-name relative default-directory)))))
        (error "Failed to match against a path \"%s\"" path)))))

(defvar nix26-flake-show-results nil)

(defun nix26-flake--ensure-show-cache ()
  (unless nix26-flake-show-results
    (setq nix26-flake-show-results (make-hash-table :test #'equal))))

(defun nix26-flake-show--put (directory-or-url result)
  (nix26-flake--ensure-show-cache)
  (puthash directory-or-url result nix26-flake-show-results))

(defun nix26-flake-show--get (directory)
  (nix26-flake--ensure-show-cache)
  (gethash (string-remove-suffix "/" directory)
           nix26-flake-show-results))

(defvar nix26-flake-metadata-results nil)

(defun nix26-flake--ensure-metadata-cache ()
  (unless nix26-flake-metadata-results
    (setq nix26-flake-metadata-results (make-hash-table :test #'equal))))

(defun nix26-flake-metadata--put (directory-or-url result)
  (nix26-flake--ensure-metadata-cache)
  (puthash directory-or-url result nix26-flake-metadata-results))

(defun nix26-flake-metadata--get (directory)
  (nix26-flake--ensure-metadata-cache)
  (gethash (string-remove-suffix "/" directory)
           nix26-flake-metadata-results))

(defun nix26-flake-insert-outputs ()
  (magit-insert-section (flake-outputs nil nix26-flake-toplevel-sections-unfolded)
    (magit-insert-heading "Flake outputs")

    (let ((result (nix26-flake--get-show-result))
          (nix-system (nix26-system)))
      (pcase-dolist (`(,type-name . ,outputs)
                     (nix26-flake--group-outputs result))
        (magit-insert-section (flake-output-group type-name)
          (magit-insert-heading
            (make-string 2 ?\s)
            (propertize type-name 'face 'nix26-flake-drv-type-face))

          (pcase-dolist (`(,branch-reverse . ,leaves)
                         (seq-group-by #'cdr outputs))
            (let* ((branch (reverse branch-reverse))
                   (name (symbol-name (car branch-reverse)))
                   (invisible (and (string-match-p (rx "-" (or "darwin" "linux") eol)
                                                   name)
                                   (not (equal name nix-system)))))
              (magit-insert-section (flake-output-subgroup branch invisible)
                (when branch
                  (magit-insert-heading
                    (make-string 4 ?\s)
                    (propertize (mapconcat #'symbol-name branch ".")
                                'face 'nix26-flake-drv-parent-face)))

                (dolist (output-reverse leaves)
                  (let* ((path (reverse output-reverse))
                         (node (nix26-flake-lookup-tree path result))
                         ;; (package-name (cdr (assq 'name node)))
                         (description (cdr (assq 'description node))))
                    (magit-insert-section (flake-output path)
                      (magit-insert-heading
                        (make-string 6 ?\s)
                        (apply #'propertize
                               (symbol-name (car output-reverse))
                               'nix-flake-output (mapconcat #'symbol-name
                                                            path ".")
                               'nix-flake-show node
                               'face 'nix26-flake-drv-name-face
                               (when description
                                 (list 'help-echo description)))))))))))))
    (insert ?\n)))

(defun nix26-flake-insert-header (url)
  (insert (propertize "Flake: " 'face 'magit-section-heading)
          (if-let (metadata (nix26-flake-metadata--get url))
              (cdr (assq 'resolvedUrl metadata))
            url)
          "\n")
  (insert ?\n))

(defun nix26-flake--group-outputs (root)
  (let (result)
    (cl-labels
        ((go (rev-path node)
             (if-let (type (cdr (assq 'type node)))
                 (push (cons type rev-path) result)
               (pcase-dolist (`(,name . ,child) node)
                 (go (cons name rev-path) child)))))
      (go nil root))
    (thread-last result
      (seq-group-by #'car)
      (mapcar (pcase-lambda (`(,type . ,cells))
                (cons type (thread-last (reverse cells)
                             (mapcar #'cdr)))))
      (seq-sort-by #'car #'string<))))

(defun nix26-flake-insert-inputs ()
  (when-let (result (nix26-flake--get-metadata-result))
    (magit-insert-section (flake-inputs nil nix26-flake-toplevel-sections-unfolded)
      (magit-insert-heading "Flake inputs")

      (when-let (nodes (thread-last
                         result
                         (assq 'locks)
                         (cdr)
                         (assq 'nodes)
                         (cdr)
                         (assq-delete-all 'root)))
        (let ((name-width (thread-last
                            nodes
                            (mapcar (lambda (cell)
                                      (symbol-name (car cell))))
                            (nix26-format--column-width
                             nix26-flake-input-name-max-width))))
          (cl-flet
              ((pad-column
                (len s)
                (s-pad-right len " " (s-truncate len s))))
            (pcase-dolist (`(,name . ,data) nodes)
              (magit-insert-section (flake-input name t)
                (let* ((is-flake (not (eq :false (cdr (assq 'flake data)))))
                       (original (cdr (assq 'original data)))
                       (name-string (symbol-name name))
                       (url (nix26-flake-ref-alist-to-url original)))
                  (insert (make-string 2 ?\s)
                          (propertize (pad-column name-width name-string)
                                      'help-echo name-string
                                      'face 'nix26-flake-input-name-face)
                          " ")
                  (if is-flake
                      (insert-text-button url
                                          'type 'nix26-flake-url-link
                                          'help-args
                                          (list (if (nix26-flake--path-p original)
                                                    (nix26-flake--resolve-path
                                                     (cdr (assq 'path original)))
                                                  url)))
                    (insert url))
                  (insert "  "
                          (propertize (if is-flake
                                          "(flake)"
                                        "(non-flake)")
                                      'face 'font-lock-constant-face)
                          "\n")
                  (nix26-put-overlay-on-region (line-beginning-position 0) (line-end-position 0)
                    'keymap nix26-flake-input-map
                    'nix26-flake-input-name name
                    'nix26-flake-input-data data)))))))
      (insert ?\n))))

(defun nix26-flake-show-buffer (dir-or-url is-url)
  (let ((default-directory (if is-url
                               "~/"
                             (file-name-as-directory dir-or-url))))
    (with-current-buffer (get-buffer-create
                          (format "*Nix Flake<%s>*"
                                  (if is-url
                                      dir-or-url
                                    (file-name-nondirectory dir-or-url))))
      (nix26-flake-show-mode)
      (when is-url
        (setq-local nix26-flake-url dir-or-url))
      (nix26-flake-show-revert)
      (current-buffer))))

(defun nix26-flake-show-revert (&rest _args)
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (flake)
      (magit-insert-heading)
      (nix26-flake-insert-header (nix26-flake--buffer-url))
      (run-hooks 'nix26-flake-show-sections))))

(defun nix26-flake--buffer-url ()
  (or nix26-flake-url
      (nix26-normalize-path default-directory)))

(defun nix26-flake--get-show-result ()
  (nix26-flake-show--get (nix26-flake--buffer-url)))

(defun nix26-flake--get-metadata-result ()
  (nix26-flake-metadata--get (nix26-flake--buffer-url)))

(defvar nix26-flake-show-mode-map
  (let ((m (make-composed-keymap nil magit-section-mode-map)))
    (define-key m (kbd "l") #'nix26-flake-back)
    (define-key m (kbd "g") #'nix26-flake-show-revert)
    m))

(define-derived-mode nix26-flake-show-mode magit-section-mode
  "Nix Flake"
  (setq-local revert-buffer-function #'nix26-flake-show-revert)
  (read-only-mode 1))

;;;###autoload
(defun nix26-flake-show (dir)
  "Show the flake at DIR."
  (interactive (list (if (equal current-prefix-arg '(4))
                         (read-directory-name "Select a flake: ")
                       (or (locate-dominating-file default-directory
                                                   "flake.nix")
                           (nix26-flake-select-locally)))))
  (if (and dir
           (file-exists-p (expand-file-name "flake.nix" dir)))
      (let ((truename (nix26-normalize-path dir))
            (default-directory dir))
        (promise-chain (nix26-flake--get-promise truename nil)
          (then (lambda (_)
                  (nix26-flake-switch-to-buffer (nix26-flake-show-buffer truename nil))))))
    (user-error "Directory %s does not contain flake.nix" dir)))

(defun nix26-flake-select-locally ()
  "Select a flake directory on the file system."
  (project-prompt-project-dir))

;;;###autoload
(defun nix26-flake-show-url (url)
  (interactive (let ((ent (nix26-registry-complete "Flake: "
                                                   :require-match nil
                                                   :no-exact t)))
                 (list (if (stringp ent)
                           ent
                         (car ent)))))
  (message "Fetching a flake...")
  (promise-chain (nix26-flake--get-promise url t)
    (then (lambda (_)
            (let (message-log-max)
              (message nil))))
    (then (lambda (_)
            (nix26-flake-switch-to-buffer (nix26-flake-show-buffer url t))))
    (catch #'error)))

(defun nix26-flake--get-promise (dir-or-url is-url)
  (promise-all (vector (promise-new (apply-partially #'nix26-flake--make-show-process
                                                     dir-or-url is-url))
                       (promise-new (apply-partially #'nix26-flake--make-metadata-process
                                                     dir-or-url is-url)))))

(cl-defmacro nix26-flake--nix-json-process (func-name &key name buffer stderr
                                                      subcommand
                                                      put-result)
  (declare (indent 1))
  `(defun ,func-name (url is-url resolve reject)
     (make-process :name ,name
                   :buffer ,buffer
                   :stderr ,stderr
                   :command (append (list nix26-nix-executable
                                          ,@subcommand
                                          url
                                          "--show-trace"
                                          "--json")
                                    (when is-url
                                      '("--no-write-lock-file")))
                   :sentinel
                   (lambda (process event)
                     (cond
                      ((equal "finished\n" event)
                       (with-current-buffer (process-buffer process)
                         (goto-char (point-min))
                         (let ((result (json-parse-buffer :object-type 'alist
                                                          :array-type 'list)))
                           (,put-result url result)
                           (funcall resolve result)))
                       (kill-buffer (process-buffer process)))
                      ((string-prefix-p "exited abnormally" event)
                       (funcall reject (format "nix %s failed on %s"
                                               ,(string-join subcommand " ")
                                               url))))))))

(nix26-flake--nix-json-process nix26-flake--make-show-process
  :name "Nix-Flake-Show-Json"
  :buffer (generate-new-buffer "*Nix Flake Show Output*")
  :stderr nix26-flake-show-error-buffer
  :subcommand ("flake" "show")
  :put-result nix26-flake-show--put)

(nix26-flake--nix-json-process nix26-flake--make-metadata-process
  :name "Nix-Flake-Metadata-Json"
  :buffer (generate-new-buffer "*Nix Flake Metadata Output*")
  :stderr nix26-flake-metadata-error-buffer
  :subcommand ("flake" "metadata")
  :put-result nix26-flake-metadata--put)

(defun nix26-flake-switch-to-buffer (buffer)
  (when (eq major-mode 'nix26-flake-show-mode)
    (push (current-buffer) nix26-flake-show-history))
  (switch-to-buffer buffer))

(defun nix26-flake-back ()
  (interactive)
  (when (eq major-mode 'nix26-flake-show-mode)
    (when-let (buffer (pop nix26-flake-show-history))
      (switch-to-buffer buffer))))

;;;###autoload
(defun nix26-flake-init (&optional template)
  (interactive)
  (when (and (file-exists-p "flake.nix")
             (not (yes-or-no-p "The directory is a flake. Are you sure?")))
    (user-error "Aborted"))
  (if template
      (nix26-flake--init-with-template template)
    (let ((item (nix26-registry-complete
                 "nix flake init: "
                 :require-match nil
                 :extra-entries nix26-flake-template-history
                 :no-exact t)))
      (if (and (stringp item)
               (string-match-p "#" item))
          (nix26-flake--init-with-template item)
        (let ((name-or-url (if (stringp item)
                               item
                             (car item)))
              (url (if (stringp item)
                       item
                     (nix26-flake-ref-alist-to-url (cdr item)))))
          (promise-chain (promise-new (apply-partially #'nix26-flake--make-show-process
                                                       url t))
            (then (lambda (_)
                    (let (message-log-max)
                      (message nil))))
            (then `(lambda (_)
                     (nix26-flake--init-with-template
                      (concat ,name-or-url
                              "#"
                              (thread-last
                                (nix26-flake-show--get ,url)
                                ;; Since Nix 2.7, the default template is templates.default, so we
                                ;; won't consider defaultTemplate.
                                (alist-get 'templates)
                                (nix26-flake--complete-template "nix flake init: "))))))
            (catch #'error)))))))

(defun nix26-flake--init-with-template (template)
  (delq template nix26-flake-template-history)
  (push template nix26-flake-template-history)
  ;; To set up a hook, we will use `start-process' rather than `compile'.
  (with-current-buffer (get-buffer-create nix26-flake-init-buffer)
    (erase-buffer))
  (message "nix26-flake-init[%s]: -t %s" default-directory template)
  (let ((proc (start-process "nix26-flake-init" nix26-flake-init-buffer
                             nix26-nix-executable "flake" "init" "-t" template)))
    (set-process-sentinel proc
                          (lambda (_proc event)
                            (cond
                             ((equal "finished\n" event)
                              (let ((message-log-max nil))
                                (message (with-current-buffer nix26-flake-init-buffer
                                           (buffer-string))))
                              (when (and nix26-flake-init-reverted-modes
                                         (apply #'derived-mode-p
                                                nix26-flake-init-reverted-modes))
                                (revert-buffer)))
                             ((string-prefix-p "exited abnormally" event)
                              (display-buffer nix26-flake-init-buffer)
                              (let ((message-log-max nil))
                                (message "Exited abnormally"))))))))

(defun nix26-flake--complete-template (prompt templates)
  (unless templates
    (user-error "The flake provides no template"))
  (setq nix26-flake-template-alist
        (mapcar (pcase-lambda (`(,name . ,alist))
                  (cons (symbol-name name) (alist-get 'description alist)))
                templates))
  (completing-read prompt
                   `(lambda (string pred action)
                      (if (eq action 'metadata)
                          '(metadata . ((category . nix26-registry-entry)
                                        (annotation-function . nix26-flake--annotate-template)))
                        (complete-with-action action ',(mapcar #'car nix26-flake-template-alist)
                                              string pred)))
                   nil t))

(defun nix26-flake--annotate-template (template)
  (if-let (cell (assoc template nix26-flake-template-alist))
      (concat " " (propertize (cdr cell) 'face 'font-lock-comment-face))
    ""))

(provide 'nix26-flake)
;;; nix26-flake.el ends here
