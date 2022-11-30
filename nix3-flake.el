;;; nix3-flake.el --- Support for Nix flakes -*- lexical-binding: t -*-

(require 'nix3-core)
(require 'nix3-repl)
(require 'nix3-utils)
(require 'nix3-registry)
(require 'nix3-section)

(require 's)
(require 'magit-section)
(require 'project)
(require 'promise)

(defgroup nix3-flake nil
  ""
  :prefix "nix3-flake-"
  :group 'nix3)

(defgroup nix3-flake-face nil
  "Faces for `nix3-flake-show-mode'."
  :group 'nix3-flake)

;;;; Constants

(defconst nix3-flake-show-error-buffer "*Nix-Flake-Show Errors*")
(defconst nix3-flake-metadata-error-buffer "*Nix-Flake-Metadata Errors*")
(defconst nix3-flake-init-buffer "*Nix-Flake-Init*")

;;;; Custom variables

(defcustom nix3-flake-show-sections
  '(nix3-flake-insert-metadata
    nix3-flake-insert-outputs
    nix3-flake-insert-inputs)
  ""
  :type 'hook)

(defcustom nix3-flake-toplevel-sections-unfolded t
  "Whether to unfold the top-level section by default."
  :type 'boolean)

(defcustom nix3-flake-input-name-max-width 20
  ""
  :type '(choice number (const nil)))

(defcustom nix3-flake-init-reverted-modes
  '(dired-mode
    magit-status-mode)
  "List of major modes that should be reverted after flake init."
  :type '(repeat symbol))

(defcustom nix3-flake-new-hook
  '(nix3-flake-git-init
    nix3-flake-remember-this-project)
  "Hook to run after `nix3-flake-new' scaffolds a new project.

Each function in this hook is called without arguments in the
created directory."
  :type 'hook)

(defcustom nix3-flake-extra-derivations
  ;; FIXME: Make it nil by default
  '("homeConfigurations.%s.*")
  "List of non-standard installables in the flake.

This is a list of attribute paths to outputs.
The following substitutes are made:

 * \"*\": Substituted with the name of an attribute in the
   parent. The parent should be an attribute set, and each
   attribute should be a derivation.

 * \"%s\": Substituted with the name of the system like
   \"x86_64-linux\".

The user should set this variable in \".dir-locals.el\" as
directory-local variables for per-project configuration."
  :type '(repeat string))

;;;; Variables

(defface nix3-flake-output-type-face
  '((t :inherit font-lock-constant-face))
  "Face for output types."
  :group 'nix3-flake-face)

(defface nix3-flake-output-parent-face
  '((t :inherit default))
  "Face for parents of outputs."
  :group 'nix3-flake-face)

(defface nix3-flake-output-name-face
  '((t :inherit magit-section-secondary-heading))
  "Face for output names."
  :group 'nix3-flake-face)

(defface nix3-flake-input-name-face
  '((t :inherit magit-section-secondary-heading))
  "Face for input names."
  :group 'nix3-flake-face)

(defface nix3-flake-flake-state-face
  '((t (:inherit font-lock-constant-face)))
  "Face for indicating whether an input is a flake."
  :group 'nix3-flake-face)

;;;; Button types

(define-button-type 'nix3-flake-url-link
  :supertype 'help-xref
  'help-function #'nix3-flake-show-url
  'help-echo (purecopy "mouse-2, RET: Show the flake"))

(define-button-type 'nix3-flake-remote-link
  :supertype 'help-xref
  'help-function #'nix3-flake-browse-remote
  'help-echo (purecopy "mouse-2, RET: Browse the remote url"))

;;;; Variables

(defvar nix3-flake-url nil
  "Set to the URL of a flake when the flake is not local.")

(defvar nix3-flake-show-history nil)

(defvar nix3-flake-template-history nil)

;;;; Small utilities

(defun nix3-flake--attr-path-string (path)
  (cl-flet ((attr-name-string (sym)
              (nix3-flake--escape-attr-name (symbol-name sym))))
    (mapconcat #'attr-name-string path ".")))

(defun nix3-flake--escape-attr-name (s)
  (if (string-match-p (rx bol (+ (any "-_" alnum)) eol) s)
      s
    (concat "\"" s "\"")))

(defun nix3-flake--path-p (url-alist)
  "Return non-nil if URL-ALIST points to a path."
  (equal "path" (assq 'type url-alist)))

(defun nix3-flake--resolve-path (path)
  (if (string-prefix-p "/" path)
      path
    (save-match-data
      (if (string-match (rx bol "./" (group (+ anything))) path)
          (let ((relative (match-string 1 path)))
            (cond
             ((and nix3-flake-url
                   (string-match-p (rx (any "=&") "dir=") nix3-flake-url))
              (error "The parent flake \"%s\" already has dir parameter, so it cannot be resolved"
                     nix3-flake-url))
             (nix3-flake-url
              (concat nix3-flake-url
                      (if (string-match-p (rx "?") nix3-flake-url)
                          "&"
                        "?")
                      "dir="
                      relative))
             (t
              (nix3-normalize-path (expand-file-name relative default-directory)))))
        (error "Failed to match against a path \"%s\"" path)))))

;;;; Browse remote

(defun nix3-flake-html-url (alist)
  (cl-labels
      ((to-url (alist)
         (let-alist alist
           (pcase \.type
             ("indirect"
              (require 'nix3-registry)
              (if-let (entry (thread-last
                               (nix3-registry--collect-entries)
                               (cl-remove-if (lambda (x)
                                               (equal (cdr (assq 'type (cddr x)))
                                                      "path")))
                               (assoc \.id)))
                  (to-url (cddr entry))
                (error "Failed to find a registry entry for %s" \.id)))
             ("github"
              (format "https://github.com/%s/%s/%s" \.owner \.repo
                      (if \.ref
                          (concat "tree/" \.ref)
                        "")))
             ("sourcehut"
              (format "https://git.sr.ht/%s/%s/%s" \.owner \.repo
                      (if \.ref
                          (concat "log/" \.ref)
                        "")))
             ("url"
              (let ((url (thread-last
                           \.url
                           (string-remove-prefix "git+")
                           (string-remove-suffix ".git"))))
                (if (string-prefix-p "https://" url)
                    url
                  (error "Not an https url, so cannot retrieve an HTML url: %s" url))))
             ("path"
              (error "Path entry, so cannot be accessed using URL"))
             (_
              (error "Unsupported scheme for HTML URL: %s" \.type))))))
    (to-url alist)))

;;;; nix flake show data

(defvar nix3-flake-show-results nil)

(defun nix3-flake--ensure-show-cache ()
  (unless nix3-flake-show-results
    (setq nix3-flake-show-results (make-hash-table :test #'equal))))

(defun nix3-flake-show--put (directory-or-url result)
  (nix3-flake--ensure-show-cache)
  (puthash directory-or-url result nix3-flake-show-results))

(defun nix3-flake-show--get (directory)
  (nix3-flake--ensure-show-cache)
  (gethash (string-remove-suffix "/" directory)
           nix3-flake-show-results))

;;;; nix flake metadata data

(defvar nix3-flake-metadata-results nil)

(defun nix3-flake--ensure-metadata-cache ()
  (unless nix3-flake-metadata-results
    (setq nix3-flake-metadata-results (make-hash-table :test #'equal))))

(defun nix3-flake-metadata--put (directory-or-url result)
  (nix3-flake--ensure-metadata-cache)
  (puthash directory-or-url result nix3-flake-metadata-results))

(defun nix3-flake-metadata--get (directory)
  (nix3-flake--ensure-metadata-cache)
  (gethash (string-remove-suffix "/" directory)
           nix3-flake-metadata-results))

;;;; Processing data

(defun nix3-flake--filter-outputs (command)
  "Return a list of apps and derivations for the system."
  (let* ((system (intern (nix3-system)))
         extra-derivations
         result)
    (cl-labels
        ((substitute-segment (s)
           (pcase s
             ("*" t)
             ("%s" system)
             (_ (intern s))))
         (decode-path (s)
           (mapcar #'substitute-segment (split-string s "\\.")))
         (prefixp (path1 path2)
           (equal path1 (take (length path1) path2)))
         (go (path tree)
           (pcase (alist-get 'type tree)
             (`nil
              (pcase-dolist (`(,key . ,subtree) tree)
                (unless (and (= 1 (length path))
                             (memq (car path) '(apps
                                                devShells
                                                packages
                                                checks))
                             (not (eq key system)))
                  (go (append path (list key)) subtree))))
             ((and (or "derivation"
                       (and "app"
                            (guard (equal command "run")))
                       (and "nixos-configuration"
                            (guard (equal command "build"))))
                   type)
              (push (cons (nix3-flake--attr-path-string path)
                          type)
                    result))
             ("unknown"
              (when-let (match (seq-find (apply-partially #'prefixp path) extra-derivations))
                (let ((rest (seq-drop match (length path))))
                  (go2 path rest))))))
         (go2 (path rest)
           (pcase (car rest)
             (`nil
              (push (cons (nix3-flake--attr-path-string path)
                          nil)
                    result))
             (`t
              (dolist (name (nix3-read-nix-json-command
                             "eval"
                             (concat (nix3-flake--buffer-url)
                                     "#" (nix3-flake--attr-path-string path))
                             "--apply" "builtins.attrNames"))
                (go2 (append path (list (intern name))) (cdr rest))))
             (name
              (when (nix3-read-nix-json-command
                     "eval"
                     (concat (nix3-flake--buffer-url)
                             "#" (nix3-flake--attr-path-string path))
                     "--apply"
                     (format "builtins.hasAttr \"%s\""
                             name))
                (go2 (append path (list name)) (cdr rest)))))))
      (setq extra-derivations (mapcar #'decode-path nix3-flake-extra-derivations))
      (go nil (nix3-flake--get-show-result)))
    result))

;;;; Magit sections

(defun nix3-flake-insert-metadata ()
  (magit-insert-section (flake-metadata nil nix3-flake-toplevel-sections-unfolded)
    (when-let (metadata (nix3-flake--get-metadata-result))
      (let-alist metadata
        (nix3-section-dlist 0
          nil
          ("Resolved URL: " (and \.resolvedUrl
                                 (not (equal \.resolvedUrl \.originalUrl)))
           (insert-text-button \.resolvedUrl
                               'type 'nix3-flake-remote-link
                               'help-args (list \.resolved)))
          ("Description:" .description
           (insert \.description))
          ("Revision:" .revision
           (insert \.revision))
          ("Last modified:" .lastModified
           (insert (nix3-format-timestamp \.lastModified))))))
    (insert ?\n)))

(put 'nix3-flake-insert-metadata 'nix3-loader #'nix3-flake--make-metadata-process)

(defun nix3-flake-insert-outputs ()
  (require 'nix3-flake-output)
  (magit-insert-section (flake-outputs nil nix3-flake-toplevel-sections-unfolded)
    (magit-insert-heading "Flake outputs")
    (nix3-section-with-keymap nix3-flake-output-map
      (let ((result (nix3-flake--get-show-result))
            (nix-system (nix3-system)))
        (pcase-dolist (`(,type-name . ,outputs)
                       (nix3-flake--group-outputs result))
          (magit-insert-section (flake-output-type type-name)
            (magit-insert-heading
              (make-string 2 ?\s)
              (propertize type-name 'face 'nix3-flake-output-type-face))
            (pcase-dolist (`(,branch-reverse . ,leaves)
                           (seq-group-by #'cdr outputs))
              (let* ((branch (reverse branch-reverse))
                     (name (symbol-name (car branch-reverse)))
                     (invisible (and (string-match-p (rx "-" (or "darwin" "linux") eol)
                                                     name)
                                     (not (equal name nix-system)))))
                (magit-insert-section (flake-output-subgroup
                                       (nix3-flake--attr-path-string branch)
                                       invisible)
                  (when branch
                    (magit-insert-heading
                      (make-string 4 ?\s)
                      (propertize (mapconcat #'symbol-name branch ".")
                                  'face 'nix3-flake-output-parent-face)))
                  (dolist (output-reverse leaves)
                    (let* ((path (reverse output-reverse))
                           (node (nix3-lookup-tree path result))
                           ;; (package-name (cdr (assq 'name node)))
                           (description (cdr (assq 'description node))))
                      (magit-insert-section (flake-output
                                             (nix3-flake--attr-path-string path))
                        (magit-insert-heading
                          (make-string 6 ?\s)
                          (apply #'propertize
                                 (symbol-name (car output-reverse))
                                 'nix-flake-output (mapconcat #'symbol-name
                                                              path ".")
                                 'nix-flake-show node
                                 'face 'nix3-flake-output-name-face
                                 (when description
                                   (list 'help-echo description))))))))))))))
    (insert ?\n)))

(put 'nix3-flake-insert-outputs 'nix3-loader #'nix3-flake--make-show-process)

(defun nix3-flake-insert-header (url)
  (insert (propertize "Flake: " 'face 'magit-section-heading))
  (if-let (metadata (nix3-flake-metadata--get url))
      (if (member (nix3-lookup-tree '(original type) metadata)
                  '("indirect" "path" "git"))
          (insert (cdr (assq 'originalUrl metadata)))
        (insert-text-button (cdr (assq 'originalUrl metadata))
                            'type 'nix3-flake-remote-link
                            'help-args (list (cdr (assq 'original metadata)))))
    (insert url))
  (insert "\n" ?\n))

(defun nix3-flake--group-outputs (root)
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

(defun nix3-flake-insert-inputs ()
  (require 'nix3-flake-input)
  (when-let (result (nix3-flake--get-metadata-result))
    (nix3-section-with-keymap nix3-flake-input-map
      (magit-insert-section (flake-inputs nil nix3-flake-toplevel-sections-unfolded)
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
                              (nix3-format--column-width
                               nix3-flake-input-name-max-width))))
            (cl-flet
                ((pad-column
                   (len s)
                   (s-pad-right len " " (s-truncate len s))))
              (pcase-dolist (`(,name . ,data) nodes)
                (magit-insert-section (flake-input (cons (symbol-name name)
                                                         data)
                                                   t)
                  (let* ((is-flake (not (eq :false (cdr (assq 'flake data)))))
                         (original (cdr (assq 'original data)))
                         (name-string (symbol-name name))
                         (url (nix3-flake-ref-alist-to-url original)))
                    (insert (make-string 2 ?\s)
                            (propertize (pad-column name-width name-string)
                                        'help-echo name-string
                                        'face 'nix3-flake-input-name-face)
                            " ")
                    (if is-flake
                        (insert-text-button url
                                            'type 'nix3-flake-url-link
                                            'help-args
                                            (list (if (nix3-flake--path-p original)
                                                      (nix3-flake--resolve-path
                                                       (cdr (assq 'path original)))
                                                    url)))
                      (insert url))
                    (insert "  "
                            (propertize (if is-flake
                                            "(flake)"
                                          "(non-flake)")
                                        'face 'nix3-flake-flake-state-face)
                            "\n")))))))
        (insert ?\n)))))

(put 'nix3-flake-insert-inputs 'nix3-loader #'nix3-flake--make-metadata-process)

;;;;; Button actions

(defun nix3-flake-browse-remote (alist)
  (require 'nix3-browse-url)
  (funcall nix3-browse-url-for-repository (nix3-flake-html-url alist)))

;;;; nix3-flake-show-mode

(defun nix3-flake-show-buffer (dir-or-url is-url)
  (let ((default-directory (if is-url
                               "~/"
                             (file-name-as-directory dir-or-url))))
    (with-current-buffer (get-buffer-create
                          (format "*Nix Flake<%s>*"
                                  (if is-url
                                      dir-or-url
                                    (file-name-nondirectory dir-or-url))))
      (nix3-flake-show-mode)
      (when is-url
        (setq-local nix3-flake-url dir-or-url))
      (nix3-flake-show-revert)
      (current-buffer))))

(defun nix3-flake-show-revert (&rest _args)
  "Revert the `nix3-flake-show' buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (flake)
      (magit-insert-heading)
      (nix3-flake-insert-header (nix3-flake--buffer-url))
      (run-hooks 'nix3-flake-show-sections)
      (goto-char (point-min)))))

(defun nix3-flake--buffer-url ()
  (or nix3-flake-url
      (nix3-normalize-path default-directory)))

(defun nix3-flake--get-show-result ()
  (nix3-flake-show--get (nix3-flake--buffer-url)))

(defun nix3-flake--get-metadata-result ()
  (nix3-flake-metadata--get (nix3-flake--buffer-url)))

(defvar nix3-flake-show-mode-map
  (let ((m (make-composed-keymap nil magit-section-mode-map)))
    (define-key m (kbd "l") #'nix3-flake-show-back)
    (define-key m (kbd "g") #'nix3-flake-show-revert)
    m))

(define-derived-mode nix3-flake-show-mode magit-section-mode
  "Nix Flake"
  (setq-local revert-buffer-function #'nix3-flake-show-revert)
  (read-only-mode 1))

;;;###autoload
(defun nix3-flake-show (dir)
  "Display information of the flake at DIR."
  (interactive (list (if (equal current-prefix-arg '(4))
                         (read-directory-name "Select a flake: ")
                       (or (locate-dominating-file default-directory
                                                   "flake.nix")
                           (nix3-flake-select-locally)))))
  (if (and dir
           (file-exists-p (expand-file-name "flake.nix" dir)))
      (let ((truename (nix3-normalize-path dir))
            (default-directory dir))
        (promise-chain (nix3-flake--get-promise truename nil)
          (then (lambda (_)
                  (nix3-flake-switch-to-buffer (nix3-flake-show-buffer truename nil))
                  (let (message-log-max)
                    (message "Fetched the flake"))))
          (promise-catch #'nix3-flake--show-process-error)))
    (user-error "Directory %s does not contain flake.nix" dir)))

(defun nix3-flake-select-locally ()
  "Select a flake directory on the file system."
  (project-prompt-project-dir))

;;;###autoload
(defun nix3-flake-show-url (url)
  "Display information of the flake at URL."
  (interactive (let ((ent (nix3-registry-complete "Flake: "
                                                  :require-match nil
                                                  :no-exact t)))
                 (list (if (stringp ent)
                           ent
                         (car ent)))))
  (message "Fetching a flake...")
  (promise-chain (nix3-flake--get-promise url t)
    (then (lambda (_)
            (nix3-flake-switch-to-buffer (nix3-flake-show-buffer url t))
            (let (message-log-max)
              (message "Fetched the flake"))))
    (promise-catch #'nix3-flake--show-process-error)))

(defun nix3-flake--get-promise (dir-or-url is-url &optional hook-var)
  (cl-flet
      ((make-loader (loader)
         (promise-new (apply-partially loader dir-or-url is-url)))
       (uniq (items)
         (cl-remove-duplicates items :test #'eq)))
    (promise-wait 0.5
      (thread-last
        (or hook-var nix3-flake-show-sections)
        (mapcar (lambda (func)
                  (when (symbolp func)
                    (get func 'nix3-loader))))
        (delq nil)
        (uniq)
        (mapcar #'make-loader)
        (apply #'vector)
        (promise-all)))))

(defun nix3-flake--show-process-error (payload)
  (let ((plist (cadr payload)))
    (message "Error from \"nix %s %s\": %s"
             (mapconcat #'shell-quote-argument (plist-get plist :subcommand) " ")
             (plist-get plist :url)
             (with-current-buffer (plist-get plist :error-buffer)
               (let ((case-fold-search t))
                 (save-excursion
                   (goto-char (point-max))
                   (when (re-search-backward (rx bol "error") nil t)
                     (buffer-substring (line-beginning-position)
                                       (line-end-position)))))))))

(cl-defmacro nix3-flake--nix-json-process (func-name &key name buffer stderr
                                                      subcommand
                                                      put-result)
  (declare (indent 1))
  `(defun ,func-name (url is-url resolve reject)
     (make-process :name ,name
                   :buffer ,buffer
                   :stderr ,stderr
                   :command (append (list nix3-nix-executable
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
                         (if-let (result (json-parse-buffer :object-type 'alist
                                                            :array-type 'list))
                             (progn
                               (,put-result url result)
                               (funcall resolve result))
                           (funcall reject (list :error-buffer ,stderr
                                                 :subcommand ',subcommand
                                                 :url url))))
                       (kill-buffer (process-buffer process)))
                      ((string-prefix-p "exited abnormally" event)
                       (funcall reject (list :error-buffer ,stderr
                                             :subcommand ',subcommand
                                             :url url))))))))

(nix3-flake--nix-json-process nix3-flake--make-show-process
  :name "Nix-Flake-Show-Json"
  :buffer (generate-new-buffer "*Nix Flake Show Output*")
  :stderr nix3-flake-show-error-buffer
  :subcommand ("flake" "show")
  :put-result nix3-flake-show--put)

(nix3-flake--nix-json-process nix3-flake--make-metadata-process
  :name "Nix-Flake-Metadata-Json"
  :buffer (generate-new-buffer "*Nix Flake Metadata Output*")
  :stderr nix3-flake-metadata-error-buffer
  :subcommand ("flake" "metadata")
  :put-result nix3-flake-metadata--put)

(defun nix3-flake-switch-to-buffer (buffer)
  (when (eq major-mode 'nix3-flake-show-mode)
    (push (current-buffer) nix3-flake-show-history))
  (switch-to-buffer buffer))

(defun nix3-flake-show-back ()
  "Go to the flake buffer."
  (interactive)
  (when (eq major-mode 'nix3-flake-show-mode)
    (when-let (buffer (pop nix3-flake-show-history))
      (switch-to-buffer buffer))))

;;;; nix flake init/new

;;;###autoload
(defun nix3-flake-init ()
  "Initialize the current project from a flake template."
  (interactive)
  (when (and (or (file-exists-p "flake.nix")
                 (project-current))
             (not (yes-or-no-p (format "Are you sure you want to run the template in \"%s\"?"
                                       default-directory))))
    (user-error "Aborted"))
  (nix3-flake--prompt-template "nix flake init: "
                               #'nix3-flake-init-with-template))

;;;###autoload
(defun nix3-flake-new ()
  "Create a new project from a flake template."
  (interactive)
  (nix3-flake--prompt-template "nix flake new: "
                               #'nix3-flake--new-with-template))

(defun nix3-flake--new-with-template (template)
  (let* ((dir (read-directory-name "New directory: "))
         (parent (file-name-directory (string-remove-suffix "/" dir))))
    (when (file-exists-p dir)
      (user-error "Directory already exists"))
    (unless (file-directory-p parent)
      (if (yes-or-no-p (format "Directory %s does not exist. Create it?" parent))
          (make-directory parent t)
        (user-error "Parent directory does not exist")))
    (nix3-flake--record-template template)
    (let ((default-directory parent))
      (nix3-flake--run-template `(lambda ()
                                   (let ((default-directory ,dir))
                                     (run-hooks 'nix3-flake-new-hook)
                                     (dired default-directory)))
                                "new" "-t" template (expand-file-name dir)))))

(defun nix3-flake--prompt-template (prompt callback)
  (let ((item (nix3-registry-complete prompt
                                      :add-to-registry t
                                      :require-match nil
                                      :extra-entries nix3-flake-template-history
                                      :no-exact t)))
    (if (nix3-flake--template-p item)
        (funcall callback item)
      (let ((name-or-url (if (stringp item)
                             item
                           (car item)))
            (url (if (stringp item)
                     item
                   (nix3-flake-ref-alist-to-url (cdr item)))))
        (promise-chain (promise-new (apply-partially #'nix3-flake--make-show-process
                                                     url t))
          (then `(lambda (_)
                   (let (message-log-max)
                     (message nil))
                   (concat ,name-or-url
                           "#"
                           (thread-last
                             (nix3-flake-show--get ,url)
                             ;; Since Nix 2.7, the default template is templates.default, so we
                             ;; won't consider defaultTemplate.
                             (alist-get 'templates)
                             (nix3-flake--complete-template ,prompt)))))
          (then callback)
          (promise-catch #'error))))))

(defun nix3-flake--template-p (url)
  (and (stringp url)
       (string-match-p "#" url)))

(defun nix3-flake-init-with-template (template)
  (nix3-flake--record-template template)
  (nix3-flake--run-template (lambda ()
                               (when (and nix3-flake-init-reverted-modes
                                          (apply #'derived-mode-p
                                                 nix3-flake-init-reverted-modes))
                                 (revert-buffer)))
                             "init" "-t" template))

(defun nix3-flake--record-template (template)
  (delq template nix3-flake-template-history)
  (push template nix3-flake-template-history))

(defun nix3-flake--run-template (success &rest args)
  ;; To set up a hook, we will use `start-process' rather than `compile'.
  (with-current-buffer (get-buffer-create nix3-flake-init-buffer)
    (erase-buffer))
  (message "nix3[%s]: %s" default-directory (mapconcat #'shell-quote-argument
                                                        args " "))
  (let ((proc (apply #'start-process
                     "nix3-flake-init" nix3-flake-init-buffer
                     nix3-nix-executable "flake" args)))
    (set-process-sentinel proc
                          (lambda (_proc event)
                            (cond
                             ((equal "finished\n" event)
                              (let ((message-log-max nil))
                                (message (with-current-buffer nix3-flake-init-buffer
                                           (buffer-string))))
                              (funcall success))
                             ((string-prefix-p "exited abnormally" event)
                              (display-buffer nix3-flake-init-buffer)
                              (let ((message-log-max nil))
                                (message "Exited abnormally"))))))))

(defun nix3-flake--complete-template (prompt templates)
  (unless templates
    (user-error "The flake provides no template"))
  (let ((template-alist (mapcar (lambda (cell)
                                  (cons (symbol-name (car cell))
                                        (alist-get 'description (cdr cell))))
                                templates)))
    (cl-labels
        ((annotator (candidate)
           (if-let (description (cdr (assoc template template-alist)))
               (concat " " description)
             ""))
         (group (candidate transform)
           (if transform
               candidate))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'nix3-registry-entry)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action candidates string pred))))
      (completing-read prompt #'completions nil t))))

;;;; Functions that can be added to nix3-flake-new-hook

(defun nix3-flake-git-init ()
  "Run git init in the current directory if there is no repository."
  (unless (locate-dominating-file default-directory ".git")
    (call-process "git" nil nil nil "init")))

(defun nix3-flake-remember-this-project ()
  "Remember the current project."
  (when-let (pr (project-current))
    (project-remember-project pr)))

(provide 'nix3-flake)
;;; nix3-flake.el ends here
