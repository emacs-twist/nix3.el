;;; nix3-flake.el --- Support for Nix flakes -*- lexical-binding: t -*-

(require 'nix3-core)
(require 'nix3-utils)
(require 'nix3-registry)
(require 'nix3-section)

(require 's)
(require 'magit-section)
(require 'project)
(require 'promise)
(require 'help-mode)

(declare-function nix3-transient "nix3-transient")
(declare-function nix3-transient-on-output "nix3-transient")
(declare-function vc-git-root "vc-git")
(declare-function bookmark-prop-get "bookmark")
(declare-function magit-init "ext:magit-status")
(defvar nix3-transient-flake-output-type)
(defvar nix3-transient-flake-output)
(defvar nix3-browse-url-for-repository)
(defvar nix3-flake-input-map)

(defgroup nix3-flake nil
  "Interactive interface to Nix flakes."
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

(defcustom nix3-flake-wait 2
  "Number of seconds to wait for nix flake show/metadata to return."
  :type 'number)

(defcustom nix3-flake-remote-wait 10
  "Number of seconds to wait for data from a remote repository."
  :type 'number)

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

(defcustom nix3-flake-init-function
  (lambda () (nix3-flake-init :no-confirm t))
  "Function called when the user wants to initialize a Nix project.

The function is called without argument, e.g. from
`nix3-transient', if the project doesn't contain flake.nix and
the user wants to initialize a Nix project."
  :type 'function)

(defcustom nix3-flake-init-reverted-modes
  '(dired-mode
    magit-status-mode)
  "List of major modes that should be reverted after flake init."
  :type '(repeat symbol))

(defcustom nix3-flake-default-json-options
  '("--show-trace"
    "--allow-import-from-derivation"
    "--accept-flake-config")
  "List of Nix options used when retrieving data."
  :type '(repeat string))

(defcustom nix3-flake-new-hook
  '(nix3-flake-git-init
    nix3-flake-git-add
    nix3-flake-remember-this-project)
  "Hook to run after `nix3-flake-new' scaffolds a new project.

Each function in this hook is called without arguments in the
created directory."
  :type 'hook)

(defcustom nix3-flake-edit-find-file-fn #'find-file
  "Function used to open flake.nix for editing.

This command is used in `nix3-flake-edit' command.

The function is called with the file name as an argument."
  :type 'function
  :options '(find-file
             find-file-other-window))

(defcustom nix3-flake-git-init-command #'magit-init
  "Interactive function used to initialize a Git repository.

The command should be run synchronously and should set
`default-directory' to the root of the created repository when it
is finished."
  :type 'function)

(defcustom nix3-flake-worktree-promise-fn #'ignore
  "Function that returns a promise to a worktree.

The function should take a url alist as an argument and return a
promise or nil. The promise should resolve to a directory when
the worktree becomes available."
  :type 'function)

(defcustom nix3-flake-extra-derivations nil
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
  '((t (:inherit font-lock-comment-face :weight bold)))
  "Face for indicating the input is a flake."
  :group 'nix3-flake-face)

(defface nix3-flake-non-flake-state-face
  '((t (:inherit font-lock-comment-face)))
  "Face for indicating the input is not a flake."
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

(cl-defun nix3-flake-location (&key allow-missing local dir)
  "Return the URL or path to the current flake.

If the current buffer is not on a remote flake and ALLOW-MISSING
is nil, the function throws an error if there is no flake.nix.

If LOCAL is non-nil, the function only returns a flake on path.

If DIR is non-nil, the function returns a flake at or above the
directory. It implies LOCAL."
  (or (unless (or local dir)
        nix3-flake-url)
      (if-let (root (locate-dominating-file (or dir default-directory) "flake.nix"))
          (nix3-normalize-path root)
        (unless allow-missing
          (error "No flake.nix is found")))))

;;;; nix eval

(cl-defun nix3-flake-eval-nix (attr &key apply)
  (apply #'nix3-read-nix-command
         "eval"
         (concat (or nix3-flake-url ".") "#" attr)
         (when apply
           (list "--apply" apply))))

(cl-defun nix3-flake-eval-json (attr &key apply)
  (apply #'nix3-read-nix-json-command
         "eval"
         (concat (or nix3-flake-url ".") "#" attr)
         "--json"
         (when apply
           (list "--apply" apply))))

(cl-defun nix3-flake-eval-raw (attr &key apply)
  (apply #'nix3-read-nix-command
         "eval"
         (concat (or nix3-flake-url ".") "#" attr)
         "--raw"
         (when apply
           (list "--apply" apply))))

;;;; Browse remote

(defun nix3-flake-html-url (alist)
  (cl-labels
      ((check-url (url)
         (if (string-prefix-p "https://" url)
             url
           (error "Not an https url, so cannot retrieve an HTML url: %s" url)))
       (to-url (alist)
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
             ("gitlab"
              (format "https://gitlab.com/%s/%s/%s" \.owner \.repo
                      (if \.ref
                          (concat "tree/" \.ref)
                        "")))
             ("sourcehut"
              (format "https://git.sr.ht/%s/%s/%s" \.owner \.repo
                      (if \.ref
                          (concat "log/" \.ref)
                        "")))
             ("url"
              (check-url (thread-last
                           \.url
                           (string-remove-prefix "git+")
                           (string-remove-suffix ".git"))))
             ("git"
              (check-url \.url))
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

(defun nix3-flake-demand-outputs ()
  (promise-new (apply-partially
                #'nix3-flake--make-show-process
                (string-remove-suffix
                 "/"
                 (file-truename (locate-dominating-file default-directory "flake.nix")))
                nil)))

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
                            (guard (equal command "run"))))
                   type)
              (push (cons (nix3-flake--attr-path-string path)
                          type)
                    result))
             ((and "nixos-configuration"
                   (guard (equal command "build"))
                   type)
              (push (cons (concat (nix3-flake--attr-path-string path)
                                  ".config.system.build.toplevel")
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
                             (concat (nix3-flake-location)
                                     "#" (nix3-flake--attr-path-string path))
                             "--apply" "builtins.attrNames"))
                (go2 (append path (list (intern name))) (cdr rest))))
             (name
              (when (nix3-read-nix-json-command
                     "eval"
                     (concat (nix3-flake-location)
                             "#" (nix3-flake--attr-path-string path))
                     "--apply"
                     (format "builtins.hasAttr \"%s\""
                             name))
                (go2 (append path (list name)) (cdr rest)))))))
      (setq extra-derivations (mapcar #'decode-path nix3-flake-extra-derivations))
      (go nil (nix3-flake--get-show-result)))
    result))

;;;; Interaction

(defun nix3-flake-select-output (prompt-format command &optional default-value)
  (promise-wait (if nix3-flake-url
                    nix3-flake-remote-wait
                  nix3-flake-wait)
    (nix3-flake-demand-outputs))
  (let ((alist (nix3-flake--filter-outputs command)))
    (cl-labels
        ((group (candidate transform)
           (if transform
               candidate
             (or (cdr (assoc candidate alist))
                 "")))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'nix3-attribute)
                           (cons 'group-function #'group)))
             (complete-with-action action alist string pred))))
      (completing-read (format prompt-format
                               (or nix3-flake-url (abbreviate-file-name default-directory)))
                       #'completions
                       nil nil nil nil default-value))))

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

(defvar nix3-flake-output-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'nix3-flake-output-return)
    ;; (define-key map "a" #'nix3-transient-explore-output)
    ;; (define-key map "b" #'nix3-flake-output-build)
    ;; (define-key map "r" #'nix3-flake-output-run)
    map))

(defun nix3-flake-output-return ()
  (interactive)
  (when-let (output (nix3-flake-output-path-at-point))
    (setq nix3-transient-flake-output output)
    (setq nix3-transient-flake-output-type (nix3-flake-output-type))
    (nix3-transient-on-output)))

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
                  (forward-line -1)))
            (throw 'output-type nil)))))))

(defun nix3-flake-insert-outputs ()
  (require 'nix3-transient)
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

(defun nix3-flake--direct-inputs ()
  (if-let (result (nix3-flake--get-metadata-result))
      (let* ((nodes (nix3-lookup-tree '(locks nodes) result))
             (names (mapcar #'car (nix3-lookup-tree '(root inputs) nodes))))
        (mapcar (lambda (name)
                  (assq name nodes))
                names))
    (nix3-lookup-tree '(locks nodes root inputs) result)
    (error "No flake metadata")))

(defun nix3-flake-insert-inputs ()
  (require 'nix3-flake-input)
  (when-let (result (nix3-flake--get-metadata-result))
    (nix3-section-with-keymap nix3-flake-input-map
      (magit-insert-section (flake-inputs nil nix3-flake-toplevel-sections-unfolded)
        (magit-insert-heading "Flake inputs")
        (when-let* ((nodes (nix3-lookup-tree '(locks nodes) result))
                    (other-nodes (assq-delete-all 'root (copy-sequence nodes))))
          (let ((name-width (thread-last
                              other-nodes
                              (mapcar (lambda (cell)
                                        (symbol-name (car cell))))
                              (nix3-format--column-width
                               nix3-flake-input-name-max-width)))
                (direct-inputs (nix3-lookup-tree '(root inputs) nodes)))
            (cl-flet
                ((pad-column
                   (len s)
                   (s-pad-right len " " (s-truncate len s)))
                 (direct-input-p (node)
                   (and (assq (car node) direct-inputs)
                        t))
                 (compare-bool (x _y)
                   x))
              (pcase-dolist (`(,group . ,group-nodes) (thread-last
                                                        other-nodes
                                                        (seq-group-by #'direct-input-p)
                                                        (seq-sort-by #'car #'compare-bool)))
                (magit-insert-section (input-group group (not group))
                  (magit-insert-heading
                    (make-string 2 ?\s)
                    (if group
                        "Direct inputs"
                      "Indirect inputs"))
                  (pcase-dolist (`(,name . ,data) group-nodes)
                    (magit-insert-section (flake-input (cons (symbol-name name)
                                                             data)
                                                       (not (assq name direct-inputs)))
                      (let* ((is-flake (not (eq :false (cdr (assq 'flake data)))))
                             (original (cdr (assq 'original data)))
                             (name-string (symbol-name name))
                             (url (nix3-flake-ref-alist-to-url original)))
                        (insert (make-string 3 ?\s)
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
                                (if is-flake
                                    (propertize "(flake)"
                                                'face 'nix3-flake-flake-state-face)
                                  (propertize "(non-flake)"
                                              'face 'nix3-flake-non-flake-state-face))
                                "\n")))))))))
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
  (nix3-flake--get-promise (nix3-flake-location) nix3-flake-url)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (flake)
      (magit-insert-heading)
      (nix3-flake-insert-header (nix3-flake--buffer-url))
      (run-hooks 'nix3-flake-show-sections)
      (goto-char (point-min)))))

(defun nix3-flake--buffer-url ()
  "Return the flake for the current `nix-flake-show' buffer."
  (or nix3-flake-url
      (nix3-normalize-path default-directory)))

(defun nix3-flake--get-show-result ()
  (nix3-flake-show--get (nix3-flake-location)))

(defun nix3-flake--get-metadata-result ()
  (nix3-flake-metadata--get (nix3-flake-location)))

(defvar nix3-flake-show-mode-map
  (let ((m (make-composed-keymap nil magit-section-mode-map)))
    (define-key m "l" #'nix3-flake-show-back)
    (define-key m "g" #'nix3-flake-show-revert)
    (define-key m "?" #'nix3-transient)
    m))

(define-derived-mode nix3-flake-show-mode magit-section-mode
  "Nix Flake"
  (setq-local revert-buffer-function #'nix3-flake-show-revert)
  (setq-local bookmark-make-record-function #'nix3-flake-show-bookmark-record)
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
          (promise-catch #'nix3-flake--handle-process-error)))
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
  (promise-chain (nix3-flake--get-promise url t :wait nix3-flake-remote-wait)
    (then (lambda (_)
            (nix3-flake-switch-to-buffer (nix3-flake-show-buffer url t))
            (let (message-log-max)
              (message "Fetched the flake"))))
    (promise-catch #'nix3-flake--handle-process-error)))

(cl-defun nix3-flake--get-promise (dir-or-url is-url &key sections wait)
  (cl-flet
      ((make-loader (loader)
         (promise-new (apply-partially loader dir-or-url is-url)))
       (uniq (items)
         (cl-remove-duplicates items :test #'eq)))
    (promise-wait (or wait nix3-flake-wait)
      (thread-last
        (or sections nix3-flake-show-sections)
        (mapcar (lambda (func)
                  (when (symbolp func)
                    (get func 'nix3-loader))))
        (delq nil)
        (uniq)
        (mapcar #'make-loader)
        (apply #'vector)
        (promise-all)))))

(defun nix3-flake-run-section (sections dir-or-url is-url)
  (nix3-flake--get-promise dir-or-url is-url
                           :sections (symbol-value sections))
  (run-hooks sections))

(defun nix3-flake--handle-process-error (payload)
  (pcase payload
    (`(:timeouted)
     (error "Timeout while fetching the Nix flake"))
    (`(:rejected ,plist)
     (message "Error from \"nix %s %s\": %s"
              (mapconcat #'shell-quote-argument (plist-get plist :subcommand) " ")
              (plist-get plist :url)
              (with-current-buffer (plist-get plist :error-buffer)
                (let ((case-fold-search t))
                  (save-excursion
                    (goto-char (point-max))
                    (when (re-search-backward (rx bol "error") nil t)
                      (buffer-substring (line-beginning-position)
                                        (line-end-position))))))))))

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
                                          "--json")
                                    nix3-flake-default-json-options
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

;;;; Bookmark integration

(defun nix3-flake-show-bookmark-record ()
  "Return a bookmark record for the `nix3-flake-show-mode' buffer."
  (list (concat "flake:" (or nix3-flake-url
                             (string-remove-suffix "/" (abbreviate-file-name default-directory))))
        '(handler . nix3-flake-show-bookmark-handler)
        (if nix3-flake-url
            `(url . ,nix3-flake-url)
          `(default-directory . ,(abbreviate-file-name default-directory)))))

;;;###autoload
(defun nix3-flake-show-bookmark-handler (bookmark)
  (if-let (url (bookmark-prop-get bookmark 'url))
      (nix3-flake-show-url url)
    (if-let (dir (bookmark-prop-get bookmark 'default-directory))
        (nix3-flake-show dir)
      (error "This bookmark handler requires either url or default-directory property"))))

;;;; nix flake init/new

;;;###autoload
(cl-defun nix3-flake-init (&key no-confirm)
  "Initialize the current project from a flake template.

If NO-CONFIRM is non-nil, "
  (interactive)
  (when (and (or (file-exists-p "flake.nix")
                 (project-current))
             (not (or no-confirm
                      (yes-or-no-p (format "Are you sure you want to run the template in \"%s\"?"
                                           default-directory)))))
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
                   (if-let (templates (thread-last
                                        (nix3-flake-show--get ,url)
                                        ;; Since Nix 2.7, the default template is templates.default, so we
                                        ;; won't consider defaultTemplate.
                                        (alist-get 'templates)))
                       (concat ,name-or-url
                               "#" (nix3-flake--complete-template ,prompt templates))
                     (error "No template"))))
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
           (if-let (description (cdr (assoc candidate template-alist)))
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
             (complete-with-action action template-alist string pred))))
      (completing-read prompt #'completions nil t))))

;;;###autoload
(defun nix3-flake-edit ()
  "Edit flake.nix in the current repository.

This command discovers a flake.nix file closest to the current
buffer and edit it. To open the buffer in a different window,
customize `nix3-flake-edit-find-file-fn'.

If there is no flake.nix found, this command initializes a new
flake by running `nix3-flake-init' command in the root of the Git
repository.

If there is neither a flake.nix nor a Git repository, this
function first initializes a Git repository by running
`nix3-flake-git-init-command' which defaults to `magit-init', and
then runs `nix3-flake-init'."
  (interactive)
  (when nix3-flake-url
    (user-error "You must run this command inside a local flake"))
  (if-let (dir (locate-dominating-file default-directory "flake.nix"))
      (funcall nix3-flake-edit-find-file-fn (expand-file-name "flake.nix" dir))
    (let ((default-directory (or (vc-git-root default-directory)
                                 (progn
                                   (call-interactively nix3-flake-git-init-command)
                                   default-directory))))
      (call-interactively #'nix3-flake-init))))

;;;; Functions that can be added to nix3-flake-new-hook

(defun nix3-flake-git-init ()
  "Run git init in the current directory if there is no repository."
  (unless (locate-dominating-file default-directory ".git")
    (call-process "git" nil nil nil "init")))

(defun nix3-flake-git-add ()
  "Add flake.nix (and flake.lock) if it exists."
  (when (file-exists-p "flake.nix")
    (call-process "git" nil nil nil "add" "flake.nix")
    (when (file-exists-p "flake.lock")
      (call-process "git" nil nil nil "add" "flake.lock"))))

(defun nix3-flake-remember-this-project ()
  "Remember the current project."
  (when-let (pr (project-current))
    (project-remember-project pr)))

;;;; Manage relationships between remote repositories and local copies

(defun nix3-flake-git-log-source (url-alist revs)
  (promise-chain (or (funcall nix3-flake-worktree-promise-fn
                              (nix3-registry--non-indirect url-alist))
                     (error "No promise"))
    (then `(lambda (dir)
             (let ((default-directory dir))
               ;; TODO: Fetch data
               (magit-log-other ',revs))))
    (promise-catch #'nix3-flake--handle-repo-error)))

(defun nix3-flake--handle-repo-error (payload)
  (error "Error in nix3-flake--handle-repo-error: %s" payload))

;;;; Commands on a flake

;;;###autoload
(defun nix3-flake-update-inputs (dir &optional names)
  "Update inputs of a flake on filesystem.

You can use this function to build your own commands that is used
to update inputs of a flake.

DIR is a directory of the flake.

Optionally, you can pass NAMES explicitly to update inputs
non-interactively."
  (interactive "DFlake: ")
  (promise-wait nix3-flake-wait
    (promise-new (apply-partially
                  #'nix3-flake--make-metadata-process
                  (nix3-flake-location :dir dir :local t)
                  nil)))
  (let* ((default-directory dir)
         (names (or names
                    (completing-read-multiple (format "Select inputs at %s: " dir)
                                              (nix3-flake--direct-inputs)))))
    (compile (concat "nix flake lock "
                     (mapconcat (lambda (name)
                                  (format "--update-input %s" name))
                                names " ")))))

(provide 'nix3-flake)
;;; nix3-flake.el ends here
