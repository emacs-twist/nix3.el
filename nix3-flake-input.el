;;; nix3-flake-input.el ---  -*- lexical-binding: t -*-

(require 'transient)
(require 'nix3-core)
(require 'nix3-utils)

;; Set to non-nil when the flake is not local.
(defvar nix3-flake-url)

(declare-function nix3-flake-show-url "nix3-flake")

(defvar nix3-flake-input-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") #'nix3-flake-input-return)
    map))

(defun nix3-flake-input--local-p ()
  "Return non-nil when the flake is local."
  (not nix3-flake-url))

(defun nix3-flake-input-return ()
  "Push the button at point or dispatch a transient interface."
  (interactive)
  (if (get-text-property (point) 'button)
      (push-button (point))
    (call-interactively #'nix3-flake-input-dispatch)))

(defun nix3-flake-input--original-url ()
  (thread-last
    (cdr (nix3-flake-input-at-point))
    (assq 'original)
    (cdr)
    (nix3-flake-ref-alist-to-url)))

(defun nix3-flake-input--locked-url ()
  (thread-last
    (cdr (nix3-flake-input-at-point))
    (assq 'locked)
    (cdr)
    (nix3-flake-ref-alist-to-url)))

(defun nix3-flake-input--last-modified ()
  (thread-last
    (cdr (nix3-flake-input-at-point))
    (assq 'locked)
    (cdr)
    (assq 'lastModified)
    (cdr)))

(defun nix3-flake-input--revision ()
  (thread-last
    (cdr (nix3-flake-input-at-point))
    (assq 'locked)
    (cdr)
    (assq 'rev)
    (cdr)))

(defun nix3-flake-input--html-url ()
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
    (to-url (assq 'original (cdr (nix3-flake-input-at-point))))))

(transient-define-prefix nix3-flake-input-dispatch ()
  [:description
   (lambda () (format "Original: %s" (nix3-flake-input--original-url)))
   ("d" "Show" nix3-flake-show-original-input)
   ("c" "Clone" ignore)]
  [:description
   (lambda () (format "Locked: %s (%s)"
                      (nix3-flake-input--locked-url)
                      (format-time-string "%F %R"
                                          (nix3-flake-input--last-modified))))
   ("u" "Update" nix3-flake-input-update :if nix3-flake-input--local-p)
   ("e" "Edit worktree" ignore)
   ("r" "Browse remote" nix3-flake-input-browse-remote)
   ("w" "Copy revision" nix3-flake-input-copy-revision)
   ("l" "Magit log" ignore)]
  (interactive)
  (unless (nix3-flake-input-at-point)
    (user-error "No flake input at point"))
  (transient-setup 'nix3-flake-input-dispatch))

(defun nix3-flake-input-at-point ()
  "Return a cons cell of (NAME . DATA) of the input."
  (when-let (section (magit-current-section))
    (when (eq (slot-value section 'type) 'flake-input)
      (oref section value))))

(defun nix3-flake-show-original-input ()
  (interactive)
  (nix3-flake-show-url (nix3-flake-input--original-url)))

(defun nix3-flake-show-locked-input ()
  (interactive)
  (nix3-flake-show-url (nix3-flake-input--locked-url)))

(defun nix3-flake-input-copy-revision ()
  (interactive)
  (kill-new (nix3-flake-input--revision))
  (message "Saved the revision into kill ring"))

(defun nix3-flake-input-browse-remote ()
  (interactive)
  (require 'nix3-browse-url)
  (funcall nix3-browse-url-for-repository
           (nix3-flake-input--html-url)))

;;;; Commands

(defun nix3-flake-input-update ()
  (interactive)
  (pcase (nix3-flake-input-at-point)
    (`(,name . ,_)
     ;; TODO This is a quick-and-dirty implementation, so rewrite it
     (nix3-run-process-background nix3-nix-executable
                                  "flake" "lock" "--update-input" name))
    (_
     (user-error "No input at point"))))

(provide 'nix3-flake-input)
;;; nix3-flake-input.el ends here
