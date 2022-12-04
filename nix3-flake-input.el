;;; nix3-flake-input.el ---  -*- lexical-binding: t -*-

(require 'transient)
(require 'nix3-core)
(require 'nix3-utils)
(require 'nix3-browse-url)
(require 'subr-x)
(require 'magit-section)

;; Set to non-nil when the flake is not local.
(defvar nix3-flake-url)

(declare-function nix3-flake-show-url "nix3-flake")
(declare-function nix3-flake--get-metadata-result "nix3-flake")
(declare-function nix3-flake-html-url "nix3-flake")

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
    (alist-get 'original)
    (nix3-flake-ref-alist-to-url)))

(defun nix3-flake-input--locked-url ()
  (thread-last
    (cdr (nix3-flake-input-at-point))
    (alist-get 'locked)
    (nix3-flake-ref-alist-to-url)))

(defun nix3-flake-input--last-modified ()
  (thread-last
    (cdr (nix3-flake-input-at-point))
    (nix3-lookup-tree '(locked lastModified))))

(defun nix3-flake-input--revision ()
  (thread-last
    (cdr (nix3-flake-input-at-point))
    (nix3-lookup-tree '(locked rev))))

(defun nix3-flake-input--html-url ()
  (nix3-flake-html-url (assq 'original (cdr (nix3-flake-input-at-point)))))

(defun nix3-flake-input--direct-p ()
  (rassoc (car (nix3-flake-input-at-point))
          (thread-last
            (nix3-flake--get-metadata-result)
            (nix3-lookup-tree '(locks nodes root inputs)))))

(defun nix3-flake-input--updatable-p ()
  (and (nix3-flake-input--local-p)
       (nix3-flake-input--direct-p)))

(transient-define-prefix nix3-flake-input-dispatch ()
  [:description
   (lambda () (format "Original: %s" (nix3-flake-input--original-url)))
   ("d" "Show the flake" nix3-flake-show-original-input)
   ("r" "Browse remote" nix3-flake-input-browse-remote)
   ("c" "Clone" ignore)]
  [:description
   (lambda () (format "Locked: %s (%s)"
                      (nix3-flake-input--locked-url)
                      (format-time-string "%F %R"
                                          (nix3-flake-input--last-modified))))
   ("u" "Update" nix3-flake-input-update :if nix3-flake-input--updatable-p)
   ("e" "Edit worktree" ignore)
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
