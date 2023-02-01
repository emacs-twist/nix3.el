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
   ("w" "Copy revision" nix3-flake-input-copy-revision)
   ;; ("l" "Magit log" ignore)
   ;; "Edit worktree"
   ]
  ["Update input"
   :if nix3-flake-input--updatable-p
   ("-c" "" "--accept-flake-config")
   ("ul" "Update to latest" nix3-flake-input-update)
   ("ur" "Update to revision" nix3-flake-input-update-to-rev)
   ("uf" "Update to ref" nix3-flake-input-update-to-ref)
   ("uu" "Update to url" nix3-flake-input-update-to-url)]
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

(defun nix3-flake-input-update (&optional url-or-alist)
  (interactive)
  (pcase (nix3-flake-input-at-point)
    (`(,name . ,_)
     ;; TODO This is a quick-and-dirty implementation, so rewrite it
     (apply #'nix3-run-process-background
            nix3-nix-executable
            "flake" "lock" "--update-input" name
            (append (transient-args 'nix3-flake-input-dispatch)
                    (pcase-exhaustive url-or-alist
                      (`nil)
                      ((pred stringp)
                       (list "--override-input" name url-or-alist))
                      ((pred sequencep)
                       (list "--override-input" name
                             (nix3-flake-ref-alist-to-url url-or-alist)))))))
    (_
     (user-error "No input at point"))))

(defun nix3-flake-input-update-to-rev (rev)
  (interactive "sRevision: ")
  (let ((alist (alist-get 'original (cdr (nix3-flake-input-at-point)))))
    (if-let (cell (assq 'rev alist))
        (setcdr cell rev)
      (setq alist (cons (cons 'rev rev) alist)))
    (nix3-flake-input-update alist)))

(defun nix3-flake-input-update-to-ref (ref)
  (interactive "sRef: ")
  (let ((alist (alist-get 'original (cdr (nix3-flake-input-at-point)))))
    (if-let (cell (assq 'ref alist))
        (setcdr cell ref)
      (setq alist (cons (cons 'ref ref) alist)))
    (nix3-flake-input-update alist)))

(defun nix3-flake-input-update-to-url (url)
  (interactive (let* ((input (nix3-flake-input-at-point))
                      (default (nix3-flake-ref-alist-to-url (cdr (assq 'original input)))))
                 (list (read-from-minibuffer (format-prompt
                                              (format "Update %s to flake url"
                                                      (car input))
                                              default)
                                             nil nil nil nil default))))
  (nix3-flake-input-update url))

(provide 'nix3-flake-input)
;;; nix3-flake-input.el ends here
