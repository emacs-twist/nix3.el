;;; nix3-flake-input.el ---  -*- lexical-binding: t -*-

(require 'transient)
(require 'nix3-core)
(require 'nix3-utils)

;; Set to non-nil when the flake is not local.
(defvar nix3-flake-url)

(defvar nix3-flake-input-name nil)
(defvar nix3-flake-input-data nil)

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

(defun nix3-flake-input--original-url (&optional data)
  (thread-last (or data nix3-flake-input-data)
    (assq 'original)
    (cdr)
    (nix3-flake-ref-alist-to-url)))

(defun nix3-flake-input--locked-url (&optional data)
  (thread-last (or data nix3-flake-input-data)
    (assq 'locked)
    (cdr)
    (nix3-flake-ref-alist-to-url)))

(defun nix3-flake-input--last-modified (&optional data)
  (thread-last (or data nix3-flake-input-data)
    (assq 'locked)
    (cdr)
    (assq 'lastModified)
    (cdr)))

(transient-define-prefix nix3-flake-input-dispatch (name data)
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
   ("g" "Browse remote" ignore)
   ("w" "Copy revision" ignore)
   ("l" "Magit log" ignore)]
  (interactive (nix3-flake--input-at-point))
  (setq nix3-flake-input-name name
        nix3-flake-input-data data)
  (transient-setup 'nix3-flake-input-dispatch))

(defun nix3-flake--input-at-point ()
  (catch 'result
    (dolist (ov (overlays-at (point)))
      (when-let* ((data (overlay-get ov 'nix3-flake-input-data))
                  (name (overlay-get ov 'nix3-flake-input-name)))
        (throw 'result (list name data))))))

(defun nix3-flake-show-original-input (&optional data)
  (interactive)
  (nix3-flake-show-url (nix3-flake-input--original-url data)))

(defun nix3-flake-show-locked-input (&optional data)
  (interactive)
  (nix3-flake-show-url (nix3-flake-input--locked-url data)))

;;;; Commands

(defun nix3-flake-input-update (name &optional arg)
  (interactive (list nix3-flake-input-name
                     current-prefix-arg))
  ;; TODO This is a quick-and-dirty implementation, so rewrite it
  (nix3-run-process-background nix3-nix-executable
                                "flake"
                                "lock"
                                "--update-input"
                                (cl-etypecase name
                                  (string name)
                                  (symbol (symbol-name name)))))

(provide 'nix3-flake-input)
;;; nix3-flake-input.el ends here
