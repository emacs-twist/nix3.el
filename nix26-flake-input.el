;;; nix26-flake-input.el ---  -*- lexical-binding: t -*-

(require 'transient)

(defvar nix26-flake-input-name nil)
(defvar nix26-flake-input-data nil)

(defvar nix26-flake-input-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") #'nix26-flake-input-return)
    map))

(defun nix26-flake-input-return ()
  "Push the button at point or dispatch a transient interface."
  (interactive)
  (if (get-text-property (point) 'button)
      (push-button (point))
    (call-interactively #'nix26-flake-input-dispatch)))

(defun nix26-flake-input--original-url (&optional data)
  (thread-last (or data nix26-flake-input-data)
    (assq 'original)
    (cdr)
    (nix26-flake--alist-to-url)))

(defun nix26-flake-input--locked-url (&optional data)
  (thread-last (or data nix26-flake-input-data)
    (assq 'locked)
    (cdr)
    (nix26-flake--alist-to-url)))

(defun nix26-flake-input--last-modified (&optional data)
  (thread-last (or data nix26-flake-input-data)
    (assq 'locked)
    (cdr)
    (assq 'lastModified)
    (cdr)))

(transient-define-prefix nix26-flake-input-dispatch (name data)
  [:description
   (lambda () (format "Input name: %s" nix26-flake-input-name))
   ("u" "Update" ignore)]
  [:description
   (lambda () (format "Original: %s" (nix26-flake-input--original-url)))
   ("d" "Show" nix26-flake-show-original-input)
   ("c" "Clone" ignore)]
  [:description
   (lambda () (format "Locked: %s (%s)"
                      (nix26-flake-input--locked-url)
                      (format-time-string "%F %R"
                                          (nix26-flake-input--last-modified))))
   ("e" "Edit worktree" ignore)
   ("g" "Browse remote" ignore)
   ("w" "Copy revision" ignore)
   ("l" "Magit log" ignore)]
  (interactive (nix26-flake--input-at-point))
  (setq nix26-flake-input-name name
        nix26-flake-input-data data)
  (transient-setup 'nix26-flake-input-dispatch))

(defun nix26-flake--input-at-point ()
  (catch 'result
    (dolist (ov (overlays-at (point)))
      (when-let* ((data (overlay-get ov 'nix26-flake-input-data))
                  (name (overlay-get ov 'nix26-flake-input-name)))
        (throw 'result (list name data))))))

(defun nix26-flake-show-original-input (&optional data)
  (interactive)
  (nix26-flake-show-url (nix26-flake-input--original-url data)))

(defun nix26-flake-show-locked-input (&optional data)
  (interactive)
  (nix26-flake-show-url (nix26-flake-input--locked-url data)))

(provide 'nix26-flake-input)
;;; nix26-flake-input.el ends here
