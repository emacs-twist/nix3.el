;;; nix3-transient.el --- Transient interface -*- lexical-binding: t -*-

(require 'nix3-flake)

(defvar-local nix3-transient-flake nil
  "Location of the flake.")

;; This command must be invoked after fetching data, so it should be private.
(transient-define-prefix nix3-transient--dispatch ()
  [:if-not
   nix3-transient--show-mode-p
   :description
   nix3-transient--flake-description
   ("h" "Show metadata and outputs" nix3-transient-show)]
  ["Nix commands"
   :class transient-row
   ("b" "build" nix3-build)
   ("r" "run" nix3-run)]
  (interactive)
  (unless nix3-transient-flake
    (user-error "Variable nix3-transient-flake must be set in advance"))
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

(provide 'nix3-transient)
;;; nix3-transient.el ends here
