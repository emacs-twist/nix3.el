;;; nix26-core.el ---  -*- lexical-binding: t -*-

(defgroup nix26 nil
  ""
  :prefix "nix26-"
  :group 'nix26)

(defcustom nix26-nix-executable "nix"
  ""
  :type 'file)

(defun nix26-read-nix-command (&rest args)
  "Run nix and return its output string."
  (with-temp-buffer
    (let ((err-file (make-temp-file "nix26")))
      (unwind-protect
          (unless (zerop (apply #'call-process
                                nix26-nix-executable
                                nil (list (current-buffer) err-file) nil
                                args))
            (error "Failed to run %s: %s"
                   (cons nix26-nix-executable args)
                   (with-temp-buffer
                     (insert-file-contents err-file)
                     (buffer-string))))
        (delete-file err-file)))
    (buffer-string)))

(defun nix26-system ()
  "Return the system name of Nix."
  (nix26-read-nix-command "eval" "--expr" "builtins.currentSystem" "--impure"
                          "--raw"))

(defun nix26-normalize-path (dir)
  (string-remove-suffix "/" (file-truename dir)))

(provide 'nix26-core)
;;; nix26-core.el ends here
