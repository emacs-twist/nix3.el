;;; nix26-core.el ---  -*- lexical-binding: t -*-

(defgroup nix26 nil
  ""
  :prefix "nix26-"
  :group 'nix26)

(defcustom nix26-nix-executable "nix"
  ""
  :type 'file)

(defun nix26-system ()
  "Return the system name of Nix."
  (with-temp-buffer
    (unless (zerop (call-process nix26-nix-executable
                                 nil (list (current-buffer) nil) nil
                                 "eval"
                                 "--expr" "builtins.currentSystem"
                                 "--impure" "--raw"))
      (error "Failed to run nix eval"))))

(provide 'nix26-core)
;;; nix26-core.el ends here
