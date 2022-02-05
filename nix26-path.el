;;; nix26-path.el --- Path utilities -*- lexical-binding: t -*-

(defun nix26-path-normalize (dir)
  (string-remove-suffix "/" (file-truename dir)))

(provide 'nix26-path)
;;; nix26-path.el ends here
