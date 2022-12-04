;;; nix3-browse-url.el --- Browse url wrappers -*- lexical-binding: t -*-

(require 'nix3-core)

(defcustom nix3-browse-url-for-repository #'browse-url
  "`browse-url' function for source repositories."
  :group 'nix3
  :type 'function)

(provide 'nix3-browse-url)
;;; nix3-browse-url.el ends here
