;;; nix3-browse-url.el --- Browse url wrappers -*- lexical-binding: t -*-

(defcustom nix3-browse-url-for-repository #'browse-url
  "`browse-url' function for source repositories."
  :type 'function)

(provide 'nix3-browse-url)
;;; nix3-browse-url.el ends here
