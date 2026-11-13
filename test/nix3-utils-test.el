;;; nix3-utils-test.el --- Tests for nix3-utils  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; URL: https://github.com/emacs-twist/nix3.el

;; This file is not part of GNU Emacs.

;;; License:

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.

;;; Commentary:

;; ERT tests for `nix3-git-url-to-flake-alist' and related utilities.

;;; Code:

(require 'ert)
(require 'nix3-utils)

(defconst nix3-utils-test--git-url-cases
  '(;; Dedicated forge flake types.
    ("https://github.com/emacs-twist/nix3.el"
     (type . "github") (owner . "emacs-twist") (repo . "nix3.el"))
    ("https://github.com/emacs-twist/nix3.el.git"
     (type . "github") (owner . "emacs-twist") (repo . "nix3.el"))
    ("https://github.com/emacs-twist/nix3.el/tree/master"
     (type . "github") (owner . "emacs-twist") (repo . "nix3.el"))
    ("https://github.com/emacs-twist/nix3.el/"
     (type . "github") (owner . "emacs-twist") (repo . "nix3.el"))
    ("https://gitlab.com/foo/bar.git"
     (type . "gitlab") (owner . "foo") (repo . "bar"))
    ("https://git.sr.ht/~foo/bar.git"
     (type . "sourcehut") (owner . "~foo") (repo . "bar"))
    ("git@github.com:emacs-twist/nix3.el.git"
     (type . "github") (owner . "emacs-twist") (repo . "nix3.el"))
    ("git@gitlab.com:foo/bar"
     (type . "gitlab") (owner . "foo") (repo . "bar"))
    ("git@git.sr.ht:~foo/bar"
     (type . "sourcehut") (owner . "~foo") (repo . "bar"))
    ("ssh://git@github.com/NixOS/nix.git"
     (type . "github") (owner . "NixOS") (repo . "nix"))
    ("ssh://git@gitlab.com:2222/foo/bar.git"
     (type . "gitlab") (owner . "foo") (repo . "bar"))
    ("https://user:password@github.com/foo/bar.git"
     (type . "github") (owner . "foo") (repo . "bar"))

    ;; Hosts without a dedicated fetcher use generic Git references.
    ("https://example.org/foo/bar.git"
     (type . "git") (url . "https://example.org/foo/bar.git"))
    ("ssh://git@example.org/foo/bar.git"
     (type . "git") (url . "ssh://git@example.org/foo/bar.git"))
    ("git@example.org:foo/bar.git"
     (type . "git") (url . "ssh://git@example.org/foo/bar.git"))
    ("git://github.com/edolstra/dwarffs?ref=unstable"
     (type . "git") (url . "git://github.com/edolstra/dwarffs?ref=unstable"))

    ;; git+ prefixes are removed here because they are added back by
    ;; `nix3-flake-ref-alist-to-url'.
    ("git+https://example.org/my/repo"
     (type . "git") (url . "https://example.org/my/repo"))
    ("git+https://example.org/my/repo?dir=flake1"
     (type . "git") (url . "https://example.org/my/repo?dir=flake1"))
    ("git+file:///home/my-user/some-repo/some-repo"
     (type . "git") (url . "file:///home/my-user/some-repo/some-repo"))
    ("git+https://github.com/foo/bar"
     (type . "github") (owner . "foo") (repo . "bar"))
    ("git+ssh://git@github.com/NixOS/nix?ref=v1.2.3"
     (type . "git") (url . "ssh://git@github.com/NixOS/nix?ref=v1.2.3"))

    ;; Queries force a generic Git reference because the compact forge
    ;; representation cannot express them.
    ("https://github.com/foo/bar?ref=main"
     (type . "git") (url . "https://github.com/foo/bar?ref=main"))

    ;; Anything else is kept as a generic Git reference.
    ("/home/my-user/some-repo/some-repo"
     (type . "git") (url . "/home/my-user/some-repo/some-repo")))
  "Test cases for `nix3-git-url-to-flake-alist'.")

(ert-deftest nix3-utils-test-git-url-to-flake-alist ()
  "Convert Git URLs into flake reference alists."
  (pcase-dolist (`(,url . ,expected) nix3-utils-test--git-url-cases)
    (ert-info ((format "Testing %s" url))
      (should (equal (nix3-git-url-to-flake-alist url)
                     expected)))))

(ert-deftest nix3-utils-test-git-url-to-flake-url ()
  "Format converted Git URLs as flake references."
  (should (equal (nix3-flake-ref-alist-to-url
                  (nix3-git-url-to-flake-alist
                   "git+https://example.org/my/repo?dir=flake1"))
                 "git+https://example.org/my/repo?dir=flake1"))
  (should (equal (nix3-flake-ref-alist-to-url
                  (nix3-git-url-to-flake-alist
                   "git+ssh://git@github.com/NixOS/nix?ref=v1.2.3"))
                 "git+ssh://git@github.com/NixOS/nix?ref=v1.2.3")))

(ert-deftest nix3-utils-test-git-url-build-clone-url ()
  "Build clone URLs from converted Git URLs."
  (should (equal (nix3-build-git-clone-url
                  (nix3-git-url-to-flake-alist
                   "git@github.com:foo/bar.git"))
                 "https://github.com/foo/bar.git"))
  (should (equal (nix3-build-git-clone-url
                  (nix3-git-url-to-flake-alist
                   "git@codeberg.org:foo/bar.git"))
                 "https://codeberg.org/foo/bar.git"))
  (should (equal (nix3-build-git-clone-url
                  (nix3-git-url-to-flake-alist
                   "git@example.org:foo/bar.git"))
                 "ssh://git@example.org/foo/bar.git")))

(provide 'nix3-utils-test)
;;; nix3-utils-test.el ends here
