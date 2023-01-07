;;; nix3-flake-lock.el --- flake.lock support -*- lexical-binding: t -*-

(require 'nix3-flake)
(require 'nix3-utils)
(require 'seq)
(require 'map)

(defvar magit-buffer-typearg)
(defvar magit-buffer-revision-hash)
(defvar magit-buffer-range-hashed)

(defconst nix3-flake-lock-range-re
  (rx bol (group (+ hex))
      ".."
      (group (+ hex))
      eol))

;; (defconst nix3-flake-lock-diff-buffer "*Nix3-Flake-Lock-Diff*")

;; (defun nix3-flake-lock-diff (base rev &optional subdir)
;;   (interactive (append (nix3-flake-lock--range)
;;                        (when (and nix3-flake-subflakes
;;                                   (equal (vc-git-root default-directory)
;;                                          default-directory))
;;                          (completing-read "Directory: "
;;                                           (cons "." nix3-flake-subflakes)))))
;;   (let* ((root (vc-git-root default-directory))
;;          (filename (file-relative-name (expand-file-name "flake.lock"
;;                                                          (unless (and subdir (not (equal subdir ".")))
;;                                                            subdir))
;;                                        root)))
;;     (with-current-buffer (get-buffer-create nix3-flake-lock-diff-buffer)
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (apply #'nix3-flake-lock--insert-diff
;;                (nix3-flake-lock--diff-entries root filename base rev))
;;         (read-only-mode t))
;;       (pop-to-buffer (current-buffer)))))

(defun nix3-flake-lock--diff-entries (root filename base rev)
  (pcase-exhaustive (mapcar (apply-partially #'nix3-flake-lock--entries-at
                                             root
                                             filename)
                            (list base rev))
    (`(,locks1 ,locks2)
     (let (added removed changed)
       (dolist (id (seq-difference (mapcar #'car locks2)
                                   (mapcar #'car locks1)))
         (push (assq id locks2) added))
       (dolist (id (seq-difference (mapcar #'car locks1)
                                   (mapcar #'car locks2)))
         (push (assq id locks1) removed))
       (dolist (id (seq-intersection (mapcar #'car locks2)
                                     (mapcar #'car locks1)))
         (unless (equal (assq id locks2)
                        (assq id locks1))
           (push (list id (assq id locks1) (assq id locks2)) changed)))
       (list :added added
             :removed removed
             :changed changed)))))

(defun nix3-flake-lock--entries-at (root filename &optional rev)
  (catch 'no-lock-file
    (with-temp-buffer
      (if rev
          (if-let* ((default-directory root)
                    (blob (nix3-flake-lock--blob root rev filename)))
              (unless (zerop (call-process nix3-git-executable nil (list t nil) nil
                                           "cat-file" "blob" blob))
                (error "git-cat-file failed on %s" blob))
            (throw 'no-lock-file t))
        (insert-file-contents (expand-file-name filename root)))
      (goto-char (point-min))
      (nix3-flake-lock--parse-buffer))))

(defun nix3-flake-lock--blob (root rev filename)
  (let ((default-directory root))
    (pcase-exhaustive rev
      (`stage
       (with-temp-buffer
         (unless (zerop (call-process nix3-git-executable nil (list t nil) nil
                                      "ls-files" "--stage" "--" filename))
           (error "git-ls-files failed"))
         (nth 1 (split-string (buffer-string) " "))))
      ((pred stringp)
       (with-temp-buffer
         (when (zerop (call-process nix3-git-executable nil (list t nil) nil
                                    "rev-parse" (concat rev ":" filename)))
           (string-chop-newline (buffer-string))))))))

(defun nix3-flake-lock--parse-buffer ()
  "Parse nodes in the buffer."
  (thread-last
    (json-parse-buffer :object-type 'alist)
    (alist-get 'nodes)
    (seq-filter #'nix3-flake-lock--locked-p)))

(defun nix3-flake-lock--locked-p (alist)
  (assq 'locked alist))

(defun nix3-flake-lock--range ()
  (cond
   (magit-buffer-range-hashed
    (when magit-buffer-range-hashed
      (if (string-match nix3-flake-lock-range-re magit-buffer-range-hashed)
          (list (match-string 1 magit-buffer-range-hashed)
                (match-string 2 magit-buffer-range-hashed))
        (error "Failed to match a revision range: %s" magit-buffer-range-hashed))))
   (magit-buffer-revision-hash
    (list (concat magit-buffer-revision-hash "^")
          magit-buffer-revision-hash))
   ((equal magit-buffer-typearg "--cached")
    (list "HEAD" 'stage))
   (t
    (list "HEAD" nil))))

(provide 'nix3-flake-lock)
;;; nix3-flake-lock.el ends here
