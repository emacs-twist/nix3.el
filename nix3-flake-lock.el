;;; nix3-flake-lock.el --- flake.lock support -*- lexical-binding: t -*-

(require 'nix3-flake)
(require 'nix3-utils)
(require 'seq)
(require 'map)

(defvar magit-buffer-typearg)
(defvar magit-buffer-revision-hash)
(defvar magit-buffer-range-hashed)

(declare-function text-property-search-backward "text-property-search")

(defconst nix3-flake-lock-range-re
  (rx bol (group (+ hex))
      ".."
      (group (+ hex))
      eol))

;;;; Custom environment

(defcustom nix3-flake-lock-file-regexp
  (rx (or bol "/")
      "flake.lock"
      eol)
  "Pattern to match the file names of lock files.

The file should conform to the format of flake.lock."
  :group 'nix3-flake
  :type 'string)

;;;; Faces

(defface nix3-flake-lock-file-heading
  '((t (:inherit magit-diff-file-heading)))
  "Face for flake.lock file paths.")

(defface nix3-flake-lock-node-heading
  '((t (:inherit magit-section-heading)))
  "Face for flake.lock nodes.")

;;;;

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

;;;; Magit integration

(defvar nix3-flake-lock-diff-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'nix3-flake-lock-source-at-point)
    map))

;;;###autoload
(defun nix3-flake-lock-diff-section ()
  (when-let (files (nix3-flake-lock-magit-sections))
    (cl-flet*
        ((format-mtime (locked)
           (format-time-string "%Y-%m-%d" (alist-get 'lastModified locked)))
         (insert-node (old-or-new node)
           (insert (format "  %s: %s\n"
                           old-or-new
                           (nix3-flake-ref-alist-to-url
                            (alist-get 'locked node)))))
         (insert-node-change (event id &optional old new)
           (let ((start (point)))
             (magit-insert-section (flake-lock-node (list id (cdr old) (cdr new)) t)
               (magit-insert-heading
                 (propertize (format "%-8s %s" event id)
                             'face 'nix3-flake-lock-node-heading)
                 (cond
                  ((and new old)
                   (format " (%s -> %s)"
                           (format-mtime (alist-get 'locked old))
                           (format-mtime (alist-get 'locked new))))
                  (new
                   (format " (%s)"
                           (format-mtime (alist-get 'locked new))))))
               (let ((ov (make-overlay start (point))))
                 (overlay-put ov 'keymap nix3-flake-lock-diff-section-map))
               (when old
                 (insert-node "old" old))
               (when new
                 (insert-node "new" new))))))
      (pcase-exhaustive (nix3-flake-lock--range)
        (`(,lrev ,rrev)
         (dolist (file files)
           (magit-insert-section (flake-lock file)
             (magit-insert-heading (propertize file 'face 'nix3-flake-lock-file-heading))
             (pcase-exhaustive (nix3-flake-lock--diff-entries default-directory
                                                              file lrev rrev)
               ((map :added :removed :changed)
                (pcase-dolist (`(,id . ,new) added)
                  (insert-node-change "added" id nil new))
                (pcase-dolist (`(,id . ,old) removed)
                  (insert-node-change "removed" id old))
                (pcase-dolist (`(,id ,old ,new) changed)
                  (insert-node-change "changed" id old new)))))))))
    (insert ?\n)))

(defun nix3-flake-lock-source-at-point ()
  "Display the source at point."
  (interactive)
  (when-let (section (magit-current-section))
    (pcase (oref section value)
      (`(,_id ,old ,new)
       (cond
        ((and old new)
         (nix3-flake-git-log-source (alist-get 'original new)
                                    (list (concat (nix3-lookup-tree '(locked rev) old)
                                                  "..."
                                                  (nix3-lookup-tree '(locked rev) new)))))
        (old
         (nix3-flake-git-log-source (alist-get 'original old)
                                    (list (nix3-lookup-tree '(locked rev) old))))
        (new
         (nix3-flake-git-log-source (alist-get 'original new)
                                    (list (nix3-lookup-tree '(locked rev) new)))))))))

(defun nix3-flake-lock-magit-sections ()
  "Return flake.lock files specified in magit sections."
  (let (files)
    (save-excursion
      (while (text-property-search-backward 'magit-section)
        (let ((section (magit-current-section)))
          (when (and (eq (oref section type) 'file)
                     (string-match-p nix3-flake-lock-file-regexp
                                     (oref section value)))
            (push (oref section value) files)))))
    (seq-uniq files #'equal)))

(provide 'nix3-flake-lock)
;;; nix3-flake-lock.el ends here
