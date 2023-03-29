;;; nix3-help.el --- Parsing data from help -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'nix3-core)

(defvar nix3-help-command-alist nil)

(defun nix3-help-parse (type &rest subcommands)
  (pcase-exhaustive (cl-ecase type
                      (commands (list (rx " commands:" eol)
                                      (rx "Synopsis")
                                      (unless subcommands
                                        (rx "Examples"))
                                      (format "· %s "
                                              (string-join (cons "nix" subcommands)
                                                           " "))
                                      (rx bol (group (+ anything)) " - "
                                          (group (+ anychar)))))
                      (options (list (rx " options:" eol)
                                     (rx "Options")
                                     nil
                                     "· "
                                     (rx bol (group "--" (+ (not (any blank))))
                                         (?  (and blank "/"
                                                  blank "-" (any alpha)))
                                         eol))))
    (`(,regexp ,start-heading ,end-heading ,needle ,capture)
     (with-temp-buffer
       (unless (zerop (apply #'call-process
                             nix3-nix-executable nil (list t nil) nil
                             (append subcommands (list "--help"))))
         (error "Failed to parse nix --help"))
       (goto-char (point-min))
       (re-search-forward start-heading)
       (delete-region (point-min) (pos-eol))
       (when end-heading
         (re-search-forward end-heading)
         (delete-region (pos-bol) (point-max)))
       (goto-char (point-min))
       ;; Parse outputs
       (while (re-search-forward regexp nil t)
         (delete-region (pos-bol) (pos-eol)))
       (goto-char (point-min))
       (let (last-end
             outputs)
         (while (search-forward needle nil t)
           (let ((start (match-beginning 0))
                 (end (match-end 0)))
             (when last-end
               (let ((string (string-trim-right (buffer-substring-no-properties
                                                 last-end start))))
                 (when (string-match capture string)
                   (push (cons (match-string 1 string)
                               (when-let (desc (match-string 2 string))
                                 (replace-regexp-in-string
                                  (rx (+ space)) " "
                                  desc)))
                         outputs))))
             (setq last-end end)))
         outputs)))))

(defun nix3-help--build-command-alist ()
  (let (alist)
    (cl-labels
        ((add-commands (subcommands &optional description)
           (if-let (subalist (apply #'nix3-help-parse 'commands subcommands))
               (dolist (x subalist)
                 (add-commands (append subcommands (list (car x)))
                               (cdr x)))
             (push (cons (string-join (cons "nix" subcommands) " ")
                         description)
                   alist))))
      (add-commands nil)
      (setq nix3-help-command-alist (nreverse alist)))))

(defun nix3-help--read-command (prompt)
  (unless nix3-help-command-alist
    (nix3-help--build-command-alist))
  (cl-labels
      ((annotator (command)
         (concat " " (cdr (assoc command nix3-help-command-alist))))
       (completions (string pred action)
         (if (eq action 'metadata)
             (cons 'metadata
                   (list (cons 'category 'category)
                         (cons 'annotation-function #'annotator)))
           (complete-with-action action nix3-help-command-alist string pred))))
    (completing-read prompt #'completions)))

(provide 'nix3-help)
;;; nix3-help.el ends here
